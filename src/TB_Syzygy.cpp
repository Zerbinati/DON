/*
Copyright (c) 2013 Ronald de Man
This file may be redistributed and/or modified without restrictions.

tbprobe.cpp contains the Stockfish-specific routines of the
tablebase probing code. It should be relatively easy to adapt
this code to other chess engines.
*/

// The probing code currently expects a little-endian architecture (e.g. x86).

// Define DECOMP64 when compiling for a 64-bit platform.
// 32-bit is only supported for 5-piece tables, because tables are mmap()ed
// into memory.

#include "TB_Syzygy.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>

#if defined(_WIN32) || defined(__CYGWIN__) || defined(__MINGW32__) || defined(__MINGW64__) || defined(__BORLANDC__)

#   include <stdlib.h>

#   ifndef  NOMINMAX
#       define NOMINMAX // disable macros min() and max()
#   endif
//#   ifndef  WIN32_LEAN_AND_MEAN
//#       define WIN32_LEAN_AND_MEAN
//#   endif

#   include <windows.h>

//#   undef WIN32_LEAN_AND_MEAN
#   undef NOMINMAX

#   define SEP_CHAR     ';'
#   define FD           HANDLE
#   define FD_ERR       INVALID_HANDLE_VALUE

#   define LOCK_T       HANDLE
#   define LOCK_INIT(x) do { x = CreateMutex(NULL, FALSE, NULL); } while (0)
#   define LOCK(x)      WaitForSingleObject(x, INFINITE)
#   define UNLOCK(x)    ReleaseMutex(x)

#else    // Linux - Unix

#   include <unistd.h>
#   include <sys/mman.h>

#   include <pthread.h>

#   define SEP_CHAR     ':'
#   define FD           int
#   define FD_ERR       -1

#   define LOCK_T       pthread_mutex_t
#   define LOCK_INIT(x) pthread_mutex_init (&(x), NULL)
#   define LOCK(x)      pthread_mutex_lock (&(x))
#   define UNLOCK(x)    pthread_mutex_unlock (&(x))

#endif

#ifdef _64BIT
#   define DECOMP64
#endif

#include "BitBoard.h"
#include "BitCount.h"
#include "Zobrist.h"
#include "Position.h"
#include "MoveGenerator.h"
#include "Searcher.h"

using namespace MoveGenerator;

// CORE
namespace {

    // CORE contains engine-independent routines of the tablebase probing code.
    // This should not need to much adaptation to add tablebase probing to
    // a particular engine, provided the engine is written in C or C++.

#define WDLSUFFIX       ".rtbw"
#define DTZSUFFIX       ".rtbz"
#define WDLDIR          "RTBWDIR"
#define DTZDIR          "RTBZDIR"
#define TBPIECES        6

#define WDL_MAGIC       0x5D23E871
#define DTZ_MAGIC       0xA50C66D7

#define TBHASHBITS      10

    typedef unsigned long long  uint64;
    typedef   signed int         int32;
    typedef unsigned int        uint32;
    typedef   signed char        byte;
    typedef unsigned char       ubyte;
    typedef unsigned short      ushort;

    struct TBHashEntry;

#ifdef DECOMP64

    typedef uint64 base_t;

#else

    typedef uint32 base_t;

#endif

    typedef struct PairsData
    {
        char    *indextable;
        ushort  *sizetable;
        ubyte   *data;
        ushort  *offset;
        ubyte   *symlen;
        ubyte   *sympat;
        int32   blocksize;
        int32   idxbits;
        int32   min_len;
        base_t  base[1]; // C++ complains about base[]...
    } PairsData;

    struct TBEntry
    {
        char    *data;
        uint64  key;
        uint64  mapping;
        ubyte   ready;
        ubyte   num;
        ubyte   symmetric;
        ubyte   has_pawns;

    }
#if defined(_MSC_VER) || defined(__INTEL_COMPILER)
    ;
#else
    __attribute__ ((__may_alias__));
#endif
    typedef struct TBEntry TBEntry;

    typedef struct TBEntry_piece
    {
        char    *data;
        uint64  key;
        uint64  mapping;
        ubyte   ready;
        ubyte   num;
        ubyte   symmetric;
        ubyte   has_pawns;
        ubyte   enc_type;
        PairsData *precomp[2];
        int32   factor[2][TBPIECES];
        ubyte   pieces[2][TBPIECES];
        ubyte   norm  [2][TBPIECES];

    } TBEntry_piece;

    typedef struct TBEntry_pawn
    {
        char    *data;
        uint64  key;
        uint64  mapping;
        ubyte   ready;
        ubyte   num;
        ubyte   symmetric;
        ubyte   has_pawns;
        ubyte   pawns[2];

        struct
        {
            PairsData   *precomp[2];
            int32   factor[2][TBPIECES];
            ubyte   pieces[2][TBPIECES];
            ubyte   norm  [2][TBPIECES];

        } file[4];
    } TBEntry_pawn;

    typedef struct DTZEntry_piece
    {
        char    *data;
        uint64  key;
        uint64  mapping;
        ubyte   ready;
        ubyte   num;
        ubyte   symmetric;
        ubyte   has_pawns;
        ubyte   enc_type;
        PairsData   *precomp;
        int32   factor[TBPIECES];
        ubyte   pieces[TBPIECES];
        ubyte   norm  [TBPIECES];
        ubyte   flags; // accurate, mapped, side
        ushort  map_idx[4];
        ubyte   *map;
    } DTZEntry_piece;

    typedef struct DTZEntry_pawn
    {
        char *data;
        uint64 key;
        uint64 mapping;
        ubyte ready;
        ubyte num;
        ubyte symmetric;
        ubyte has_pawns;
        ubyte pawns[2];

        struct
        {
            PairsData *precomp;
            int32     factor[TBPIECES];
            ubyte   pieces[TBPIECES];
            ubyte   norm[TBPIECES];

        } file[4];

        ubyte   flags[4];
        ushort  map_idx[4][4];
        ubyte   *map;
    } DTZEntry_pawn;

    typedef struct TBHashEntry
    {
        uint64  key;
        TBEntry *ptr;
    } TBHashEntry;

    typedef struct DTZTableEntry
    {
        uint64 key1;
        uint64 key2;
        TBEntry *entry;
    } DTZTableEntry;

    // -----------------------------


#define TBMAX_PIECE 254
#define TBMAX_PAWN  256
#define HSHMAX      5

    // for variants where kings can connect and/or captured
    // #define CONNECTED_KINGS

#define Swap(a,b) { int tmp=a; a=b; b=tmp; }

#define TB_PAWN     0 //1
#define TB_KNIGHT   1 //2
#define TB_BISHOP   2 //3
#define TB_ROOK     3 //4
#define TB_QUEEN    4 //5
#define TB_KING     5 //6

#define TB_TOTAL    5


#define TB_WPAWN (TB_PAWN)
#define TB_BPAWN (TB_PAWN|8)

    LOCK_T TB_mutex;

    bool  initialized = false;
    int32 num_paths = 0;
    char *path_string = NULL;
    char **paths = NULL;

    int32 TBnum_piece, TBnum_pawn;
    TBEntry_piece TB_piece[TBMAX_PIECE];
    TBEntry_pawn TB_pawn[TBMAX_PAWN];

    TBHashEntry TB_hash[1 << TBHASHBITS][HSHMAX];

#define DTZ_ENTRIES 64

    DTZTableEntry DTZ_table[DTZ_ENTRIES];

    void init_indices (void);
    uint64 calc_key_from_pcs (int32 *pcs, int32 mirror);
    void free_wdl_entry (TBEntry *entry);
    void free_dtz_entry (TBEntry *entry);

    FD open_tb (const char *str, const char *suffix)
    {
        int32 i;
        FD fd;
        char file[256];

        for (i = 0; i < num_paths; ++i)
        {
            strcpy (file, paths[i]);
            strcat (file, "/");
            strcat (file, str);
            strcat (file, suffix);
#ifndef _WIN32
            fd = open (file, O_RDONLY);
#else

            fd = CreateFile (file, GENERIC_READ, FILE_SHARE_READ, NULL,
                OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
#endif
            if (fd != FD_ERR) return fd;
        }
        return FD_ERR;
    }

    void close_tb (FD fd)
    {
#ifndef _WIN32
        close (fd);
#else
        CloseHandle (fd);
#endif
    }

    char *map_file (const char *name, const char *suffix, uint64 *mapping)
    {
        FD fd = open_tb (name, suffix);
        if (fd == FD_ERR) return NULL;

#ifndef _WIN32
        
        stat statbuf;
        fstat (fd, &statbuf);
        *mapping = statbuf.st_size;
        char *data = (char *) mmap (NULL, statbuf.st_size, PROT_READ,
            MAP_SHARED, fd, 0);
        if (data == (char *) (-1))
        {
            printf ("Could not mmap() %s.\n", name);
            exit (1);
        }

#else

        DWORD size_low, size_high;
        size_low = GetFileSize (fd, &size_high);
        //  *size = ((uint64)size_high) << 32 | ((uint64)size_low);
        HANDLE map = CreateFileMapping (fd, NULL, PAGE_READONLY, size_high, size_low,
            NULL);
        if (map == NULL)
        {
            printf ("CreateFileMapping() failed.\n");
            exit (1);
        }
        *mapping = (uint64) map;
        char *data = (char *) MapViewOfFile (map, FILE_MAP_READ, 0, 0, 0);
        if (data == NULL)
        {
            printf ("MapViewOfFile() failed, name = %s%s, error = %lu.\n", name, suffix, GetLastError ());
            exit (1);
        }

#endif

        close_tb (fd);
        return data;
    }

#ifndef _WIN32

    void unmap_file (char *data, uint64 size)
    {
        if (!data) return;
        munmap (data, size);
    }

#else

    void unmap_file (char *data, uint64 mapping)
    {
        if (!data) return;
        UnmapViewOfFile (data);
        CloseHandle ((HANDLE) mapping);
    }

#endif

    void add_to_hash (TBEntry *ptr, uint64 key)
    {
        int32 hshidx = key >> (64 - TBHASHBITS);
        int32 i = 0;
        while (i < HSHMAX && TB_hash[hshidx][i].ptr)
        {
            ++i;
        }
        if (i == HSHMAX)
        {
            printf ("HSHMAX too low!\n");
            exit (1);
        }
        else
        {
            TB_hash[hshidx][i].key = key;
            TB_hash[hshidx][i].ptr = ptr;
        }
    }

    char pchr[] = { 'K', 'Q', 'R', 'B', 'N', 'P' };

    void init_tb (char *str)
    {
        FD fd;
        TBEntry *entry;
        int32 i, j, pcs[16];
        uint64 key, key2;
        int32 color;
        char *s;

        fd = open_tb (str, WDLSUFFIX);
        if (fd == FD_ERR) return;
        close_tb (fd);

        //for (i = 0; i < 16; ++i)
        //{
        //    pcs[i] = 0;
        //}
        memset (pcs, 0, sizeof (pcs));

        color = 0;
        for (s = str; *s; s++)
        {
            switch (*s)
            {
            case 'P':
                pcs[TB_PAWN|color]++;
                break;
            case 'N':
                pcs[TB_KNIGHT|color]++;
                break;
            case 'B':
                pcs[TB_BISHOP|color]++;
                break;
            case 'R':
                pcs[TB_ROOK|color]++;
                break;
            case 'Q':
                pcs[TB_QUEEN|color]++;
                break;
            case 'K':
                pcs[TB_KING|color]++;
                break;
            case 'v':
                color = 0x08;
                break;
            }
        }
        for (i = 0; i < 8; ++i)
        {
            if (pcs[i] != pcs[i+8])
            {
                break;
            }
        }
        key = calc_key_from_pcs (pcs, 0);
        key2 = calc_key_from_pcs (pcs, 1);
        if (pcs[TB_WPAWN] + pcs[TB_BPAWN] == 0)
        {
            if (TBnum_piece == TBMAX_PIECE)
            {
                printf ("TBMAX_PIECE limit too low!\n");
                exit (1);
            }
            entry = (TBEntry *) &TB_piece[TBnum_piece++];
        }
        else
        {
            if (TBnum_pawn == TBMAX_PAWN)
            {
                printf ("TBMAX_PAWN limit too low!\n");
                exit (1);
            }
            entry = (TBEntry *) &TB_pawn[TBnum_pawn++];
        }
        entry->key = key;
        entry->ready = 0;
        entry->num = 0;
        for (i = 0; i < 16; ++i)
        {
            entry->num += pcs[i];
        }

        entry->symmetric = (key == key2);
        entry->has_pawns = (pcs[TB_WPAWN] + pcs[TB_BPAWN] > 0);
        if (entry->num > Tablebases::TBLargest)
        {
            Tablebases::TBLargest = entry->num;
        }

        if (entry->has_pawns)
        {
            TBEntry_pawn *ptr = (TBEntry_pawn *)entry;
            ptr->pawns[0] = pcs[TB_WPAWN];
            ptr->pawns[1] = pcs[TB_BPAWN];
            if (    pcs[TB_BPAWN] > 0
                && (pcs[TB_WPAWN] == 0 || pcs[TB_BPAWN] < pcs[TB_WPAWN]))
            {
                ptr->pawns[0] = pcs[TB_BPAWN];
                ptr->pawns[1] = pcs[TB_WPAWN];
            }
        }
        else
        {
            TBEntry_piece *ptr = (TBEntry_piece *)entry;
            for (i = 0, j = 0; i < 16; ++i)
            {
                if (pcs[i] == 1) ++j;
            }

            if      (j >= 3)
            {
                ptr->enc_type = 0;
            }
            else if (j == 2)
            {
                ptr->enc_type = 2;
            }
            else
            { /* only for suicide */
                j = 16;
                for (i = 0; i < 16; ++i)
                {
                    if (pcs[i] < j && pcs[i] > 1) j = pcs[i];
                    ptr->enc_type = 1 + j;
                }
            }
        }

        add_to_hash (entry, key);
        if (key2 != key) add_to_hash (entry, key2);
    }


    const char offdiag[] = 
    {
        0, -1, -1, -1, -1, -1, -1, -1,
        +1, 0, -1, -1, -1, -1, -1, -1,
        +1, +1, 0, -1, -1, -1, -1, -1,
        +1, +1, +1, 0, -1, -1, -1, -1,
        +1, +1, +1, +1, 0, -1, -1, -1,
        +1, +1, +1, +1, +1, 0, -1, -1,
        +1, +1, +1, +1, +1, +1, 0, -1,
        +1, +1, +1, +1, +1, +1, +1, 0
    };

    const ubyte triangle[] = 
    {
        6, 0, 1, 2, 2, 1, 0, 6,
        0, 7, 3, 4, 4, 3, 7, 0,
        1, 3, 8, 5, 5, 8, 3, 1,
        2, 4, 5, 9, 9, 5, 4, 2,
        2, 4, 5, 9, 9, 5, 4, 2,
        1, 3, 8, 5, 5, 8, 3, 1,
        0, 7, 3, 4, 4, 3, 7, 0,
        6, 0, 1, 2, 2, 1, 0, 6
    };

    const ubyte invtriangle[] = 
    {
        1, 2, 3, 10, 11, 19, 0, 9, 18, 27
    };

    const ubyte invdiag[] = 
    {
        0, 9, 18, 27, 36, 45, 54, 63,
        7, 14, 21, 28, 35, 42, 49, 56
    };

    const ubyte flipdiag[] =
    {
        0,  8, 16, 24, 32, 40, 48, 56,
        1,  9, 17, 25, 33, 41, 49, 57,
        2, 10, 18, 26, 34, 42, 50, 58,
        3, 11, 19, 27, 35, 43, 51, 59,
        4, 12, 20, 28, 36, 44, 52, 60,
        5, 13, 21, 29, 37, 45, 53, 61,
        6, 14, 22, 30, 38, 46, 54, 62,
        7, 15, 23, 31, 39, 47, 55, 63
    };

    const ubyte lower[] =
    {
        28, 0,  1,  2,  3,  4,  5,  6,
        0, 29,  7,  8,  9, 10, 11, 12,
        1,  7, 30, 13, 14, 15, 16, 17,
        2,  8, 13, 31, 18, 19, 20, 21,
        3,  9, 14, 18, 32, 22, 23, 24,
        4, 10, 15, 19, 22, 33, 25, 26,
        5, 11, 16, 20, 23, 25, 34, 27,
        6, 12, 17, 21, 24, 26, 27, 35
    };

    const ubyte diag[] =
    {
        0, 0, 0, 0, 0, 0, 0, 8,
        0, 1, 0, 0, 0, 0, 9, 0,
        0, 0, 2, 0, 0, 10, 0, 0,
        0, 0, 0, 3, 11, 0, 0, 0,
        0, 0, 0, 12, 4, 0, 0, 0,
        0, 0, 13, 0, 0, 5, 0, 0,
        0, 14, 0, 0, 0, 0, 6, 0,
        15, 0, 0, 0, 0, 0, 0, 7
    };

    const ubyte flap[] =
    {
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 6, 12, 18, 18, 12, 6, 0,
        1, 7, 13, 19, 19, 13, 7, 1,
        2, 8, 14, 20, 20, 14, 8, 2,
        3, 9, 15, 21, 21, 15, 9, 3,
        4, 10, 16, 22, 22, 16, 10, 4,
        5, 11, 17, 23, 23, 17, 11, 5,
        0, 0, 0, 0, 0, 0, 0, 0
    };

    const ubyte ptwist[] =
    {
        0, 0, 0, 0, 0, 0, 0, 0,
        47, 35, 23, 11, 10, 22, 34, 46,
        45, 33, 21, 9, 8, 20, 32, 44,
        43, 31, 19, 7, 6, 18, 30, 42,
        41, 29, 17, 5, 4, 16, 28, 40,
        39, 27, 15, 3, 2, 14, 26, 38,
        37, 25, 13, 1, 0, 12, 24, 36,
        0, 0, 0, 0, 0, 0, 0, 0
    };

    const ubyte invflap[] =
    {
        8, 16, 24, 32, 40, 48,
        9, 17, 25, 33, 41, 49,
        10, 18, 26, 34, 42, 50,
        11, 19, 27, 35, 43, 51
    };

    const ubyte invptwist[] =
    {
        52, 51, 44, 43, 36, 35, 28, 27, 20, 19, 12, 11,
        53, 50, 45, 42, 37, 34, 29, 26, 21, 18, 13, 10,
        54, 49, 46, 41, 38, 33, 30, 25, 22, 17, 14, 9,
        55, 48, 47, 40, 39, 32, 31, 24, 23, 16, 15, 8
    };

    const ubyte file_to_file[] =
    {
        0, 1, 2, 3, 3, 2, 1, 0
    };

#ifndef CONNECTED_KINGS
    const short KK_idx[10][64] =
    {
        { -1, -1, -1, 0, 1, 2, 3, 4,
        -1, -1, -1, 5, 6, 7, 8, 9,
        10, 11, 12, 13, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 23, 24, 25,
        26, 27, 28, 29, 30, 31, 32, 33,
        34, 35, 36, 37, 38, 39, 40, 41,
        42, 43, 44, 45, 46, 47, 48, 49,
        50, 51, 52, 53, 54, 55, 56, 57 },
        { 58, -1, -1, -1, 59, 60, 61, 62,
        63, -1, -1, -1, 64, 65, 66, 67,
        68, 69, 70, 71, 72, 73, 74, 75,
        76, 77, 78, 79, 80, 81, 82, 83,
        84, 85, 86, 87, 88, 89, 90, 91,
        92, 93, 94, 95, 96, 97, 98, 99,
        100, 101, 102, 103, 104, 105, 106, 107,
        108, 109, 110, 111, 112, 113, 114, 115 },
        { 116, 117, -1, -1, -1, 118, 119, 120,
        121, 122, -1, -1, -1, 123, 124, 125,
        126, 127, 128, 129, 130, 131, 132, 133,
        134, 135, 136, 137, 138, 139, 140, 141,
        142, 143, 144, 145, 146, 147, 148, 149,
        150, 151, 152, 153, 154, 155, 156, 157,
        158, 159, 160, 161, 162, 163, 164, 165,
        166, 167, 168, 169, 170, 171, 172, 173 },
        { 174, -1, -1, -1, 175, 176, 177, 178,
        179, -1, -1, -1, 180, 181, 182, 183,
        184, -1, -1, -1, 185, 186, 187, 188,
        189, 190, 191, 192, 193, 194, 195, 196,
        197, 198, 199, 200, 201, 202, 203, 204,
        205, 206, 207, 208, 209, 210, 211, 212,
        213, 214, 215, 216, 217, 218, 219, 220,
        221, 222, 223, 224, 225, 226, 227, 228 },
        { 229, 230, -1, -1, -1, 231, 232, 233,
        234, 235, -1, -1, -1, 236, 237, 238,
        239, 240, -1, -1, -1, 241, 242, 243,
        244, 245, 246, 247, 248, 249, 250, 251,
        252, 253, 254, 255, 256, 257, 258, 259,
        260, 261, 262, 263, 264, 265, 266, 267,
        268, 269, 270, 271, 272, 273, 274, 275,
        276, 277, 278, 279, 280, 281, 282, 283 },
        { 284, 285, 286, 287, 288, 289, 290, 291,
        292, 293, -1, -1, -1, 294, 295, 296,
        297, 298, -1, -1, -1, 299, 300, 301,
        302, 303, -1, -1, -1, 304, 305, 306,
        307, 308, 309, 310, 311, 312, 313, 314,
        315, 316, 317, 318, 319, 320, 321, 322,
        323, 324, 325, 326, 327, 328, 329, 330,
        331, 332, 333, 334, 335, 336, 337, 338 },
        { -1, -1, 339, 340, 341, 342, 343, 344,
        -1, -1, 345, 346, 347, 348, 349, 350,
        -1, -1, 441, 351, 352, 353, 354, 355,
        -1, -1, -1, 442, 356, 357, 358, 359,
        -1, -1, -1, -1, 443, 360, 361, 362,
        -1, -1, -1, -1, -1, 444, 363, 364,
        -1, -1, -1, -1, -1, -1, 445, 365,
        -1, -1, -1, -1, -1, -1, -1, 446 },
        { -1, -1, -1, 366, 367, 368, 369, 370,
        -1, -1, -1, 371, 372, 373, 374, 375,
        -1, -1, -1, 376, 377, 378, 379, 380,
        -1, -1, -1, 447, 381, 382, 383, 384,
        -1, -1, -1, -1, 448, 385, 386, 387,
        -1, -1, -1, -1, -1, 449, 388, 389,
        -1, -1, -1, -1, -1, -1, 450, 390,
        -1, -1, -1, -1, -1, -1, -1, 451 },
        { 452, 391, 392, 393, 394, 395, 396, 397,
        -1, -1, -1, -1, 398, 399, 400, 401,
        -1, -1, -1, -1, 402, 403, 404, 405,
        -1, -1, -1, -1, 406, 407, 408, 409,
        -1, -1, -1, -1, 453, 410, 411, 412,
        -1, -1, -1, -1, -1, 454, 413, 414,
        -1, -1, -1, -1, -1, -1, 455, 415,
        -1, -1, -1, -1, -1, -1, -1, 456 },
        { 457, 416, 417, 418, 419, 420, 421, 422,
        -1, 458, 423, 424, 425, 426, 427, 428,
        -1, -1, -1, -1, -1, 429, 430, 431,
        -1, -1, -1, -1, -1, 432, 433, 434,
        -1, -1, -1, -1, -1, 435, 436, 437,
        -1, -1, -1, -1, -1, 459, 438, 439,
        -1, -1, -1, -1, -1, -1, 460, 440,
        -1, -1, -1, -1, -1, -1, -1, 461 }
    };

#else

    const short PP_idx[10][64] =
    {
        { 0, -1, 1, 2, 3, 4, 5, 6,
        7, 8, 9, 10, 11, 12, 13, 14,
        15, 16, 17, 18, 19, 20, 21, 22,
        23, 24, 25, 26, 27, 28, 29, 30,
        31, 32, 33, 34, 35, 36, 37, 38,
        39, 40, 41, 42, 43, 44, 45, 46,
        -1, 47, 48, 49, 50, 51, 52, 53,
        54, 55, 56, 57, 58, 59, 60, 61 },
        { 62, -1, -1, 63, 64, 65, -1, 66,
        -1, 67, 68, 69, 70, 71, 72, -1,
        73, 74, 75, 76, 77, 78, 79, 80,
        81, 82, 83, 84, 85, 86, 87, 88,
        89, 90, 91, 92, 93, 94, 95, 96,
        -1, 97, 98, 99, 100, 101, 102, 103,
        -1, 104, 105, 106, 107, 108, 109, -1,
        110, -1, 111, 112, 113, 114, -1, 115 },
        { 116, -1, -1, -1, 117, -1, -1, 118,
        -1, 119, 120, 121, 122, 123, 124, -1,
        -1, 125, 126, 127, 128, 129, 130, -1,
        131, 132, 133, 134, 135, 136, 137, 138,
        -1, 139, 140, 141, 142, 143, 144, 145,
        -1, 146, 147, 148, 149, 150, 151, -1,
        -1, 152, 153, 154, 155, 156, 157, -1,
        158, -1, -1, 159, 160, -1, -1, 161 },
        { 162, -1, -1, -1, -1, -1, -1, 163,
        -1, 164, -1, 165, 166, 167, 168, -1,
        -1, 169, 170, 171, 172, 173, 174, -1,
        -1, 175, 176, 177, 178, 179, 180, -1,
        -1, 181, 182, 183, 184, 185, 186, -1,
        -1, -1, 187, 188, 189, 190, 191, -1,
        -1, 192, 193, 194, 195, 196, 197, -1,
        198, -1, -1, -1, -1, -1, -1, 199 },
        { 200, -1, -1, -1, -1, -1, -1, 201,
        -1, 202, -1, -1, 203, -1, 204, -1,
        -1, -1, 205, 206, 207, 208, -1, -1,
        -1, 209, 210, 211, 212, 213, 214, -1,
        -1, -1, 215, 216, 217, 218, 219, -1,
        -1, -1, 220, 221, 222, 223, -1, -1,
        -1, 224, -1, 225, 226, -1, 227, -1,
        228, -1, -1, -1, -1, -1, -1, 229 },
        { 230, -1, -1, -1, -1, -1, -1, 231,
        -1, 232, -1, -1, -1, -1, 233, -1,
        -1, -1, 234, -1, 235, 236, -1, -1,
        -1, -1, 237, 238, 239, 240, -1, -1,
        -1, -1, -1, 241, 242, 243, -1, -1,
        -1, -1, 244, 245, 246, 247, -1, -1,
        -1, 248, -1, -1, -1, -1, 249, -1,
        250, -1, -1, -1, -1, -1, -1, 251 },
        { -1, -1, -1, -1, -1, -1, -1, 259,
        -1, 252, -1, -1, -1, -1, 260, -1,
        -1, -1, 253, -1, -1, 261, -1, -1,
        -1, -1, -1, 254, 262, -1, -1, -1,
        -1, -1, -1, -1, 255, -1, -1, -1,
        -1, -1, -1, -1, -1, 256, -1, -1,
        -1, -1, -1, -1, -1, -1, 257, -1,
        -1, -1, -1, -1, -1, -1, -1, 258 },
        { -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, 268, -1,
        -1, -1, 263, -1, -1, 269, -1, -1,
        -1, -1, -1, 264, 270, -1, -1, -1,
        -1, -1, -1, -1, 265, -1, -1, -1,
        -1, -1, -1, -1, -1, 266, -1, -1,
        -1, -1, -1, -1, -1, -1, 267, -1,
        -1, -1, -1, -1, -1, -1, -1, -1 },
        { -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, 274, -1, -1,
        -1, -1, -1, 271, 275, -1, -1, -1,
        -1, -1, -1, -1, 272, -1, -1, -1,
        -1, -1, -1, -1, -1, 273, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1 },
        { -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, 277, -1, -1, -1,
        -1, -1, -1, -1, 276, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1 }
    };

    const ubyte test45[] =
    {
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        1, 1, 1, 0, 0, 0, 0, 0,
        1, 1, 0, 0, 0, 0, 0, 0,
        1, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
    };

    const ubyte mtwist[] =
    {
        15, 63, 55, 47, 40, 48, 56, 12,
        62, 11, 39, 31, 24, 32, 8, 57,
        54, 38, 7, 23, 16, 4, 33, 49,
        46, 30, 22, 3, 0, 17, 25, 41,
        45, 29, 21, 2, 1, 18, 26, 42,
        53, 37, 6, 20, 19, 5, 34, 50,
        61, 10, 36, 28, 27, 35, 9, 58,
        14, 60, 52, 44, 43, 51, 59, 13
    };
#endif

    int32 binomial[5][64];
    int32 pawnidx[5][24];
    int32 pfactor[5][4];

#ifdef CONNECTED_KINGS

    int32 multidx[5][10];
    int32 mfactor[5];

#endif

    void init_indices (void)
    {
        int32 i, j, k;

        // binomial[k-1][n] = Bin(n, k)
        for (i = 0; i < 5; ++i)
        {
            for (j = 0; j < 64; ++j)
            {
                int32 f = j;
                int32 l = 1;
                for (k = 1; k <= i; ++k)
                {
                    f *= (j - k);
                    l *= (k + 1);
                }
                binomial[i][j] = f / l;
            }
        }
        for (i = 0; i < 5; ++i)
        {
            int32 s = 0;
            for (j = 0; j < 6; ++j)
            {
                pawnidx[i][j] = s;
                s += (i == 0) ? 1 : binomial[i - 1][ptwist[invflap[j]]];
            }
            pfactor[i][0] = s;
            s = 0;
            for (; j < 12; ++j)
            {
                pawnidx[i][j] = s;
                s += (i == 0) ? 1 : binomial[i - 1][ptwist[invflap[j]]];
            }
            pfactor[i][1] = s;
            s = 0;
            for (; j < 18; ++j)
            {
                pawnidx[i][j] = s;
                s += (i == 0) ? 1 : binomial[i - 1][ptwist[invflap[j]]];
            }
            pfactor[i][2] = s;
            s = 0;
            for (; j < 24; ++j)
            {
                pawnidx[i][j] = s;
                s += (i == 0) ? 1 : binomial[i - 1][ptwist[invflap[j]]];
            }
            pfactor[i][3] = s;
        }

#ifdef CONNECTED_KINGS

        for (i = 0; i < 5; ++i)
        {
            int32 s = 0;
            for (j = 0; j < 10; ++j)
            {
                multidx[i][j] = s;
                s += (i == 0) ? 1 : binomial[i - 1][mtwist[invtriangle[j]]];
            }
            mfactor[i] = s;
        }

#endif

    }

#ifndef CONNECTED_KINGS

    uint64 encode_piece (TBEntry_piece *ptr, ubyte *norm, int32 *pos, int32 *factor)
    {
        uint64 idx;
        int32 i, j, k, m, l, p;
        int32 n = ptr->num;

        if (pos[0] & 0x04)
        {
            for (i = 0; i < n; ++i)
                pos[i] ^= 0x07;
        }
        if (pos[0] & 0x20)
        {
            for (i = 0; i < n; ++i)
                pos[i] ^= 0x38;
        }

        for (i = 0; i < n; ++i)
        {
            if (offdiag[pos[i]]) break;
        }
        if (i < (ptr->enc_type == 0 ? 3 : 2) && offdiag[pos[i]] > 0)
        {
            for (i = 0; i < n; ++i)
                pos[i] = flipdiag[pos[i]];
        }
        switch (ptr->enc_type)
        {

        case 0: /* 111 */
            i = (pos[1] > pos[0]);
            j = (pos[2] > pos[0]) + (pos[2] > pos[1]);

            if (offdiag[pos[0]])
                idx = triangle[pos[0]] * 63*62 + (pos[1] - i) * 62 + (pos[2] - j);
            else if (offdiag[pos[1]])
                idx = 6*63*62 + diag[pos[0]] * 28*62 + lower[pos[1]] * 62 + pos[2] - j;
            else if (offdiag[pos[2]])
                idx = 6*63*62 + 4*28*62 + (diag[pos[0]]) * 7*28 + (diag[pos[1]] - i) * 28 + lower[pos[2]];
            else
                idx = 6*63*62 + 4*28*62 + 4*7*28 + (diag[pos[0]] * 7*6) + (diag[pos[1]] - i) * 6 + (diag[pos[2]] - j);
            i = 3;
            break;

        case 1: /* K3 */
            j = (pos[2] > pos[0]) + (pos[2] > pos[1]);

            idx = KK_idx[triangle[pos[0]]][pos[1]];
            if (idx < 441)
                idx = idx + 441 * (pos[2] - j);
            else
            {
                idx = 441*62 + (idx - 441) + 21 * lower[pos[2]];
                if (!offdiag[pos[2]])
                    idx -= j * 21;
            }
            i = 3;
            break;

        default: /* K2 */
            idx = KK_idx[triangle[pos[0]]][pos[1]];
            i = 2;
            break;
        }
        idx *= factor[0];

        for (; i < n;)
        {
            int32 t = norm[i];
            for (j = i; j < i + t; ++j)
            {
                for (k = j + 1; k < i + t; ++k)
                {
                    if (pos[j] > pos[k]) Swap (pos[j], pos[k]);
                }
            }
            int32 s = 0;
            for (m = i; m < i + t; ++m)
            {
                p = pos[m];
                for (l = 0, j = 0; l < i; ++l)
                {
                    j += (p > pos[l]);
                }
                s += binomial[m - i][p - j];
            }
            idx += ((uint64) s) * ((uint64) factor[i]);
            i += t;
        }

        return idx;
    }
#else
    uint64 encode_piece (TBEntry_piece *ptr, ubyte *norm, int32 *pos, int32 *factor)
    {
        uint64 idx;
        int32 i, j, k, m, l, p;
        int32 n = ptr->num;

        if (ptr->enc_type < 3)
        {
            if (pos[0] & 0x04)
            {
                for (i = 0; i < n; ++i)
                {
                    pos[i] ^= 0x07;
                }
            }
            if (pos[0] & 0x20)
            {
                for (i = 0; i < n; ++i)
                {
                    pos[i] ^= 0x38;
                }
            }

            for (i = 0; i < n; ++i)
            {
                if (offdiag[pos[i]]) break;
            }
            if (i < (ptr->enc_type == 0 ? 3 : 2) && offdiag[pos[i]] > 0)
            {
                for (i = 0; i < n; ++i)
                {
                    pos[i] = flipdiag[pos[i]];
                }
            }

            switch (ptr->enc_type)
            {

            case 0: /* 111 */
                i = (pos[1] > pos[0]);
                j = (pos[2] > pos[0]) + (pos[2] > pos[1]);

                if (offdiag[pos[0]])
                {
                    idx = triangle[pos[0]] * 63*62 + (pos[1] - i) * 62 + (pos[2] - j);
                }
                else if (offdiag[pos[1]])
                {
                    idx = 6*63*62 + diag[pos[0]] * 28*62 + lower[pos[1]] * 62 + pos[2] - j;
                }
                else if (offdiag[pos[2]])
                {
                    idx = 6*63*62 + 4*28*62 + (diag[pos[0]]) * 7*28 + (diag[pos[1]] - i) * 28 + lower[pos[2]];
                }
                else
                {
                    idx = 6*63*62 + 4*28*62 + 4*7*28 + (diag[pos[0]] * 7*6) + (diag[pos[1]] - i) * 6 + (diag[pos[2]] - j);
                }
                i = 3;
                break;

            case 2: /* 11 */
                i = (pos[1] > pos[0]);

                if (offdiag[pos[0]])
                {
                    idx = triangle[pos[0]] * 63 + (pos[1] - i);
                }
                else if (offdiag[pos[1]])
                {
                    idx = 6*63 + diag[pos[0]] * 28 + lower[pos[1]];
                }
                else
                {
                    idx = 6*63 + 4*28 + (diag[pos[0]]) * 7 + (diag[pos[1]] - i);
                }
                i = 2;
                break;

            }
        }
        else if (ptr->enc_type == 3)
        { /* 2, e.g. KKvK */
            if (triangle[pos[0]] > triangle[pos[1]])
            {
                Swap (pos[0], pos[1]);
            }
            if (pos[0] & 0x04)
            {
                for (i = 0; i < n; ++i)
                {
                    pos[i] ^= 0x07;
                }
            }
            if (pos[0] & 0x20)
            {
                for (i = 0; i < n; ++i)
                {
                    pos[i] ^= 0x38;
                }
            }
            if (offdiag[pos[0]] > 0 || (offdiag[pos[0]] == 0 && offdiag[pos[1]] > 0))
            {
                for (i = 0; i < n; ++i)
                {
                    pos[i] = flipdiag[pos[i]];
                }
            }
            if (test45[pos[1]] && triangle[pos[0]] == triangle[pos[1]])
            {
                Swap (pos[0], pos[1]);
                for (i = 0; i < n; ++i)
                {
                    pos[i] = flipdiag[pos[i] ^ 0x38];
                }
            }
            idx = PP_idx[triangle[pos[0]]][pos[1]];
            i = 2;
        }
        else
        { /* 3 and higher, e.g. KKKvK and KKKKvK */
            for (i = 1; i < norm[0]; ++i)
            {
                if (triangle[pos[0]] > triangle[pos[i]])
                {
                    Swap (pos[0], pos[i]);
                }
            }
            if (pos[0] & 0x04)
            {
                for (i = 0; i < n; ++i)
                {         
                    pos[i] ^= 0x07;
                }
            }
            if (pos[0] & 0x20)
            {
                for (i = 0; i < n; ++i)
                {
                    pos[i] ^= 0x38;
                }
            }
            if (offdiag[pos[0]] > 0)
            {
                for (i = 0; i < n; ++i)
                {
                    pos[i] = flipdiag[pos[i]];
                }
            }
            for (i = 1; i < norm[0]; ++i)
            {
                for (j = i + 1; j < norm[0]; ++j)
                {
                    if (mtwist[pos[i]] > mtwist[pos[j]])
                    {
                        Swap (pos[i], pos[j]);
                    }
                }
            }
            idx = multidx[norm[0] - 1][triangle[pos[0]]];
            for (i = 1; i < norm[0]; ++i)
            {
                idx += binomial[i - 1][mtwist[pos[i]]];
            }
        }
        idx *= factor[0];

        for (; i < n;)
        {
            int32 t = norm[i];
            for (j = i; j < i + t; ++j)
            {
                for (k = j + 1; k < i + t; ++k)
                {
                    if (pos[j] > pos[k])
                    {
                        Swap (pos[j], pos[k]);
                    }
                }
            }
            int32 s = 0;
            for (m = i; m < i + t; ++m)
            {
                p = pos[m];
                for (l = 0, j = 0; l < i; ++l)
                {
                    j += (p > pos[l]);
                }
                s += binomial[m - i][p - j];
            }
            idx += ((uint64) s) * ((uint64) factor[i]);
            i += t;
        }

        return idx;
    }
#endif

    // determine file of leftmost pawn and sort pawns
    int32 pawn_file (TBEntry_pawn *ptr, int32 *pos)
    {
        int32 i;

        for (i = 1; i < ptr->pawns[0]; ++i)
        {
            if (flap[pos[0]] > flap[pos[i]])
            {
                Swap (pos[0], pos[i]);
            }
        }
        return file_to_file[pos[0] & 0x07];
    }

    uint64 encode_pawn (TBEntry_pawn *ptr, ubyte *norm, int32 *pos, int32 *factor)
    {
        uint64 idx;
        int32 i, j, k, m, s, t;
        int32 n = ptr->num;

        if (pos[0] & 0x04)
        {
            for (i = 0; i < n; ++i)
            {
                pos[i] ^= 0x07;
            }
        }
        for (i = 1; i < ptr->pawns[0]; ++i)
        {
            for (j = i + 1; j < ptr->pawns[0]; ++j)
            {
                if (ptwist[pos[i]] < ptwist[pos[j]])
                {
                    Swap (pos[i], pos[j]);
                }
            }
        }
        t = ptr->pawns[0] - 1;
        idx = pawnidx[t][flap[pos[0]]];
        for (i = t; i > 0; --i)
        {
            idx += binomial[t - i][ptwist[pos[i]]];
        }
        idx *= factor[0];

        // remaining pawns
        i = ptr->pawns[0];
        t = i + ptr->pawns[1];
        if (t > i)
        {
            for (j = i; j < t; ++j)
            {
                for (k = j + 1; k < t; ++k)
                {
                    if (pos[j] > pos[k]) Swap (pos[j], pos[k]);
                }
            }
            s = 0;
            for (m = i; m < t; ++m)
            {
                int32 p = pos[m];
                for (k = 0, j = 0; k < i; ++k)
                {
                    j += (p > pos[k]);
                }
                s += binomial[m - i][p - j - 8];
            }
            idx += ((uint64) s) * ((uint64) factor[i]);
            i = t;
        }

        for (; i < n;)
        {
            t = norm[i];
            for (j = i; j < i + t; ++j)
            {
                for (k = j + 1; k < i + t; ++k)
                {
                    if (pos[j] > pos[k])
                    {
                        Swap (pos[j], pos[k]);
                    }
                }
            }

            s = 0;
            for (m = i; m < i + t; ++m)
            {
                int32 p = pos[m];
                for (k = 0, j = 0; k < i; ++k)
                {
                    j += (p > pos[k]);
                }
                s += binomial[m - i][p - j];
            }
            idx += uint64 (s) * uint64 (factor[i]);
            i += t;
        }

        return idx;
    }

    ubyte decompress_pairs (PairsData *d, uint64 index);

    // place k like pieces on n squares
    int32 subfactor (int32 k, int32 n)
    {
        int32 i, f, l;

        f = n;
        l = 1;
        for (i = 1; i < k; ++i)
        {
            f *= n - i;
            l *= i + 1;
        }

        return f / l;
    }

    uint64 calc_factors_piece (int32 *factor, int32 num, int32 order, ubyte *norm, ubyte enc_type)
    {
        int32 i, k, n;
        uint64 f;

        static int32 pivfac[] =
#ifndef CONNECTED_KINGS
        { 31332, 28056, 462 };
#else
        { 31332, 0, 518, 278 };
#endif

        n = 64 - norm[0];

        f = 1;
        for (i = norm[0], k = 0; i < num || k == order; ++k)
        {
            if (k == order)
            {
                factor[0] = f;

#ifndef CONNECTED_KINGS

                f *= pivfac[enc_type];

#else

                if (enc_type < 4)
                {
                    f *= pivfac[enc_type];
                }
                else
                {
                    f *= mfactor[enc_type - 2];
                }

#endif

            }
            else
            {
                factor[i] = f;
                f *= subfactor (norm[i], n);
                n -= norm[i];
                i += norm[i];
            }
        }

        return f;
    }

    uint64 calc_factors_pawn (int32 *factor, int32 num, int32 order, int32 order2, ubyte *norm, int32 file)
    {
        int32 i, k, n;
        uint64 f;

        i = norm[0];
        if (order2 < 0x0f) i += norm[i];
        n = 64 - i;

        f = 1;
        for (k = 0; i < num || k == order || k == order2; ++k)
        {
            if (k == order)
            {
                factor[0] = f;
                f *= pfactor[norm[0] - 1][file];
            }
            else if (k == order2)
            {
                factor[norm[0]] = f;
                f *= subfactor (norm[norm[0]], 48 - norm[0]);
            }
            else
            {
                factor[i] = f;
                f *= subfactor (norm[i], n);
                n -= norm[i];
                i += norm[i];
            }
        }

        return f;
    }

    void set_norm_piece (TBEntry_piece *ptr, ubyte *norm, ubyte *pieces)
    {
        int32 i, j;

        for (i = 0; i < ptr->num; ++i)
        {
            norm[i] = 0;
        }

        switch (ptr->enc_type)
        {
        case 0:
            norm[0] = 3;
            break;
        case 2:
            norm[0] = 2;
            break;
        default:
            norm[0] = ptr->enc_type - 1;
            break;
        }

        for (i = norm[0]; i < ptr->num; i += norm[i])
        {
            for (j = i; j < ptr->num && pieces[j] == pieces[i]; ++j)
            {
                ++norm[i];
            }
        }
    }

    void set_norm_pawn (TBEntry_pawn *ptr, ubyte *norm, ubyte *pieces)
    {
        int32 i;

        for (i = 0; i < ptr->num; ++i)
        {
            norm[i] = 0;
        }

        norm[0] = ptr->pawns[0];
        if (ptr->pawns[1]) norm[ptr->pawns[0]] = ptr->pawns[1];

        for (i = ptr->pawns[0] + ptr->pawns[1]; i < ptr->num; i += norm[i])
        {
            for (int32 j = i; j < ptr->num && pieces[j] == pieces[i]; ++j)
            {
                norm[i]++;
            }
        }
    }

    void setup_pieces_piece (TBEntry_piece *ptr, unsigned char *data, uint64 *tb_size)
    {
        int32 i;
        int32 order;

        for (i = 0; i < ptr->num; ++i)
        {
            ptr->pieces[0][i] = data[i + 1] & 0x0f;
        }
        order = data[0] & 0x0f;
        set_norm_piece (ptr, ptr->norm[0], ptr->pieces[0]);
        tb_size[0] = calc_factors_piece (ptr->factor[0], ptr->num, order, ptr->norm[0], ptr->enc_type);

        for (i = 0; i < ptr->num; ++i)
        {
            ptr->pieces[1][i] = data[i + 1] >> 4;
        }
        order = data[0] >> 4;
        set_norm_piece (ptr, ptr->norm[1], ptr->pieces[1]);
        tb_size[1] = calc_factors_piece (ptr->factor[1], ptr->num, order, ptr->norm[1], ptr->enc_type);
    }

    void setup_pieces_piece_dtz (DTZEntry_piece *ptr, unsigned char *data, uint64 *tb_size)
    {
        int32 i;
        int32 order;

        for (i = 0; i < ptr->num; ++i)
        {
            ptr->pieces[i] = data[i + 1] & 0x0f;
        }
        order = data[0] & 0x0f;
        set_norm_piece ((TBEntry_piece *)ptr, ptr->norm, ptr->pieces);
        tb_size[0] = calc_factors_piece (ptr->factor, ptr->num, order, ptr->norm, ptr->enc_type);
    }

    void setup_pieces_pawn (TBEntry_pawn *ptr, unsigned char *data, uint64 *tb_size, int32 f)
    {
        int32 i, j;
        int32   order
            , order2;

        j = 1 + (ptr->pawns[1] > 0);
        order = data[0] & 0x0f;
        order2 = ptr->pawns[1] ? (data[1] & 0x0f) : 0x0f;
        for (i = 0; i < ptr->num; ++i)
        {
            ptr->file[f].pieces[0][i] = data[i + j] & 0x0f;
        }
        set_norm_pawn (ptr, ptr->file[f].norm[0], ptr->file[f].pieces[0]);
        tb_size[0] = calc_factors_pawn (ptr->file[f].factor[0], ptr->num, order, order2, ptr->file[f].norm[0], f);

        order = data[0] >> 4;
        order2 = ptr->pawns[1] ? (data[1] >> 4) : 0x0f;
        for (i = 0; i < ptr->num; ++i)
        {
            ptr->file[f].pieces[1][i] = data[i + j] >> 4;
        }
        set_norm_pawn (ptr, ptr->file[f].norm[1], ptr->file[f].pieces[1]);
        tb_size[1] = calc_factors_pawn (ptr->file[f].factor[1], ptr->num, order, order2, ptr->file[f].norm[1], f);
    }

    void setup_pieces_pawn_dtz (DTZEntry_pawn *ptr, unsigned char *data, uint64 *tb_size, int32 f)
    {
        int32 i, j;
        int32   order
            , order2;

        j = 1 + (ptr->pawns[1] > 0);
        order = data[0] & 0x0f;
        order2 = ptr->pawns[1] ? (data[1] & 0x0f) : 0x0f;
        for (i = 0; i < ptr->num; ++i)
        {
            ptr->file[f].pieces[i] = data[i + j] & 0x0f;
        }
        set_norm_pawn ((TBEntry_pawn *)ptr, ptr->file[f].norm, ptr->file[f].pieces);
        tb_size[0] = calc_factors_pawn (ptr->file[f].factor, ptr->num, order, order2, ptr->file[f].norm, f);
    }

    void calc_symlen (PairsData *d, int32 s, char *tmp)
    {
        int32 s1, s2;

        int32 w = *(int32 *) (d->sympat + 3 * s);
        s2 = (w >> 12) & 0x0fff;
        if (s2 == 0x0fff)
        {
            d->symlen[s] = 0;
        }
        else
        {
            s1 = w & 0x0fff;
            if (!tmp[s1]) calc_symlen (d, s1, tmp);
            if (!tmp[s2]) calc_symlen (d, s2, tmp);
            d->symlen[s] = d->symlen[s1] + d->symlen[s2] + 1;
        }
        tmp[s] = 1;
    }

    PairsData *setup_pairs (unsigned char *data, uint64 tb_size, uint64 *size, unsigned char **next, ubyte *flags, int32 wdl)
    {
        PairsData *d;
        int32 i;

        *flags = data[0];
        if (data[0] & 0x80)
        {
            d = (PairsData *) malloc (sizeof (PairsData));
            d->idxbits = 0;
            if (wdl)
                d->min_len = data[1];
            else
                d->min_len = 0;
            *next = data + 2;
            size[0] = size[1] = size[2] = 0;
            return d;
        }

        int32 blocksize = data[1];
        int32 idxbits = data[2];
        int32 real_num_blocks = *(uint32 *) (&data[4]);
        int32 num_blocks = real_num_blocks + *(ubyte *) (&data[3]);
        int32 max_len = data[8];
        int32 min_len = data[9];
        int32 h = max_len - min_len + 1;
        int32 num_syms = *(ushort *) (&data[10 + 2 * h]);
        d = (PairsData *)malloc (sizeof (PairsData) + (h - 1) * sizeof (base_t) +num_syms);
        d->blocksize = blocksize;
        d->idxbits = idxbits;
        d->offset = (ushort *) (&data[10]);
        d->symlen = ((ubyte *) d) + sizeof (PairsData) + (h - 1) * sizeof (base_t);
        d->sympat = &data[12 + 2 * h];
        d->min_len = min_len;
        *next = &data[12 + 2 * h + 3 * num_syms + (num_syms & 1)];

        int32 num_indices = (tb_size + (1ULL << idxbits) - 1) >> idxbits;
        size[0] = 6ULL * num_indices;
        size[1] = 2ULL * num_blocks;
        size[2] = (1ULL << blocksize) * real_num_blocks;

        // char tmp[num_syms];
        char tmp[4096];
        for (i = 0; i < num_syms; ++i)
        {
            tmp[i] = 0;
        }
        for (i = 0; i < num_syms; ++i)
        {
            if (!tmp[i])
            {
                calc_symlen (d, i, tmp);
            }
        }

        d->base[h - 1] = 0;
        for (i = h - 2; i >= 0; --i)
        {
            d->base[i] = (d->base[i + 1] + d->offset[i] - d->offset[i + 1]) / 2;
        }

#ifdef DECOMP64
        
        for (i = 0; i < h; ++i)
        {
            d->base[i] <<= 64 - (min_len + i);
        }

#else

        for (i = 0; i < h; ++i)
        {
            d->base[i] <<= 32 - (min_len + i);
        }

#endif

        d->offset -= d->min_len;

        return d;
    }

    int32 init_table_wdl (TBEntry *entry, char *str)
    {
        ubyte *next;
        int32 f, s;
        uint64 tb_size[8];
        uint64 size[8 * 3];
        ubyte flags;

        // first mmap the table into memory

        entry->data = map_file (str, WDLSUFFIX, &entry->mapping);
        if (!entry->data)
        {
            printf ("Could not find %s" WDLSUFFIX, str);
            return 0;
        }

        ubyte *data = (ubyte *) entry->data;
        if (((uint32 *) data)[0] != WDL_MAGIC)
        {
            printf ("Corrupted table.\n");
            unmap_file (entry->data, entry->mapping);
            entry->data = 0;
            return 0;
        }

        int32 split = data[4] & 0x01;
        int32 files = data[4] & 0x02 ? 4 : 1;

        data += 5;

        if (!entry->has_pawns)
        {
            TBEntry_piece *ptr = (TBEntry_piece *)entry;
            setup_pieces_piece (ptr, data, &tb_size[0]);
            data += ptr->num + 1;
            data += ((uintptr_t) data) & 0x01;

            ptr->precomp[0] = setup_pairs (data, tb_size[0], &size[0], &next, &flags, 1);
            data = next;
            if (split)
            {
                ptr->precomp[1] = setup_pairs (data, tb_size[1], &size[3], &next, &flags, 1);
                data = next;
            }
            else
            {
                ptr->precomp[1] = NULL;
            }

            ptr->precomp[0]->indextable = (char *) data;
            data += size[0];
            if (split)
            {
                ptr->precomp[1]->indextable = (char *) data;
                data += size[3];
            }

            ptr->precomp[0]->sizetable = (ushort *) data;
            data += size[1];
            if (split)
            {
                ptr->precomp[1]->sizetable = (ushort *) data;
                data += size[4];
            }

            data = (ubyte *) ((((uintptr_t) data) + 0x3f) & ~0x3f);
            ptr->precomp[0]->data = data;
            data += size[2];
            if (split)
            {
                data = (ubyte *) ((((uintptr_t) data) + 0x3f) & ~0x3f);
                ptr->precomp[1]->data = data;
            }
        }
        else
        {
            TBEntry_pawn *ptr = (TBEntry_pawn *)entry;
            s = 1 + (ptr->pawns[1] > 0);
            for (f = 0; f < 4; f++)
            {
                setup_pieces_pawn ((TBEntry_pawn *)ptr, data, &tb_size[2 * f], f);
                data += ptr->num + s;
            }
            data += ((uintptr_t) data) & 0x01;

            for (f = 0; f < files; f++)
            {
                ptr->file[f].precomp[0] = setup_pairs (data, tb_size[2 * f], &size[6 * f], &next, &flags, 1);
                data = next;
                if (split)
                {
                    ptr->file[f].precomp[1] = setup_pairs (data, tb_size[2 * f + 1], &size[6 * f + 3], &next, &flags, 1);
                    data = next;
                }
                else
                {
                    ptr->file[f].precomp[1] = NULL;
                }
            }

            for (f = 0; f < files; f++)
            {
                ptr->file[f].precomp[0]->indextable = (char *) data;
                data += size[6 * f];
                if (split)
                {
                    ptr->file[f].precomp[1]->indextable = (char *) data;
                    data += size[6 * f + 3];
                }
            }

            for (f = 0; f < files; f++)
            {
                ptr->file[f].precomp[0]->sizetable = (ushort *) data;
                data += size[6 * f + 1];
                if (split)
                {
                    ptr->file[f].precomp[1]->sizetable = (ushort *) data;
                    data += size[6 * f + 4];
                }
            }

            for (f = 0; f < files; f++)
            {
                data = (ubyte *) ((((uintptr_t) data) + 0x3f) & ~0x3f);
                ptr->file[f].precomp[0]->data = data;
                data += size[6 * f + 2];
                if (split)
                {
                    data = (ubyte *) ((((uintptr_t) data) + 0x3f) & ~0x3f);
                    ptr->file[f].precomp[1]->data = data;
                    data += size[6 * f + 5];
                }
            }
        }

        return 1;
    }

    int32 init_table_dtz (TBEntry *entry)
    {
        ubyte *data = (ubyte *) entry->data;
        ubyte *next;
        int32 f, s;
        uint64 tb_size[4];
        uint64 size[4 * 3];

        if (!data)
        {
            return 0;
        }
        if (((uint32 *) data)[0] != DTZ_MAGIC)
        {
            printf ("Corrupted table.\n");
            return 0;
        }

        int32 files = data[4] & 0x02 ? 4 : 1;

        data += 5;

        if (!entry->has_pawns)
        {
            DTZEntry_piece *ptr = (DTZEntry_piece *)entry;
            setup_pieces_piece_dtz (ptr, data, &tb_size[0]);
            data += ptr->num + 1;
            data += ((uintptr_t) data) & 0x01;

            ptr->precomp = setup_pairs (data, tb_size[0], &size[0], &next, &(ptr->flags), 0);
            data = next;

            ptr->map = data;
            if (ptr->flags & 2)
            {
                int32 i;
                for (i = 0; i < 4; ++i)
                {
                    ptr->map_idx[i] = (data + 1 - ptr->map);
                    data += 1 + data[0];
                }
                data += ((uintptr_t) data) & 0x01;
            }

            ptr->precomp->indextable = (char *) data;
            data += size[0];

            ptr->precomp->sizetable = (ushort *) data;
            data += size[1];

            data = (ubyte *) ((((uintptr_t) data) + 0x3f) & ~0x3f);
            ptr->precomp->data = data;
            data += size[2];
        }
        else
        {
            DTZEntry_pawn *ptr = (DTZEntry_pawn *) entry;
            s = 1 + (ptr->pawns[1] > 0);
            for (f = 0; f < 4; f++)
            {
                setup_pieces_pawn_dtz (ptr, data, &tb_size[f], f);
                data += ptr->num + s;
            }
            data += ((uintptr_t) data) & 0x01;

            for (f = 0; f < files; f++)
            {
                ptr->file[f].precomp = setup_pairs (data, tb_size[f], &size[3 * f], &next, &(ptr->flags[f]), 0);
                data = next;
            }

            ptr->map = data;
            for (f = 0; f < files; f++)
            {
                if (ptr->flags[f] & 2)
                {
                    int32 i;
                    for (i = 0; i < 4; ++i)
                    {
                        ptr->map_idx[f][i] = (data + 1 - ptr->map);
                        data += 1 + data[0];
                    }
                }
            }
            data += ((uintptr_t) data) & 0x01;

            for (f = 0; f < files; f++)
            {
                ptr->file[f].precomp->indextable = (char *) data;
                data += size[3 * f];
            }

            for (f = 0; f < files; f++)
            {
                ptr->file[f].precomp->sizetable = (ushort *) data;
                data += size[3 * f + 1];
            }

            for (f = 0; f < files; f++)
            {
                data = (ubyte *) ((((uintptr_t) data) + 0x3f) & ~0x3f);
                ptr->file[f].precomp->data = data;
                data += size[3 * f + 2];
            }
        }

        return 1;
    }

    ubyte decompress_pairs (PairsData *d, uint64 idx)
    {
        if (!d->idxbits)
        {
            return d->min_len;
        }

        uint32 mainidx = idx >> d->idxbits;
        int32 litidx = (idx & ((1 << d->idxbits) - 1)) - (1 << (d->idxbits - 1));
        uint32 block = *(uint32 *) (d->indextable + 6 * mainidx);
        litidx += *(ushort *) (d->indextable + 6 * mainidx + 4);
        if (litidx < 0)
        {
            do
            {
                litidx += d->sizetable[--block] + 1;
            }
            while (litidx < 0);
        }
        else
        {
            while (litidx > d->sizetable[block])
            {
                litidx -= d->sizetable[block++] + 1;
            }
        }

        uint32 *ptr = (uint32 *) (d->data + (block << d->blocksize));

        int32 m = d->min_len;
        ushort *offset = d->offset;
        base_t *base = d->base - m;
        ubyte *symlen = d->symlen;
        int32 sym, bitcnt;

#ifdef DECOMP64

        uint64 code =
#ifdef _MSC_VER
            _byteswap_uint64 (*((uint64 *) ptr));
#else
            __builtin_bswap64 (*((uint64 *) ptr));
#endif
        ptr += 2;
        bitcnt = 0; // number of "empty bits" in code
        for (;;)
        {
            int32 l = m;
            while (code < base[l]) ++l;
            sym = offset[l] + ((code - base[l]) >> (64 - l));
            if (litidx < (int32) symlen[sym] + 1) break;
            litidx -= (int32) symlen[sym] + 1;
            code <<= l;
            bitcnt += l;
            if (bitcnt >= 32)
            {
                bitcnt -= 32;
                code |=
#ifdef _MSC_VER
            ((uint64) (_byteswap_uint64 (*ptr++))) << bitcnt;
#else
            ((uint64) (__builtin_bswap32 (*ptr++))) << bitcnt;
#endif
            }
        }

#else

        uint32 next = 0;
        uint32 code = __builtin_bswap32 (*ptr++);
        bitcnt = 0; // number of bits in next
        for (;;)
        {
            int32 l = m;
            while (code < base[l]) ++l;
            sym = offset[l] + ((code - base[l]) >> (32 - l));
            if (litidx < (int32) symlen[sym] + 1) break;
            litidx -= (int32) symlen[sym] + 1;
            code <<= l;
            if (bitcnt < l)
            {
                if (bitcnt)
                {
                    code |= (next >> (32 - l));
                    l -= bitcnt;
                }
                next = __builtin_bswap32 (*ptr++);
                bitcnt = 32;
            }
            code |= (next >> (32 - l));
            next <<= l;
            bitcnt -= l;
        }

#endif

        ubyte *sympat = d->sympat;
        while (symlen[sym] != 0)
        {
            int32 w = *(int32 *) (sympat + 3 * sym);
            int32 s1 = w & 0x0fff;
            if (litidx < (int32) symlen[s1] + 1)
                sym = s1;
            else
            {
                litidx -= (int32) symlen[s1] + 1;
                sym = (w >> 12) & 0x0fff;
            }
        }

        return *(sympat + 3 * sym);
    }

    void load_dtz_table (char *str, uint64 key1, uint64 key2)
    {
        int32 i;
        TBEntry *ptr, *ptr3;
        TBHashEntry *ptr2;

        DTZ_table[0].key1 = key1;
        DTZ_table[0].key2 = key2;
        DTZ_table[0].entry = NULL;

        // find corresponding WDL entry
        ptr2 = TB_hash[key1 >> (64 - TBHASHBITS)];
        for (i = 0; i < HSHMAX; ++i)
        {
            if (ptr2[i].key == key1) break;
        }
        if (i == HSHMAX) return;
        ptr = ptr2[i].ptr;

        ptr3 = (TBEntry *) malloc (ptr->has_pawns
            ? sizeof (DTZEntry_pawn)
            : sizeof (DTZEntry_piece));

        ptr3->data = map_file (str, DTZSUFFIX, &ptr3->mapping);
        ptr3->key = ptr->key;
        ptr3->num = ptr->num;
        ptr3->symmetric = ptr->symmetric;
        ptr3->has_pawns = ptr->has_pawns;
        if (ptr3->has_pawns)
        {
            DTZEntry_pawn *entry = (DTZEntry_pawn *)ptr3;
            entry->pawns[0] = ((TBEntry_pawn *)ptr)->pawns[0];
            entry->pawns[1] = ((TBEntry_pawn *)ptr)->pawns[1];
        }
        else
        {
            DTZEntry_piece *entry = (DTZEntry_piece *)ptr3;
            entry->enc_type = ((TBEntry_piece *)ptr)->enc_type;
        }
        if (!init_table_dtz (ptr3))
        {
            free (ptr3);
        }
        else
        {
            DTZ_table[0].entry = ptr3;
        }
    }

    void free_wdl_entry (TBEntry *entry)
    {
        unmap_file (entry->data, entry->mapping);
        if (!entry->has_pawns)
        {
            TBEntry_piece *ptr = (TBEntry_piece *)entry;
            free (ptr->precomp[0]);
            if (ptr->precomp[1])
            {
                free (ptr->precomp[1]);
            }
        }
        else
        {
            TBEntry_pawn *ptr = (TBEntry_pawn *)entry;
            int32 f;
            for (f = 0; f < 4; f++)
            {
                free (ptr->file[f].precomp[0]);
                if (ptr->file[f].precomp[1])
                    free (ptr->file[f].precomp[1]);
            }
        }
    }

    void free_dtz_entry (TBEntry *entry)
    {
        unmap_file (entry->data, entry->mapping);
        if (!entry->has_pawns)
        {
            DTZEntry_piece *ptr = (DTZEntry_piece *)entry;
            free (ptr->precomp);
        }
        else
        {
            DTZEntry_pawn *ptr = (DTZEntry_pawn *)entry;
            int32 f;
            for (f = 0; f < 4; f++)
            {
                free (ptr->file[f].precomp);
            }
        }

        free (entry);
    }

    int32 wdl_to_map[5] = { 1, 3, 0, 2, 0 };
    ubyte pa_flags[5] = { 8, 0, 0, 0, 4 };

}

namespace {

    // Given a position with 6 or fewer pieces, produce a text string
    // of the form KQPvKRP, where "KQP" represents the white pieces if
    // mirror == 0 and the black pieces if mirror == 1.
    void prt_str (Position &pos, char *str, int32 mirror)
    {
        Color color;
        PieceT pt;
        int32 i;

        color = !mirror ? WHITE : BLACK;
        for (pt = KING; pt >= PAWN; --pt)
        {
            for (i = pop_count<MAX15> (pos.pieces (color, pt)); i > 0; --i)
            {
                *str++ = pchr[TB_TOTAL - pt];
            }
        }
        *str++ = 'v';
        color = ~color;
        for (pt = KING; pt >= PAWN; --pt)
        {
            for (i = pop_count<MAX15> (pos.pieces (color, pt)); i > 0; --i)
            {
                *str++ = pchr[TB_TOTAL - pt];
            }
        }
        *str++ = 0;
    }

    // Given a position, produce a 64-bit material signature key.
    // If the engine supports such a key, it should equal the engine's key.
    uint64 calc_key (Position &pos, int32 mirror)
    {
        Color color;
        PieceT pt;
        int32 i;
        uint64 key = 0;

        color = !mirror ? WHITE : BLACK;
        for (pt = PAWN; pt <= KING; ++pt)
        {
            for (i = pop_count<MAX15> (pos.pieces (color, pt)); i > 0; --i)
            {
                key ^= ZobGlob._.piecesq[WHITE][pt][i - 1];
            }
        }
        color = ~color;
        for (pt = PAWN; pt <= KING; ++pt)
        {
            for (i = pop_count<MAX15> (pos.pieces (color, pt)); i > 0; --i)
            {
                key ^= ZobGlob._.piecesq[BLACK][pt][i - 1];
            }
        }

        return key;
    }

    // Produce a 64-bit material key corresponding to the material combination
    // defined by pcs[16], where pcs[1], ..., pcs[6] is the number of white
    // pawns, ..., kings and pcs[9], ..., pcs[14] is the number of black
    // pawns, ..., kings.
    uint64 calc_key_from_pcs (int32 *pcs, int32 mirror)
    {
        int32 color;
        PieceT pt;
        int32 i;
        uint64 key = 0;

        color = !mirror ? 0 : 8;
        for (pt = PAWN; pt <= KING; ++pt)
        {
            for (i = 0; i < pcs[color + pt]; ++i)
            {
                key ^= ZobGlob._.piecesq[WHITE][pt][i];
            }
        }
        color ^= 8;
        for (pt = PAWN; pt <= KING; ++pt)
        {
            for (i = 0; i < pcs[color + pt]; ++i)
            {
                key ^= ZobGlob._.piecesq[BLACK][pt][i];
            }
        }

        return key;
    }

    // probe_wdl_table and probe_dtz_table require similar adaptations.
    int32 probe_wdl_table (Position &pos, int32 *success)
    {
        TBEntry *ptr;
        TBHashEntry *ptr2;
        uint64 idx;
        uint64 key;
        int32 i;
        ubyte res;
        int32 p[TBPIECES];

        // Obtain the position's material signature key.
        key = pos.matl_key ();

        // Test for KvK.
        if (key == (ZobGlob._.piecesq[WHITE][KING][0] ^ ZobGlob._.piecesq[BLACK][KING][0]))
        {
            return 0;
        }

        ptr2 = TB_hash[key >> (64 - TBHASHBITS)];
        for (i = 0; i < HSHMAX; ++i)
        {
            if (ptr2[i].key == key) break;
        }
        if (i == HSHMAX)
        {
            *success = 0;
            return 0;
        }

        ptr = ptr2[i].ptr;
        if (!ptr->ready)
        {
            LOCK (TB_mutex);
            if (!ptr->ready)
            {
                char str[16];
                prt_str (pos, str, ptr->key != key);
                if (!init_table_wdl (ptr, str))
                {
                    ptr2[i].key = 0ULL;
                    *success = 0;
                    UNLOCK (TB_mutex);
                    return 0;
                }

#ifdef _MSC_VER

#else
                // Memory barrier to ensure ptr->ready = 1 is not reordered.
                __asm__ __volatile__ ("" ::: "memory");
#endif
                ptr->ready = 1;
            }
            UNLOCK (TB_mutex);
        }

        int32 bside, mirror, cmirror;
        if (!ptr->symmetric)
        {
            if (key != ptr->key)
            {
                cmirror = 8;
                mirror = 0x38;
                bside = (pos.active () == WHITE);
            }
            else
            {
                cmirror = mirror = 0;
                bside = !(pos.active () == WHITE);
            }
        }
        else
        {
            cmirror = pos.active () == WHITE ? 0 : 8;
            mirror = pos.active () == WHITE ? 0 : 0x38;
            bside = 0;
        }

        // p[i] is to contain the square 0-63 (A1-H8) for a piece of type
        // pc[i] ^ cmirror, where 1 = white pawn, ..., 14 = black king.
        // Pieces of the same type are guaranteed to be consecutive.
        if (!ptr->has_pawns)
        {
            TBEntry_piece *entry = (TBEntry_piece *) ptr;
            ubyte *pc = entry->pieces[bside];
            for (i = 0; i < entry->num;)
            {
                Bitboard bb = pos.pieces (Color ((pc[i] ^ cmirror) >> 3), PieceT (pc[i] & 0x07));
                do
                {
                    p[i++] = pop_lsq (bb);
                }
                while (bb);
            }
            idx = encode_piece (entry, entry->norm[bside], p, entry->factor[bside]);
            res = decompress_pairs (entry->precomp[bside], idx);
        }
        else
        {
            TBEntry_pawn *entry = (TBEntry_pawn *)ptr;
            int32 k = entry->file[0].pieces[0][0] ^ cmirror;
            Bitboard bb = pos.pieces ((Color) (k >> 3), (PieceT) (k & 0x07));
            i = 0;
            do
            {
                p[i++] = pop_lsq (bb) ^ mirror;
            }
            while (bb);
            int32 f = pawn_file (entry, p);
            ubyte *pc = entry->file[f].pieces[bside];
            for ( ; i < entry->num; )
            {
                bb = pos.pieces (Color ((pc[i] ^ cmirror) >> 3), PieceT (pc[i] & 0x07));
                do
                {
                    p[i++] = pop_lsq (bb) ^ mirror;
                }
                while (bb);
            }
            idx = encode_pawn (entry, entry->file[f].norm[bside], p, entry->file[f].factor[bside]);
            res = decompress_pairs (entry->file[f].precomp[bside], idx);
        }

        return ((int32) res) - 2;
    }

    int32 probe_dtz_table (Position &pos, int32 wdl, int32 *success)
    {
        TBEntry *ptr;
        uint64 idx;
        int32 i, res;
        int32 p[TBPIECES];

        // Obtain the position's material signature key.
        uint64 key = pos.matl_key ();

        if (DTZ_table[0].key1 != key && DTZ_table[0].key2 != key)
        {
            for (i = 1; i < DTZ_ENTRIES; ++i)
            {
                if (DTZ_table[i].key1 == key) break;
            }
            if (i < DTZ_ENTRIES)
            {
                DTZTableEntry table_entry = DTZ_table[i];
                for (; i > 0; --i)
                {
                    DTZ_table[i] = DTZ_table[i - 1];
                }
                DTZ_table[0] = table_entry;
            }
            else
            {
                TBHashEntry *ptr2 = TB_hash[key >> (64 - TBHASHBITS)];
                for (i = 0; i < HSHMAX; ++i)
                {
                    if (ptr2[i].key == key) break;
                }
                if (i == HSHMAX)
                {
                    *success = 0;
                    return 0;
                }
                ptr = ptr2[i].ptr;
                char str[16];
                int32 mirror = (ptr->key != key);
                prt_str (pos, str, mirror);

                if (DTZ_table[DTZ_ENTRIES - 1].entry)
                {
                    free_dtz_entry (DTZ_table[DTZ_ENTRIES-1].entry);
                }
                for (i = DTZ_ENTRIES - 1; i > 0; --i)
                {
                    DTZ_table[i] = DTZ_table[i - 1];
                }
                load_dtz_table (str, calc_key (pos, mirror), calc_key (pos, !mirror));
            }
        }

        ptr = DTZ_table[0].entry;
        if (!ptr)
        {
            *success = 0;
            return 0;
        }

        int32 bside, mirror, cmirror;
        if (!ptr->symmetric)
        {
            if (key != ptr->key)
            {
                cmirror = 8;
                mirror = 0x38;
                bside = (pos.active () == WHITE);
            }
            else
            {
                cmirror = mirror = 0;
                bside = !(pos.active () == WHITE);
            }
        }
        else
        {
            cmirror = pos.active () == WHITE ? 0 : 8;
            mirror = pos.active () == WHITE ? 0 : 0x38;
            bside = 0;
        }

        if (!ptr->has_pawns)
        {
            DTZEntry_piece *entry = (DTZEntry_piece *)ptr;
            if ((entry->flags & 1) != bside && !entry->symmetric)
            {
                *success = -1;
                return 0;
            }
            ubyte *pc = entry->pieces;
            for (i = 0; i < entry->num;)
            {
                Bitboard bb = pos.pieces ((Color) ((pc[i] ^ cmirror) >> 3),
                    (PieceT) (pc[i] & 0x07));
                do
                {
                    p[i++] = pop_lsq (bb);
                }
                while (bb);
            }
            idx = encode_piece ((TBEntry_piece *)entry, entry->norm, p, entry->factor);
            res = decompress_pairs (entry->precomp, idx);

            if (entry->flags & 2)
            {
                res = entry->map[entry->map_idx[wdl_to_map[wdl + 2]] + res];
            }
            if (!(entry->flags & pa_flags[wdl + 2]) || (wdl & 1))
            {
                res *= 2;
            }
        }
        else
        {
            DTZEntry_pawn *entry = (DTZEntry_pawn *)ptr;
            int32 k = entry->file[0].pieces[0] ^ cmirror;
            Bitboard bb = pos.pieces ((Color) (k >> 3), (PieceT) (k & 0x07));
            i = 0;
            do
            {
                p[i++] = pop_lsq (bb) ^ mirror;
            }
            while (bb);
            
            int32 f = pawn_file ((TBEntry_pawn *)entry, p);
            if ((entry->flags[f] & 1) != bside)
            {
                *success = -1;
                return 0;
            }

            ubyte *pc = entry->file[f].pieces;
            for ( ; i < entry->num; )
            {
                bb = pos.pieces ((Color) ((pc[i] ^ cmirror) >> 3), (PieceT) (pc[i] & 0x07));
                do
                {
                    p[i++] = pop_lsq (bb) ^ mirror;
                }
                while (bb);
            }

            idx = encode_pawn ((TBEntry_pawn *) entry, entry->file[f].norm, p, entry->file[f].factor);
            res = decompress_pairs (entry->file[f].precomp, idx);

            if (entry->flags[f] & 2)
            {
                res = entry->map[entry->map_idx[f][wdl_to_map[wdl + 2]] + res];
            }
            if (!(entry->flags[f] & pa_flags[wdl + 2]) || (wdl & 1))
            {
                res *= 2;
            }
        }

        return res;
    }

    // Add underpromotion captures to list of captures.
    ValMove *add_underprom_caps (Position &pos, ValMove *m_list, ValMove *end)
    {
        ValMove *moves, *extra = end;

        for (moves = m_list; moves < end; ++moves)
        {
            Move move = moves->move;
            if (mtype (move) == PROMOTE && !pos.empty (dst_sq (move)))
            {
                (*extra++).move = Move (move - (1 << 12));
                (*extra++).move = Move (move - (2 << 12));
                (*extra++).move = Move (move - (3 << 12));
            }
        }

        return extra;
    }

    int32 probe_ab (Position &pos, int32 alpha, int32 beta, int32 *success)
    {
        int32 v;
        ValMove m_list[64];
        ValMove *moves, *end;
        StateInfo st;

        // Generate (at least) all legal non-ep captures including (under)promotions.
        // It is OK to generate more, as long as they are filtered out below.
        if (pos.checkers ())
        {
            end = generate<EVASION> (m_list, pos);
        }
        else
        {
            end = generate<CAPTURE> (m_list, pos);
            // Since underpromotion captures are not included, we need to add them.
            end = add_underprom_caps (pos, m_list, end);
        }

        CheckInfo ci (pos);

        for (moves = m_list; moves < end; ++moves)
        {
            Move capture = moves->move;
            if (!pos.capture (capture) || mtype (capture) == ENPASSANT
                || !pos.legal (capture, ci.pinneds))
            {
                continue;
            }

            pos.do_move (capture, st, pos.gives_check (capture, ci) ? &ci : NULL);
            v = -probe_ab (pos, -beta, -alpha, success);
            pos.undo_move ();

            if (*success == 0) return 0;
            if (v > alpha)
            {
                if (v >= beta)
                {
                    *success = 2;
                    return v;
                }
                alpha = v;
            }
        }

        v = probe_wdl_table (pos, success);

        if (*success == 0) return 0;
        if (alpha >= v)
        {
            *success = 1 + (alpha > 0);
            return alpha;
        }
        else
        {
            *success = 1;
            return v;
        }
    }

    // This routine treats a position with en passant captures as one without.
    int32 probe_dtz_no_ep (Position &pos, int32 *success)
    {
        int32 wdl, dtz;

        wdl = probe_ab (pos, -2, 2, success);

        if (*success == 0) return 0;

        if (wdl == 0) return 0;

        if (*success == 2)
        {
            return wdl == 2 ? 1 : 101;
        }

        ValMove m_list[MAX_MOVES];
        ValMove *moves, *end = NULL;
        StateInfo st;
        CheckInfo ci (pos);

        if (wdl > 0)
        {
            // Generate at least all legal non-capturing pawn moves
            // including non-capturing promotions.
            end = pos.checkers ()
                ? generate<EVASION> (m_list, pos)
                : generate<RELAX> (m_list, pos);

            for (moves = m_list; moves < end; ++moves)
            {
                Move move = moves->move;
                if (_ptype (pos.moved_piece (move)) != PAWN || pos.capture (move)
                    || !pos.legal (move, ci.pinneds))
                    continue;
                pos.do_move (move, st, pos.gives_check (move, ci) ? &ci : NULL);
                int32 v = -probe_ab (pos, -2, -wdl + 1, success);
                pos.undo_move ();
                if (*success == 0) return 0;
                if (v == wdl)
                    return v == 2 ? 1 : 101;
            }
        }

        dtz = 1 + probe_dtz_table (pos, wdl, success);
        if (*success >= 0)
        {
            if (wdl & 1) dtz += 100;
            return wdl >= 0 ? dtz : -dtz;
        }

        if (wdl > 0)
        {
            int32 best = 0xffff;
            for (moves = m_list; moves < end; ++moves)
            {
                Move move = moves->move;
                if (pos.capture (move) || _ptype (pos.moved_piece (move)) == PAWN
                    || !pos.legal (move, ci.pinneds))
                {
                    continue;
                }
                pos.do_move (move, st, pos.gives_check (move, ci) ? &ci : NULL);
                int32 v = -Tablebases::probe_dtz (pos, success);
                pos.undo_move ();
                if (*success == 0) return 0;
                if (v > 0 && v + 1 < best)
                    best = v + 1;
            }
            return best;
        }
        else
        {
            int32 best = -1;
            end = pos.checkers ()
                ? generate<EVASION> (m_list, pos)
                : generate<RELAX> (m_list, pos);

            for (moves = m_list; moves < end; ++moves)
            {
                int32 v;
                Move move = moves->move;
                if (!pos.legal (move, ci.pinneds)) continue;

                pos.do_move (move, st, pos.gives_check (move, ci) ? &ci : NULL);
                if (st.clock50 == 0)
                {
                    if (wdl == -2)
                    {
                        v = -1;
                    }
                    else
                    {
                        v = probe_ab (pos, 1, 2, success);
                        v = (v == 2) ? 0 : -101;
                    }
                }
                else
                {
                    v = -Tablebases::probe_dtz (pos, success) - 1;
                }

                pos.undo_move ();
                if (*success == 0) return 0;
                if (v < best)
                    best = v;
            }
            return best;
        }
    }

    int32 wdl_to_dtz[] = {
        -1, -101, 0, 101, 1
    };

    // Check whether there has been at least one repetition of positions
    // since the last capture or pawn move.
    bool has_repeated (StateInfo *st)
    {
        while (1)
        {
            int32 i = 4, e = std::min (st->clock50, st->null_ply);
            if (e < i) return false;
            StateInfo *stp = st->p_si->p_si;
            do
            {
                stp = stp->p_si->p_si;
                if (stp->posi_key == st->posi_key)
                {
                    return true;
                }
                i += 2;
            }
            while (i <= e);
            st = st->p_si;
        }

        //const StateInfo *stp = st;
        //uint8_t ply = min (st->null_ply, st->clock50);
        //while (ply >= 2)
        //{
        //    if (sip->p_si == NULL) break;
        //    sip = sip->p_si;
        //    if (sip->p_si == NULL) break;
        //    sip = sip->p_si;
        //
        //    if (sip->posi_key == st->posi_key)
        //    {
        //        return true; // Draw at first repetition
        //    }
        //    ply -= 2;
        //}

    }

    Value wdl_to_Value[5] =
    {
        VALUE_MATED_IN_MAX_PLY + 1,
        VALUE_DRAW - 2,
        VALUE_DRAW,
        VALUE_DRAW + 2,
        VALUE_MATES_IN_MAX_PLY - 1
    };

}

namespace Tablebases {

    int32_t TBLargest = 0;

    // Probe the WDL table for a particular position.
    // If *success != 0, the probe was successful.
    // The return value is from the point of view of the side to move:
    // -2 : loss
    // -1 : loss, but draw under 50-move rule
    //  0 : draw
    //  1 : win, but draw under 50-move rule
    //  2 : win
    int32_t probe_wdl   (Position &pos, int32_t *success)
    {
        int32 v;

        *success = 1;
        v = probe_ab (pos, -2, 2, success);

        // If en passant is not possible, we are done.
        if (pos.en_passant () == SQ_NO)
            return v;
        if (!(*success)) return 0;

        // Now handle en passant.
        int32 v1 = -3;
        // Generate (at least) all legal en passant captures.
        ValMove m_list[MAX_MOVES];
        ValMove *moves;

        ValMove *end = pos.checkers ()
            ? generate<EVASION> (m_list, pos)
            : generate<CAPTURE> (m_list, pos);

        CheckInfo ci (pos);

        for (moves = m_list; moves < end; ++moves)
        {
            Move capture = moves->move;
            if (mtype (capture) != ENPASSANT
                || !pos.legal (capture, ci.pinneds))
            {
                continue;
            }

            StateInfo st;
            pos.do_move (capture, st, pos.gives_check (capture, ci) ? &ci : NULL);
            int32 v0 = -probe_ab (pos, -2, 2, success);
            pos.undo_move ();
            if (*success == 0) return 0;
            if (v1 < v0) v1 = v0;
        }
        if (v1 > -3)
        {
            if (v <= v1)
            {
                v = v1;
            }
            else if (v == 0)
            {
                // Check whether there is at least one legal non-ep move.
                for (moves = m_list; moves < end; ++moves)
                {
                    Move capture = moves->move;
                    if (mtype (capture) == ENPASSANT) continue;
                    if (pos.legal (capture, ci.pinneds)) break;
                }
                if (moves == end && !pos.checkers ())
                {
                    end = generate<QUIET> (end, pos);
                    for (; moves < end; ++moves)
                    {
                        Move move = moves->move;
                        if (pos.legal (move, ci.pinneds)) break;
                    }
                }
                // If not, then we are forced to play the losing ep capture.
                if (moves == end)
                {
                    v = v1;
                }
            }
        }

        return v;
    }

    // Probe the DTZ table for a particular position.
    // If *success != 0, the probe was successful.
    // The return value is from the point of view of the side to move:
    //         n < -100 : loss, but draw under 50-move rule
    // -100 <= n < -1   : loss in n ply (assuming 50-move counter == 0)
    //         0	    : draw
    //     1 < n <= 100 : win in n ply (assuming 50-move counter == 0)
    //   100 < n        : win, but draw under 50-move rule
    //
    // The return value n can be off by 1: a return value -n can mean a loss
    // in n+1 ply and a return value +n can mean a win in n+1 ply. This
    // cannot happen for tables with positions exactly on the "edge" of
    // the 50-move rule.
    //
    // This implies that if dtz > 0 is returned, the position is certainly
    // a win if dtz + 50-move-counter <= 99. Care must be taken that the engine
    // picks moves that preserve dtz + 50-move-counter <= 99.
    //
    // If n = 100 immediately after a capture or pawn move, then the position
    // is also certainly a win, and during the whole phase until the next
    // capture or pawn move, the inequality to be preserved is
    // dtz + 50-movecounter <= 100.
    //
    // In short, if a move is available resulting in dtz + 50-move-counter <= 99,
    // then do not accept moves leading to dtz + 50-move-counter == 100.
    //
    int32_t probe_dtz   (Position &pos, int32_t *success)
    {
        *success = 1;
        int32 v = probe_dtz_no_ep (pos, success);

        if (pos.en_passant () == SQ_NO) return v;
        if (*success == 0) return 0;

        // Now handle en passant.
        int32 v1 = -3;

        ValMove m_list[MAX_MOVES];
        ValMove *moves;

        ValMove *end = pos.checkers ()
            ? generate<EVASION> (m_list, pos)
            : generate<CAPTURE> (m_list, pos);

        CheckInfo ci (pos);

        for (moves = m_list; moves < end; ++moves)
        {
            Move capture = moves->move;
            if (mtype (capture) != ENPASSANT
                || !pos.legal (capture, ci.pinneds))
            {
                continue;
            }

            StateInfo st;
            pos.do_move (capture, st, pos.gives_check (capture, ci) ? &ci : NULL);
            int32 v0 = -probe_ab (pos, -2, 2, success);
            pos.undo_move ();
            if (*success == 0) return 0;
            if (v0 > v1) v1 = v0;
        }
        if (v1 > -3)
        {
            v1 = wdl_to_dtz[v1 + 2];
            if (v < -100)
            {
                if (v1 >= 0)
                {
                    v = v1;
                }
            }
            else if (v < 0)
            {
                if (v1 >= 0 || v1 < 100)
                {
                    v = v1;
                }
            }
            else if (v > 100)
            {
                if (v1 > 0)
                {
                    v = v1;
                }
            }
            else if (v > 0)
            {
                if (v1 == 1)
                {
                    v = v1;
                }
            }
            else if (v1 >= 0)
            {
                v = v1;
            }
            else
            {
                for (moves = m_list; moves < end; ++moves)
                {
                    Move move = moves->move;
                    if (mtype (move) == ENPASSANT) continue;
                    if (pos.legal (move, ci.pinneds)) break;
                }
                if (moves == end && !pos.checkers ())
                {
                    end = generate<QUIET> (end, pos);
                    for (; moves < end; ++moves)
                    {
                        Move move = moves->move;
                        if (pos.legal (move, ci.pinneds)) break;
                    }
                }
                if (moves == end)
                {
                    v = v1;
                }
            }
        }

        return v;
    }

    // Use the DTZ tables to filter out moves that don't preserve the win or draw.
    // If the position is lost, but DTZ is fairly high, only keep moves that
    // maximise DTZ.
    //
    // A return value false indicates that not all probes were successful and that
    // no moves were filtered out.
    bool root_probe     (Position &pos, Value &TBScore)
    {
        int32 success;

        int32 dtz = probe_dtz (pos, &success);
        if (!success) return false;

        StateInfo st;
        CheckInfo ci (pos);

        // Probe each move.
        for (size_t i = 0; i < Searcher::RootMoves.size (); ++i)
        {
            Move move = Searcher::RootMoves[i].pv[0];
            
            pos.do_move (move, st, pos.gives_check (move, ci) ? &ci : NULL);
            
            int32 v = 0;
            if (pos.checkers () && dtz > 0)
            {
                ValMove s[MAX_MOVES];
                if (generate<LEGAL> (s, pos) == s)
                {
                    v = 1;
                }
            }
            
            if (v == 0)
            {
                if (st.clock50 != 0)
                {
                    v = -Tablebases::probe_dtz (pos, &success);
                    if (v > 0)
                    {
                        v++;
                    }
                    else if (v < 0)
                    {
                        v--;
                    }
                }
                else
                {
                    v = -Tablebases::probe_wdl (pos, &success);
                    v = wdl_to_dtz[v + 2];
                }
            }

            pos.undo_move ();
            if (!success) return false;
            Searcher::RootMoves[i].value[0] = (Value) v;
        }

        // Obtain 50-move counter for the root position.
        // In Stockfish there seems to be no clean way, so we do it like this:
        int32 cnt50 = st.p_si->clock50;

        // Use 50-move counter to determine whether the root position is
        // won, lost or drawn.
        int32 wdl = 0;
        if (dtz > 0)
        {
            wdl = (dtz + cnt50 <= 100) ? 2 : 1;
        }
        else if (dtz < 0)
        {
            wdl = (-dtz + cnt50 <= 100) ? -2 : -1;
        }

        // Determine the score to report to the user.
        TBScore = wdl_to_Value[wdl + 2];
        // If the position is winning or losing, but too few moves left, adjust the
        // score to show how close it is to winning or losing. Weird rounding is
        // because of the way Stockfish converts values to printed scores.
        if (wdl == 1 && dtz <= 100)
        {
            TBScore = (Value) (((200 - dtz - cnt50) + 1) & ~1);
        }
        else if (wdl == -1 && dtz >= -100)
        {
            TBScore = -(Value) (((200 + dtz - cnt50) + 1) & ~1);
        }

        // Now be a bit smart about filtering out moves.
        size_t j = 0;
        if (dtz > 0)
        { // winning (or 50-move rule draw)
            int32 best = 0xffff;
            for (size_t i = 0; i < Searcher::RootMoves.size (); ++i)
            {
                int32 v = Searcher::RootMoves[i].value[0];
                if (v > 0 && best > v)
                {
                    best = v;
                }
            }
            int32 max = best;
            // If the current phase has not seen repetitions, then try all moves
            // that stay safely within the 50-move budget, if there are any.
            if (!has_repeated (st.p_si) && best + cnt50 <= 99)
            {
                max = 99 - cnt50;
            }
            for (size_t i = 0; i < Searcher::RootMoves.size (); ++i)
            {
                int32 v = Searcher::RootMoves[i].value[0];
                if (v > 0 && v <= max)
                {
                    Searcher::RootMoves[j++] = Searcher::RootMoves[i];
                }
            }
        }
        else if (dtz < 0)
        { // losing (or 50-move rule draw)
            int32 best = 0;
            for (size_t i = 0; i < Searcher::RootMoves.size (); ++i)
            {
                int32 v = Searcher::RootMoves[i].value[0];
                if (best > v)
                {
                    best = v;
                }
            }
            // Try all moves, unless we approach or have a 50-move rule draw.
            if (-best * 2 + cnt50 < 100)
            {
                return true;
            }
            for (size_t i = 0; i < Searcher::RootMoves.size (); ++i)
            {
                if (Searcher::RootMoves[i].value[0] == best)
                {
                    Searcher::RootMoves[j++] = Searcher::RootMoves[i];
                }
            }
        }
        else
        { // drawing
            // Try all moves that preserve the draw.
            for (size_t i = 0; i < Searcher::RootMoves.size (); ++i)
            {
                if (Searcher::RootMoves[i].value[0] == 0)
                {
                    Searcher::RootMoves[j++] = Searcher::RootMoves[i];
                }
            }
        }
        Searcher::RootMoves.resize (j, Searcher::RootMove (MOVE_NONE));

        return true;
    }

    // Use the WDL tables to filter out moves that don't preserve the win or draw.
    // This is a fallback for the case that some or all DTZ tables are missing.
    //
    // A return value false indicates that not all probes were successful and that
    // no moves were filtered out.
    bool root_probe_wdl (Position &pos, Value &TBScore)
    {
        int32 success;

        int32 wdl = Tablebases::probe_wdl (pos, &success);
        if (!success) return false;
        TBScore = wdl_to_Value[wdl + 2];

        StateInfo st;
        CheckInfo ci (pos);

        int32 best = -2;

        // Probe each move.
        for (size_t i = 0; i < Searcher::RootMoves.size (); ++i)
        {
            Move move = Searcher::RootMoves[i].pv[0];
            pos.do_move (move, st, pos.gives_check (move, ci) ? &ci : NULL);
            int32 v = -Tablebases::probe_wdl (pos, &success);
            pos.undo_move ();
            if (!success) return false;
            Searcher::RootMoves[i].value[0] = (Value) v;
            if (best < v)
            {
                best = v;
            }
        }

        size_t j = 0;
        for (size_t i = 0; i < Searcher::RootMoves.size (); ++i)
        {
            if (Searcher::RootMoves[i].value[0] == best)
            {
                Searcher::RootMoves[j++] = Searcher::RootMoves[i];
            }
        }
        Searcher::RootMoves.resize (j, Searcher::RootMove (MOVE_NONE));

        return true;
    }


    void initialize (const std::string &path)
    {
        char str[16];
        int32 i, j, k, l;

        if (initialized)
        {
            free (path_string);
            free (paths);
            TBEntry *entry;
            for (i = 0; i < TBnum_piece; ++i)
            {
                entry = (TBEntry *)&TB_piece[i];
                free_wdl_entry (entry);
            }
            for (i = 0; i < TBnum_pawn; ++i)
            {
                entry = (TBEntry *)&TB_pawn[i];
                free_wdl_entry (entry);
            }
            for (i = 0; i < DTZ_ENTRIES; ++i)
            {
                if (DTZ_table[i].entry)
                    free_dtz_entry (DTZ_table[i].entry);
            }
        }
        else
        {
            init_indices ();
            initialized = true;
        }

        if (path.empty ()) return;
        
        path_string = (char *) malloc (path.length () + 1);
        const char *p = path.c_str ();
        strcpy (path_string, p);
        num_paths = 0;

        for (i = 0; ; ++i)
        {
            if (path_string[i] != SEP_CHAR)
            {
                ++num_paths;
            }
            while (path_string[i] && path_string[i] != SEP_CHAR)
            {
                ++i;
            }
            if (!path_string[i]) break;
            path_string[i] = 0;
        }
        paths = (char **) malloc (num_paths * sizeof (char *));
        for (i = j = 0; i < num_paths; ++i)
        {
            while (!path_string[j]) ++j;
            paths[i] = &path_string[j];
            while (path_string[j]) ++j;
        }

        LOCK_INIT (TB_mutex);

        TBnum_piece = TBnum_pawn = 0;
        TBLargest = 0;

        for (i = 0; i < (1 << TBHASHBITS); ++i)
        {
            for (j = 0; j < HSHMAX; ++j)
            {
                TB_hash[i][j].key = 0ULL;
                TB_hash[i][j].ptr = NULL;
            }
        }
        for (i = 0; i < DTZ_ENTRIES; ++i)
        {
            DTZ_table[i].entry = NULL;
        }

        for (i = 1; i < 6; ++i)
        {
            sprintf (str, "K%cvK", pchr[i]);
            init_tb (str);
        }

        for (i = 1; i < 6; ++i)
        {
            for (j = i; j < 6; ++j)
            {
                sprintf (str, "K%cvK%c", pchr[i], pchr[j]);
                init_tb (str);
            }
        }

        for (i = 1; i < 6; ++i)
        {
            for (j = i; j < 6; ++j)
            {
                sprintf (str, "K%c%cvK", pchr[i], pchr[j]);
                init_tb (str);
            }
        }

        for (i = 1; i < 6; ++i)
        {
            for (j = i; j < 6; ++j)
            {
                for (k = 1; k < 6; ++k)
                {
                    sprintf (str, "K%c%cvK%c", pchr[i], pchr[j], pchr[k]);
                    init_tb (str);
                }
            }
        }

        for (i = 1; i < 6; ++i)
        {
            for (j = i; j < 6; ++j)
            {
                for (k = j; k < 6; ++k)
                {
                    sprintf (str, "K%c%c%cvK", pchr[i], pchr[j], pchr[k]);
                    init_tb (str);
                }
            }
        }

        for (i = 1; i < 6; ++i)
        {
            for (j = i; j < 6; ++j)
            {
                for (k = i; k < 6; ++k)
                {
                    for (l = (i == k) ? j : k; l < 6; ++l)
                    {
                        sprintf (str, "K%c%cvK%c%c", pchr[i], pchr[j], pchr[k], pchr[l]);
                        init_tb (str);
                    }
                }
            }
        }

        for (i = 1; i < 6; ++i)
        {
            for (j = i; j < 6; ++j)
            {
                for (k = j; k < 6; ++k)
                {
                    for (l = 1; l < 6; ++l)
                    {
                        sprintf (str, "K%c%c%cvK%c", pchr[i], pchr[j], pchr[k], pchr[l]);
                        init_tb (str);
                    }
                }
            }
        }
        for (i = 1; i < 6; ++i)
        {
            for (j = i; j < 6; ++j)
            {
                for (k = j; k < 6; ++k)
                {
                    for (l = k; l < 6; ++l)
                    {
                        sprintf (str, "K%c%c%c%cvK", pchr[i], pchr[j], pchr[k], pchr[l]);
                        init_tb (str);
                    }
                }
            }
        }

        printf ("info string Tablebases found %d.\n", TBnum_piece + TBnum_pawn);
        //std::cout << "info string Tablebases found " << (TBnum_piece + TBnum_pawn) << ".\n" << std::endl;

    }

}
