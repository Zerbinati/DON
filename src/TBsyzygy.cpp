#include "TBsyzygy.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string>
#include <iostream>
#include <sstream>
#include <fstream>
#include <algorithm>

#include "Engine.h"

#if !defined(_WIN32)

#   include <unistd.h>
#   include <sys/mman.h>

#   include <pthread.h>
#   define FD       i32
#   define FD_ERR   -1

#   define LOCK_T       pthread_mutex_t
#   define LOCK_INIT(x) pthread_mutex_init(&(x), nullptr)
#   define LOCK(x)      pthread_mutex_lock(&(x))
#   define UNLOCK(x)    pthread_mutex_unlock(&(x))

const char SepChar = ':';

#else

#   if !defined(NOMINMAX)
#       define NOMINMAX // Disable macros min() and max()
#   endif
#   if !defined(WIN32_LEAN_AND_MEAN)
#       define WIN32_LEAN_AND_MEAN
#   endif
#   include <windows.h>
#   undef WIN32_LEAN_AND_MEAN
#   undef NOMINMAX

#   define FD       HANDLE
#   define FD_ERR   INVALID_HANDLE_VALUE

#   define LOCK_T   HANDLE
#   define LOCK_INIT(x) do { x = CreateMutex(nullptr, FALSE, nullptr); } while (false)
#   define LOCK(x)      WaitForSingleObject(x, INFINITE)
#   define UNLOCK(x)    ReleaseMutex(x)

const char SepChar = ';';

#endif

#if !defined(_MSC_VER)
#   define BSWAP32(v) __builtin_bswap32(v)
#   define BSWAP64(v) __builtin_bswap64(v)
#else
#   define BSWAP32(v) _byteswap_ulong(v)
#   define BSWAP64(v) _byteswap_uint64(v)
#endif

#include "BitCount.h"
#include "BitBoard.h"
#include "Position.h"
#include "MoveGenerator.h"
#include "Searcher.h"

namespace TBSyzygy {

    using namespace std;
    using namespace BitBoard;
    using namespace MoveGen;
    using namespace Searcher;

    string  PathString      = "<empty>";
    i32     MaxPieceLimit   = 0;
    Value   ProbeValue      = VALUE_NONE;

    namespace {

        // --->Core

        // CORE contains engine-independent routines of the tablebase probing code.
        // This should not need to much adaptation to add tablebase probing to
        // a particular engine, provided the engine is written in C or C++.

        struct TBHashEntry;

        typedef u64 base_t;

        struct PairsData
        {
            char *table_index;
            u16  *table_size;
            u08  *data;
            u16  *offset;
            u08  *symlen;
            u08  *sympat;
            i32   blocksize;
            i32   idxbits;
            i32   min_len;
            base_t base[1]; // C++ complains about base[]...
        };

        struct TBEntry
        {
            i08 *data;
            Key  key;
            u64  mapping;
            u08  num;
            bool ready;
            bool symmetric;
            bool has_pawns;
        }
#if !defined(_WIN32)
        __attribute__ ((__may_alias__))
#endif
            ;

        struct TBEntry_piece
        {
            i08 *data;
            Key  key;
            u64  mapping;
            u08  num;
            bool ready;
            bool symmetric;
            bool has_pawns;
            u08  enc_type;
            PairsData *precomp[2];
            i32   factor[2][NONE];
            Piece pieces[2][NONE];
            u08   norm[2][NONE];
        };

        struct TBEntry_pawn
        {
            i08 *data;
            Key  key;
            u64  mapping;
            u08  num;
            bool ready;
            bool symmetric;
            bool has_pawns;
            u08  pawns[2];
            struct
            {
                PairsData *precomp[2];
                i32   factor[2][NONE];
                Piece pieces[2][NONE];
                u08   norm[2][NONE];
            } file[4];
        };

        struct DTZEntry_piece
        {
            i08 *data;
            Key  key;
            u64  mapping;
            u08  num;
            bool ready;
            bool symmetric;
            bool has_pawns;
            u08  enc_type;
            PairsData *precomp;
            i32   factor[NONE];
            Piece pieces[NONE];
            u08   norm[NONE];
            u08   flags; // accurate, mapped, side
            u16   map_idx[4];
            u08  *map;
        };

        struct DTZEntry_pawn
        {
            i08 *data;
            Key  key;
            u64  mapping;
            u08  num;
            bool ready;
            bool symmetric;
            bool has_pawns;
            u08  pawns[2];
            struct
            {
                PairsData *precomp;
                i32   factor[NONE];
                Piece pieces[NONE];
                u08   norm[NONE];
            } file[4];
            u08  flags[4];
            u16  map_idx[4][4];
            u08 *map;
        };

        struct TBHashEntry
        {
            Key key;
            TBEntry *tbe;
        };

        struct DTZTableEntry
        {
            u64 key1;
            u64 key2;
            TBEntry *tbe;
        };

        PieceType tb_ptype (Piece p) { return (p & MAX_PTYPE) != 0 ? PieceType((p & MAX_PTYPE) - 1) : NONE; }

        const char PieceChar[NONE] ={ 'P', 'N', 'B', 'R', 'Q', 'K' };

        const string WDL_Suffix = ".rtbw";
        const string DTZ_Suffix = ".rtbz";

        const u08 WDL_Magic[4] ={ 0x71, 0xE8, 0x23, 0x5D };
        const u08 DTZ_Magic[4] ={ 0xD7, 0x66, 0x0C, 0xA5 };

        vector<string> Paths;

        LOCK_T TB_mutex;

        const u08 TB_PieceLimit = 6;

        u16 TB_PieceCount   = 0,
            TB_PawnCount    = 0;
        
        const u16 MaxTBPiece = 254;
        TBEntry_piece TB_Piece[MaxTBPiece];
        const u16 MaxTBPawn  = 256;
        TBEntry_pawn  TB_Pawn [MaxTBPawn];

        const u08 TBHashBits = 10;
        const u08 MaxHash    = 6;
        TBHashEntry TB_Hash[1 << TBHashBits][MaxHash];

        const u08 DTZ_Entries = 64;
        DTZTableEntry DTZ_Table[DTZ_Entries];

        FD open_tb (const string &filename, const string &suffix)
        {
            for (const auto &path : Paths)
            {
                string fullpath = append_path (path, filename + suffix);

                FD fd;
#if !defined(_WIN32)
                fd = open (fullpath.c_str (), O_RDONLY);
#else
                fd = CreateFile (fullpath.c_str (), GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);
#endif
                if (fd != FD_ERR) return fd;
            }
            return FD_ERR;
        }

        void close_tb (FD fd)
        {
#if !defined(_WIN32)
            close (fd);
#else
            CloseHandle (fd);
#endif
        }

        i08* map_file (const string &filename, const string &suffix, u64 *mapping)
        {
            FD fd = open_tb (filename, suffix);
            if (fd == FD_ERR)
            {
                return nullptr;
            }
#if !defined(_WIN32)
            stat statbuf;
            fstat (fd, &statbuf);
            *mapping = statbuf.st_size;
            i08 *data = (i08 *)mmap (nullptr, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0);
            if (data == (i08 *)(-1))
            {
                std::cout << "Could not mmap() " << filename << std::endl;
                Engine::stop (EXIT_FAILURE);
            }
#else
            DWORD size_low, size_high;
            size_low = GetFileSize (fd, &size_high);
            //size = u64(size_high) << 32 | u64(size_low);
            HANDLE map = CreateFileMapping (fd, nullptr, PAGE_READONLY, size_high, size_low, nullptr);
            if (map == nullptr)
            {
                std::cout << "CreateFileMapping() failed." << std::endl;
                Engine::stop (EXIT_FAILURE);
            }
            *mapping = (u64)map;
            i08 *data = (i08 *)MapViewOfFile (map, FILE_MAP_READ, 0, 0, 0);
            if (data == nullptr)
            {
                std::cout << "MapViewOfFile() failed, filename = " << filename << suffix << ", error = " << GetLastError () << std::endl;
                Engine::stop (EXIT_FAILURE);
            }
#endif
            close_tb (fd);
            return data;
        }

        void unmap_file (i08 *data, u64 size)
        {
#if !defined(_WIN32)
            if (data == nullptr) return;
            munmap (data, size);
#else
            if (data == nullptr) return;
            UnmapViewOfFile (data);
            CloseHandle ((HANDLE) size);
#endif
        }

        void add_to_hash (Key key, TBEntry *tbe)
        {
            const u16 hash_idx = u16(key >> (64 - TBHashBits));
            assert(hash_idx < (1 << TBHashBits));

            u08 i = 0;
            while (   i < MaxHash
                   && TB_Hash[hash_idx][i].tbe != nullptr
                  )
            {
                ++i;
            }
            if (i == MaxHash)
            {
                std::cout << "MaxHash too low!." << std::endl;
                Engine::stop (EXIT_FAILURE);
            }
            else
            {
                TB_Hash[hash_idx][i].key = key;
                TB_Hash[hash_idx][i].tbe = tbe;
            }
        }

        // Produce a 64-bit material key corresponding to the material combination
        // defined by pcs[16], where pcs[1], ..., pcs[6] is the number of white
        // pawns, ..., kings and pcs[9], ..., pcs[14] is the number of black
        // pawns, ..., kings.
        Key calc_key (u08 *pcs, bool mirror)
        {
            Key key = 0;
            auto color = mirror ? BLACK : WHITE;
            for (auto pt = PAWN; pt <= KING; ++pt)
            {
                for (auto pc = pcs[(color|pt) + 1]; pc > 0; --pc)
                {
                    key ^= Zob.piece_square[WHITE][pt][pc - 1];
                }
            }
            color = ~color;
            for (auto pt = PAWN; pt <= KING; ++pt)
            {
                for (auto pc = pcs[(color|pt) + 1]; pc > 0; --pc)
                {
                    key ^= Zob.piece_square[BLACK][pt][pc - 1];
                }
            }
            return key;
        }

        void init_tb (const string &filename)
        {
            static const u08 MaxPiece = 16;

            FD fd = open_tb (filename, WDL_Suffix);
            if (fd == FD_ERR) return;
            close_tb (fd);
            
            u08 pcs[MaxPiece];
            std::memset (pcs, 0x00, MaxPiece);
            Color color = WHITE;
            for (const auto &ch : filename)
            {
                switch (ch)
                {
                case 'P':
                    pcs[(color|PAWN) + 1]++;
                    break;
                case 'N':
                    pcs[(color|NIHT) + 1]++;
                    break;
                case 'B':
                    pcs[(color|BSHP) + 1]++;
                    break;
                case 'R':
                    pcs[(color|ROOK) + 1]++;
                    break;
                case 'Q':
                    pcs[(color|QUEN) + 1]++;
                    break;
                case 'K':
                    pcs[(color|KING) + 1]++;
                    break;
                case 'v':
                    color = BLACK;
                    break;
                }
            }

            Key key1 = calc_key (pcs, false);
            Key key2 = calc_key (pcs, true);
            
            TBEntry *tbe = nullptr;
            if ((pcs[(WHITE|PAWN) + 1] + pcs[(BLACK|PAWN) + 1]) == 0)
            {
                if (TB_PieceCount == MaxTBPiece)
                {
                    std::cout << "MaxTBPiece limit too low!." << std::endl;
                    Engine::stop (EXIT_FAILURE);
                }
                tbe = reinterpret_cast<TBEntry *> (&TB_Piece[TB_PieceCount++]);
            }
            else
            {
                if (TB_PawnCount == MaxTBPawn)
                {
                    std::cout << "MaxTBPawn limit too low!" << std::endl;
                    Engine::stop (EXIT_FAILURE);
                }
                tbe = reinterpret_cast<TBEntry *> (&TB_Pawn[TB_PawnCount++]);
            }
            assert(tbe != nullptr);

            tbe->key = key1;
            tbe->ready = false;
            tbe->num = 0;
            for (u08 i = 0; i < MaxPiece; ++i)
            {
                tbe->num += pcs[i];
            }

            tbe->symmetric = (key1 == key2);
            tbe->has_pawns = (pcs[(WHITE|PAWN) + 1] + pcs[(BLACK|PAWN) + 1]) > 0;

            if (MaxPieceLimit < tbe->num)
            {
                MaxPieceLimit = tbe->num;
            }

            if (tbe->has_pawns)
            {
                auto *tbep = reinterpret_cast<TBEntry_pawn *> (tbe);
                tbep->pawns[0] = pcs[(WHITE|PAWN) + 1];
                tbep->pawns[1] = pcs[(BLACK|PAWN) + 1];
                if (   pcs[(BLACK|PAWN) + 1] > 0
                    && (pcs[(WHITE|PAWN) + 1] == 0 || pcs[(WHITE|PAWN) + 1] > pcs[(BLACK|PAWN) + 1])
                   )
                {
                    tbep->pawns[0] = pcs[(BLACK|PAWN) + 1];
                    tbep->pawns[1] = pcs[(WHITE|PAWN) + 1];
                }
            }
            else
            {
                auto *tbep = reinterpret_cast<TBEntry_piece *> (tbe);
                
                u08 count = 0;
                for (u08 i = 0; i < MaxPiece; ++i)
                {
                    if (pcs[i] == 1)
                    {
                        ++count;
                    }
                }
                
                if (count >= 3)
                {
                    tbep->enc_type = 0;
                }
                else
                if (count == 2)
                {
                    tbep->enc_type = 2;
                }
                else
                { /* only for suicide */
                    count = 16;
                    for (u08 i = 0; i < MaxPiece; ++i)
                    {
                        if (pcs[i] > 1)
                        {
                            if (count > pcs[i])
                            {
                                count = pcs[i];
                            }
                        }
                        tbep->enc_type = u08(1 + count);
                    }
                }
            }

            add_to_hash (key1, tbe);
            if (key1 != key2)
            {
                add_to_hash (key2, tbe);
            }
        }

        void clear_tb ()
        {
            TB_PieceCount  = 0;
            TB_PawnCount   = 0;
            MaxPieceLimit  = 0;

            for (u16 i = 0; i < (1 << TBHashBits); ++i)
            {
                for (u08 j = 0; j < MaxHash; ++j)
                {
                    TB_Hash[i][j].key = 0;
                    TB_Hash[i][j].tbe = nullptr;
                }
            }
            for (u08 i = 0; i < DTZ_Entries; ++i)
            {
                DTZ_Table[i].tbe = nullptr;
            }
        }

        const i08 OffDiag[SQ_NO] =
        {
            0,-1,-1,-1,-1,-1,-1,-1,
            1, 0,-1,-1,-1,-1,-1,-1,
            1, 1, 0,-1,-1,-1,-1,-1,
            1, 1, 1, 0,-1,-1,-1,-1,
            1, 1, 1, 1, 0,-1,-1,-1,
            1, 1, 1, 1, 1, 0,-1,-1,
            1, 1, 1, 1, 1, 1, 0,-1,
            1, 1, 1, 1, 1, 1, 1, 0
        };

        const u08 Triangle[SQ_NO] =
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

        const u08 InvTriangle[10] =
        {
            1, 2, 3, 10, 11, 19, 0, 9, 18, 27
        };

        const u08 InvDiag[16] =
        {
            0,  9, 18, 27, 36, 45, 54, 63,
            7, 14, 21, 28, 35, 42, 49, 56
        };

        const u08 FlipDiag[SQ_NO] =
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

        const u08 Lower[SQ_NO] =
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

        const u08 Diag[SQ_NO] =
        {
            0,  0,  0,  0,  0,  0,  0,  8,
            0,  1,  0,  0,  0,  0,  9,  0,
            0,  0,  2,  0,  0, 10,  0,  0,
            0,  0,  0,  3, 11,  0,  0,  0,
            0,  0,  0, 12,  4,  0,  0,  0,
            0,  0, 13,  0,  0,  5,  0,  0,
            0, 14,  0,  0,  0,  0,  6,  0,
            15,  0,  0,  0,  0,  0,  0,  7
        };

        const u08 Flap[SQ_NO] =
        {
            0,  0,  0,  0,  0,  0,  0, 0,
            0,  6, 12, 18, 18, 12,  6, 0,
            1,  7, 13, 19, 19, 13,  7, 1,
            2,  8, 14, 20, 20, 14,  8, 2,
            3,  9, 15, 21, 21, 15,  9, 3,
            4, 10, 16, 22, 22, 16, 10, 4,
            5, 11, 17, 23, 23, 17, 11, 5,
            0,  0,  0,  0,  0,  0,  0, 0
        };

        const u08 InvFlap[24] =
        {
            8,  16, 24, 32, 40, 48,
            9,  17, 25, 33, 41, 49,
            10, 18, 26, 34, 42, 50,
            11, 19, 27, 35, 43, 51
        };

        const u08 Ptwist[SQ_NO] =
        {
            0,   0,  0,  0,  0,  0,  0,  0,
            47, 35, 23, 11, 10, 22, 34, 46,
            45, 33, 21,  9,  8, 20, 32, 44,
            43, 31, 19,  7,  6, 18, 30, 42,
            41, 29, 17,  5,  4, 16, 28, 40,
            39, 27, 15,  3,  2, 14, 26, 38,
            37, 25, 13,  1,  0, 12, 24, 36,
            0,   0,  0,  0,  0,  0,  0,  0
        };
        const u08 InvPtwist[48] =
        {
            52, 51, 44, 43, 36, 35, 28, 27, 20, 19, 12, 11,
            53, 50, 45, 42, 37, 34, 29, 26, 21, 18, 13, 10,
            54, 49, 46, 41, 38, 33, 30, 25, 22, 17, 14,  9,
            55, 48, 47, 40, 39, 32, 31, 24, 23, 16, 15,  8
        };

        const File FileToFile[F_NO] =
        {
            F_A, F_B, F_C, F_D, F_D, F_C, F_B, F_A
        };

        const i16 KK_idx[10][SQ_NO] =
        {
            {
                -1, -1, -1,  0,  1,  2,  3,  4,
                -1, -1, -1,  5,  6,  7,  8,  9,
                10, 11, 12, 13, 14, 15, 16, 17,
                18, 19, 20, 21, 22, 23, 24, 25,
                26, 27, 28, 29, 30, 31, 32, 33,
                34, 35, 36, 37, 38, 39, 40, 41,
                42, 43, 44, 45, 46, 47, 48, 49,
                50, 51, 52, 53, 54, 55, 56, 57
            },
            {
                58, -1, -1, -1, 59, 60, 61, 62,
                63, -1, -1, -1, 64, 65, 66, 67,
                68, 69, 70, 71, 72, 73, 74, 75,
                76, 77, 78, 79, 80, 81, 82, 83,
                84, 85, 86, 87, 88, 89, 90, 91,
                92, 93, 94, 95, 96, 97, 98, 99,
                100,101,102,103,104,105,106,107,
                108,109,110,111,112,113,114,115
            },
            {
                116,117, -1, -1, -1,118,119,120,
                121,122, -1, -1, -1,123,124,125,
                126,127,128,129,130,131,132,133,
                134,135,136,137,138,139,140,141,
                142,143,144,145,146,147,148,149,
                150,151,152,153,154,155,156,157,
                158,159,160,161,162,163,164,165,
                166,167,168,169,170,171,172,173
            },
            {
                174, -1, -1, -1,175,176,177,178,
                179, -1, -1, -1,180,181,182,183,
                184, -1, -1, -1,185,186,187,188,
                189,190,191,192,193,194,195,196,
                197,198,199,200,201,202,203,204,
                205,206,207,208,209,210,211,212,
                213,214,215,216,217,218,219,220,
                221,222,223,224,225,226,227,228
            },
            {
                229,230, -1, -1, -1,231,232,233,
                234,235, -1, -1, -1,236,237,238,
                239,240, -1, -1, -1,241,242,243,
                244,245,246,247,248,249,250,251,
                252,253,254,255,256,257,258,259,
                260,261,262,263,264,265,266,267,
                268,269,270,271,272,273,274,275,
                276,277,278,279,280,281,282,283
            },
            {
                284,285,286,287,288,289,290,291,
                292,293, -1, -1, -1,294,295,296,
                297,298, -1, -1, -1,299,300,301,
                302,303, -1, -1, -1,304,305,306,
                307,308,309,310,311,312,313,314,
                315,316,317,318,319,320,321,322,
                323,324,325,326,327,328,329,330,
                331,332,333,334,335,336,337,338
            },
            {
                -1, -1,339,340,341,342,343,344,
                -1, -1,345,346,347,348,349,350,
                -1, -1,441,351,352,353,354,355,
                -1, -1, -1,442,356,357,358,359,
                -1, -1, -1, -1,443,360,361,362,
                -1, -1, -1, -1, -1,444,363,364,
                -1, -1, -1, -1, -1, -1,445,365,
                -1, -1, -1, -1, -1, -1, -1,446
            },
            {
                -1, -1, -1,366,367,368,369,370,
                -1, -1, -1,371,372,373,374,375,
                -1, -1, -1,376,377,378,379,380,
                -1, -1, -1,447,381,382,383,384,
                -1, -1, -1, -1,448,385,386,387,
                -1, -1, -1, -1, -1,449,388,389,
                -1, -1, -1, -1, -1, -1,450,390,
                -1, -1, -1, -1, -1, -1, -1,451
            },
            {
                452,391,392,393,394,395,396,397,
                -1, -1, -1, -1,398,399,400,401,
                -1, -1, -1, -1,402,403,404,405,
                -1, -1, -1, -1,406,407,408,409,
                -1, -1, -1, -1,453,410,411,412,
                -1, -1, -1, -1, -1,454,413,414,
                -1, -1, -1, -1, -1, -1,455,415,
                -1, -1, -1, -1, -1, -1, -1,456
            },
            {
                457,416,417,418,419,420,421,422,
                -1,458,423,424,425,426,427,428,
                -1, -1, -1, -1, -1,429,430,431,
                -1, -1, -1, -1, -1,432,433,434,
                -1, -1, -1, -1, -1,435,436,437,
                -1, -1, -1, -1, -1,459,438,439,
                -1, -1, -1, -1, -1, -1,460,440,
                -1, -1, -1, -1, -1, -1, -1,461
            }
        };

        const i32 WDL_To_Map[5] ={ 1, 3, 0, 2, 0 };
        const u08 PA_Flags  [5] ={ 8, 0, 0, 0, 4 };

        i32 Binomial[5][SQ_NO];
        i32 PawnIdx[5][24];
        i32 PFactor[5][4];

        void init_indices ()
        {
            // Binomial[k-1][n] = Bin(n, k)
            for (u08 i = 0; i < 5; ++i)
            {
                for (u08 j = 0; j < SQ_NO; ++j)
                {
                    u32 numerator = j;
                    u32 denominator = 1;
                    for (u08 k = 1; k <= i; ++k)
                    {
                        numerator *= (j - k);
                        denominator *= (k + 1);
                    }
                    Binomial[i][j] = numerator / denominator;
                }
            }
            for (u08 i = 0; i < 5; ++i)
            {
                i32 s = 0;
                u08 j = 0;
                for (; j < 6; ++j)
                {
                    PawnIdx[i][j] = s;
                    s += (i == 0) ? 1 : Binomial[i - 1][Ptwist[InvFlap[j]]];
                }
                PFactor[i][0] = s;

                s = 0;
                for (; j < 12; ++j)
                {
                    PawnIdx[i][j] = s;
                    s += (i == 0) ? 1 : Binomial[i - 1][Ptwist[InvFlap[j]]];
                }
                PFactor[i][1] = s;

                s = 0;
                for (; j < 18; ++j)
                {
                    PawnIdx[i][j] = s;
                    s += (i == 0) ? 1 : Binomial[i - 1][Ptwist[InvFlap[j]]];
                }
                PFactor[i][2] = s;

                s = 0;
                for (; j < 24; ++j)
                {
                    PawnIdx[i][j] = s;
                    s += (i == 0) ? 1 : Binomial[i - 1][Ptwist[InvFlap[j]]];
                }
                PFactor[i][3] = s;
            }
        }

        u64 encode_piece (TBEntry_piece *tbep, u08 *norm, Square *pos, i32 *factor)
        {
            u08 n = tbep->num;

            if ((pos[0] & 0x04) != 0)
            {
                for (u08 i = 0; i < n; ++i)
                {
                    pos[i] = !pos[i];
                }
            }
            if ((pos[0] & 0x20) != 0)
            {
                for (u08 i = 0; i < n; ++i)
                {
                    pos[i] = ~pos[i];
                }
            }

            i32 i;
            for (i = 0; i < n; ++i)
            {
                if (OffDiag[pos[i]] != 0)
                {
                    break;
                }
            }
            if (i < (tbep->enc_type == 0 ? 3 : 2) && OffDiag[pos[i]] > 0)
            {
                for (i = 0; i < n; ++i)
                {
                    pos[i] = Square(FlipDiag[pos[i]]);
                }
            }

            u64 idx;
            i32 j;
            switch (tbep->enc_type)
            {

            case 0: /* 111 */
                i = (pos[1] > pos[0]);
                j = (pos[2] > pos[0]) + (pos[2] > pos[1]);

                if (OffDiag[pos[0]])
                    idx = Triangle[pos[0]] * 63*62 + (pos[1] - i) * 62 + (pos[2] - j);
                else
                if (OffDiag[pos[1]])
                    idx = 6*63*62 + Diag[pos[0]] * 28*62 + Lower[pos[1]] * 62 + (pos[2] - j);
                else
                if (OffDiag[pos[2]])
                    idx = 6*63*62 + 4*28*62 + (Diag[pos[0]]) * 7*28 + (Diag[pos[1]] - i) * 28 + Lower[pos[2]];
                else
                    idx = 6*63*62 + 4*28*62 + 4*7*28 + (Diag[pos[0]] * 7*6) + (Diag[pos[1]] - i) * 6 + (Diag[pos[2]] - j);

                i = 3;
                break;

            case 1: /* K3 */
                j = (pos[2] > pos[0]) + (pos[2] > pos[1]);

                idx = KK_idx[Triangle[pos[0]]][pos[1]];
                if (idx < 441)
                {
                    idx = idx + 441 * (pos[2] - j);
                }
                else
                {
                    idx = 441*62 + (idx - 441) + 21 * Lower[pos[2]];
                    if (!OffDiag[pos[2]])
                    {
                        idx -= j * 21;
                    }
                }
                i = 3;
                break;

            default: /* K2 */
                idx = KK_idx[Triangle[pos[0]]][pos[1]];
                i = 2;
                break;
            }
            idx *= factor[0];

            while (i < n)
            {
                u08 t = norm[i];
                for (j = i; j < i + t; ++j)
                {
                    for (i32 k = j + 1; k < i + t; ++k)
                    {
                        if (pos[j] > pos[k])
                        {
                            swap (pos[j], pos[k]);
                        }
                    }
                }
                i32 s = 0;
                for (i32 m = i; m < i + t; ++m)
                {
                    u08 p = pos[m], l;
                    for (l = 0, j = 0; l < i; ++l)
                    {
                        j += (p > pos[l]);
                    }
                    s += Binomial[m - i][p - j];
                }
                idx += (u64)s * factor[i];
                i += t;
            }

            return idx;
        }

        // determine file of leftmost pawn and sort pawns
        File pawn_file (TBEntry_pawn *tbep, Square *pos)
        {
            for (u08 i = 1; i < tbep->pawns[0]; ++i)
            {
                if (Flap[pos[0]] > Flap[pos[i]])
                {
                    swap (pos[0], pos[i]);
                }
            }
            return FileToFile[_file (pos[0])];
        }

        u64 encode_pawn (TBEntry_pawn *tbep, u08 *norm, Square *pos, i32 *factor)
        {
            i32 n = tbep->num;

            if ((pos[0] & 0x04) != 0)
            {
                for (u08 i = 0; i < n; ++i)
                {
                    pos[i] = !pos[i];
                }
            }
            for (u08 i = 1; i < tbep->pawns[0]; ++i)
            {
                for (u08 j = i + 1; j < tbep->pawns[0]; ++j)
                {
                    if (Ptwist[pos[i]] < Ptwist[pos[j]])
                    {
                        swap (pos[i], pos[j]);
                    }
                }
            }
            assert(tbep->pawns[0] != 0);

            u08 t = tbep->pawns[0] - 1;

            u64 idx;
            idx = PawnIdx[t][Flap[pos[0]]];
            for (i08 i = t; i > 0; --i)
            {
                idx += Binomial[t - i][Ptwist[pos[i]]];
            }
            idx *= factor[0];

            // Remaining pawns
            u08 i = tbep->pawns[0];
            t = i + tbep->pawns[1];
            if (t > i)
            {
                for (u08 j = i; j < t; ++j)
                {
                    for (u08 k = j + 1; k < t; ++k)
                    {
                        if (pos[j] > pos[k])
                        {
                            swap (pos[j], pos[k]);
                        }
                    }
                }
                i32 s = 0;
                for (i32 m = i; m < t; ++m)
                {
                    u08 p = pos[m], j, k;
                    for (k = 0, j = 0; k < i; ++k)
                    {
                        j += (p > pos[k]);
                    }
                    s += Binomial[m - i][p - j - 8];
                }
                idx += (u64)s * factor[i];
                i = t;
            }

            for (; i < n;)
            {
                t = norm[i];
                for (u08 j = i; j < i + t; ++j)
                {
                    for (u08 k = j + 1; k < i + t; ++k)
                    {
                        if (pos[j] > pos[k])
                        {
                            swap (pos[j], pos[k]);
                        }
                    }
                }
                i32 s = 0;
                for (i32 m = i; m < i + t; ++m)
                {
                    u08 p = pos[m], j, k;
                    for (k = 0, j = 0; k < i; ++k)
                    {
                        j += (p > pos[k]);
                    }
                    s += Binomial[m - i][p - j];
                }
                idx += (u64)s * factor[i];
                i += t;
            }
            return idx;
        }

        // place k like pieces on n squares
        i32 subfactor (i32 k, i32 n)
        {
            i64 f = n;
            i64 l = 1;
            for (i32 i = 1; i < k; ++i)
            {
                f *= n - i;
                l *= i + 1;
            }
            return i32(f / l);
        }

        u64 calc_factors_piece (i32 *factor, i32 num, i32 order, u08 *norm, u08 enc_type)
        {
            static i32 piv_fac[3] ={ 31332, 28056, 462 };

            i32 n = 64 - norm[0];
            u64 f = 1;
            for (i32 i = norm[0], k = 0; i < num || k == order; ++k)
            {
                if (k == order)
                {
                    factor[0] = static_cast<i32>(f);
                    f *= piv_fac[enc_type];
                }
                else
                {
                    factor[i] = static_cast<i32>(f);
                    f *= subfactor (norm[i], n);
                    n -= norm[i];
                    i += norm[i];
                }
            }
            return f;
        }

        u64 calc_factors_pawn (i32 *factor, u08 num, u08 order1, u08 order2, u08 *norm, u08 file)
        {
            u08 i = norm[0];
            if (order2 < 0x0F)
            {
                i += norm[i];
            }
            i32 n = 64 - i;
            u64 f = 1;
            for (u08 k = 0; i < num || k == order1 || k == order2; ++k)
            {
                if (k == order1)
                {
                    factor[0] = static_cast<i32>(f);
                    f *= PFactor[norm[0] - 1][file];
                }
                else
                if (k == order2)
                {
                    factor[norm[0]] = static_cast<i32>(f);
                    f *= subfactor (norm[norm[0]], 48 - norm[0]);
                }
                else
                {
                    factor[i] = static_cast<i32>(f);
                    f *= subfactor (norm[i], n);
                    n -= norm[i];
                    i += norm[i];
                }
            }
            return f;
        }

        void set_norm_piece (TBEntry_piece *tbep, u08 *norm, Piece *pieces)
        {
            for (u08 i = 0; i < tbep->num; ++i)
            {
                norm[i] = 0;
            }
            switch (tbep->enc_type)
            {
            case 0:
                norm[0] = 3;
                break;
            case 2:
                norm[0] = 2;
                break;
            default:
                norm[0] = u08(tbep->enc_type - 1);
                break;
            }
            for (u08 i = norm[0]; i < tbep->num; i += norm[i])
            {
                for (u08 j = i; j < tbep->num && pieces[j] == pieces[i]; ++j)
                {
                    norm[i]++;
                }
            }
        }

        void set_norm_pawn (TBEntry_pawn *tbep, u08 *norm, Piece *pieces)
        {
            for (u08 i = 0; i < tbep->num; ++i)
            {
                norm[i] = 0;
            }
            norm[0] = tbep->pawns[0];
            if (tbep->pawns[1] != 0)
            {
                norm[tbep->pawns[0]] = tbep->pawns[1];
            }
            for (u08 i = tbep->pawns[0] + tbep->pawns[1]; i < tbep->num; i += norm[i])
            {
                for (u08 j = i; j < tbep->num && pieces[j] == pieces[i]; ++j)
                {
                    norm[i]++;
                }
            }
        }

        void setup_piece (TBEntry_piece *tbep, u08 *data, u64 *tb_size)
        {
            i32 order;
            for (u08 i = 0; i < tbep->num; ++i)
            {
                tbep->pieces[0][i] = Piece(data[i + 1] & 0x0F);
            }
            order = data[0] & 0x0F;
            set_norm_piece (tbep, tbep->norm[0], tbep->pieces[0]);
            tb_size[0] = calc_factors_piece (tbep->factor[0], tbep->num, order, tbep->norm[0], tbep->enc_type);

            for (u08 i = 0; i < tbep->num; ++i)
            {
                tbep->pieces[1][i] = Piece(data[i + 1] >> 4);
            }
            order = data[0] >> 4;
            set_norm_piece (tbep, tbep->norm[1], tbep->pieces[1]);
            tb_size[1] = calc_factors_piece (tbep->factor[1], tbep->num, order, tbep->norm[1], tbep->enc_type);
        }

        void setup_piece_dtz (DTZEntry_piece *dtzep, u08 *data, u64 *tb_size)
        {
            i32 order;
            for (u08 i = 0; i < dtzep->num; ++i)
            {
                dtzep->pieces[i] = Piece(data[i + 1] & 0x0F);
            }
            order = data[0] & 0x0F;
            set_norm_piece (reinterpret_cast<TBEntry_piece *> (dtzep), dtzep->norm, dtzep->pieces);
            tb_size[0] = calc_factors_piece (dtzep->factor, dtzep->num, order, dtzep->norm, dtzep->enc_type);
        }

        void setup_pawn (TBEntry_pawn *tbep, u08 *data, u64 *tb_size, u08 f)
        {
            u08 j = 1 + (tbep->pawns[1] > 0 ? 1 : 0);
            u08 order1 = data[0] & 0x0F;
            u08 order2 = tbep->pawns[1] != 0 ? data[1] & 0x0F : 0x0F;
            for (u08 i = 0; i < tbep->num; ++i)
            {
                tbep->file[f].pieces[0][i] = Piece(data[i + j] & 0x0F);
            }
            set_norm_pawn (tbep, tbep->file[f].norm[0], tbep->file[f].pieces[0]);
            tb_size[0] = calc_factors_pawn (tbep->file[f].factor[0], tbep->num, order1, order2, tbep->file[f].norm[0], f);

            order1 = data[0] >> 4;
            order2 = tbep->pawns[1] != 0 ? data[1] >> 4 : 0x0F;
            for (u08 i = 0; i < tbep->num; ++i)
            {
                tbep->file[f].pieces[1][i] = Piece(data[i + j] >> 4);
            }
            set_norm_pawn (tbep, tbep->file[f].norm[1], tbep->file[f].pieces[1]);
            tb_size[1] = calc_factors_pawn (tbep->file[f].factor[1], tbep->num, order1, order2, tbep->file[f].norm[1], f);
        }

        void setup_pawn_dtz (DTZEntry_pawn *dtzep, u08 *data, u64 *tb_size, u08 f)
        {
            u08 j = 1 + (dtzep->pawns[1] > 0 ? 1 : 0);
            u08 order1 = data[0] & 0x0F;
            u08 order2 = dtzep->pawns[1] != 0 ? data[1] & 0x0F : 0x0F;
            for (u08 i = 0; i < dtzep->num; ++i)
            {
                dtzep->file[f].pieces[i] = Piece(data[i + j] & 0x0F);
            }
            set_norm_pawn (reinterpret_cast<TBEntry_pawn *> (dtzep), dtzep->file[f].norm, dtzep->file[f].pieces);
            tb_size[0] = calc_factors_pawn (dtzep->file[f].factor, dtzep->num, order1, order2, dtzep->file[f].norm, f);
        }

        void calc_symlen (PairsData *pairs_data, i32 s, char *tmp)
        {
            i32 s1, s2;

            u08 *w = pairs_data->sympat + 3 * s;
            s2 = (w[2] << 4) | (w[1] >> 4);
            if (s2 == 0x0FFF)
            {
                pairs_data->symlen[s] = 0;
            }
            else
            {
                s1 = ((w[1] & 0x0F) << 8) | w[0];
                if (!tmp[s1]) calc_symlen (pairs_data, s1, tmp);
                if (!tmp[s2]) calc_symlen (pairs_data, s2, tmp);
                pairs_data->symlen[s] = u08(pairs_data->symlen[s1] + pairs_data->symlen[s2] + 1);
            }
            tmp[s] = 1;
        }

        u16 read_u16 (u08 *d)
        {
            return u16(d[0] | (d[1] << 8));
        }

        u32 read_u32 (u08 *d)
        {
            return u32(d[0] | (d[1] << 8) | (d[2] << 16) | (d[3] << 24));
        }

        PairsData* setup_pairs (u08 *pairs_data, u64 tb_size, u64 *size, u08 **next, u08 *flags, bool wdl)
        {
            PairsData *p_data;
            *flags = pairs_data[0];
            if ((pairs_data[0] & 0x80) != 0)
            {
                p_data = reinterpret_cast<PairsData *> (malloc (sizeof (PairsData)));
                p_data->idxbits = 0;
                if (wdl)
                {
                    p_data->min_len = pairs_data[1];
                }
                else
                {
                    p_data->min_len = 0;
                }
                *next = pairs_data + 2;
                size[0] = size[1] = size[2] = 0;
                return p_data;
            }

            i32 blocksize = pairs_data[1];
            i32 idxbits = pairs_data[2];
            i32 real_num_blocks = read_u32 (&pairs_data[4]);
            i32 num_blocks = real_num_blocks + *(u08 *)(&pairs_data[3]);
            i32 max_len = pairs_data[8];
            i32 min_len = pairs_data[9];
            i32 h = max_len - min_len + 1;
            i32 num_syms = read_u16 (&pairs_data[10 + 2 * h]);
            p_data = reinterpret_cast<PairsData *> (malloc (sizeof (PairsData) + (h - 1) * sizeof (base_t) + num_syms));
            p_data->blocksize = blocksize;
            p_data->idxbits = idxbits;
            p_data->offset = (u16 *)(&pairs_data[10]);
            p_data->symlen = ((u08 *)p_data) + sizeof (PairsData) + (h - 1) * sizeof (base_t);
            p_data->sympat = &pairs_data[12 + 2 * h];
            p_data->min_len = min_len;
            *next = &pairs_data[12 + 2 * h + 3 * num_syms + (num_syms & 1)];

            u64 num_indices = (tb_size + (1ULL << idxbits) - 1) >> idxbits;
            size[0] = 6ULL * num_indices;
            size[1] = 2ULL * num_blocks;
            size[2] = (1ULL << blocksize) * real_num_blocks;

            // char tmp[num_syms];
            char tmp[4096];
            for (i32 i = 0; i < num_syms; ++i)
            {
                tmp[i] = 0;
            }
            for (i32 i = 0; i < num_syms; ++i)
            {
                if (tmp[i] == 0)
                {
                    calc_symlen (p_data, i, tmp);
                }
            }
            p_data->base[h - 1] = 0;
            for (i32 i = h - 2; i >= 0; --i)
            {
                p_data->base[i] = (p_data->base[i + 1] + read_u16 ((u08 *)(p_data->offset + i)) - read_u16 ((u08 *)(p_data->offset + i + 1))) / 2;
            }
            for (i32 i = 0; i < h; ++i)
            {
                p_data->base[i] <<= 64 - (min_len + i);
            }
            p_data->offset -= p_data->min_len;

            return p_data;
        }

        bool init_table_wdl (TBEntry *tbe, const string &filename)
        {
            u08 *next;
            u64 tb_size[8];
            u64 size[8 * 3];
            u08 flags;

            // first map the table into memory
            tbe->data = map_file (filename, WDL_Suffix, &tbe->mapping);
            if (tbe->data == nullptr)
            {
                std::cout << "Could not find " << filename << WDL_Suffix << std::endl;
                return false;
            }

            u08 *data = (u08 *)tbe->data;
            if (   data[0] != WDL_Magic[0]
                || data[1] != WDL_Magic[1]
                || data[2] != WDL_Magic[2]
                || data[3] != WDL_Magic[3]
               )
            {
                std::cout << "Corrupted table." << std::endl;
                unmap_file (tbe->data, tbe->mapping);
                tbe->data = nullptr;
                return false;
            }

            u08 split = (data[4] & 0x01);
            u08 files = (data[4] & 0x02) != 0 ? 4 : 1;

            data += 5;

            if (tbe->has_pawns)
            {
                auto *tbep = reinterpret_cast<TBEntry_pawn *> (tbe);
                u08 s = 1 + (tbep->pawns[1] > 0);
                u08 f;
                for (f = 0; f < 4; ++f)
                {
                    setup_pawn (reinterpret_cast<TBEntry_pawn *> (tbep), data, &tb_size[2 * f], f);
                    data += tbep->num + s;
                }
                data += ((uintptr_t)data) & 0x01;

                for (f = 0; f < files; ++f)
                {
                    tbep->file[f].precomp[0] = setup_pairs (data, tb_size[2 * f], &size[6 * f], &next, &flags, true);
                    data = next;
                    if (split != 0)
                    {
                        tbep->file[f].precomp[1] = setup_pairs (data, tb_size[2 * f + 1], &size[6 * f + 3], &next, &flags, true);
                        data = next;
                    }
                    else
                    {
                        tbep->file[f].precomp[1] = nullptr;
                    }
                }
                for (f = 0; f < files; ++f)
                {
                    tbep->file[f].precomp[0]->table_index = (char *)data;
                    data += size[6 * f];
                    if (split != 0)
                    {
                        tbep->file[f].precomp[1]->table_index = (char *)data;
                        data += size[6 * f + 3];
                    }
                }
                for (f = 0; f < files; ++f)
                {
                    tbep->file[f].precomp[0]->table_size = (u16 *)data;
                    data += size[6 * f + 1];
                    if (split != 0)
                    {
                        tbep->file[f].precomp[1]->table_size = (u16 *)data;
                        data += size[6 * f + 4];
                    }
                }
                for (f = 0; f < files; ++f)
                {
                    data = (u08 *)((((uintptr_t)data) + 0x3F) & ~0x3F);
                    tbep->file[f].precomp[0]->data = data;
                    data += size[6 * f + 2];
                    if (split != 0)
                    {
                        data = (u08 *)((((uintptr_t)data) + 0x3F) & ~0x3F);
                        tbep->file[f].precomp[1]->data = data;
                        data += size[6 * f + 5];
                    }
                }
            }
            else
            {
                auto *tbep = reinterpret_cast<TBEntry_piece *> (tbe);
                setup_piece (tbep, data, &tb_size[0]);
                data += tbep->num + 1;
                data += ((uintptr_t)data) & 0x01;

                tbep->precomp[0] = setup_pairs (data, tb_size[0], &size[0], &next, &flags, true);
                data = next;
                if (split != 0)
                {
                    tbep->precomp[1] = setup_pairs (data, tb_size[1], &size[3], &next, &flags, true);
                    data = next;
                }
                else
                {
                    tbep->precomp[1] = nullptr;
                }
                tbep->precomp[0]->table_index = (char *)data;
                data += size[0];
                if (split != 0)
                {
                    tbep->precomp[1]->table_index = (char *)data;
                    data += size[3];
                }

                tbep->precomp[0]->table_size = (u16 *)data;
                data += size[1];
                if (split != 0)
                {
                    tbep->precomp[1]->table_size = (u16 *)data;
                    data += size[4];
                }

                data = (u08 *)((((uintptr_t)data) + 0x3F) & ~0x3F);
                tbep->precomp[0]->data = data;
                data += size[2];
                if (split != 0)
                {
                    data = (u08 *)((((uintptr_t)data) + 0x3F) & ~0x3F);
                    tbep->precomp[1]->data = data;
                }
            }
            return true;
        }

        bool init_table_dtz (TBEntry *tbe)
        {
            u08 *data = (u08 *)tbe->data;
            if (data == nullptr)
            {
                return false;
            }

            if (   data[0] != DTZ_Magic[0]
                || data[1] != DTZ_Magic[1]
                || data[2] != DTZ_Magic[2]
                || data[3] != DTZ_Magic[3]
               )
            {
                std::cout << "Corrupted table." << std::endl;
                return false;
            }

            u64 tb_size[4];
            u64 size[4 * 3];

            u08 files = (data[4] & 0x02) != 0 ? 4 : 1;

            data += 5;

            if (tbe->has_pawns)
            {
                auto *dtzep = reinterpret_cast<DTZEntry_pawn *> (tbe);
                u08 s = 1 + (dtzep->pawns[1] > 0);
                for (u08 f = 0; f < 4; ++f)
                {
                    setup_pawn_dtz (dtzep, data, &tb_size[f], f);
                    data += dtzep->num + s;
                }
                data += ((uintptr_t)data) & 0x01;
                
                u08 *next;
                for (u08 f = 0; f < files; ++f)
                {
                    dtzep->file[f].precomp = setup_pairs (data, tb_size[f], &size[3 * f], &next, &(dtzep->flags[f]), false);
                    data = next;
                }

                dtzep->map = data;
                for (u08 f = 0; f < files; ++f)
                {
                    if (dtzep->flags[f] & 2)
                    {
                        for (u08 i = 0; i < 4; ++i)
                        {
                            dtzep->map_idx[f][i] = static_cast<u16>(data + 1 - dtzep->map);
                            data += 1 + data[0];
                        }
                    }
                }
                data += ((uintptr_t)data) & 0x01;

                for (u08 f = 0; f < files; ++f)
                {
                    dtzep->file[f].precomp->table_index = (char *)data;
                    data += size[3 * f];
                }
                for (u08 f = 0; f < files; ++f)
                {
                    dtzep->file[f].precomp->table_size = (u16 *)data;
                    data += size[3 * f + 1];
                }
                for (u08 f = 0; f < files; ++f)
                {
                    data = (u08 *)((((uintptr_t)data) + 0x3F) & ~0x3F);
                    dtzep->file[f].precomp->data = data;
                    data += size[3 * f + 2];
                }
            }
            else
            {
                auto *dtzep = reinterpret_cast<DTZEntry_piece *> (tbe);
                setup_piece_dtz (dtzep, data, &tb_size[0]);
                data += dtzep->num + 1;
                data += ((uintptr_t)data) & 0x01;

                u08 *next;
                dtzep->precomp = setup_pairs (data, tb_size[0], &size[0], &next, &(dtzep->flags), false);
                data = next;

                dtzep->map = data;
                if ((dtzep->flags & 2) != 0)
                {
                    for (u08 i = 0; i < 4; ++i)
                    {
                        dtzep->map_idx[i] = static_cast<u16>(data + 1 - dtzep->map);
                        data += 1 + data[0];
                    }
                    data += ((uintptr_t)data) & 0x01;
                }

                dtzep->precomp->table_index = (char *)data;
                data += size[0];

                dtzep->precomp->table_size = (u16 *)data;
                data += size[1];

                data = (u08 *)((((uintptr_t)data) + 0x3F) & ~0x3F);
                dtzep->precomp->data = data;
                data += size[2];
            }
            return true;
        }

        template<bool LittleEndian>
        u08 decompress_pairs (PairsData *pairs_data, u64 idx)
        {
            if (pairs_data->idxbits == 0)
            {
                return u08(pairs_data->min_len);
            }
            u32 mainidx = static_cast<u32>(idx >> pairs_data->idxbits);
            i32 litidx = (idx & ((1ULL << pairs_data->idxbits) - 1)) - (1ULL << (pairs_data->idxbits - 1));
            u32 block = *(u32 *)(pairs_data->table_index + 6 * mainidx);
            if (!LittleEndian)
            {
                block = BSWAP32 (block);
            }
            u16 idxOffset = *(u16 *)(pairs_data->table_index + 6 * mainidx + 4);
            if (!LittleEndian)
            {
                idxOffset = u16 ((idxOffset << 8) | (idxOffset >> 8));
            }
            litidx += idxOffset;

            if (litidx < 0)
            {
                do
                {
                    litidx += pairs_data->table_size[--block] + 1;
                } while (litidx < 0);
            }
            else
            {
                while (litidx > pairs_data->table_size[block])
                {
                    litidx -= pairs_data->table_size[block++] + 1;
                }
            }

            u32 *ptr = (u32 *)(pairs_data->data + (block << pairs_data->blocksize));

            i32 m = pairs_data->min_len;
            u16 *offset = pairs_data->offset;
            base_t *base = pairs_data->base - m;
            u08 *symlen = pairs_data->symlen;
            i32 sym, bitcnt;

            u64 code = *((u64 *)ptr);
            if (LittleEndian)
            {
                code = BSWAP64 (code);
            }
            ptr += 2;
            bitcnt = 0; // number of "empty bits" in code
            while (true)
            {
                i32 l = m;
                while (code < base[l])
                {
                    ++l;
                }
                sym = offset[l];
                if (!LittleEndian)
                {
                    sym = ((sym & 0xFF) << 8) | (sym >> 8);
                }
                sym += static_cast<i32>((code - base[l]) >> (64 - l));
                if (litidx < (i32)symlen[sym] + 1)
                {
                    break;
                }
                litidx -= (i32)symlen[sym] + 1;
                code <<= l;
                bitcnt += l;
                if (bitcnt >= 32)
                {
                    bitcnt -= 32;
                    u32 tmp = *ptr++;
                    if (LittleEndian)
                    {
                        tmp = BSWAP32 (tmp);
                    }
                    code |= (u64)tmp << bitcnt;
                }
            }

            u08 *sympat = pairs_data->sympat;
            while (symlen[sym] != 0)
            {
                u08 *w = sympat + (3 * sym);
                i32 s1 = ((w[1] & 0x0F) << 8) | w[0];
                if (litidx < (i32)symlen[s1] + 1)
                {
                    sym = s1;
                }
                else
                {
                    litidx -= (i32)symlen[s1] + 1;
                    sym = (w[2] << 4) | (w[1] >> 4);
                }
            }
            return sympat[3 * sym];
        }

        void load_dtz_table (const string &filename, u64 key1, u64 key2)
        {
            DTZ_Table[0].key1 = key1;
            DTZ_Table[0].key2 = key2;
            DTZ_Table[0].tbe  = nullptr;

            // find corresponding WDL entry
            auto *tbhe = TB_Hash[key1 >> (64 - TBHashBits)];
            u08 i;
            for (i = 0; i < MaxHash; ++i)
            {
                if (tbhe[i].key == key1)
                {
                    break;
                }
            }
            if (i == MaxHash)
            {
                return;
            }

            auto *tbe = tbhe[i].tbe;
            auto *ptbe = reinterpret_cast<TBEntry *> (malloc (tbe->has_pawns ? sizeof (DTZEntry_pawn) : sizeof (DTZEntry_piece)));

            ptbe->data = map_file (filename, DTZ_Suffix, &ptbe->mapping);
            ptbe->key = tbe->key;
            ptbe->num = tbe->num;
            ptbe->symmetric = tbe->symmetric;
            ptbe->has_pawns = tbe->has_pawns;
            if (ptbe->has_pawns)
            {
                auto *dtzep = reinterpret_cast<DTZEntry_pawn *> (ptbe);
                dtzep->pawns[0] = reinterpret_cast<TBEntry_pawn *> (tbe)->pawns[0];
                dtzep->pawns[1] = reinterpret_cast<TBEntry_pawn *> (tbe)->pawns[1];
            }
            else
            {
                auto *dtzep = reinterpret_cast<DTZEntry_piece *> (ptbe);
                dtzep->enc_type = reinterpret_cast<TBEntry_piece *> (tbe)->enc_type;
            }
            if (init_table_dtz (ptbe))
            {
                DTZ_Table[0].tbe = ptbe;
            }
            else
            {
                free (ptbe);
            }
        }

        void free_wdl_entry (TBEntry *tbe)
        {
            unmap_file (tbe->data, tbe->mapping);
            if (tbe->has_pawns)
            {
                auto *tbep = reinterpret_cast<TBEntry_pawn *> (tbe);
                for (u08 f = 0; f < 4; ++f)
                {
                    if (tbep->file[f].precomp[0] != nullptr)
                    {
                        free (tbep->file[f].precomp[0]);
                    }
                    if (tbep->file[f].precomp[1] != nullptr)
                    {
                        free (tbep->file[f].precomp[1]);
                    }
                }
            }
            else
            {
                auto *tbep = reinterpret_cast<TBEntry_piece *> (tbe);
                if (tbep->precomp[0] != nullptr)
                {
                    free (tbep->precomp[0]);
                }
                if (tbep->precomp[1] != nullptr)
                {
                    free (tbep->precomp[1]);
                }
            }
        }

        void free_dtz_entry (TBEntry *tbe)
        {
            unmap_file (tbe->data, tbe->mapping);
            if (tbe->has_pawns)
            {
                auto *dtzep = reinterpret_cast<DTZEntry_pawn *> (tbe);
                for (u08 f = 0; f < 4; ++f)
                {
                    if (dtzep->file[f].precomp != nullptr)
                    {
                        free (dtzep->file[f].precomp);
                    }
                }
            }
            else
            {
                auto *dtzep = reinterpret_cast<DTZEntry_piece *> (tbe);
                if (dtzep->precomp != nullptr)
                {
                    free (dtzep->precomp);
                }
            }
            free (tbe);
        }

        void free_entries ()
        {
            for (u16 pc = 0; pc < TB_PieceCount; ++pc)
            {
                auto *tbe = reinterpret_cast<TBEntry *> (&TB_Piece[pc]);
                if (tbe != nullptr)
                {
                    free_wdl_entry (tbe);
                }
            }
            for (u16 pc = 0; pc < TB_PawnCount; ++pc)
            {
                auto *tbe = reinterpret_cast<TBEntry *> (&TB_Pawn[pc]);
                if (tbe != nullptr)
                {
                    free_wdl_entry (tbe);
                }
            }
            for (u08 i = 0; i < DTZ_Entries; ++i)
            {
                auto *tbe = DTZ_Table[i].tbe;
                if (tbe != nullptr)
                {
                    free_dtz_entry (tbe);
                }
            }
        }

        // <---Core

        // Given a position with 6 or fewer pieces, produce a text string of the form KQPvKRP,
        // where "KQP" represents the white pieces if mirror == false and the black pieces if mirror == true.
        string get_filename (const Position &pos, bool mirror)
        {
            assert(pos.count<KING> (WHITE) == 1
                && pos.count<KING> (BLACK) == 1);
            
            string filename = "";
            Color color = mirror ? BLACK : WHITE;
            for (auto pt = KING; pt >= PAWN; --pt)
            {
                for (auto i = pos.count (color, pt); i > 0; --i)
                {
                    filename += PieceChar[pt];
                }
            }
            filename += 'v';
            color = ~color;
            for (auto pt = KING; pt >= PAWN; --pt)
            {
                for (auto i = pos.count (color, pt); i > 0; --i)
                {
                    filename += PieceChar[pt];
                }
            }
            return filename;
        }


        bool is_little_endian ()
        {
            union
            {
                i32 integer;
                char character[sizeof (i32)];
            } x;
            x.integer = 1;
            return x.character[0] == 1;
        }

        u08 decompress_pairs (PairsData *pairs_data, u64 idx)
        {
            static const bool IsLittleEndian = is_little_endian ();
            return IsLittleEndian ?
                decompress_pairs<true > (pairs_data, idx) :
                decompress_pairs<false> (pairs_data, idx);
        }

        // Given a position, produce a 64-bit material signature key.
        // If the engine supports such a key, it should equal the engine's key.
        Key calc_key (Position &pos, bool mirror)
        {
            assert(pos.count<KING> (WHITE) == 1
                && pos.count<KING> (BLACK) == 1);

            Key key = 0;
            auto color = mirror ? BLACK : WHITE;
            for (auto pt = PAWN; pt <= KING; ++pt)
            {
                for (auto i = pos.count (color, pt); i > 0; --i)
                {
                    key ^= Zob.piece_square[WHITE][pt][i - 1];
                }
            }
            color = ~color;
            for (auto pt = PAWN; pt <= KING; ++pt)
            {
                for (auto i = pos.count (color, pt); i > 0; --i)
                {
                    key ^= Zob.piece_square[BLACK][pt][i - 1];
                }
            }
            return key;
        }

        // probe_wdl_table()
        Value probe_wdl_table (Position &pos, i32 &success)
        {
            // Obtain the position's material signature key.
            Key matl_key = pos.matl_key ();

            // Test for KvK.
            if (matl_key == (Zob.piece_square[WHITE][KING][0] ^ Zob.piece_square[BLACK][KING][0]))
            {
                return VALUE_ZERO;
            }

            auto *tbhe = TB_Hash[matl_key >> (64 - TBHashBits)];

            i08 i;
            for (i = 0; i < MaxHash; ++i)
            {
                if (tbhe[i].key == matl_key)
                {
                    break;
                }
            }
            if (i == MaxHash)
            {
                success = 0;
                return VALUE_ZERO;
            }

            auto *tbe = tbhe[i].tbe;
            if (!tbe->ready)
            {
                LOCK (TB_mutex);
                if (!tbe->ready)
                {
                    bool mirror = tbe->key != matl_key;
                    string filename = get_filename (pos, mirror);
                    if (init_table_wdl (tbe, filename))
                    {
                    }
                    else
                    {
                        tbhe[i].key = 0;
                        success = 0;
                        UNLOCK (TB_mutex);
                        return VALUE_ZERO;
                    }
                    // Memory barrier to ensure tbe->ready = 1 is not reordered.
#if defined(_MSC_VER)
                    _ReadWriteBarrier ();
#else
                    __asm__ __volatile__ ("" ::: "memory");
#endif
                    tbe->ready = true;
                }
                UNLOCK (TB_mutex);
            }

            Color side = tbe->symmetric ?
                            WHITE : matl_key == tbe->key ?
                                pos.active () : ~pos.active ();
            u16 res;
            Square sq[TB_PieceLimit];
            std::memset (sq, SQ_NO, TB_PieceLimit);
            // sq[i] is to contain the square 0-63 (A1-H8) for a piece of type
            // pc[i] ^ opp, where 1 = white pawn, ..., 14 = black king.
            // Pieces of the same type are guaranteed to be consecutive.
            if (tbe->has_pawns)
            {
                auto *tbep = reinterpret_cast<TBEntry_pawn *> (tbe);
                Piece *pc;
                
                pc = tbep->file[0].pieces[0];
                Bitboard bb = pos.pieces (side == pos.active () ? color (pc[0]) : ~color (pc[0]), tb_ptype (pc[0]));
                u08 s = 0;
                do
                {
                    sq[std::min (s++, u08(TB_PieceLimit-1))] = side == pos.active () ? pop_lsq (bb) : ~pop_lsq (bb);
                } while (bb != 0);

                File f = pawn_file (tbep, sq);
                pc = tbep->file[f].pieces[side];
                while (s < tbep->num)
                {
                    bb = pos.pieces (side == pos.active () ? color (pc[s]) : ~color (pc[s]), tb_ptype (pc[s]));
                    do
                    {
                        sq[std::min (s++, u08(TB_PieceLimit-1))] = side == pos.active () ? pop_lsq (bb) : ~pop_lsq (bb);
                    } while (bb != 0);
                }
                u64 idx = encode_pawn (tbep, tbep->file[f].norm[side], sq, tbep->file[f].factor[side]);
                res = decompress_pairs (tbep->file[f].precomp[side], idx);
            }
            else
            {
                auto *tbep = reinterpret_cast<TBEntry_piece *> (tbe);
                Piece *pc = tbep->pieces[side];
                for (u08 s = 0; s < tbep->num;)
                {
                    Bitboard bb = pos.pieces (side == pos.active () ? color (pc[s]) : ~color (pc[s]), tb_ptype (pc[s]));
                    do
                    {
                        sq[std::min (s++, u08(TB_PieceLimit-1))] = pop_lsq (bb);
                    } while (bb != 0);
                }
                u64 idx = encode_piece (tbep, tbep->norm[side], sq, tbep->factor[side]);
                res = decompress_pairs (tbep->precomp[side], idx);
            }
            return Value(i32(res) - 2);
        }
        // probe_dtz_table()
        Value probe_dtz_table (Position &pos, i32 wdl, i32 &success)
        {
            // Obtain the position's material signature key.
            Key matl_key = pos.matl_key ();

            if (   DTZ_Table[0].key1 != matl_key
                && DTZ_Table[0].key2 != matl_key
               )
            {
                u08 i;
                for (i = 1; i < DTZ_Entries; ++i)
                {
                    if (DTZ_Table[i].key1 == matl_key)
                    {
                        break;
                    }
                }
                if (i < DTZ_Entries)
                {
                    auto dtzte = DTZ_Table[i];
                    for (; i > 0; --i)
                    {
                        DTZ_Table[i] = DTZ_Table[i - 1];
                    }
                    DTZ_Table[0] = dtzte;
                }
                else
                {
                    auto *tbhe = TB_Hash[matl_key >> (64 - TBHashBits)];
                    for (i = 0; i < MaxHash; ++i)
                    {
                        if (tbhe[i].key == matl_key)
                        {
                            break;
                        }
                    }
                    if (i == MaxHash)
                    {
                        success = 0;
                        return VALUE_ZERO;
                    }

                    auto *tbe = tbhe[i].tbe;
                    
                    if (DTZ_Table[DTZ_Entries - 1].tbe != nullptr)
                    {
                        free_dtz_entry (DTZ_Table[DTZ_Entries-1].tbe);
                    }
                    for (i = DTZ_Entries - 1; i > 0; --i)
                    {
                        DTZ_Table[i] = DTZ_Table[i - 1];
                    }

                    bool mirror = tbe->key != matl_key;
                    string filename = get_filename (pos, mirror);
                    load_dtz_table (filename, calc_key (pos, mirror), calc_key (pos, !mirror));
                }
            }

            auto *tbe = DTZ_Table[0].tbe;
            if (tbe == nullptr)
            {
                success = 0;
                return VALUE_ZERO;
            }

            Color side = tbe->symmetric ?
                            WHITE : matl_key == tbe->key ?
                                pos.active () : ~pos.active ();
            u16 res;
            Square sq[TB_PieceLimit];
            std::memset (sq, SQ_NO, TB_PieceLimit);
            if (tbe->has_pawns)
            {
                auto *dtzep = reinterpret_cast<DTZEntry_pawn *> (tbe);
                Piece p = side == pos.active () ? dtzep->file[0].pieces[0] : ~dtzep->file[0].pieces[0];
                Bitboard bb = pos.pieces (color (p), tb_ptype (p));
                u08 s = 0;
                do
                {
                    sq[std::min (s++, u08(TB_PieceLimit-1))] = side == pos.active () ? pop_lsq (bb) : ~pop_lsq (bb);
                } while (bb != 0);
                
                File f = pawn_file (reinterpret_cast<TBEntry_pawn *> (dtzep), sq);
                if ((dtzep->flags[f] & 1) != side)
                {
                    success = -1;
                    return VALUE_ZERO;
                }
                Piece *pc = dtzep->file[f].pieces;
                while (s < dtzep->num)
                {
                    bb = pos.pieces (side == pos.active () ? color (pc[s]) : ~color (pc[s]), tb_ptype (pc[s]));
                    do
                    {
                        sq[std::min (s++, u08(TB_PieceLimit-1))] = side == pos.active () ? pop_lsq (bb) : ~pop_lsq (bb);
                    } while (bb != 0);
                }
                u64 idx = encode_pawn (reinterpret_cast<TBEntry_pawn *> (dtzep), dtzep->file[f].norm, sq, dtzep->file[f].factor);
                res = decompress_pairs (dtzep->file[f].precomp, idx);

                if ((dtzep->flags[f] & 2) != 0)
                {
                    res = dtzep->map[dtzep->map_idx[f][WDL_To_Map[wdl + 2]] + res];
                }
                if ((dtzep->flags[f] & PA_Flags[wdl + 2]) == 0 || (wdl & 1) != 0)
                {
                    res *= 2;
                }
            }
            else
            {
                auto *dtzep = reinterpret_cast<DTZEntry_piece *> (tbe);
                if ((dtzep->flags & 1) != side && !dtzep->symmetric)
                {
                    success = -1;
                    return VALUE_ZERO;
                }
                Piece *pc = dtzep->pieces;
                for (u08 s = 0; s < dtzep->num;)
                {
                    Bitboard bb = pos.pieces (side == pos.active () ? color (pc[s]) : ~color (pc[s]), tb_ptype (pc[s]));
                    do
                    {
                        sq[std::min (s++, u08(TB_PieceLimit-1))] = pop_lsq (bb);
                    } while (bb != 0);
                }
                u64 idx = encode_piece (reinterpret_cast<TBEntry_piece *> (dtzep), dtzep->norm, sq, dtzep->factor);
                res = decompress_pairs (dtzep->precomp, idx);

                if ((dtzep->flags & 2) != 0)
                {
                    res = dtzep->map[dtzep->map_idx[WDL_To_Map[wdl + 2]] + res];
                }
                if ((dtzep->flags & PA_Flags[wdl + 2]) == 0 || (wdl & 1) != 0)
                {
                    res *= 2;
                }
            }
            return Value(res);
        }

        // Add underpromotion captures to list of captures.
        ValMove* generate_underprom_cap (ValMove *beg_move, ValMove *end_move, const Position &pos)
        {
            ValMove *extra = end_move;
            for (ValMove *cur_move = beg_move; cur_move < end_move; ++cur_move)
            {
                auto move = cur_move->move;
                if (   mtype (move) == PROMOTE
                    && !pos.empty (dst_sq (move))
                   )
                {
                    (*extra++).move = Move(move - (NIHT << 12));
                    (*extra++).move = Move(move - (BSHP << 12));
                    (*extra++).move = Move(move - (ROOK << 12));
                }
            }
            return extra;
        }

        Value probe_ab (Position &pos, Value alfa, Value beta, i32 &success)
        {
            ValMove moves[MaxMoves];
            ValMove *end_move;
            
            // Generate (at least) all legal non-ep captures including (under)promotions.
            // It is OK to generate more, as long as they are filtered out below.
            if (pos.checkers () == 0)
            {
                end_move = generate<CAPTURE> (moves, pos);
                // Since underpromotion captures are not included, we need to add them.
                end_move = generate_underprom_cap (moves, end_move, pos);
            }
            else
            {
                end_move = generate<EVASION> (moves, pos);
            }

            Value v;
            CheckInfo ci (pos);
            
            for (ValMove *cur_move = moves; cur_move < end_move; ++cur_move)
            {
                auto move = cur_move->move;
                if (   !pos.capture (move)
                    || mtype (move) == ENPASSANT
                    || !pos.legal (move, ci.pinneds)
                   )
                {
                    continue;
                }

                StateInfo si;
                pos.do_move (move, si, pos.gives_check (move, ci));
                v = -probe_ab (pos, -beta, -alfa, success);
                pos.undo_move ();
                if (success == 0) return VALUE_ZERO;
                if (alfa < v)
                {
                    if (v >= beta)
                    {
                        success = 2;
                        return v;
                    }
                    alfa = v;
                }
            }

            v = probe_wdl_table (pos, success);
            if (success == 0) return VALUE_ZERO;
            if (alfa >= v)
            {
                success = 1 + (alfa > VALUE_ZERO);
                return alfa;
            }
            else
            {
                success = 1;
                return v;
            }
        }
        // This routine treats a position with en-passant captures as one without.
        Value probe_dtz_no_ep (Position &pos, i32 &success)
        {
            Value wdl = probe_ab (pos, Value(-2), Value(2), success);
            if (success == 0) return VALUE_ZERO;

            if (wdl == 0) return VALUE_ZERO;

            if (success == 2)
            {
                return Value(wdl == 2 ? 1 : 101);
            }

            ValMove moves[MaxMoves];
            ValMove *end_move = nullptr;
            
            CheckInfo ci (pos);

            if (wdl > 0)
            {
                // Generate at least all legal non-capturing pawn moves
                // including non-capturing promotions.
                end_move = pos.checkers () == 0 ?
                    generate<RELAX  > (moves, pos) :
                    generate<EVASION> (moves, pos);

                for (ValMove *cur_move = moves; cur_move < end_move; ++cur_move)
                {
                    auto move = cur_move->move;
                    if (   ptype (pos[org_sq (move)]) != PAWN
                        || pos.capture (move)
                        || !pos.legal (move, ci.pinneds)
                       )
                    {
                        continue;
                    }

                    StateInfo si;
                    pos.do_move (move, si, pos.gives_check (move, ci));
                    Value v = -probe_ab (pos, Value(-2), Value(-wdl + 1), success);
                    pos.undo_move ();
                    if (success == 0) return VALUE_ZERO;
                    if (v == wdl)
                    {
                        return Value(v == 2 ? 1 : 101);
                    }
                }
            }

            i32 dtz = 1 + probe_dtz_table (pos, wdl, success);
            if (success >= 0)
            {
                if ((wdl & 1) != 0)
                {
                    dtz += 100;
                }
                return Value(wdl >= 0 ? dtz : -dtz);
            }

            if (wdl > 0)
            {
                Value best_value = +VALUE_INFINITE;
                for (ValMove *cur_move = moves; cur_move < end_move; ++cur_move)
                {
                    auto move = cur_move->move;
                    if (   pos.capture (move)
                        || ptype (pos[org_sq (move)]) == PAWN
                        || !pos.legal (move, ci.pinneds)
                       )
                    {
                        continue;
                    }

                    StateInfo si;
                    pos.do_move (move, si, pos.gives_check (move, ci));
                    Value v = -probe_dtz (pos, success);
                    pos.undo_move ();
                    if (success == 0) return VALUE_ZERO;
                    if (v > VALUE_ZERO)
                    {
                        if (best_value > v + 1)
                        {
                            best_value = v + 1;
                        }
                    }
                }
                return best_value;
            }
            else
            {
                Value best_value = Value(-1);
                end_move = pos.checkers () == 0 ?
                    generate<RELAX  > (moves, pos) :
                    generate<EVASION> (moves, pos);

                for (ValMove *cur_move = moves; cur_move < end_move; ++cur_move)
                {
                    Value v;
                    auto move = cur_move->move;
                    if (!pos.legal (move, ci.pinneds)) continue;
                    StateInfo si;
                    pos.do_move (move, si, pos.gives_check (move, ci));
                    if (si.clock_ply == 0)
                    {
                        if (wdl == -2)
                        {
                            v = Value(-1);
                        }
                        else
                        {
                            v = probe_ab (pos, Value(1), Value(2), success);
                            v = (v == 2) ? VALUE_ZERO : Value(-101);
                        }
                    }
                    else
                    {
                        v = -probe_dtz (pos, success) - 1;
                    }
                    pos.undo_move ();
                    if (success == 0) return VALUE_ZERO;
                    if (best_value > v)
                    {
                        best_value = v;
                    }
                }
                return best_value;
            }
        }

        // Check whether there has been at least one repetition of positions
        // since the last capture or pawn move.
        bool has_repeated (StateInfo *si)
        {
            while (true)
            {
                i32 i = 4, e = std::min (si->clock_ply, si->null_ply);
                if (e < i)
                {
                    return false;
                }
                StateInfo *psi = si->ptr->ptr;
                do
                {
                    psi = psi->ptr->ptr;
                    if (psi->posi_key == si->posi_key)
                    {
                        return true;
                    }
                    i += 2;
                } while (i <= e);
                si = si->ptr;
            }
        }

        Value Wdl_to_Dtz[5] =
        {
            Value(-1),
            Value(-101),
            Value(0),
            Value(+101),
            Value(+1)
        };

        Value Wdl_to_Value[5] =
        {
            -VALUE_MATE + i32(MaxPlies + 1),
            VALUE_ZERO - 2,
            VALUE_ZERO,
            VALUE_ZERO + 2,
            +VALUE_MATE - i32(MaxPlies + 1),
        };
    }

    // Probe the DTZ table for a particular position.
    // If success != 0, the probe was successful.
    // The return value is from the point of view of the side to move:
    //         n < -100 : loss, but draw under 50-move rule
    // -100 <= n < -1   : loss in n ply (assuming 50-move counter == 0)
    //         0        : draw
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
    Value probe_dtz (Position &pos, i32 &success)
    {
        success = 1;
        Value v = probe_dtz_no_ep (pos, success);
        // If en-passant is not possible, we are done.
        if (pos.en_passant_sq () == SQ_NO)
        {
            return v;
        }
        if (success == 0) return VALUE_ZERO;

        // Now handle en-passant.
        Value ep = Value(-3);
        Value v1 = ep;

        ValMove moves[MaxMoves];
        ValMove *end_move = pos.checkers () == 0 ?
            generate<CAPTURE> (moves, pos) :
            generate<EVASION> (moves, pos);

        CheckInfo ci (pos);
        for (ValMove *cur_move = moves; cur_move < end_move; ++cur_move)
        {
            auto move = cur_move->move;
            if (   mtype (move) != ENPASSANT
                || !pos.legal (move, ci.pinneds)
               )
            {
                continue;
            }

            StateInfo si;
            pos.do_move (move, si, pos.gives_check (move, ci));
            Value v0 = -probe_ab (pos, Value(-2), Value(+2), success);
            pos.undo_move ();
            if (success == 0) return VALUE_ZERO;
            if (v1 < v0)
            {
                v1 = v0;
            }
        }
        if (v1 > ep)
        {
            v1 = Wdl_to_Dtz[v1 + 2];
            if (v < -100)
            {
                if (v1 >= 0)
                {
                    v = v1;
                }
            }
            else
            if (v < 0)
            {
                if (v1 < -100 || 0 <= v1)
                {
                    v = v1;
                }
            }
            else
            if (v > 100)
            {
                if (v1 > 0)
                {
                    v = v1;
                }
            }
            else
            if (v > 0)
            {
                if (v1 == 1)
                {
                    v = v1;
                }
            }
            else
            if (v1 >= 0)
            {
                v = v1;
            }
            else
            {
                // Check whether there is at least one legal non-ep move.
                ValMove *cur_move;
                for (cur_move = moves; cur_move < end_move; ++cur_move)
                {
                    auto move = cur_move->move;
                    if (   mtype (move) != ENPASSANT
                        && pos.legal (move, ci.pinneds)
                       )
                    {
                        break;
                    }
                }
                if (   cur_move == end_move
                    && pos.checkers () == 0
                   )
                {
                    end_move = generate<QUIET> (end_move, pos);
                    for (; cur_move < end_move; ++cur_move)
                    {
                        auto move = cur_move->move;
                        if (pos.legal (move, ci.pinneds))
                        {
                            break;
                        }
                    }
                }
                // If not, then we are forced to play the losing ep capture.
                if (cur_move == end_move)
                {
                    v = v1;
                }
            }
        }
        return v;
    }
    // Probe the WDL table for a particular position.
    // If success != 0, the probe was successful.
    // The return value is from the point of view of the side to move:
    // -2 : loss
    // -1 : loss, but draw under 50-move rule
    //  0 : draw
    //  1 : win, but draw under 50-move rule
    //  2 : win
    Value probe_wdl (Position &pos, i32 &success)
    {
        success = 1;
        Value v = probe_ab (pos, Value(-2), Value(+2), success);
        // If en-passant is not possible, we are done.
        if (pos.en_passant_sq () == SQ_NO)
        {
            return v;
        }
        if (success == 0) return VALUE_ZERO;

        // Now handle en-passant.
        Value ep = Value(-3);
        Value v1 = ep;
        // Generate (at least) all legal en-passant captures.
        ValMove moves[MaxMoves];
        ValMove *end_move = pos.checkers () == 0 ?
            generate<CAPTURE> (moves, pos) :
            generate<EVASION> (moves, pos);

        CheckInfo ci (pos);
        for (ValMove *cur_move = moves; cur_move < end_move; ++cur_move)
        {
            auto move = cur_move->move;
            if (   mtype (move) != ENPASSANT
                || !pos.legal (move, ci.pinneds)
               )
            {
                continue;
            }
            StateInfo si;
            pos.do_move (move, si, pos.gives_check (move, ci));
            Value v0 = -probe_ab (pos, Value(-2), Value(+2), success);
            pos.undo_move ();
            if (success == 0) return VALUE_ZERO;
            if (v1 < v0)
            {
                v1 = v0;
            }
        }
        if (v1 > ep)
        {
            if (v <= v1)
            {
                v = v1;
            }
            else
            if (v == VALUE_ZERO)
            {
                // Check whether there is at least one legal non-ep move.
                ValMove *cur_move;
                for (cur_move = moves; cur_move < end_move; ++cur_move)
                {
                    auto move = cur_move->move;
                    if (   mtype (move) != ENPASSANT
                        && pos.legal (move, ci.pinneds)
                       )
                    {
                        break;
                    }
                }
                if (   cur_move == end_move
                    && pos.checkers () == 0
                   )
                {
                    end_move = generate<QUIET> (end_move, pos);
                    for (; cur_move < end_move; ++cur_move)
                    {
                        auto move = cur_move->move;
                        if (pos.legal (move, ci.pinneds))
                        {
                            break;
                        }
                    }
                }
                // If not, then we are forced to play the losing ep capture.
                if (cur_move == end_move)
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
    bool root_probe_dtz (Position &pos, RootMoveVector &root_moves)
    {
        assert(!root_moves.empty ());

        i32 success;
        Value dtz = probe_dtz (pos, success);
        if (success == 0) return false;

        StateInfo si;
        CheckInfo ci (pos);

        // Probe each move.
        for (size_t i = 0; i < root_moves.size (); ++i)
        {
            auto move = root_moves[i][0];
            pos.do_move (move, si, pos.gives_check (move, ci));

            Value value = VALUE_ZERO;
            if (pos.checkers () != 0 && dtz > VALUE_ZERO)
            {
                ValMove moves[MaxMoves];
                if (generate<LEGAL> (moves, pos) == moves)
                {
                    value = Value(1);
                }
            }

            if (value == VALUE_ZERO)
            {
                if (si.clock_ply != 0)
                {
                    value = -probe_dtz (pos, success);
                    if (value > 0)
                    {
                        ++value;
                    }
                    else
                    if (value < 0)
                    {
                        --value;
                    }
                }
                else
                {
                    value = -probe_wdl (pos, success);
                    value = Wdl_to_Dtz[value + 2];
                }
            }

            pos.undo_move ();
            if (success == 0) return false;
            root_moves[i].new_value = value;
        }

        // Obtain 50-move counter for the root position.
        // In Stockfish there seems to be no clean way, so we do it like this:
        i32 clock_ply = si.ptr->clock_ply;

        // Use 50-move counter to determine whether the root position is
        // won, lost or drawn.
        i32 wdl = dtz > VALUE_ZERO ?
                    (+dtz + clock_ply <= 100) ? +2 : +1 :
                  dtz < VALUE_ZERO ?
                    (-dtz + clock_ply <= 100) ? -2 : -1 :
                    0;
        assert(-2 <= wdl && wdl <= 2);

        // Determine the score to report to the user.
        ProbeValue = Wdl_to_Value[wdl + 2];
        // If the position is winning or losing, but too few moves left, adjust the
        // score to show how close it is to winning or losing.
        // NOTE: i32(PawnValueEg) is used as scaling factor in score_to_uci().
        if (wdl == 1 && dtz <= 100)
        {
            ProbeValue = Value(((200 - dtz - clock_ply) * i32(VALUE_EG_PAWN)) / 200);
        }
        else
        if (wdl == -1 && dtz >= -100)
        {
            ProbeValue = -Value(((200 + dtz - clock_ply) * i32(VALUE_EG_PAWN)) / 200);
        }
        // Now be a bit smart about filtering out moves.
        size_t size = 0;
        if (dtz > VALUE_ZERO)
        { // winning (or 50-move rule draw)
            Value best_value = +VALUE_INFINITE;
            for (size_t i = 0; i < root_moves.size (); ++i)
            {
                Value v = root_moves[i].new_value;
                if (v > VALUE_ZERO)
                {
                    if (best_value > v)
                    {
                        best_value = v;
                    }
                }
            }
            // If the current phase has not seen repetitions, then try all moves
            // that stay safely within the 50-move budget, if there are any.
            if (   !has_repeated (si.ptr)
                && best_value + clock_ply <= 99
               )
            {
                best_value = Value(99 - clock_ply);
            }

            for (size_t i = 0; i < root_moves.size (); ++i)
            {
                Value v = root_moves[i].new_value;
                if (VALUE_ZERO < v && v <= best_value)
                {
                    root_moves[size++] = root_moves[i];
                }
            }
        }
        else
        if (dtz < VALUE_ZERO)
        { // losing (or 50-move rule draw)
            Value best_value = VALUE_ZERO;
            for (size_t i = 0; i < root_moves.size (); ++i)
            {
                Value v = root_moves[i].new_value;
                if (best_value > v)
                {
                    best_value = v;
                }
            }
            // Try all moves, unless we approach or have a 50-move rule draw.
            if (-best_value * 2 + clock_ply < 100)
            {
                return true;
            }

            for (size_t i = 0; i < root_moves.size (); ++i)
            {
                if (root_moves[i].new_value == best_value)
                {
                    root_moves[size++] = root_moves[i];
                }
            }
        }
        else
        { // drawing
            // Try all moves that preserve the draw.
            for (size_t i = 0; i < root_moves.size (); ++i)
            {
                if (root_moves[i].new_value == VALUE_ZERO)
                {
                    root_moves[size++] = root_moves[i];
                }
            }
        }

        assert(size != 0 && size <= root_moves.size ());
        root_moves.resize (size, RootMove ());
        return true;
    }
    // Use the WDL tables to filter out moves that don't preserve the win or draw.
    // This is a fallback for the case that some or all DTZ tables are missing.
    //
    // A return value false indicates that not all probes were successful and that
    // no moves were filtered out.
    bool root_probe_wdl (Position &pos, RootMoveVector &root_moves)
    {
        assert(!root_moves.empty ());

        i32 success;
        Value wdl = probe_wdl (pos, success);
        assert(-2 <= wdl && wdl <= 2);

        if (success == 0) return false;
        
        ProbeValue = Wdl_to_Value[wdl + 2];

        StateInfo si;
        CheckInfo ci (pos);

        Value best_value = -VALUE_INFINITE;
        // Probe each move.
        for (size_t i = 0; i < root_moves.size (); ++i)
        {
            auto move = root_moves[i][0];
            pos.do_move (move, si, pos.gives_check (move, ci));
            Value v = -probe_wdl (pos, success);
            pos.undo_move ();
            if (success == 0) return false;
            if (best_value < v)
            {
                best_value = v;
            }
            root_moves[i].new_value = v;
        }

        size_t size = 0;
        for (size_t i = 0; i < root_moves.size (); ++i)
        {
            if (root_moves[i].new_value == best_value)
            {
                root_moves[size++] = root_moves[i];
            }
        }

        assert(size != 0 && size < root_moves.size ());
        root_moves.resize (size, RootMove ());
        return true;
    }

    void initialize ()
    {
        static bool initialized = false;
        if (!initialized)
        {
            init_indices ();
            initialized = true;
        }

        free_entries ();
        clear_tb ();

        convert_path (PathString);
        if (   !white_spaces (PathString)
            && PathString != "<empty>"
           )
        {
            Paths = split (PathString, SepChar, false, true);

            LOCK_INIT (TB_mutex);

            ostringstream oss;

            // 3-men files
            for (i08 wp1 = QUEN; wp1 >= PAWN; --wp1)
            {
                oss.str (""); oss << "K" << PieceChar[wp1] << "vK";
                init_tb (oss.str ());
            }
            // 4-men files
            for (i08 wp1 = QUEN; wp1 >= PAWN; --wp1)
            {
                for (i08 bp1 = wp1; bp1 >= PAWN; --bp1)
                {
                    oss.str (""); oss << "K" << PieceChar[wp1] << "vK" << PieceChar[bp1];
                    init_tb (oss.str ());
                }
            }
            for (i08 wp1 = QUEN; wp1 >= PAWN; --wp1)
            {
                for (i08 wp2 = wp1; wp2 >= PAWN; --wp2)
                {
                    oss.str (""); oss << "K" << PieceChar[wp1] << PieceChar[wp2] << "vK";
                    init_tb (oss.str ());
                }
            }
            // 5-men files
            for (i08 wp1 = QUEN; wp1 >= PAWN; --wp1)
            {
                for (i08 wp2 = wp1; wp2 >= PAWN; --wp2)
                {
                    for (i08 bp1 = QUEN; bp1 >= PAWN; --bp1)
                    {
                        oss.str (""); oss << "K" << PieceChar[wp1] << PieceChar[wp2] << "vK" << PieceChar[bp1];
                        init_tb (oss.str ());
                    }
                }
            }
            for (i08 wp1 = QUEN; wp1 >= PAWN; --wp1)
            {
                for (i08 wp2 = wp1; wp2 >= PAWN; --wp2)
                {
                    for (i08 wp3 = wp2; wp3 >= PAWN; --wp3)
                    {
                        oss.str (""); oss << "K" << PieceChar[wp1] << PieceChar[wp2] << PieceChar[wp3] << "vK";
                        init_tb (oss.str ());
                    }
                }
            }
            // 6-men files
            for (i08 wp1 = QUEN; wp1 >= PAWN; --wp1)
            {
                for (i08 wp2 = wp1; wp2 >= PAWN; --wp2)
                {
                    for (i08 bp1 = wp1; bp1 >= PAWN; --bp1)
                    {
                        for (i08 bp2 = (wp1 == bp1) ? wp2 : bp1; bp2 >= PAWN; --bp2)
                        {
                            oss.str (""); oss << "K" << PieceChar[wp1] << PieceChar[wp2] << "vK" << PieceChar[bp1] << PieceChar[bp2];
                            init_tb (oss.str ());
                        }
                    }
                }
            }
            for (i08 wp1 = QUEN; wp1 >= PAWN; --wp1)
            {
                for (i08 wp2 = wp1; wp2 >= PAWN; --wp2)
                {
                    for (i08 wp3 = wp2; wp3 >= PAWN; --wp3)
                    {
                        for (i08 bp1 = QUEN; bp1 >= PAWN; --bp1)
                        {
                            oss.str (""); oss << "K" << PieceChar[wp1] << PieceChar[wp2] << PieceChar[wp3] << "vK" << PieceChar[bp1];
                            init_tb (oss.str ());
                        }
                    }
                }
            }
            for (i08 wp1 = QUEN; wp1 >= PAWN; --wp1)
            {
                for (i08 wp2 = wp1; wp2 >= PAWN; --wp2)
                {
                    for (i08 wp3 = wp2; wp3 >= PAWN; --wp3)
                    {
                        for (i08 wp4 = wp3; wp4 >= PAWN; --wp4)
                        {
                            oss.str (""); oss << "K" << PieceChar[wp1] << PieceChar[wp2] << PieceChar[wp3] << PieceChar[wp4] << "vK";
                            init_tb (oss.str ());
                        }
                    }
                }
            }
            /*
            // 7-men files
            for (i08 wp1 = QUEN; wp1 >= PAWN; --wp1)
            {
                for (i08 wp2 = wp1; wp2 >= PAWN; --wp2)
                {
                    for (i08 wp3 = wp2; wp3 >= PAWN; --wp3)
                    {
                        for (i08 bp1 = QUEN; bp1 >= PAWN; --bp1)
                        {
                            for (i08 bp2 = bp1; bp2 >= PAWN; --bp2)
                            {
                                oss.str (""); oss << "K" << PieceChar[wp1] << PieceChar[wp2] << PieceChar[wp3] << "vK" << PieceChar[bp1] << PieceChar[bp2];
                                init_tb (oss.str ());
                            }
                        }
                    }
                }
            }
            for (i08 wp1 = QUEN; wp1 >= PAWN; --wp1)
            {
                for (i08 wp2 = wp1; wp2 >= PAWN; --wp2)
                {
                    for (i08 wp3 = wp2; wp3 >= PAWN; --wp3)
                    {
                        for (i08 wp4 = wp3; wp4 >= PAWN; --wp4)
                        {
                            for (i08 bp1 = QUEN; bp1 >= PAWN; --bp1)
                            {
                                oss.str (""); oss << "K" << PieceChar[wp1] << PieceChar[wp2] << PieceChar[wp3] << PieceChar[wp4] << "vK" << PieceChar[bp1];
                                init_tb (oss.str ());
                            }
                        }
                    }
                }
            }
            for (i08 wp1 = QUEN; wp1 >= PAWN; --wp1)
            {
                for (i08 wp2 = wp1; wp2 >= PAWN; --wp2)
                {
                    for (i08 wp3 = wp2; wp3 >= PAWN; --wp3)
                    {
                        for (i08 wp4 = wp3; wp4 >= PAWN; --wp4)
                        {
                            for (i08 wp5 = wp4; wp5 >= PAWN; --wp5)
                            {
                                oss.str (""); oss << "K" << PieceChar[wp1] << PieceChar[wp2] << PieceChar[wp3] << PieceChar[wp4] << PieceChar[wp5] << "vK";
                                init_tb (oss.str ());
                            }
                        }
                    }
                }
            }
            */
        }

        std::cout << "info string Syzygy Tablebases found " << (TB_PieceCount + TB_PawnCount) << std::endl;
    }

}