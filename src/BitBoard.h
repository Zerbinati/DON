#ifndef _BITBOARD_H_INC_
#define _BITBOARD_H_INC_

#include "Type.h"

namespace BitBoard {

    ///// mk_bitboard() returns a bitboard compile-time constructed from a list of squares, files, ranks
    //constexpr Bitboard mk_bitboard () { return 0; }

    //template<typename ...Squares>
    //constexpr Bitboard mk_bitboard (Square s, Squares... squares)
    //{
    //    return U64(0x0000000000000001) << s | mk_bitboard (squares...);
    //}
    //template<typename ...Files>
    //constexpr Bitboard make_bitboard (File f, Files... files)
    //{
    //    return U64(0x0101010101010101) << f | make_bitboard (files...);
    //}
    //template<typename ...Ranks>
    //constexpr Bitboard make_bitboard (Rank r, Ranks... ranks)
    //{
    //    return U64(0x00000000000000FF) << (r * 8) | make_bitboard (ranks...);
    //}

    constexpr Bitboard FA_bb = U64(0x0101010101010101);
    constexpr Bitboard FB_bb = FA_bb << 1;
    constexpr Bitboard FC_bb = FA_bb << 2;
    constexpr Bitboard FD_bb = FA_bb << 3;
    constexpr Bitboard FE_bb = FA_bb << 4;
    constexpr Bitboard FF_bb = FA_bb << 5;
    constexpr Bitboard FG_bb = FA_bb << 6;
    constexpr Bitboard FH_bb = FA_bb << 7;

    constexpr Bitboard R1_bb = U64(0x00000000000000FF);
    constexpr Bitboard R2_bb = R1_bb << (8 * 1);
    constexpr Bitboard R3_bb = R1_bb << (8 * 2);
    constexpr Bitboard R4_bb = R1_bb << (8 * 3);
    constexpr Bitboard R5_bb = R1_bb << (8 * 4);
    constexpr Bitboard R6_bb = R1_bb << (8 * 5);
    constexpr Bitboard R7_bb = R1_bb << (8 * 6);
    constexpr Bitboard R8_bb = R1_bb << (8 * 7);

    constexpr Bitboard Diagonals_bb = U64(0x8142241818244281); // A1..H8 | H1..A8
    constexpr Bitboard Center_bb = (FD_bb|FE_bb) & (R4_bb|R5_bb);

    constexpr Bitboard Color_bb[CLR_NO] =
    {
        U64(0x55AA55AA55AA55AA),
        U64(0xAA55AA55AA55AA55)
    };

    constexpr Bitboard Side_bb[3] =
    {
        FE_bb|FF_bb|FG_bb|FH_bb,
        FA_bb|FB_bb|FC_bb|FD_bb,
        FC_bb|FD_bb|FE_bb|FF_bb
    };
    constexpr Bitboard KingFlank_bb[F_NO] =
    {
        Side_bb[CS_QUEN] ^ FD_bb,
        Side_bb[CS_QUEN],
        Side_bb[CS_QUEN],
        Side_bb[CS_NO],
        Side_bb[CS_NO],
        Side_bb[CS_KING],
        Side_bb[CS_KING],
        Side_bb[CS_KING] ^ FE_bb
    };
    constexpr Bitboard Outposts_bb[CLR_NO] =
    {
        R4_bb|R5_bb|R6_bb,
        R5_bb|R4_bb|R3_bb
    };
    constexpr Bitboard Camp_bb[CLR_NO] =
    {
        R1_bb|R2_bb|R3_bb|R4_bb|R5_bb,
        R8_bb|R7_bb|R6_bb|R5_bb|R4_bb
    };
    constexpr Bitboard LowRanks_bb[CLR_NO] =
    {
        R2_bb|R3_bb,
        R7_bb|R6_bb
    };
    constexpr Bitboard Space_bb[CLR_NO] =
    {
        R2_bb|R3_bb|R4_bb,
        R7_bb|R6_bb|R5_bb
    };

#   define S_02(n)  u64(1)<<(2*(n)),  u64(1)<<(2*(n)+1)
#   define S_04(n)      S_02(2*(n)),      S_02(2*(n)+1)
#   define S_08(n)      S_04(2*(n)),      S_04(2*(n)+1)
#   define S_16(n)      S_08(2*(n)),      S_08(2*(n)+1)
    constexpr Bitboard Square_bb[SQ_NO] =
    {
        S_16(0), S_16(1), S_16(2), S_16(3),
    };
#   undef S_16
#   undef S_08
#   undef S_04
#   undef S_02

    constexpr Bitboard File_bb[F_NO] = { FA_bb, FB_bb, FC_bb, FD_bb, FE_bb, FF_bb, FG_bb, FH_bb };
    constexpr Bitboard Rank_bb[R_NO] = { R1_bb, R2_bb, R3_bb, R4_bb, R5_bb, R6_bb, R7_bb, R8_bb };

    constexpr Bitboard AdjFile_bb[F_NO] =
    {
        FB_bb,
        FA_bb|FC_bb,
        FB_bb|FD_bb,
        FC_bb|FE_bb,
        FD_bb|FF_bb,
        FE_bb|FG_bb,
        FF_bb|FH_bb,
        FG_bb
    };
    constexpr Bitboard AdjRank_bb[R_NO] =
    {
        R2_bb,
        R1_bb|R3_bb,
        R2_bb|R4_bb,
        R3_bb|R5_bb,
        R4_bb|R6_bb,
        R5_bb|R7_bb,
        R6_bb|R8_bb,
        R7_bb
    };
    constexpr Bitboard FrontRank_bb[CLR_NO][R_NO] =
    {
        {
            R2_bb|R3_bb|R4_bb|R5_bb|R6_bb|R7_bb|R8_bb,
            R3_bb|R4_bb|R5_bb|R6_bb|R7_bb|R8_bb,
            R4_bb|R5_bb|R6_bb|R7_bb|R8_bb,
            R5_bb|R6_bb|R7_bb|R8_bb,
            R6_bb|R7_bb|R8_bb,
            R7_bb|R8_bb,
            R8_bb,
            0
        },
        {
            0,
            R1_bb,
            R2_bb|R1_bb,
            R3_bb|R2_bb|R1_bb,
            R4_bb|R3_bb|R2_bb|R1_bb,
            R5_bb|R4_bb|R3_bb|R2_bb|R1_bb,
            R6_bb|R5_bb|R4_bb|R3_bb|R2_bb|R1_bb,
            R7_bb|R6_bb|R5_bb|R4_bb|R3_bb|R2_bb|R1_bb
        }
    };

    extern u08      SquareDist[SQ_NO][SQ_NO];

    extern Bitboard FrontLine_bb[CLR_NO][SQ_NO];

    extern Bitboard Between_bb[SQ_NO][SQ_NO];
    extern Bitboard StrLine_bb[SQ_NO][SQ_NO];

    extern Bitboard DistRings_bb[SQ_NO][8];

    extern Bitboard PawnAttackSpan[CLR_NO][SQ_NO];
    extern Bitboard PawnPassSpan[CLR_NO][SQ_NO];

    extern Bitboard PawnAttacks[CLR_NO][SQ_NO];
    extern Bitboard PieceAttacks[NONE][SQ_NO];

    // Magic holds all magic relevant data for a single square
    struct Magic
    {
        Bitboard  mask;

#   if !defined(BM2)
        Bitboard  number;
        u08       shift;
#   endif

        Bitboard *attacks;

        u16 index (Bitboard occ) const
        {
            return
#       if defined(BM2)
            u16(PEXT(occ, mask));
#       elif defined(BIT64)
            u16(((occ & mask) * number) >> shift);
#       else
            u16((  u32((u32(occ >> 0x00) & u32(mask >> 0x00)) * u32(number >> 0x00))
                 ^ u32((u32(occ >> 0x20) & u32(mask >> 0x20)) * u32(number >> 0x20))) >> shift);
#       endif
        }

        Bitboard attacks_bb (Bitboard occ) const
        {
            return attacks[index (occ)];
        }
    };

    extern Magic BMagics[SQ_NO]
        ,        RMagics[SQ_NO];

#if !defined(ABM)
    extern u08 PopCount16[1 << 16];
#endif

    template<typename T>
    inline i32 dist (T t1, T t2) { return t1 != t2 ? t1 < t2 ? t2 - t1 : t1 - t2 : 0; }
    template<> inline i32 dist (Square s1, Square s2) { return SquareDist[s1][s2]; }

    template<typename T1, typename T2>
    inline i32 dist (T2, T2) { return 0; }
    template<> inline i32 dist<File> (Square s1, Square s2) { assert(_ok (s1) && _ok (s2)); return dist (_file (s1), _file (s2)); }
    template<> inline i32 dist<Rank> (Square s1, Square s2) { assert(_ok (s1) && _ok (s2)); return dist (_rank (s1), _rank (s2)); }

    inline bool contains (Bitboard bb, Square s) { assert(_ok (s)); return 0 != (bb & Square_bb[s]); }

    inline Bitboard operator| (Bitboard  bb, Square s) { assert(_ok (s)); return bb | Square_bb[s]; }
    inline Bitboard operator^ (Bitboard  bb, Square s) { assert(_ok (s)); return bb ^ Square_bb[s]; }

    inline Bitboard& operator|= (Bitboard &bb, Square s) { assert(_ok (s)); return bb |= Square_bb[s]; }
    inline Bitboard& operator^= (Bitboard &bb, Square s) { assert(_ok (s)); return bb ^= Square_bb[s]; }

    inline Bitboard square_bb (Square s) { assert(_ok (s)); return Square_bb[s]; }

    inline Bitboard file_bb (File f) { assert(_ok (f)); return File_bb[f]; }
    inline Bitboard file_bb (Square s) { assert(_ok (s)); return File_bb[_file (s)]; }

    inline Bitboard rank_bb (Rank r) { assert(_ok (r)); return Rank_bb[r]; }
    inline Bitboard rank_bb (Square s) { assert(_ok (s)); return Rank_bb[_rank (s)]; }

    inline Bitboard adj_file_bb (File f)
    {
        assert(_ok (f));
        return AdjFile_bb[f];
        //return shift<DEL_E> (File_bb[f]) | shift<DEL_W> (File_bb[f]);
    }
    inline Bitboard adj_rank_bb (Rank r) { assert(_ok (r)); return AdjRank_bb[r]; }

    inline Bitboard front_rank_bb (Color c, Square s) { assert(_ok (s)); return FrontRank_bb[c][_rank (s)]; }
    inline Bitboard front_line_bb (Color c, Square s) { assert(_ok (s)); return FrontLine_bb[c][s]; }

    inline Bitboard between_bb (Square s1, Square s2) { assert(_ok (s1) && _ok (s2)); return Between_bb[s1][s2]; }
    inline Bitboard strline_bb (Square s1, Square s2) { assert(_ok (s1) && _ok (s2)); return StrLine_bb[s1][s2]; }

    inline Bitboard dist_rings_bb (Square s, u08 d) { assert(_ok (s)); return DistRings_bb[s][d]; }

    inline Bitboard pawn_attack_span (Color c, Square s) { assert(_ok (s)); return PawnAttackSpan[c][s]; }
    inline Bitboard pawn_pass_span (Color c, Square s) { assert(_ok (s)); return PawnPassSpan[c][s]; }

    /// Check the squares s1, s2 and s3 are aligned on a straight line.
    inline bool sqrs_aligned (Square s1, Square s2, Square s3) { assert(_ok (s1) && _ok (s2) && _ok (s3)); return contains (StrLine_bb[s1][s2], s3); }

    constexpr bool more_than_one (Bitboard bb)
    {
        return
//#   if defined(BM2)
//      0 != BLSR(bb);
//#   else
        0 != (bb & (bb - 1));
//#   endif
    }
    
    inline bool opposite_colors (Square s1, Square s2)
    {
        //assert(_ok (s1) && _ok (s2));
        //i08 s = i08(s1) ^ i08(s2);
        //return 0 != (((s >> 3) ^ s) & 1);
        return (contains (Color_bb[WHITE], s1) && contains (Color_bb[BLACK], s2))
            || (contains (Color_bb[BLACK], s1) && contains (Color_bb[WHITE], s2));
    }

    /// Shift the bitboard using delta
    template<Delta DEL>
    constexpr Bitboard shift (Bitboard bb) { return 0; }

    template<> constexpr Bitboard shift<DEL_N > (Bitboard bb) { return (bb         ) << 8; }
    template<> constexpr Bitboard shift<DEL_S > (Bitboard bb) { return (bb         ) >> 8; }
    template<> constexpr Bitboard shift<DEL_NN> (Bitboard bb) { return (bb         ) << 16; }
    template<> constexpr Bitboard shift<DEL_SS> (Bitboard bb) { return (bb         ) >> 16; }
    template<> constexpr Bitboard shift<DEL_E > (Bitboard bb) { return (bb & ~FH_bb) << 1; }
    template<> constexpr Bitboard shift<DEL_W > (Bitboard bb) { return (bb & ~FA_bb) >> 1; }
    template<> constexpr Bitboard shift<DEL_NE> (Bitboard bb) { return (bb & ~FH_bb) << 9; }
    template<> constexpr Bitboard shift<DEL_SE> (Bitboard bb) { return (bb & ~FH_bb) >> 7; }
    template<> constexpr Bitboard shift<DEL_NW> (Bitboard bb) { return (bb & ~FA_bb) << 7; }
    template<> constexpr Bitboard shift<DEL_SW> (Bitboard bb) { return (bb & ~FA_bb) >> 9; }

    //// Rotate Right (toward LSB)
    //inline Bitboard rotate_R (Bitboard bb, i08 k) { return (bb >> k) | (bb << (SQ_NO - k)); }
    //// Rotate Left  (toward MSB)
    //inline Bitboard rotate_L (Bitboard bb, i08 k) { return (bb << k) | (bb >> (SQ_NO - k)); }

    inline Bitboard pawn_pushes_bb (Color c, Bitboard bb)
    {
        switch (c)
        {
        case WHITE: return shift<DEL_N> (bb);
        case BLACK: return shift<DEL_S> (bb);
        default: assert(false); return 0;
        }
    }

    inline Bitboard pawn_l_attacks_bb (Color c, Bitboard bb)
    {
        switch (c)
        {
        case WHITE: return shift<DEL_NW> (bb);
        case BLACK: return shift<DEL_SE> (bb);
        default: assert(false); return 0;
        }
    }
    inline Bitboard pawn_r_attacks_bb (Color c, Bitboard bb)
    {
        switch (c)
        {
        case WHITE: return shift<DEL_NE> (bb);
        case BLACK: return shift<DEL_SW> (bb);
        default: assert(false); return 0;
        }
    }

    inline Bitboard pawn_sgl_attacks_bb (Color c, Bitboard bb)
    {
        switch (c)
        {
        case WHITE: return shift<DEL_NE> (bb) | shift<DEL_NW> (bb);
        case BLACK: return shift<DEL_SE> (bb) | shift<DEL_SW> (bb);
        default: assert(false); return 0;
        }
    }
    inline Bitboard pawn_dbl_attacks_bb (Color c, Bitboard bb)
    {
        switch (c)
        {
        case WHITE: return shift<DEL_NE> (bb) & shift<DEL_NW> (bb);
        case BLACK: return shift<DEL_SE> (bb) & shift<DEL_SW> (bb);
        default: assert(false); return 0;
        }
    }

    /// attacks_bb(s, occ) takes a square and a bitboard of occupied squares,
    /// and returns a bitboard representing all squares attacked by PT (Bishop or Rook or Queen) on the given square.
    template<PieceType PT> Bitboard attacks_bb (Square, Bitboard);

    template<> inline Bitboard attacks_bb<NIHT> (Square s, Bitboard    ) { return PieceAttacks[NIHT][s]; }
    template<> inline Bitboard attacks_bb<KING> (Square s, Bitboard    ) { return PieceAttacks[KING][s]; }
    /// Attacks of the Bishop with occupancy
    template<> inline Bitboard attacks_bb<BSHP> (Square s, Bitboard occ) { return BMagics[s].attacks_bb (occ); }
    /// Attacks of the Rook with occupancy
    template<> inline Bitboard attacks_bb<ROOK> (Square s, Bitboard occ) { return RMagics[s].attacks_bb (occ); }
    /// Attacks of the Queen with occupancy
    template<> inline Bitboard attacks_bb<QUEN> (Square s, Bitboard occ) { return BMagics[s].attacks_bb (occ)
                                                                                | RMagics[s].attacks_bb (occ); }

#if !defined(ABM) // PopCount Table

    inline i32 pop_count (Bitboard bb)
    {
        union
        {
            Bitboard b;
            u16 u16[4];
        } v = { bb };
        return PopCount16[v.u16[0]]
             + PopCount16[v.u16[1]]
             + PopCount16[v.u16[2]]
             + PopCount16[v.u16[3]];
    }

#else

#   if defined(_MSC_VER) || defined(__INTEL_COMPILER) // MSVC or Intel compiler
//#       include <intrin.h> // Microsoft header for pop count instrinsics __popcnt64() & __popcnt()
#       include <nmmintrin.h> // Microsoft or Intel header for pop count intrinsics _mm_popcnt_u64() & _mm_popcnt_u32()
    inline i32 pop_count (Bitboard bb)
    {
        return
#   if defined(BIT64)
        //i32(__popcnt64 (bb));
        i32(_mm_popcnt_u64 (bb));
#   else
        //i32(__popcnt (u32(bb >> 0x00))
        //  + __popcnt (u32(bb >> 0x20)));
        i32(_mm_popcnt_u32 (bb >> 0x00)
          + _mm_popcnt_u32 (bb >> 0x20));
#   endif
    }

#   else // GCC, Clang, ICC or compatible compiler

    inline i32 pop_count (Bitboard bb)
    {
        return
#   if defined(BIT64)
        i32(__builtin_popcountll (bb));
#   else
        i32(__builtin_popcountl (bb >> 0x00)
          + __builtin_popcountl (bb >> 0x20));
#   endif
    }

#   endif

#endif

#if defined(_MSC_VER) // MSVC compiler

#   include <intrin.h> // Microsoft header for instrinsics _BitScanForward64() & _BitScanReverse64()

    inline Square scan_lsq (Bitboard bb)
    {
        assert(0 != bb);

        unsigned long index;
#   if defined(_WIN64)
        _BitScanForward64 (&index, bb);
#   else
        if (0 != u32(bb >> 0))
        {
            _BitScanForward (&index, u32(bb >> 0x00));
        }
        else
        {
            _BitScanForward (&index, u32(bb >> 0x20));
            index += 0x20;
        }
#   endif
        return Square(index);
    }

    inline Square scan_msq (Bitboard bb)
    {
        assert(0 != bb);

        unsigned long index;
#   if defined(_WIN64)
        _BitScanReverse64 (&index, bb);
#   else
        if (0 != u32(bb >> 0x20))
        {
            _BitScanReverse (&index, u32(bb >> 0x20));
            index += 0x20;
        }
        else
        {
            _BitScanReverse (&index, u32(bb >> 0x00));
        }
#   endif
        return Square(index);
    }

#elif defined(__GNUC__) // GCC, Clang, ICC compiler

    inline Square scan_lsq (Bitboard bb)
    {
        assert(0 != bb);
        return
#   if defined(BIT64)
        Square(__builtin_ctzll (bb));
#   else
        Square(0 != u32(bb >> 0x00) ?
                __builtin_ctz (bb >> 0x00) :
                __builtin_ctz (bb >> 0x20) + 0x20);
#   endif
    }
    inline Square scan_msq (Bitboard bb)
    {
        assert(0 != bb);
        return
#   if defined(BIT64)
        Square(__builtin_clzll (bb) ^ i08(SQ_H8));
#   else
        Square(0 != ((u32(bb >> 0x20) ^ i08(SQ_H8)) ?
                __builtin_clz (bb >> 0x20) :
                __builtin_clz (bb >> 0x00) + 0x20));
#   endif
    }

//#else

    //// Assembly code by Heinz van Saanen
    //inline Square scan_lsq (Bitboard bb)
    //{
    //    assert(0 != bb);
    //    Bitboard index;
    //    __asm__ ("bsfq %1, %0": "=r" (index) : "rm" (bb));
    //    return Square(index);
    //}
    //
    //inline Square scan_msq (Bitboard bb)
    //{
    //    assert(0 != bb);
    //    Bitboard index;
    //    __asm__ ("bsrq %1, %0": "=r" (index) : "rm" (bb));
    //    return Square(index);
    //}

#else // Compiler is neither GCC nor MSVC compatible

#   if defined(BIT64)

    // * @author Kim Walisch (2012)
    constexpr u64 DeBruijn_64 = U64(0x03F79D71B4CB0A89);
    constexpr u08 BSF_Table[SQ_NO] =
    {
         0, 47,  1, 56, 48, 27,  2, 60,
        57, 49, 41, 37, 28, 16,  3, 61,
        54, 58, 35, 52, 50, 42, 21, 44,
        38, 32, 29, 23, 17, 11,  4, 62,
        46, 55, 26, 59, 40, 36, 15, 53,
        34, 51, 20, 43, 31, 22, 10, 45,
        25, 39, 14, 33, 19, 30,  9, 24,
        13, 18,  8, 12,  7,  6,  5, 63
    };

#   else

    constexpr u32 DeBruijn_32 = U32(0x783A9B23);
    constexpr u08 BSF_Table[SQ_NO] =
    {
        63, 30,  3, 32, 25, 41, 22, 33,
        15, 50, 42, 13, 11, 53, 19, 34,
        61, 29,  2, 51, 21, 43, 45, 10,
        18, 47,  1, 54,  9, 57,  0, 35,
        62, 31, 40,  4, 49,  5, 52, 26,
        60,  6, 23, 44, 46, 27, 56, 16,
         7, 39, 48, 24, 59, 14, 12, 55,
        38, 28, 58, 20, 37, 17, 36,  8
    };

    constexpr u08 MSB_Table[(1 << 8)] =
    {
        0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
        5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    };

#   endif

    inline Square scan_lsq (Bitboard bb)
    {
        assert(0 != bb);
        bb ^= (bb - 1); // Set all bits including the LS1B and below
        u08 index =
#   if defined(BIT64)
        // Use Kim Walisch extending trick for 64-bit
        (bb * DeBruijn_64) >> 58;
#   else
        // Use Matt Taylor's folding trick for 32-bit
        (u32((bb >> 0) ^ (bb >> 32)) * DeBruijn_32) >> 26;
#   endif
        return Square(BSF_Table[index]);
    }

    inline Square scan_msq (Bitboard bb)
    {
        assert(0 != bb);

#   if defined(BIT64)
        // Set all bits including the MS1B and below
        bb |= bb >> 0x01;
        bb |= bb >> 0x02;
        bb |= bb >> 0x04;
        bb |= bb >> 0x08;
        bb |= bb >> 0x10;
        bb |= bb >> 0x20;
        u08 index = (bb * DeBruijn_64) >> 58;
        return Square(BSF_Table[index]);
#   else
        u08 msb = 0;
        if (bb > 0xFFFFFFFF)
        {
            bb >>= 32;
            msb = 32;
        }
        u32 bb32 = u32(bb);
        if (bb32 > 0xFFFF)
        {
            bb32 >>= 16;
            msb += 16;
        }
        u16 bb16 = u16(bb32);
        if (bb16 > 0xFF)
        {
            bb16 >>= 8;
            msb += 8;
        }
        return Square(msb + MSB_Table[bb16]);
#   endif

    }

#endif

    // Find the square corresponding to the most/least advanced bit relative to the given color.

    inline Square scan_frntmost_sq (Color c, Bitboard bb)
    {
        switch (c)
        {
        case WHITE: return scan_msq (bb);
        case BLACK: return scan_lsq (bb);
        default: assert(false); return SQ_NO;
        }
    }
    inline Square scan_backmost_sq (Color c, Bitboard bb)
    {
        switch (c)
        {
        case WHITE: return scan_lsq (bb);
        case BLACK: return scan_msq (bb);
        default: assert(false); return SQ_NO;
        }
    }

    inline Square pop_lsq (Bitboard &bb)
    {
        Square sq = scan_lsq (bb);
//#   if defined(BM2)
//        bb = BLSR(bb);
//#   else
        bb &= (bb - 1);
//#   endif
        return sq;
    }

    template<PieceType PT>
    extern Bitboard slide_attacks (Square, Bitboard = 0);

    extern void initialize ();

#if !defined(NDEBUG)

    extern std::string pretty (Bitboard);

#endif

}

#endif // _BITBOARD_H_INC_
