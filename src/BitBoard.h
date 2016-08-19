#ifndef _BITBOARD_H_INC_
#define _BITBOARD_H_INC_

#include "Type.h"

namespace BitBoard {

    const Bitboard FA_bb = U64(0x0101010101010101);
    const Bitboard FB_bb = FA_bb << 1;
    const Bitboard FC_bb = FA_bb << 2;
    const Bitboard FD_bb = FA_bb << 3;
    const Bitboard FE_bb = FA_bb << 4;
    const Bitboard FF_bb = FA_bb << 5;
    const Bitboard FG_bb = FA_bb << 6;
    const Bitboard FH_bb = FA_bb << 7;

    const Bitboard R1_bb = U64(0x00000000000000FF);
    const Bitboard R2_bb = R1_bb << (8 * 1);
    const Bitboard R3_bb = R1_bb << (8 * 2);
    const Bitboard R4_bb = R1_bb << (8 * 3);
    const Bitboard R5_bb = R1_bb << (8 * 4);
    const Bitboard R6_bb = R1_bb << (8 * 5);
    const Bitboard R7_bb = R1_bb << (8 * 6);
    const Bitboard R8_bb = R1_bb << (8 * 7);

    const Bitboard Corner_bb = (FA_bb|FH_bb)&(R1_bb|R8_bb); // 04 CORNER squares

    const Bitboard Color_bb[CLR_NO] =
    {
        U64(0x55AA55AA55AA55AA),    // 32 LIGHT squares
        U64(0xAA55AA55AA55AA55)     // 32 DARK  squares
    };

    const Bitboard Square_bb[SQ_NO] =
    {
#undef S_16
#undef S_08
#undef S_04
#undef S_02
#define S_02(n)  U64(1)<<(2*(n)),  U64(1)<<(2*(n)+1)
#define S_04(n)      S_02(2*(n)),      S_02(2*(n)+1)
#define S_08(n)      S_04(2*(n)),      S_04(2*(n)+1)
#define S_16(n)      S_08(2*(n)),      S_08(2*(n)+1)
        S_16(0), S_16(1), S_16(2), S_16(3),
#undef S_16
#undef S_08
#undef S_04
#undef S_02
    };
    const Bitboard File_bb[F_NO] =
    {
        FA_bb, FB_bb, FC_bb, FD_bb, FE_bb, FF_bb, FG_bb, FH_bb
    };
    const Bitboard Rank_bb[R_NO] =
    {
        R1_bb, R2_bb, R3_bb, R4_bb, R5_bb, R6_bb, R7_bb, R8_bb
    };

    const Bitboard AdjFile_bb  [F_NO] =
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
    const Bitboard AdjRank_bb  [R_NO] =
    {
        R2_bb,
        R1_bb|R3_bb,
        R2_bb|R4_bb,
        R3_bb|R5_bb,
        R4_bb|R6_bb,
        R5_bb|R7_bb,
        R6_bb|R8_bb,
        R7_bb,
    };
    const Bitboard FrontRank_bb[CLR_NO][R_NO] =
    {
        {
            R2_bb|R3_bb|R4_bb|R5_bb|R6_bb|R7_bb|R8_bb,
            R3_bb|R4_bb|R5_bb|R6_bb|R7_bb|R8_bb,
            R4_bb|R5_bb|R6_bb|R7_bb|R8_bb,
            R5_bb|R6_bb|R7_bb|R8_bb,
            R6_bb|R7_bb|R8_bb,
            R7_bb|R8_bb,
            R8_bb,
            0,
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

    extern Bitboard FrontSqrs_bb[CLR_NO][SQ_NO];

    extern Bitboard Between_bb[SQ_NO][SQ_NO];
    extern Bitboard StrLine_bb[SQ_NO][SQ_NO];

    extern Bitboard DistRings_bb[SQ_NO][8];

    extern Bitboard PawnAttackSpan[CLR_NO][SQ_NO];
    extern Bitboard PawnPassSpan[CLR_NO][SQ_NO];

    extern Bitboard PawnAttacks[CLR_NO][SQ_NO];
    extern Bitboard PieceAttacks[NONE][SQ_NO];

    extern Bitboard *B_Attacks_bb[SQ_NO];
    extern Bitboard *R_Attacks_bb[SQ_NO];

    extern Bitboard B_Masks_bb[SQ_NO];
    extern Bitboard R_Masks_bb[SQ_NO];

#if !defined(BM2)
    extern Bitboard B_Magics_bb[SQ_NO];
    extern Bitboard R_Magics_bb[SQ_NO];

    extern u08      B_Shifts[SQ_NO];
    extern u08      R_Shifts[SQ_NO];
#endif

#if !defined(ABM)
    extern u08 PopCount16[1 << 16];
#endif

    template<class T>
    inline i32 dist (T t1, T t2) { return t1 < t2 ? t2 - t1 : t1 - t2; }
    template<> inline i32 dist (Square s1, Square s2) { return SquareDist[s1][s2]; }

    template<class T1, class T2>
    inline i32 dist (T2, T2) { return i32 (); }
    template<> inline i32 dist<File> (Square s1, Square s2) { return dist (_file (s1), _file (s2)); }
    template<> inline i32 dist<Rank> (Square s1, Square s2) { return dist (_rank (s1), _rank (s2)); }

    inline Bitboard  operator&  (Bitboard  bb, Square s) { return bb &  Square_bb[s]; }
    inline Bitboard  operator|  (Bitboard  bb, Square s) { return bb |  Square_bb[s]; }
    inline Bitboard  operator^  (Bitboard  bb, Square s) { return bb ^  Square_bb[s]; }
    inline Bitboard  operator+  (Bitboard  bb, Square s) { return bb |  Square_bb[s]; }
    inline Bitboard  operator-  (Bitboard  bb, Square s) { return bb &~ Square_bb[s]; }
    inline Bitboard& operator&= (Bitboard &bb, Square s) { return bb &= Square_bb[s]; }
    inline Bitboard& operator|= (Bitboard &bb, Square s) { return bb |= Square_bb[s]; }
    inline Bitboard& operator^= (Bitboard &bb, Square s) { return bb ^= Square_bb[s]; }
    inline Bitboard& operator+= (Bitboard &bb, Square s) { return bb |= Square_bb[s]; }
    inline Bitboard& operator-= (Bitboard &bb, Square s) { return bb &=~Square_bb[s]; }
    /*
    inline Bitboard  operator&  (Bitboard  bb, File f) { return bb &  File_bb[f]; }
    inline Bitboard  operator|  (Bitboard  bb, File f) { return bb |  File_bb[f]; }
    inline Bitboard  operator^  (Bitboard  bb, File f) { return bb ^  File_bb[f]; }
    inline Bitboard  operator+  (Bitboard  bb, File f) { return bb |  File_bb[f]; }
    inline Bitboard  operator-  (Bitboard  bb, File f) { return bb & ~File_bb[f]; }
    inline Bitboard& operator&= (Bitboard &bb, File f) { return bb &= File_bb[f]; }
    inline Bitboard& operator|= (Bitboard &bb, File f) { return bb |= File_bb[f]; }
    inline Bitboard& operator^= (Bitboard &bb, File f) { return bb ^= File_bb[f]; }
    inline Bitboard& operator+= (Bitboard &bb, File f) { return bb |= File_bb[f]; }
    inline Bitboard& operator-= (Bitboard &bb, File f) { return bb &=~File_bb[f]; }

    inline Bitboard  operator&  (Bitboard  bb, Rank r) { return bb &  Rank_bb[r]; }
    inline Bitboard  operator|  (Bitboard  bb, Rank r) { return bb |  Rank_bb[r]; }
    inline Bitboard  operator^  (Bitboard  bb, Rank r) { return bb ^  Rank_bb[r]; }
    inline Bitboard  operator+  (Bitboard  bb, Rank r) { return bb |  Rank_bb[r]; }
    inline Bitboard  operator-  (Bitboard  bb, Rank r) { return bb & ~Rank_bb[r]; }
    inline Bitboard& operator&= (Bitboard &bb, Rank r) { return bb &= Rank_bb[r]; }
    inline Bitboard& operator|= (Bitboard &bb, Rank r) { return bb |= Rank_bb[r]; }
    inline Bitboard& operator^= (Bitboard &bb, Rank r) { return bb ^= Rank_bb[r]; }
    inline Bitboard& operator+= (Bitboard &bb, Rank r) { return bb |= Rank_bb[r]; }
    inline Bitboard& operator-= (Bitboard &bb, Rank r) { return bb &=~Rank_bb[r]; }
    */

    inline Bitboard square_bb (Square s) { return Square_bb[s]; }

    inline Bitboard file_bb (File f) { return File_bb[f]; }
    inline Bitboard file_bb (Square s) { return File_bb[_file (s)]; }

    inline Bitboard rank_bb (Rank r) { return Rank_bb[r]; }
    inline Bitboard rank_bb (Square s) { return Rank_bb[_rank (s)]; }

    inline Bitboard adj_file_bb (File f) { return AdjFile_bb[f]; }
    inline Bitboard adj_rank_bb (Rank r) { return AdjRank_bb[r]; }

    inline Bitboard front_rank_bb (Color c, Square s) { return FrontRank_bb[c][_rank (s)]; }
    inline Bitboard front_sqrs_bb (Color c, Square s) { return FrontSqrs_bb[c][s]; }

    inline Bitboard between_bb (Square s1, Square s2) { return Between_bb[s1][s2]; }
    inline Bitboard strline_bb (Square s1, Square s2) { return StrLine_bb[s1][s2]; }

    inline Bitboard dist_rings_bb (Square s, u08 d) { return DistRings_bb[s][d]; }

    inline Bitboard pawn_attack_span (Color c, Square s) { return PawnAttackSpan[c][s]; }
    inline Bitboard pawn_pass_span (Color c, Square s) { return PawnPassSpan[c][s]; }

    // Check the squares s1, s2 and s3 are aligned on a straight line.
    inline bool sqrs_aligned (Square s1, Square s2, Square s3) { return (StrLine_bb[s1][s2] & s3) != 0; }

    inline bool more_than_one (Bitboard bb)
    {
#   if defined(BM2)
        return BLSR(bb) != 0;
#   else
        return (bb & (bb - 1)) != 0;
#   endif
    }

    // Shift the bitboard using delta
    template<Delta DEL> inline Bitboard shift_bb (Bitboard bb);

    template<> inline Bitboard shift_bb<DEL_N > (Bitboard bb) { return (bb) << +DEL_N; }
    template<> inline Bitboard shift_bb<DEL_S > (Bitboard bb) { return (bb) >> -DEL_S; }
    template<> inline Bitboard shift_bb<DEL_NN> (Bitboard bb) { return (bb) << +DEL_NN; }
    template<> inline Bitboard shift_bb<DEL_SS> (Bitboard bb) { return (bb) >> -DEL_SS; }
    template<> inline Bitboard shift_bb<DEL_E > (Bitboard bb) { return (bb & ~FH_bb) << +DEL_E; }
    template<> inline Bitboard shift_bb<DEL_W > (Bitboard bb) { return (bb & ~FA_bb) >> -DEL_W; }
    template<> inline Bitboard shift_bb<DEL_NE> (Bitboard bb) { return (bb & ~FH_bb) << +DEL_NE; }
    template<> inline Bitboard shift_bb<DEL_SE> (Bitboard bb) { return (bb & ~FH_bb) >> -DEL_SE; }
    template<> inline Bitboard shift_bb<DEL_NW> (Bitboard bb) { return (bb & ~FA_bb) << +DEL_NW; }
    template<> inline Bitboard shift_bb<DEL_SW> (Bitboard bb) { return (bb & ~FA_bb) >> -DEL_SW; }

    //// Rotate Right (toward LSB)
    //inline Bitboard rotate_R (Bitboard bb, i08 k) { return (bb >> k) | (bb << (i08(SQ_NO) - k)); }
    //// Rotate Left  (toward MSB)
    //inline Bitboard rotate_L (Bitboard bb, i08 k) { return (bb << k) | (bb >> (i08(SQ_NO) - k)); }

    inline Bitboard sliding_attacks (const Delta deltas[], Square s, Bitboard occ = 0)
    {
        Bitboard slide_attacks = 0;
        u08 i = 0;
        Delta del;
        while ((del = deltas[i++]) != DEL_O)
        {
            for (auto sq = s + del;
                 _ok (sq) && dist (sq, sq - del) == 1;
                 sq += del)
            {
                slide_attacks += sq;
                if ((occ & sq) != 0)
                {
                    break;
                }
            }
        }
        return slide_attacks;
    }

    // Attacks of the PieceType with occupancy
    template<PieceType PT>
    extern Bitboard attacks_bb (Square s, Bitboard occ);

    // Function 'magic_index(s, occ)' for computing index for sliding attack bitboards.
    // Function 'attacks_bb(s, occ)' takes a square and a bitboard of occupied squares as input,
    // and returns a bitboard representing all squares attacked by PT (Bishop or Rook) on the given square.
    template<PieceType PT>
    extern inline u16 magic_index (Square s, Bitboard occ);

    template<>
    inline u16 magic_index<BSHP> (Square s, Bitboard occ)
    {
#   if defined(BM2)
        return u16(PEXT(occ, B_Masks_bb[s]));
#   elif defined(BIT64)
        return u16(((occ & B_Masks_bb[s]) * B_Magics_bb[s]) >> B_Shifts[s]);
#   else
        return u16((u32((u32(occ >> 0x00) & u32(B_Masks_bb[s] >> 0x00)) * u32(B_Magics_bb[s] >> 0x00))
                  ^ u32((u32(occ >> 0x20) & u32(B_Masks_bb[s] >> 0x20)) * u32(B_Magics_bb[s] >> 0x20))) >> B_Shifts[s]);
#   endif
    }

    template<>
    inline u16 magic_index<ROOK> (Square s, Bitboard occ)
    {
#   if defined(BM2)
        return u16(PEXT(occ, R_Masks_bb[s]));
#   elif defined(BIT64)
        return u16(((occ & R_Masks_bb[s]) * R_Magics_bb[s]) >> R_Shifts[s]);
#   else
        return u16((u32((u32(occ >> 0x00) & u32(R_Masks_bb[s] >> 0x00)) * u32(R_Magics_bb[s] >> 0x00))
                  ^ u32((u32(occ >> 0x20) & u32(R_Masks_bb[s] >> 0x20)) * u32(R_Magics_bb[s] >> 0x20))) >> R_Shifts[s]);
#   endif
    }

    // Attacks of the Bishop with occupancy
    template<>
    inline Bitboard attacks_bb<BSHP> (Square s, Bitboard occ)
    {
        return B_Attacks_bb[s][magic_index<BSHP> (s, occ)];
    }
    // Attacks of the Rook with occupancy
    template<>
    inline Bitboard attacks_bb<ROOK> (Square s, Bitboard occ)
    {
        return R_Attacks_bb[s][magic_index<ROOK> (s, occ)];
    }
    // Attacks of the Queen with occupancy
    template<>
    inline Bitboard attacks_bb<QUEN> (Square s, Bitboard occ)
    {
        //assert((B_Attacks_bb[s][magic_index<BSHP> (s, occ)]
        //      & R_Attacks_bb[s][magic_index<ROOK> (s, occ)]) == 0);
        return B_Attacks_bb[s][magic_index<BSHP> (s, occ)]
             | R_Attacks_bb[s][magic_index<ROOK> (s, occ)];
    }

    // Piece attacks from square
    inline Bitboard attacks_bb (Piece p, Square s, Bitboard occ)
    {
        switch (ptype (p))
        {
        case PAWN: return PawnAttacks[color (p)][s]; break;
        case NIHT: return PieceAttacks[NIHT][s];     break;
        case BSHP: return attacks_bb<BSHP> (s, occ); break;
        case ROOK: return attacks_bb<ROOK> (s, occ); break;
        case QUEN: return attacks_bb<QUEN> (s, occ); break;
        case KING: return PieceAttacks[KING][s];     break;
        default:   assert(false); return 0;          break;
        }
    }

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

#   if defined(_MSC_VER) || defined(__INTEL_COMPILER)
//#       include <intrin.h> // Microsoft header for pop count instrinsics __popcnt64() & __popcnt()
#       include <nmmintrin.h> // Microsoft or Intel header for pop count intrinsics _mm_popcnt_u64() & _mm_popcnt_u32()
    inline i32 pop_count (Bitboard bb)
    {
#   if defined(BIT64)
        //return i32(__popcnt64 (bb));
        return i32(_mm_popcnt_u64 (bb));
#   else
        //return i32(__popcnt (u32(bb >> 0))
        //         + __popcnt (u32(bb >> 32)));
        return i32(_mm_popcnt_u32 (bb >> 0)
                 + _mm_popcnt_u32 (bb >> 32));
#   endif
    }

#   else // GCC or compatible compiler

    inline i32 pop_count (Bitboard bb)
    {
        // Assembly code by Heinz van Saanen
        //__asm__ ("popcnt %1, %0" : "=r" (bb) : "r" (bb));
        //return bb;
#   if defined(BIT64)
        return i32(__builtin_popcountll (bb));
#   else
        return i32(__builtin_popcountl (bb >> 0)
                 + __builtin_popcountl (bb >> 32));
#   endif
    }

#   endif

#endif

#if defined(_MSC_VER)

#   include <intrin.h> // Microsoft header for instrinsics _BitScanForward64() & _BitScanReverse64()

    inline Square scan_lsq (Bitboard bb)
    {
        assert(bb != 0);

        unsigned long index;
#   if defined(BIT64)
        _BitScanForward64 (&index, bb);
#   else
        if (u32(bb >> 0) != 0)
        {
            _BitScanForward (&index, u32(bb >> 0));
        }
        else
        {
            _BitScanForward (&index, u32(bb >> 32));
            index += 32;
        }
#   endif
        return Square(index);
    }

    inline Square scan_msq (Bitboard bb)
    {
        assert(bb != 0);

        unsigned long index;
#   if defined(BIT64)
        _BitScanReverse64 (&index, bb);
#   else
        if (u32(bb >> 32) != 0)
        {
            _BitScanReverse (&index, u32(bb >> 32));
            index += 32;
        }
        else
        {
            _BitScanReverse (&index, u32(bb >> 0));
        }
#   endif
        return Square(index);
    }

#elif defined(__GNUC__)

    inline Square scan_lsq (Bitboard bb)
    {
        assert(bb != 0);

#   if defined(BIT64)
        return Square(__builtin_ctzll (bb));
#   else
        return Square(u32(bb >> 0) != 0 ?
                __builtin_ctz (bb >> 0) :
                __builtin_ctz (bb >> 32) + 32);
#   endif
    }
    inline Square scan_msq (Bitboard bb)
    {
        assert(bb != 0);

#   if defined(BIT64)
        return Square(i08(SQ_H8) - __builtin_clzll (bb));
#   else
        return Square(i08(SQ_H8) - (u32(bb >> 32) != 0 ?
                                    __builtin_clz (bb >> 32) :
                                    __builtin_clz (bb >> 0) + 32));
#   endif
    }

//#else

    //// Assembly code by Heinz van Saanen
    //inline Square scan_lsq (Bitboard bb)
    //{
    //    assert(bb != 0);
    //    Bitboard index;
    //    __asm__ ("bsfq %1, %0": "=r" (index) : "rm" (bb));
    //    return Square(index);
    //}
    //
    //inline Square scan_msq (Bitboard bb)
    //{
    //    assert(bb != 0);
    //    Bitboard index;
    //    __asm__ ("bsrq %1, %0": "=r" (index) : "rm" (bb));
    //    return Square(index);
    //}

#else

#   define NO_BSFQ

#endif

#if defined(NO_BSFQ)

#   if defined(BIT64)

    // * @author Kim Walisch (2012)
    const u64 DeBruijn_64 = U64(0x03F79D71B4CB0A89);
    const u08 BSF_Table[SQ_NO] =
    {
        00, 47, 01, 56, 48, 27, 02, 60,
        57, 49, 41, 37, 28, 16, 03, 61,
        54, 58, 35, 52, 50, 42, 21, 44,
        38, 32, 29, 23, 17, 11, 04, 62,
        46, 55, 26, 59, 40, 36, 15, 53,
        34, 51, 20, 43, 31, 22, 10, 45,
        25, 39, 14, 33, 19, 30,  9, 24,
        13, 18,  8, 12, 07, 06, 05, 63
    };

#   else

    const u32 DeBruijn_32 = U32(0x783A9B23);
    const u08 BSF_Table[SQ_NO] =
    {
        63, 30, 03, 32, 25, 41, 22, 33,
        15, 50, 42, 13, 11, 53, 19, 34,
        61, 29, 02, 51, 21, 43, 45, 10,
        18, 47, 01, 54,  9, 57, 00, 35,
        62, 31, 40, 04, 49, 05, 52, 26,
        60, 06, 23, 44, 46, 27, 56, 16,
        07, 39, 48, 24, 59, 14, 12, 55,
        38, 28, 58, 20, 37, 17, 36,  8
    };

    const u08 MSB_Table[(1 << 8)] =
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
        assert(bb != 0);
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
        assert(bb != 0);

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

    inline Square scan_frntmost_sq (Color c, Bitboard bb) { return c == WHITE ? scan_msq (bb) : scan_lsq (bb); }
    inline Square scan_backmost_sq (Color c, Bitboard bb) { return c == WHITE ? scan_lsq (bb) : scan_msq (bb); }

    inline Square pop_lsq (Bitboard &bb)
    {
        Square sq = scan_lsq (bb);
#   if defined(BM2)
        bb = BLSR(bb);
#   else
        bb &= (bb - 1);
#   endif
        return sq;
    }

    extern void initialize ();

#if !defined(NDEBUG)
    extern std::string pretty (Bitboard bb);
#endif

}

#endif // _BITBOARD_H_INC_
