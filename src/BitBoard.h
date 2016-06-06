#ifndef _BITBOARD_H_INC_
#define _BITBOARD_H_INC_

#include "Type.h"
#include "BitCount.h"
#include "BitScan.h"

namespace BitBoard {

    extern const Bitboard FA_bb;
    extern const Bitboard FB_bb;
    extern const Bitboard FC_bb;
    extern const Bitboard FD_bb;
    extern const Bitboard FE_bb;
    extern const Bitboard FF_bb;
    extern const Bitboard FG_bb;
    extern const Bitboard FH_bb;

    extern const Bitboard R1_bb;
    extern const Bitboard R2_bb;
    extern const Bitboard R3_bb;
    extern const Bitboard R4_bb;
    extern const Bitboard R5_bb;
    extern const Bitboard R6_bb;
    extern const Bitboard R7_bb;
    extern const Bitboard R8_bb;

    //extern const Bitboard D18_bb;
    //extern const Bitboard D81_bb;

    extern const Bitboard Liht_bb;
    extern const Bitboard Dark_bb;

    extern const Bitboard Corner_bb;

    extern const Delta PawnDeltas[CLR_NO][3];
    extern const Delta PieceDeltas[NONE][9];

    extern const Bitboard Square_bb[SQ_NO];
    extern const Bitboard File_bb[F_NO];
    extern const Bitboard Rank_bb[R_NO];

    // Adjacent files used for isolated-pawn
    extern const Bitboard AdjFile_bb[F_NO];
    extern const Bitboard AdjRank_bb[R_NO];
    extern const Bitboard FrontRank_bb[CLR_NO][R_NO];

    extern Bitboard   FrontSqrs_bb[CLR_NO][SQ_NO];

    extern Bitboard     Between_bb[SQ_NO][SQ_NO];
    extern Bitboard     RayLine_bb[SQ_NO][SQ_NO];

    extern Bitboard   DistRings_bb[SQ_NO][8];

    extern Bitboard PawnAttackSpan[CLR_NO][SQ_NO];
    extern Bitboard   PawnPassSpan[CLR_NO][SQ_NO];

    // Attacks of the pawns & pieces
    extern Bitboard   PawnAttacks[CLR_NO][SQ_NO];
    extern Bitboard  PieceAttacks[NONE][SQ_NO];

    extern Bitboard *B_Attacks_bb[SQ_NO];
    extern Bitboard *R_Attacks_bb[SQ_NO];

    extern Bitboard    B_Masks_bb[SQ_NO];
    extern Bitboard    R_Masks_bb[SQ_NO];

#if !defined(BM2)
    extern Bitboard   B_Magics_bb[SQ_NO];
    extern Bitboard   R_Magics_bb[SQ_NO];

    extern u08           B_Shifts[SQ_NO];
    extern u08           R_Shifts[SQ_NO];
#endif

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

    // ----------------------------------------------------

    inline Bitboard file_bb (File f) { return File_bb[f]; }
    inline Bitboard file_bb (Square s) { return File_bb[_file (s)]; }

    inline Bitboard rank_bb (Rank r) { return Rank_bb[r]; }
    inline Bitboard rank_bb (Square s) { return Rank_bb[_rank (s)]; }

    inline Bitboard front_rank_bb (Color c, Square s) { return FrontRank_bb[c][_rank (s)]; }
    inline Bitboard front_sqrs_bb (Color c, Square s) { return FrontSqrs_bb[c][s]; }

    inline Bitboard between_bb (Square s1, Square s2) { return Between_bb[s1][s2]; }
    inline Bitboard rayline_bb (Square s1, Square s2) { return RayLine_bb[s1][s2]; }

    inline Bitboard dist_rings_bb (Square s, u08 d) { return DistRings_bb[s][d]; }

    inline Bitboard pawn_attack_span (Color c, Square s) { return PawnAttackSpan[c][s]; }
    inline Bitboard pawn_pass_span (Color c, Square s) { return PawnPassSpan[c][s]; }

    // Check the squares s1, s2 and s3 are aligned either on a straight/diagonal line.
    inline bool sqrs_aligned (Square s1, Square s2, Square s3) { return (RayLine_bb[s1][s2] & s3) != 0; }

    // board_edges() returns a bitboard of edges of the board for given square
    inline Bitboard board_edges (Square s) { return ((FA_bb|FH_bb) & ~file_bb (s)) | ((R1_bb|R8_bb) & ~rank_bb (s)); }

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

    template<> inline Bitboard shift_bb<DEL_N > (Bitboard bb) { return (bb) << (+DEL_N); }
    template<> inline Bitboard shift_bb<DEL_S > (Bitboard bb) { return (bb) >> (-DEL_S); }
    template<> inline Bitboard shift_bb<DEL_NN> (Bitboard bb) { return (bb) << (+DEL_NN); }
    template<> inline Bitboard shift_bb<DEL_SS> (Bitboard bb) { return (bb) >> (-DEL_SS); }
    template<> inline Bitboard shift_bb<DEL_E > (Bitboard bb) { return (bb & ~FH_bb) << (+DEL_E); }
    template<> inline Bitboard shift_bb<DEL_W > (Bitboard bb) { return (bb & ~FA_bb) >> (-DEL_W); }
    template<> inline Bitboard shift_bb<DEL_NE> (Bitboard bb) { return (bb & ~FH_bb) << (+DEL_NE); } //(bb << +DEL_NE) & ~FA_bb;
    template<> inline Bitboard shift_bb<DEL_SE> (Bitboard bb) { return (bb & ~FH_bb) >> (-DEL_SE); } //(bb >> -DEL_SE) & ~FA_bb;
    template<> inline Bitboard shift_bb<DEL_NW> (Bitboard bb) { return (bb & ~FA_bb) << (+DEL_NW); } //(bb << +DEL_NW) & ~FH_bb;
    template<> inline Bitboard shift_bb<DEL_SW> (Bitboard bb) { return (bb & ~FA_bb) >> (-DEL_SW); } //(bb >> -DEL_SW) & ~FH_bb;

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
            auto sq = s + del;
            while (   _ok (sq)
                   && dist (sq, sq - del) == 1)
            {
                slide_attacks += sq;
                if ((occ & sq) != 0) break;
                sq += del;
            }
        }
        return slide_attacks;
    }

    // --------------------------------
    // Attacks of the PieceType with occupancy
    template<PieceType PT>
    extern Bitboard attacks_bb (Square s, Bitboard occ);

    // Knight attacks
    template<>
    inline Bitboard attacks_bb<NIHT> (Square s, Bitboard) { return PieceAttacks[NIHT][s]; }
    // King attacks
    template<>
    inline Bitboard attacks_bb<KING> (Square s, Bitboard) { return PieceAttacks[KING][s]; }

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
#   else
#       if defined(BIT64)
            return u16(((occ & B_Masks_bb[s]) * B_Magics_bb[s]) >> B_Shifts[s]);
#       else
            u32 lo = (u32(occ >> 0x00) & u32(B_Masks_bb[s] >> 0x00)) * u32(B_Magics_bb[s] >> 0x00);
            u32 hi = (u32(occ >> 0x20) & u32(B_Masks_bb[s] >> 0x20)) * u32(B_Magics_bb[s] >> 0x20);
            return ((lo ^ hi) >> B_Shifts[s]);
#       endif
#   endif
    }

    template<>
    inline u16 magic_index<ROOK> (Square s, Bitboard occ)
    {
#   if defined(BM2)
        return u16(PEXT(occ, R_Masks_bb[s]));
#   else
#       if defined(BIT64)
            return u16(((occ & R_Masks_bb[s]) * R_Magics_bb[s]) >> R_Shifts[s]);
#       else
            u32 lo = (u32(occ >> 0x00) & u32(R_Masks_bb[s] >> 0x00)) * u32(R_Magics_bb[s] >> 0x00);
            u32 hi = (u32(occ >> 0x20) & u32(R_Masks_bb[s] >> 0x20)) * u32(R_Magics_bb[s] >> 0x20);
            return ((lo ^ hi) >> R_Shifts[s]);
#       endif
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
        return B_Attacks_bb[s][magic_index<BSHP> (s, occ)]
             | R_Attacks_bb[s][magic_index<ROOK> (s, occ)];
    }
    // --------------------------------

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
        default:   return 0; break;
        }
    }

    extern void initialize ();

#if !defined(NDEBUG)
    extern std::string pretty (Bitboard bb, char p = 'o');

    //extern void test_attacks ();
#endif

}

#endif // _BITBOARD_H_INC_
