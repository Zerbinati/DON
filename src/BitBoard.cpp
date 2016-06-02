#include "BitBoard.h"

#include "PRNG.h"
#include "Notation.h"

u08 SquareDist[SQ_NO][SQ_NO];

namespace BitBoard {

    using namespace std;

    const Bitboard FA_bb = U64(0x0101010101010101);
    const Bitboard FB_bb = FA_bb << 1;//U64(0x0202020202020202);
    const Bitboard FC_bb = FA_bb << 2;//U64(0x0404040404040404);
    const Bitboard FD_bb = FA_bb << 3;//U64(0x0808080808080808);
    const Bitboard FE_bb = FA_bb << 4;//U64(0x1010101010101010);
    const Bitboard FF_bb = FA_bb << 5;//U64(0x2020202020202020);
    const Bitboard FG_bb = FA_bb << 6;//U64(0x4040404040404040);
    const Bitboard FH_bb = FA_bb << 7;//U64(0x8080808080808080);

    const Bitboard R1_bb = U64(0x00000000000000FF);
    const Bitboard R2_bb = R1_bb << (8 * 1);//U64(0x000000000000FF00);
    const Bitboard R3_bb = R1_bb << (8 * 2);//U64(0x0000000000FF0000);
    const Bitboard R4_bb = R1_bb << (8 * 3);//U64(0x00000000FF000000);
    const Bitboard R5_bb = R1_bb << (8 * 4);//U64(0x000000FF00000000);
    const Bitboard R6_bb = R1_bb << (8 * 5);//U64(0x0000FF0000000000);
    const Bitboard R7_bb = R1_bb << (8 * 6);//U64(0x00FF000000000000);
    const Bitboard R8_bb = R1_bb << (8 * 7);//U64(0xFF00000000000000);

    //const Bitboard D18_bb = U64(0x8040201008040201);        // 08 DIAG-18 squares
    //const Bitboard D81_bb = U64(0x0102040810204080);        // 08 DIAG-81 squares

    const Bitboard Liht_bb = U64 (0x55AA55AA55AA55AA);      // 32 LIGHT squares
    const Bitboard Dark_bb = U64 (0xAA55AA55AA55AA55);      // 32 DARK  squares

    const Bitboard Corner_bb = (FA_bb|FH_bb)&(R1_bb|R8_bb); // 04 CORNER squares

    const Delta PawnDeltas[CLR_NO][3] =
    {
        { DEL_NW, DEL_NE, DEL_O },
        { DEL_SE, DEL_SW, DEL_O },
    };
    const Delta PieceDeltas[NONE][9] =
    {
        { DEL_O },
        { DEL_SSW, DEL_SSE, DEL_WWS, DEL_EES, DEL_WWN, DEL_EEN, DEL_NNW, DEL_NNE, DEL_O },
        { DEL_SW, DEL_SE, DEL_NW, DEL_NE, DEL_O },
        { DEL_S, DEL_W, DEL_E, DEL_N, DEL_O },
        { DEL_SW, DEL_S, DEL_SE, DEL_W, DEL_E, DEL_NW, DEL_N, DEL_NE, DEL_O },
        { DEL_SW, DEL_S, DEL_SE, DEL_W, DEL_E, DEL_NW, DEL_N, DEL_NE, DEL_O },
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

    // Front Squares
    Bitboard  FrontSqrs_bb[CLR_NO][SQ_NO];

    Bitboard    Between_bb[SQ_NO][SQ_NO];
    Bitboard    RayLine_bb[SQ_NO][SQ_NO];

    Bitboard  DistRings_bb[SQ_NO][8];

    // Span of the attacks of pawn
    Bitboard PawnAttackSpan[CLR_NO][SQ_NO];
    // Path of the passed pawn
    Bitboard   PawnPassSpan[CLR_NO][SQ_NO];

    // Attacks of the pawns & pieces
    Bitboard   PawnAttacks[CLR_NO][SQ_NO];
    Bitboard  PieceAttacks[NONE][SQ_NO];

    Bitboard *B_Attacks_bb[SQ_NO];
    Bitboard *R_Attacks_bb[SQ_NO];

    Bitboard    B_Masks_bb[SQ_NO];
    Bitboard    R_Masks_bb[SQ_NO];

#if !defined(BM2)
    Bitboard   B_Magics_bb[SQ_NO];
    Bitboard   R_Magics_bb[SQ_NO];

    u08           B_Shifts[SQ_NO];
    u08           R_Shifts[SQ_NO];
#endif

    namespace {

//        // De Bruijn sequences. See chessprogramming.wikispaces.com/BitScan
//        const u64 DeBruijn_64 = U64(0x3F79D71B4CB0A89);
//        const u32 DeBruijn_32 = U32(0x783A9B23);
//
//        i08 MSB_Table[256];
//        Square BSF_Table[SQ_NO];
//
//        unsigned bsf_index (Bitboard bb)
//        {
//            assert(bb != 0);
//            bb ^= (bb - 1);
//            return
//#       if defined(BIT64)
//          // Use Kim Walisch extending trick for 64-bit
//              (bb * DeBruijn_64) >> 58;
//#       else
//          // Use Matt Taylor's folding trick for 32-bit
//              (u32 ((bb >> 0) ^ (bb >> 32)) * DeBruijn_32) >> 26;
//#       endif
//        }

        // Max Linear Table Size (for rook from any corner square)
        // 2 ^ 12 = 4096 = 0x1000
        const u16 MaxLTSize  = U32(0x1000);

        // Max Bishop Table Size
        // 4 * 2^9 + 4 * 2^6 + 12 * 2^7 + 44 * 2^5
        // 4 * 512 + 4 *  64 + 12 * 128 + 44 *  32
        //    2048 +     256 +     1536 +     1408
        //                                    5248 = 0x1480
        const u32 MaxBTSize = U32(0x1480);

        // Max Rook Table Size
        // 4 * 2^12 + 24 * 2^11 + 36 * 2^10
        // 4 * 4096 + 24 * 2048 + 36 * 1024
        //    16384 +     49152 +     36864
        //                           102400 = 0x19000
        const u32 MaxRTSize = U32(0x19000);

        Bitboard B_Tables_bb[MaxBTSize];
        Bitboard R_Tables_bb[MaxRTSize];

        typedef u16(*Indexer) (Square s, Bitboard occ);

        // initialize_table() computes all rook and bishop attacks at startup.
        // Magic bitboards are used to look up attacks of sliding pieces.
        // As a reference see chessprogramming.wikispaces.com/Magic+Bitboards.
        // In particular, here we use the so called "fancy" approach.
        void initialize_table (Bitboard tables_bb[], Bitboard *attacks_bb[], Bitboard masks_bb[], Bitboard magics_bb[], u08 shifts[], const Delta deltas[], const Indexer magic_index)
        {

#       if !defined(BM2)
            const u32 Seeds[R_NO] =
#           if defined(BIT64)
                { 0x002D8, 0x0284C, 0x0D6E5, 0x08023, 0x02FF9, 0x03AFC, 0x04105, 0x000FF };
#           else
                { 0x02311, 0x0AE10, 0x0D447, 0x09856, 0x01663, 0x173E5, 0x199D0, 0x0427C };
#           endif

            Bitboard occupancy[MaxLTSize]
                   , reference[MaxLTSize];

            i32 max_ages[MaxLTSize] = {0}, cur_age = 0;

#       endif

            // attacks_bb[s] is a pointer to the beginning of the attacks table for square 's'
            attacks_bb[SQ_A1] = tables_bb;

            for (auto s = SQ_A1; s <= SQ_H8; ++s)
            {
                // Board edges are not considered in the relevant occupancies
                Bitboard edges = board_edges (s);

                // Given a square 's', the mask is the bitboard of sliding attacks from 's'
                // computed on an empty board. The index must be big enough to contain
                // all the attacks for each possible subset of the mask and so is 2 power
                // the number of 1s of the mask. Hence deduce the size of the shift to
                // apply to the 64 or 32 bits word to get the index.
                Bitboard moves = sliding_attacks (deltas, s);

                Bitboard mask = masks_bb[s] = moves & ~edges;

#       if defined(BM2)
                (void) shifts;
#       else
                shifts[s] =
#           if defined(BIT64)
                    64
#           else
                    32
#           endif
                    - u08(pop_count (mask));
#       endif

                // Use Carry-Rippler trick to enumerate all subsets of masks_bb[s] and
                // store the corresponding sliding attack bitboard in reference[].
                u32 size = 0;
                Bitboard occ = 0;
                do {
#               if defined(BM2)
                    attacks_bb[s][PEXT(occ, mask)] = sliding_attacks (deltas, s, occ);
#               else
                    occupancy[size] = occ;
                    reference[size] = sliding_attacks (deltas, s, occ);
#               endif

                    ++size;
                    occ = (occ - mask) & mask;
                } while (occ != 0);

                // Set the offset for the table_bb of the next square. Have individual
                // table_bb sizes for each square with "Fancy Magic Bitboards".
                if (s < SQ_H8)
                {
                    attacks_bb[s + 1] = attacks_bb[s] + size;
                }

#       if defined(BM2)
                (void) magics_bb;
                (void) magic_index;
#       else
                PRNG rng (Seeds[_rank (s)]);
                u32 i;
                
                // Find a magic for square 's' picking up an (almost) random number
                // until found the one that passes the verification test.
                do {
                    do {
                        magics_bb[s] = rng.sparse_rand<Bitboard> ();
                    } while (pop_count ((mask * magics_bb[s]) >> 0x38) < 6);

                    // A good magic must map every possible occupancy to an index that
                    // looks up the correct sliding attack in the attacks_bb[s] database.
                    // Note that build up the database for square 's' as a side
                    // effect of verifying the magic.
                    for (++cur_age, i = 0; i < size; ++i)
                    {
                        u16 idx = magic_index (s, occupancy[i]);
                        
                        if (max_ages[idx] < cur_age)
                        {
                            max_ages[idx] = cur_age;
                            attacks_bb[s][idx] = reference[i];
                        }
                        else
                        {
                            if (attacks_bb[s][idx] != reference[i]) break;
                        }
                    }
                } while (i < size);
#       endif
            }
        }

        void initialize_sliding ()
        {
#       if defined(BM2)
            initialize_table (B_Tables_bb, B_Attacks_bb, B_Masks_bb, nullptr, nullptr, PieceDeltas[BSHP], magic_index<BSHP>);
            initialize_table (R_Tables_bb, R_Attacks_bb, R_Masks_bb, nullptr, nullptr, PieceDeltas[ROOK], magic_index<ROOK>);
#       else
            initialize_table (B_Tables_bb, B_Attacks_bb, B_Masks_bb, B_Magics_bb, B_Shifts, PieceDeltas[BSHP], magic_index<BSHP>);
            initialize_table (R_Tables_bb, R_Attacks_bb, R_Masks_bb, R_Magics_bb, R_Shifts, PieceDeltas[ROOK], magic_index<ROOK>);
#       endif
        }

    }

    void initialize ()
    {
        //for (auto s = SQ_A1; s <= SQ_H8; ++s)
        //{
        //    BSF_Table[bsf_index (Square_bb[s] = 1ULL << s)] = s;
        //    BSF_Table[bsf_index (Square_bb[s])] = s;
        //}
        //for (auto b = 2; b <= UCHAR_MAX; ++b)
        //{
        //    MSB_Table[b] =  MSB_Table[b - 1] + !more_than_one (Bitboard(b));
        //}

        for (auto s1 = SQ_A1; s1 <= SQ_H8; ++s1)
        {
            for (auto s2 = SQ_A1; s2 <= SQ_H8; ++s2)
            {
                if (s1 != s2)
                {
                    SquareDist[s1][s2] = u08(max (dist<File> (s1, s2) , dist<Rank> (s1, s2)));
                    DistRings_bb[s1][SquareDist[s1][s2] - 1] += s2;
                }
            }
        }

        for (auto c = WHITE; c <= BLACK; ++c)
        {
            for (auto s = SQ_A1; s <= SQ_H8; ++s)
            {
                FrontSqrs_bb  [c][s] = FrontRank_bb[c][_rank (s)] &    File_bb[_file (s)];
                PawnAttackSpan[c][s] = FrontRank_bb[c][_rank (s)] & AdjFile_bb[_file (s)];
                PawnPassSpan  [c][s] = FrontSqrs_bb[c][s] | PawnAttackSpan[c][s];
            }
        }

        for (auto s = SQ_A1; s <= SQ_H8; ++s)
        {
            u08 k;
            Delta del;

            for (auto c = WHITE; c <= BLACK; ++c)
            {
                k = 0;
                while ((del = PawnDeltas[c][k++]) != DEL_O)
                {
                    auto sq = s + del;
                    if (_ok (sq) && dist (s, sq) == 1)
                    {
                        PawnAttacks[c][s] += sq;
                    }
                }
            }

            PieceType pt;

            pt = NIHT;
            k = 0;
            while ((del = PieceDeltas[pt][k++]) != DEL_O)
            {
                auto sq = s + del;
                if (_ok (sq) && dist (s, sq) == 2)
                {
                    PieceAttacks[pt][s] += sq;
                }
            }

            pt = KING;
            k = 0;
            while ((del = PieceDeltas[pt][k++]) != DEL_O)
            {
                auto sq = s + del;
                if (_ok (sq) && dist (s, sq) == 1)
                {
                    PieceAttacks[pt][s] += sq;
                }
            }

            PieceAttacks[BSHP][s] = sliding_attacks (PieceDeltas[BSHP], s);
            PieceAttacks[ROOK][s] = sliding_attacks (PieceDeltas[ROOK], s);
            PieceAttacks[QUEN][s] = PieceAttacks[BSHP][s] | PieceAttacks[ROOK][s];
        }

        initialize_sliding ();

#if !defined(NDEBUG)
        //test_attacks ();
#endif

        // NOTE:: must be after initialize_sliding()
        for (auto pt = BSHP; pt <= ROOK; ++pt)
        {
            for (auto s1 = SQ_A1; s1 <= SQ_H8; ++s1)
            {
                for (auto s2 = SQ_A1; s2 <= SQ_H8; ++s2)
                {
                    if ((PieceAttacks[pt][s1] & s2) != 0)
                    {
                        Between_bb[s1][s2] = (attacks_bb (Piece(pt), s1, Square_bb[s2]) & attacks_bb (Piece(pt), s2, Square_bb[s1]));
                        RayLine_bb[s1][s2] = (attacks_bb (Piece(pt), s1,        0) & attacks_bb (Piece(pt), s2,        0)) + s1 + s2;
                    }
                }
            }
        }

    }

#if !defined(NDEBUG)

    // pretty() returns an ASCII representation of a bitboard to print on console output
    // Bitboard in an easily readable format. This is sometimes useful for debugging.
    string pretty (Bitboard bb, char p)
    {
        const string ROW  = "|. . . . . . . .|\n";

        string sbb;
        sbb = " /---------------\\\n";
        for (auto r = R_8; r >= R_1; --r)
        {
            sbb += Notation::to_char (r) + ROW;
        }
        sbb += " \\---------------/\n ";
        for (auto f = F_A; f <= F_H; ++f)
        {
            sbb += " "; sbb += Notation::to_char (f);
        }
        sbb += "\n";

        while (bb != 0)
        {
            auto s = pop_lsq (bb);
            sbb[2 + (ROW.length () + 1) * (8 - _rank (s)) + 2 * _file (s)] = p;
        }

        return sbb;
    }

    //void test_attacks ()
    //{
    //    Bitboard occ = U64(0x1234);
    //    std::cout << "occupancy:\n" << pretty (occ);
    //    // Knight
    //    for (auto s1 = SQ_A1; s1 <= SQ_H8; ++s1)
    //    {
    //        std::cout << pretty (attacks_bb<NIHT> (s1, occ));
    //        if (s1 && (s1+1)%8 == 0) system ("PAUSE");
    //    }
    //    std::cout << "occupancy:\n" << pretty (occ);
    //    // Bishop
    //    for (auto s1 = SQ_A1; s1 <= SQ_H8; ++s1)
    //    {
    //        std::cout << pretty (attacks_bb<BSHP> (s1, occ));
    //        if (s1 && (s1+1)%8 == 0) system("PAUSE");
    //    }
    //    std::cout << "occupancy:\n" << pretty (occ);
    //    // Rook
    //    for (auto s1 = SQ_A1; s1 <= SQ_H8; ++s1)
    //    {
    //        std::cout << pretty (attacks_bb<ROOK> (s1, occ));
    //        if (s1 && (s1+1)%8 == 0) system ("PAUSE");
    //    }
    //}

#endif

}
