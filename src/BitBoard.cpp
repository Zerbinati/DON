#include "BitBoard.h"

#include "PRNG.h"
#include "Notation.h"

namespace BitBoard {

    using namespace std;

    u08      SquareDist[SQ_NO][SQ_NO];

    Bitboard FrontLine_bb[CLR_NO][SQ_NO];

    Bitboard Between_bb[SQ_NO][SQ_NO];
    Bitboard StrLine_bb[SQ_NO][SQ_NO];

    Bitboard DistRings_bb[SQ_NO][8];

    Bitboard PawnAttackSpan[CLR_NO][SQ_NO];
    Bitboard PawnPassSpan[CLR_NO][SQ_NO];

    Bitboard PawnAttacks[CLR_NO][SQ_NO];
    Bitboard PieceAttacks[NONE][SQ_NO];

    Magic BMagics[SQ_NO]
        , RMagics[SQ_NO];

#if !defined(ABM)
    u08 PopCount16[1 << 16];
#endif

    namespace {

        constexpr Delta PawnDeltas[CLR_NO][2] =
        {
            { DEL_NW, DEL_NE },
            { DEL_SE, DEL_SW },
        };
        constexpr Delta PieceDeltas[NONE][8] =
        {
            { },
            { DEL_SSW, DEL_SSE, DEL_WWS, DEL_EES, DEL_WWN, DEL_EEN, DEL_NNW, DEL_NNE },
            { DEL_SW, DEL_SE, DEL_NW, DEL_NE },
            { DEL_S, DEL_W, DEL_E, DEL_N },
            { DEL_SW, DEL_S, DEL_SE, DEL_W, DEL_E, DEL_NW, DEL_N, DEL_NE },
            { DEL_SW, DEL_S, DEL_SE, DEL_W, DEL_E, DEL_NW, DEL_N, DEL_NE },
        };

//        // De Bruijn sequences.
//#   if defined(BIT64)
//        constexpr u64 DeBruijn_64 = U64(0x3F79D71B4CB0A89);
//#   else
//        constexpr u32 DeBruijn_32 = U32(0x783A9B23);
//#   endif
//
//        Square BSF_Table[SQ_NO];
//        unsigned bsf_index (Bitboard bb)
//        {
//            assert(0 != bb);
//            bb ^= (bb - 1);
//            return
//#       if defined(BIT64)
//            // Use Kim Walisch extending trick for 64-bit
//            (bb * DeBruijn_64) >> 58;
//#       else
//            // Use Matt Taylor's folding trick for 32-bit
//            (u32 ((bb >> 0) ^ (bb >> 32)) * DeBruijn_32) >> 26;
//#       endif
//        }
//
//        u08 MSB_Table[(1 << 8)];

#   if !defined(ABM)
        // Counts the non-zero bits using SWAR-Popcount algorithm
        u08 pop_count16 (u32 u)
        {
            u -= (u >> 1) & 0x5555U;
            u = ((u >> 2) & 0x3333U) + (u & 0x3333U);
            u = ((u >> 4) + u) & 0x0F0FU;
            return u08((u * 0x0101U) >> 8);
        }
#   endif

        // Max Bishop Table Size
        // 4 * 2^9 + 4 * 2^6 + 12 * 2^7 + 44 * 2^5
        // 4 * 512 + 4 *  64 + 12 * 128 + 44 *  32 = 4 * 0x200 + 4 * 0x40 + 12 * 0x80 + 32 * 0x20
        //    2048 +     256 +     1536 +     1408 =     0x800 +    0x100 +     0x600 +     0x580
        //                                    5248 =                                       0x1480
        constexpr u32 MaxBTSize = U32(0x1480);
        Bitboard BTable[MaxBTSize];

        // Max Rook Table Size
        // 4 * 2^12 + 24 * 2^11 + 36 * 2^10
        // 4 * 4096 + 24 * 2048 + 36 * 1024 = 4 * 0x1000 + 24 * 0x800 + 36 * 0x400
        //    16384 +     49152 +     36864 =     0x4000 +     0xC000 +     0x9000
        //                           102400 =                              0x19000
        constexpr u32 MaxRTSize = U32(0x19000);
        Bitboard RTable[MaxRTSize];

        /// Initialize all bishop and rook attacks at startup.
        /// Magic bitboards are used to look up attacks of sliding pieces.
        /// In particular, here we use the so called "fancy" approach.
        void initialize_table (PieceType pt, Bitboard *const table, Magic *const magics)
        {

#       if !defined(BM2)
            constexpr i16 MaxIndex = 0x1000;
            Bitboard occupancy[MaxIndex]
                ,    reference[MaxIndex];

            constexpr u32 Seeds[R_NO] =
#           if defined(BIT64)
                { 0x002D8, 0x0284C, 0x0D6E5, 0x08023, 0x02FF9, 0x03AFC, 0x04105, 0x000FF };
#           else
                { 0x02311, 0x0AE10, 0x0D447, 0x09856, 0x01663, 0x173E5, 0x199D0, 0x0427C };
#           endif

#       endif

            u32 offset = 0;
            for (const auto &s : SQ)
            {
                auto &magic = magics[s];

                // magics[s].attacks is a pointer to the beginning of the attacks table for square
                magic.attacks = &table[offset];

                // Given a square, the mask is the bitboard of sliding attacks from
                // computed on an empty board. The index must be big enough to contain
                // all the attacks for each possible subset of the mask and so is 2 power
                // the number of 1s of the mask. Hence deduce the size of the shift to
                // apply to the 64 or 32 bits word to get the index.
                magic.mask = slide_attacks (pt, s)
                            // Board edges are not considered in the relevant occupancies
                           & ~(((FA_bb|FH_bb) & ~file_bb (s)) | ((R1_bb|R8_bb) & ~rank_bb (s)));

#           if !defined(BM2)
                magic.shift =
#               if defined(BIT64)
                    64
#               else
                    32
#               endif
                    - u08(pop_count (magic.mask));
#           endif

                // Use Carry-Rippler trick to enumerate all subsets of magics[s].mask
                // Have individual table sizes for each square with "Fancy Magic Bitboards".
                u32 size = 0;
                Bitboard occ = 0;
                do
                {
#               if defined(BM2)
                    magic.attacks[PEXT(occ, magic.mask)] = slide_attacks (pt, s, occ);
#               else
                    occupancy[size] = occ;
                    // Store the corresponding slide attack bitboard in reference[].
                    reference[size] = slide_attacks (pt, s, occ);
#               endif

                    ++size;
                    occ = (occ - magic.mask) & magic.mask;
                }
                while (0 != occ);

#           if !defined(BM2)
                
                PRNG rng (Seeds[_rank (s)]);
                
                u32 i = 0;
                // Find a magic for square picking up an (almost) random number
                // until found the one that passes the verification test.
                while (i < size)
                {
                    magic.number = 0;
                    while (pop_count ((magic.mask * magic.number) >> 0x38) < 6)
                    {
                        magic.number = rng.sparse_rand<Bitboard> ();
                    }

                    // A good magic must map every possible occupancy to an index that
                    // looks up the correct slide attack in the magics[s].attacks database.
                    // Note that build up the database for square as a side effect of verifying the magic.
                    bool used[MaxIndex] = {false};
                    for (i = 0; i < size; ++i)
                    {
                        u16 idx = magic.index (occupancy[i]);
                        assert(idx < size);
                        if (used[idx])
                        {
                            if (magic.attacks[idx] != reference[i])
                            {
                                break;
                            }
                            continue;
                        }
                        used[idx] = true;
                        magic.attacks[idx] = reference[i];
                    }
                }
#           endif
                // Set the offset of the table for the next square.
                offset += size;
            }
        }

    }

    Bitboard slide_attacks (PieceType pt, Square s, Bitboard occ)
    {
        assert(BSHP <= pt && pt <= QUEN);
        Bitboard attacks = 0;
        for (auto del : PieceDeltas[pt])
        {
            for (auto sq = s + del; _ok (sq) && 1 == dist (sq, sq - del); sq += del)
            {
                attacks |= sq;
                if (contains (occ, sq))
                {
                    break;
                }
            }
        }
        return attacks;
    }

    void initialize ()
    {
        assert((Color_bb[WHITE] & Color_bb[BLACK]) == 0
            && (Color_bb[WHITE] | Color_bb[BLACK]) == (Color_bb[WHITE] ^ Color_bb[BLACK]));

        //for (const auto &s : SQ)
        //{
        //    //Square_bb[s] = U64(1) << s;
        //    BSF_Table[bsf_index (Square_bb[s])] = s;
        //}
        //for (u32 b = 2; b < (1 << 8); ++b)
        //{
        //    MSB_Table[b] =  MSB_Table[b - 1] + !more_than_one (b);
        //}

#   if !defined(ABM)
        for (u32 i = 0; i < (1 << 16); ++i)
        {
            PopCount16[i] = pop_count16 (i);
        }
#   endif

        for (const auto &s1 : SQ)
        {
            for (const auto &s2 : SQ)
            {
                if (s1 != s2)
                {
                    SquareDist[s1][s2] = u08(std::max (dist<File> (s1, s2), dist<Rank> (s1, s2)));
                    DistRings_bb[s1][SquareDist[s1][s2]] |= s2;
                }
            }
        }

        for (const auto &c : { WHITE, BLACK })
        {
            for (const auto &s : SQ)
            {
                FrontLine_bb  [c][s] = FrontRank_bb[c][_rank (s)] &    File_bb[_file (s)];
                PawnAttackSpan[c][s] = FrontRank_bb[c][_rank (s)] & AdjFile_bb[_file (s)];
                PawnPassSpan  [c][s] = FrontLine_bb[c][s] | PawnAttackSpan[c][s];
            }
        }

        for (const auto &s : SQ)
        {
            for (const auto &c : { WHITE, BLACK })
            {
                for (auto del : PawnDeltas[c])
                {
                    auto sq = s + del;
                    if (   _ok (sq)
                        && 1 == dist (s, sq))
                    {
                        PawnAttacks[c][s] |= sq;
                    }
                }
            }

            for (auto del : PieceDeltas[NIHT])
            {
                auto sq = s + del;
                if (   _ok (sq)
                    && 2 == dist (s, sq))
                {
                    PieceAttacks[NIHT][s] |= sq;
                }
            }

            for (auto del : PieceDeltas[KING])
            {
                auto sq = s + del;
                if (   _ok (sq)
                    && 1 == dist (s, sq))
                {
                    PieceAttacks[KING][s] |= sq;
                }
            }

            PieceAttacks[BSHP][s] = slide_attacks (BSHP, s);
            PieceAttacks[ROOK][s] = slide_attacks (ROOK, s);
            PieceAttacks[QUEN][s] = PieceAttacks[BSHP][s]
                                  | PieceAttacks[ROOK][s];
        }

        // Initialize Bishop & Rook Table
        initialize_table (BSHP, BTable, BMagics);
        initialize_table (ROOK, RTable, RMagics);

        // NOTE:: must be after initialize Bishop & Rook Table
        for (const auto &s1 : SQ)
        {
            for (const auto &s2 : SQ)
            {
                if (s1 != s2)
                {
                    if (contains (PieceAttacks[BSHP][s1], s2))
                    {
                        Between_bb[s1][s2] = attacks_bb<BSHP> (s1, Square_bb[s2])
                                           & attacks_bb<BSHP> (s2, Square_bb[s1]);
                        StrLine_bb[s1][s2] = (PieceAttacks[BSHP][s1] & PieceAttacks[BSHP][s2]) | s1 | s2;
                    }
                    else
                    if (contains (PieceAttacks[ROOK][s1], s2))
                    {
                        Between_bb[s1][s2] = attacks_bb<ROOK> (s1, Square_bb[s2])
                                           & attacks_bb<ROOK> (s2, Square_bb[s1]);
                        StrLine_bb[s1][s2] = (PieceAttacks[ROOK][s1] & PieceAttacks[ROOK][s2]) | s1 | s2;
                    }
                }
            }
        }

    }

#if !defined(NDEBUG)

    /// Returns an ASCII representation of a bitboard to print on console output
    /// Bitboard in an easily readable format. This is sometimes useful for debugging.
    string pretty (Bitboard bb)
    {
        ostringstream oss;
        oss << " /---------------\\\n";
        for (const auto &r : { R_8, R_7, R_6, R_5, R_4, R_3, R_2, R_1 })
        {
            oss << to_char (r) << "|";
            for (const auto &f : { F_A, F_B, F_C, F_D, F_E, F_F, F_G, F_H })
            {
                oss << (contains (bb, f|r) ? "+" : "-");
                if (f < F_H)
                {
                    oss << " ";
                }
            }
            oss << "|\n";
        }
        oss << " \\---------------/\n ";
        for (const auto &f : { F_A, F_B, F_C, F_D, F_E, F_F, F_G, F_H })
        {
            oss << " " << to_char (f, false);
        }
        oss << "\n";
        return oss.str ();
    }

#endif

}
