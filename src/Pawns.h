#pragma once

#include <array>
#include "Type.h"
#include "Position.h"

namespace Pawns {

    constexpr u08 MaxCache = 4;
    /// Pawns::Entry contains various information about a pawn structure.
    struct Entry
    {
    public:
        Key key;

        std::array<Score   , CLR_NO> scores;
        std::array<Bitboard, CLR_NO> attack_span;
        std::array<Bitboard, CLR_NO> passers;

        std::array<u08, CLR_NO>                          index;
        std::array<std::array<Square, MaxCache>, CLR_NO> king_square;
        std::array<std::array<u08,    MaxCache>, CLR_NO> king_pawn_dist;
        std::array<std::array<Score,  MaxCache>, CLR_NO> king_safety;

        i32 passed_count() const
        {
            return BitBoard::pop_count(passers[WHITE] | passers[BLACK]);
        }

        template<Color Own>
        u08 king_safety_on(Position const &pos, Square own_k_sq)
        {
            auto idx = std::find(king_square[Own].begin(), king_square[Own].begin() + index[Own] + 1, own_k_sq)
                     - king_square[Own].begin();
            assert(0 <= idx);
            if (idx <= index[Own])
            {
                return u08(idx);
            }

            idx = index[Own];
            if (idx < MaxCache - 1)
            {
                ++index[Own];
            }

            // In endgame, king near to closest pawn
            Bitboard pawns = pos.pieces(Own, PAWN);
            u08 kp_dist;
            if (0 != pawns)
            {
                if (0 != (pawns & BitBoard::PieceAttacks[KING][own_k_sq]))
                {
                    kp_dist = 1;
                }
                else
                {
                    kp_dist = 8;
                    while (0 != pawns)
                    {
                        kp_dist = std::min((u08)BitBoard::dist(own_k_sq, BitBoard::pop_lsq(pawns)), kp_dist);
                    }
                }
            }
            else
            {
                kp_dist = 0;
            }

            king_square[Own][idx] = own_k_sq;
            king_pawn_dist[Own][idx] = kp_dist;
            king_safety[Own][idx] = evaluate_safety<Own>(pos, own_k_sq);
            return u08(idx);
        }

        template<Color>
        Score evaluate_safety(Position const&, Square) const;

        template<Color>
        void evaluate(Position const&);

    };

    typedef HashTable<Entry, 0x20000> Table;

    extern Entry* probe(Position const&);
}
