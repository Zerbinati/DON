#ifndef _PAWNS_H_INC_
#define _PAWNS_H_INC_

#include "Type.h"
#include "Position.h"

namespace Pawns {

    constexpr u08 MaxCache = 4;
    /// Pawns::Entry contains various information about a pawn structure.
    struct Entry
    {
    public:
        Key key;

        Score    scores[CLR_NO];
        Bitboard any_attacks[CLR_NO];
        Bitboard dbl_attacks[CLR_NO];
        Bitboard attack_span[CLR_NO];
        Bitboard passers[CLR_NO];
        i32      weak_unopposed_count[CLR_NO];

        u08      index[CLR_NO];
        Square   king_square[CLR_NO][MaxCache];
        Value    king_safety[CLR_NO][MaxCache];
        u08      king_pawn_dist[CLR_NO][MaxCache];

        i32 passed_count () const
        {
            return pop_count (passers[WHITE] | passers[BLACK]);
        }

        template<Color Own>
        Value evaluate_safety (const Position&, Square) const;

        template<Color Own>
        u08 king_safety_on (const Position &pos, Square fk_sq)
        {
            auto idx = std::find (king_square[Own], king_square[Own] + index[Own] + 1, fk_sq) - king_square[Own];
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

            Bitboard pawns = pos.pieces (Own, PAWN);
            u08 kp_dist;
            if (0 != pawns)
            {
                if (0 != (pawns & PieceAttacks[KING][fk_sq]))
                {
                    kp_dist = 1;
                }
                else
                {
                    kp_dist = 8;
                    while (0 != pawns)
                    {
                        kp_dist = std::min ((u08)dist (fk_sq, pop_lsq (pawns)), kp_dist);
                    }
                }
            }
            else
            {
                kp_dist = 0;
            }

            king_square[Own][idx] = fk_sq;
            king_pawn_dist[Own][idx] = kp_dist;
            king_safety[Own][idx] = evaluate_safety<Own> (pos, fk_sq);
            return u08(idx);
        }

    };

    typedef HashTable<Entry, 0x4000> Table;

    extern Entry* probe (const Position&);
}

#endif // _PAWNS_H_INC_
