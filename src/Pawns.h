#ifndef _PAWNS_H_INC_
#define _PAWNS_H_INC_

#include "Type.h"
#include "Position.h"

namespace Pawns {

    const i08 MaxCache = 4;
    /// Pawns::Entry contains various information about a pawn structure.
    struct Entry
    {
    public:
        Key   key;
        Score scores[+Color::NO];
        u08   open_count;
        u08   asymmetry;

        Bitboard any_attacks[+Color::NO];
        Bitboard dbl_attacks[+Color::NO];
        Bitboard attack_span[+Color::NO];
        Bitboard passers[+Color::NO];
        Bitboard weak_unopposed[+Color::NO];

        u08    semiopens[+Color::NO];
        u08    color_count[+Color::NO][+Color::NO];

        u08    index[+Color::NO];
        Square king_square[+Color::NO][MaxCache];
        Value  king_safety[+Color::NO][MaxCache];
        u08    king_pawn_dist[+Color::NO][MaxCache];

        bool file_semiopen (Color c, File f) const
        {
            return 0 != (  semiopens[+c]
                         & (1 << +f));
        }

        template<Color Own>
        Value pawn_shelter_storm (const Position&, Square) const;

        template<Color Own>
        u08 king_safety_on (const Position &pos, Square fk_sq)
        {
            auto p = std::find (king_square[+Own], king_square[+Own] + index[+Own] + 1, fk_sq);
            if (p != king_square[+Own] + index[+Own] + 1)
            {
                return u08(p - king_square[+Own]);
            }

            u08 kp_dist = 0;
            Bitboard pawns = pos.pieces (Own, PieceType::PAWN);
            if (0 != pawns)
            {
                while (0 == (pawns & BitBoard::dist_rings_bb (fk_sq, kp_dist++))) {}
            }

            king_square[+Own][index[+Own]] = fk_sq;
            king_pawn_dist[+Own][index[+Own]] = kp_dist;
            king_safety[+Own][index[+Own]] = pawn_shelter_storm<Own> (pos, fk_sq);
            return index[+Own] < MaxCache - 1 ? index[+Own]++ : index[+Own];
        }

    };

    typedef HashTable<Entry, 0x4000> Table;

    extern Entry* probe (const Position&);

    extern void initialize ();
}

#endif // _PAWNS_H_INC_
