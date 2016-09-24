#ifndef _PAWNS_H_INC_
#define _PAWNS_H_INC_

#include "Type.h"
#include "Position.h"

namespace Pawns {

    // Pawns::Entry contains various information about a pawn structure.
    struct Entry
    {
    public:
        bool    used;
        Key     key;
        Score   score;
        u08     asymmetry;
        u08     open_count;

        Bitboard attacks    [CLR_NO];
        Bitboard attack_span[CLR_NO];
        Bitboard passers    [CLR_NO];
        u08      semiopens  [CLR_NO];
        u08      color_count[CLR_NO][CLR_NO];
        Value    castle_safety[CLR_NO][CS_NO];

        Square   king_square    [CLR_NO];
        Value    king_safety    [CLR_NO];
        u08      king_pawn_dist [CLR_NO];

        bool file_semiopen (Color c, File f) const
        {
            return (semiopens[c] & (1 << f)) != 0;
        }
        bool side_semiopen (Color c, File f, bool left) const
        {
            return (semiopens[c] & (left ? ((1 << f) - 1) : ~((1 << (f+1)) - 1))) != 0;
        }

        template<Color Own>
        Score evaluate_unstoppable_pawns () const;

        template<Color Own>
        Value pawn_shelter_storm (const Position &pos, Square k_sq) const;

        template<Color Own>
        Value do_king_safety (const Position &pos, Square k_sq)
        {
            if (king_square[Own] != k_sq)
            {
                u08 kp_dist = 0;
                Bitboard pawns = pos.pieces (Own, PAWN);
                if (pawns != 0)
                {
                    while ((pawns & dist_rings_bb (k_sq, kp_dist++)) == 0) {}
                }
                king_pawn_dist[Own] = kp_dist;
                king_safety[Own] = pawn_shelter_storm<Own> (pos, k_sq);
                king_square[Own] = k_sq;
            }
            return king_safety[Own];
        }

    };

    typedef HashTable<Entry, 0x4000> Table;

    extern Entry* probe (const Position &pos);

    extern void initialize ();
}

#endif // _PAWNS_H_INC_
