#ifndef _PAWNS_H_INC_
#define _PAWNS_H_INC_

#include "Type.h"
#include "Position.h"

namespace Pawns {

    using namespace BitBoard;

    // Pawns::Entry contains various information about a pawn structure.
    // A lookup to the pawn hash table (performed by calling the probe function)
    // returns a pointer to an Entry object.
    struct Entry
    {
    private:
        template<Color Own>
        Value pawn_shelter_storm (const Position &pos, Square k_sq) const;

    public:

        Key      pawn_key;
        Score    pawn_score;

        i32      asymmetry;

        Bitboard pawn_attacks  [CLR_NO];
        Bitboard passed_pawns  [CLR_NO];
        Bitboard pawn_attack_span[CLR_NO];

        u08      semiopen_files[CLR_NO];
        u08      pawn_span     [CLR_NO];
        // Count of pawns on LIGHT and DARK squares
        u08      pawns_on_sqrs [CLR_NO][CLR_NO]; // [color][square color]

        Square      king_sq       [CLR_NO];
        CastleRight castle_rights [CLR_NO];
        Value       king_safety   [CLR_NO][3];
        u08         king_pawn_dist[CLR_NO];

        template<Color Own>
        bool file_semiopen (File f) const
        {
            return semiopen_files[Own] & (1 << f);
        }
        template<Color Own>
        bool side_semiopen (File f, bool left) const
        {
            return semiopen_files[Own] & (left ? ((1 << f) - 1) : ~((1 << (f+1)) - 1));
        }

        template<Color Own>
        i32 pawns_on_squarecolor (Square s) const
        {
            return pawns_on_sqrs[Own][(Liht_bb & s) != U64(0) ? WHITE : BLACK];
        }

        template<Color Own>
        Score evaluate_unstoppable_pawns () const;

        template<Color Own>
        void evaluate_king_safety (const Position &pos)
        {
            if (   king_sq[Own] != pos.square<KING> (Own)
                || castle_rights[Own] != pos.can_castle (Own)
               )
            {
                king_sq      [Own] = pos.square<KING> (Own);
                castle_rights[Own] = pos.can_castle (Own);

                Rank kr = rel_rank (Own, king_sq[Own]);
                king_safety[Own][CS_KING] = kr == R_1 ? pawn_shelter_storm<Own> (pos, rel_sq (Own, SQ_WKOO )) : VALUE_ZERO;
                king_safety[Own][CS_QUEN] = kr == R_1 ? pawn_shelter_storm<Own> (pos, rel_sq (Own, SQ_WKOOO)) : VALUE_ZERO;
                king_safety[Own][CS_NO  ] = kr <= R_4 ? pawn_shelter_storm<Own> (pos, king_sq[Own]) : VALUE_ZERO;

                king_pawn_dist[Own] = 0;
                if (pos.pieces (Own, PAWN) != U64(0))
                {
                    while ((DistRings_bb[king_sq[Own]][king_pawn_dist[Own]++] & pos.pieces (Own, PAWN)) == U64(0)) {}
                }
            }
        }

    };

    typedef HashTable<Entry, 0x4000> Table; // 16384

    extern Entry* probe (const Position &pos);

    extern void initialize ();
}

#endif // _PAWNS_H_INC_
