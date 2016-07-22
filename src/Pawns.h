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
        // pawn_shelter_storm() calculates shelter and storm penalties
        // for the file the king is on, as well as the two adjacent files.
        template<Color Own>
        Value pawn_shelter_storm (const Position &pos, Square k_sq) const;

    public:
        bool     used;
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

        bool file_semiopen (Color c, File f) const
        {
            return (semiopen_files[c] & (1 << f)) != 0;
        }
        bool side_semiopen (Color c, File f, bool left) const
        {
            return (semiopen_files[c] & (left ? ((1 << f) - 1) : ~((1 << (f+1)) - 1))) != 0;
        }
        i32 pawns_on_squarecolor (Color c, Square s) const
        {
            return pawns_on_sqrs[c][(Liht_bb & s) != 0 ? WHITE : BLACK];
        }

        // evaluate_unstoppable_pawns<>() scores the most advanced passed pawns.
        template<Color Own>
        Score evaluate_unstoppable_pawns () const;

        template<Color Own>
        void evaluate_king_safety (const Position &pos)
        {
            if (   king_sq      [Own] != pos.square<KING> (Own)
                || castle_rights[Own] != pos.can_castle (Own))
            {
                king_sq      [Own] = pos.square<KING> (Own);
                castle_rights[Own] = pos.can_castle (Own);

                if (   rel_rank (Own, king_sq[Own]) == R_1
                    && castle_rights[Own] != CR_NONE)
                {
                    king_safety[Own][CS_KING] =
                        pos.can_castle (Castling<Own, CS_KING>::Right) != CR_NONE ?
                            pawn_shelter_storm<Own> (pos, rel_sq (Own, SQ_G1)) : VALUE_ZERO;
                    king_safety[Own][CS_QUEN] =
                        pos.can_castle (Castling<Own, CS_QUEN>::Right) != CR_NONE ?
                            pawn_shelter_storm<Own> (pos, rel_sq (Own, SQ_C1)) : VALUE_ZERO;
                }
                else
                {
                    king_safety[Own][CS_KING] = VALUE_ZERO;
                    king_safety[Own][CS_QUEN] = VALUE_ZERO;
                }

                king_safety[Own][CS_NO] = pawn_shelter_storm<Own> (pos, king_sq[Own]);

                king_pawn_dist[Own] = 0;
                Bitboard own_pawns = pos.pieces (Own, PAWN);
                if (own_pawns != 0)
                {
                    while ((dist_rings_bb (king_sq[Own], king_pawn_dist[Own]++) & own_pawns) == 0) {}
                }
            }
        }

    };

    typedef HashTable<Entry, 0x4000> Table;

    // probe() takes a position object as input, computes a Pawn::Entry object,
    // and returns a pointer to Pawn::Entry object.
    // The result is also stored in a hash table, so don't have
    // to recompute everything when the same pawn structure occurs again.
    extern Entry* probe (const Position &pos);

    // initialize() instead of hard-coded tables, when makes sense,
    // prefer to calculate them with a formula to reduce independent parameters
    // and to allow easier tuning and better insight.
    extern void initialize ();
}

#endif // _PAWNS_H_INC_
