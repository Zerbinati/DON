#ifndef _PAWNS_H_INC_
#define _PAWNS_H_INC_

#include "Type.h"
#include "Position.h"

namespace Pawns {

    // Pawns::Entry contains various information about a pawn structure.
    // A lookup to the pawn hash table (performed by calling the probe function)
    // returns a pointer to an Entry object.
    struct Entry
    {
    public:
        bool     used;
        Key      pawn_key;
        Score    pawn_score;
        i08      asymmetry;

        Bitboard pawn_attacks    [CLR_NO];
        Bitboard passed_pawns    [CLR_NO];
        Bitboard pawn_attack_span[CLR_NO];
        u08      semiopen_files  [CLR_NO];
        u08      pawn_span       [CLR_NO];
        u08      pawns_on_sq_clr [CLR_NO][CLR_NO];
        Value    king_safety     [CLR_NO][CS_NO];

        bool file_semiopen (Color c, File f) const
        {
            return (semiopen_files[c] & (1 << f)) != 0;
        }
        bool side_semiopen (Color c, File f, bool left) const
        {
            return (semiopen_files[c] & (left ? ((1 << f) - 1) : ~((1 << (f+1)) - 1))) != 0;
        }

        // evaluate_unstoppable_pawns<>() scores the most advanced passed pawns.
        template<Color Own>
        Score evaluate_unstoppable_pawns () const;

        // pawn_shelter_storm() calculates shelter and storm penalties
        // for the file the king is on, as well as the two adjacent files.
        template<Color Own>
        Value pawn_shelter_storm (const Position &pos, Square k_sq) const;

        template<Color Own>
        void evaluate_king_safety (const Position &pos)
        {
            king_safety[Own][CS_KING] = pawn_shelter_storm<Own> (pos, rel_sq (Own, SQ_G1));
            king_safety[Own][CS_QUEN] = pawn_shelter_storm<Own> (pos, rel_sq (Own, SQ_C1));
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
