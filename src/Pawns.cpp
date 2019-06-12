#include "Pawns.h"

#include <algorithm>
#include <cassert>
#include "BitBoard.h"
#include "Thread.h"

namespace Pawns {

    using namespace std;
    using namespace BitBoard;

    namespace {

        // Connected pawn bonus
        constexpr i32 Connected[R_NO] = { 0, 7, 8, 12, 29, 48, 86, 0 };

        // Safety of friend pawns shelter for our king by [distance from edge][rank].
        // RANK_1 is used for files where we have no pawn, or pawn is behind our king.
        constexpr i32 Shelter[F_NO/2][R_NO] =
        {
            {  -6,  81,  93,  58,  39,  18,   25, 0 },
            { -43,  61,  35, -49, -29, -11,  -63, 0 },
            { -10,  75,  23,  -2,  32,   3,  -45, 0 },
            { -39, -13, -29, -52, -48, -67, -166, 0 }
        };

        // Danger of unblocked enemy pawns strom toward our king by [distance from edge][rank].
        // RANK_1 is used for files where the enemy has no pawn, or their pawn is behind our king.
        constexpr i32 Storm[F_NO/2][R_NO] =
        {
            {  89, 107, 123,  93,  57,  45,   51, 0 },
            {  44, -18, 123,  46,  39,  -7,   23, 0 },
            {   4,  52, 162,  37,   7, -14,   -2, 0 },
            { -10, -14,  90,  15,   2,  -7,  -16, 0 }
        };

#   define S(mg, eg) mk_score(mg, eg)

        constexpr Score Backward = S( 9,24);
        constexpr Score Blocked =  S(11,56);
        constexpr Score Isolated = S( 5,15);
        constexpr Score WeakUnopposed = S(13, 27);

#   undef S

        template<Color Own>
        Score evaluate (const Position &pos, Entry *e)
        {
            constexpr auto Opp = WHITE == Own ? BLACK : WHITE;
            Bitboard *const Attack = PawnAttacks[Own];

            Bitboard pawns = pos.pieces (PAWN);
            Bitboard empty = ~pawns;
            Bitboard own_pawns = pos.pieces (Own) & pawns;
            Bitboard opp_pawns = pos.pieces (Opp) & pawns;

            e->attack_span[Own] = 0;
            e->passers[Own] = 0;
            
            e->index[Own] = 0;
            std::fill_n (e->king_square[Own], MaxCache, SQ_NO);
            std::fill_n (e->king_safety[Own], MaxCache, SCORE_ZERO);
            std::fill_n (e->king_pawn_dist[Own], MaxCache, 0);

            e->king_safety_on<Own> (pos, rel_sq (Own, SQ_G1));
            e->king_safety_on<Own> (pos, rel_sq (Own, SQ_C1));

            Score score = SCORE_ZERO;

            Bitboard b;
            for (const auto &s : pos.squares[Own|PAWN])
            {
                assert((Own|PAWN) == pos[s]);

                auto r = rel_rank (Own, s);
                e->attack_span[Own] |= pawn_attack_span (Own, s);

                Bitboard neighbours = own_pawns & adj_file_bb (s);
                Bitboard supporters = neighbours & rank_bb (s-pawn_push (Own));
                Bitboard phalanxes  = neighbours & rank_bb (s);
                Bitboard stoppers   = opp_pawns & pawn_pass_span (Own, s);
                Bitboard levers     = opp_pawns & Attack[s];
                Bitboard escapes    = opp_pawns & Attack[s+pawn_push (Own)]; // Push levers

                bool blocked = contains (own_pawns, s-pawn_push (Own));
                bool opposed = 0 != (opp_pawns & front_squares_bb (Own, s));

                // A pawn is backward when it is behind all pawns of the same color on the adjacent files and cannot be safely advanced.
                bool backward = 0 == (own_pawns & pawn_attack_span (Opp, s+pawn_push (Own)))
                             && 0 != (stoppers & (escapes | (s+pawn_push (Own))));

                assert(!backward
                    || 0 == (pawn_attack_span (Opp, s+pawn_push (Own)) & neighbours));

                // Include also which could become passed after pawn push
                if (   stoppers == (levers | escapes)
                    && (   stoppers == levers
                        || pop_count (phalanxes) >= pop_count (escapes)))
                {
                    e->passers[Own] |= s;
                }
                else
                if (   R_4 < r
                    && stoppers == square_bb (s+pawn_push (Own)))
                {
                    b = pawn_sgl_pushes_bb (Own, supporters)
                      & empty;
                    while (0 != b)
                    {
                        if (!more_than_one (opp_pawns & Attack[pop_lsq (b)]))
                        {
                            e->passers[Own] |= s;
                        }
                    }
                }

                if (0 != (supporters | phalanxes))
                {
                    i32 v = Connected[r] * (phalanxes ? 3 : 2) / (opposed ? 2 : 1)
                          + 17 * pop_count (supporters);
                    score += mk_score (v, v * (r - R_3) / 4);
                }
                else
                if (0 == neighbours)
                {
                    score -= Isolated;
                    if (!opposed)
                    {
                        score += WeakUnopposed;
                    }
                }
                else
                if (backward)
                {
                    score -= Backward;
                    if (!opposed)
                    {
                        score += WeakUnopposed;
                    }
                }

                if (   blocked
                    && 0 == supporters)
                {
                    score -= Blocked;
                }
            }

            return score;
        }
        // Explicit template instantiations
        template Score evaluate<WHITE> (const Position&, Entry*);
        template Score evaluate<BLACK> (const Position&, Entry*);
    }

    /// Entry::evaluate_safety() calculates shelter & storm for a king,
    /// looking at the king file and the two closest files.
    template<Color Own>
    Score Entry::evaluate_safety (const Position &pos, Square own_k_sq) const
    {
        constexpr auto Opp = WHITE == Own ? BLACK : WHITE;
        constexpr Bitboard BlockSquares = (FA_bb | FH_bb) & (R1_bb | R2_bb | R8_bb | R7_bb);

        Bitboard front_pawns = ~front_rank_bb (Opp, own_k_sq) & pos.pieces (PAWN);
        Bitboard own_front_pawns = pos.pieces (Own) & front_pawns;
        Bitboard opp_front_pawns = pos.pieces (Opp) & front_pawns;

        i32 mg_value = contains (pawn_sgl_pushes_bb (Opp, opp_front_pawns) & BlockSquares, own_k_sq) ? 374 : 5,
            eg_value = 0;

        auto kf = clamp (F_B, _file (own_k_sq), F_G);
        for (const auto &f : { kf - File(1), kf, kf + File(1) })
        {
            assert(F_A <= f && f <= F_H);
            Bitboard own_front_f_pawns = own_front_pawns & file_bb (f);
            auto own_r = 0 != own_front_f_pawns ? rel_rank (Own, scan_backmost_sq (Own, own_front_f_pawns)) : R_1;
            Bitboard opp_front_f_pawns = opp_front_pawns & file_bb (f);
            auto opp_r = 0 != opp_front_f_pawns ? rel_rank (Own, scan_frntmost_sq (Opp, opp_front_f_pawns)) : R_1;
            assert((R_1 == own_r
                 && R_1 == opp_r)
                || (own_r != opp_r));

            auto ff = std::min (f, ~f);
            assert(ff < F_E);
            mg_value += Shelter[ff][own_r];

            if (   R_1 != own_r
                && (own_r + 1) == opp_r)
            {
                mg_value -= 82, eg_value -= 82;
            }
            else
            {
                mg_value -= Storm[ff][opp_r];
            }
        }

        return mk_score (mg_value, eg_value);
    }
    // Explicit template instantiations
    template Score Entry::evaluate_safety<WHITE> (const Position&, Square) const;
    template Score Entry::evaluate_safety<BLACK> (const Position&, Square) const;

    /// Pawns::probe() looks up a current position's pawn configuration in the pawn hash table
    /// and returns a pointer to it if found, otherwise a new Entry is computed and stored there.
    Entry* probe (const Position &pos)
    {
        auto *e = pos.thread->pawn_table[pos.si->pawn_key];

        if (e->key == pos.si->pawn_key)
        {
            return e;
        }

        e->key = pos.si->pawn_key;
        e->scores[WHITE] = evaluate<WHITE> (pos, e);
        e->scores[BLACK] = evaluate<BLACK> (pos, e);

        return e;
    }
}
