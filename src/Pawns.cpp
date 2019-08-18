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
        // [0][1 - 2] accommodate opponent pawn on edge (likely blocked by king)
        constexpr i32 Storm[F_NO/2][R_NO] =
        {
            {  89, -285, -185,  93,  57,  45,   51, 0 },
            {  44,  -18,  123,  46,  39,  -7,   23, 0 },
            {   4,   52,  162,  37,   7, -14,   -2, 0 },
            { -10,  -14,   90,  15,   2,  -7,  -16, 0 }
        };

#   define S(mg, eg) make_score(mg, eg)

        Score constexpr Backward =      S( 9,24);
        Score constexpr Blocked =       S(11,56);
        Score constexpr Isolated =      S( 5,15);
        Score constexpr WeakUnopposed = S(13,27);
        Score constexpr WeakLever =     S(0, 56);

#   undef S

        template<Color Own>
        Score evaluate(Position const &pos, Entry *e)
        {
            auto constexpr Opp = WHITE == Own ? BLACK : WHITE;
            auto const Attack = PawnAttacks[Own];

            Bitboard pawns = pos.pieces(PAWN);
            Bitboard own_pawns = pos.pieces(Own) & pawns;
            Bitboard opp_pawns = pos.pieces(Opp) & pawns;
            
            Bitboard opp_pawn_dbl_att = pawn_dbl_attacks_bb(Opp, opp_pawns);

            e->attack_span[Own] = 0;
            e->passers[Own] = 0;
            
            e->index[Own] = 0;
            e->king_square[Own].fill(SQ_NO);
            e->king_pawn_dist[Own].fill(0);
            e->king_safety[Own].fill(SCORE_ZERO);

            e->king_safety_on<Own>(pos, rel_sq(Own, SQ_G1));
            e->king_safety_on<Own>(pos, rel_sq(Own, SQ_C1));

            // Unsupported enemy pawns attacked twice by friend pawns
            Score score = SCORE_ZERO;

            for (auto const &s : pos.squares[Own|PAWN])
            {
                assert((Own|PAWN) == pos[s]);

                auto r = rel_rank(Own, s);
                e->attack_span[Own] |= pawn_attack_span(Own, s);

                Bitboard neighbours = own_pawns & adj_file_bb(s);
                Bitboard supporters = neighbours & rank_bb(s-pawn_push(Own));
                Bitboard phalanxes  = neighbours & rank_bb(s);
                Bitboard stoppers   = opp_pawns & pawn_pass_span(Own, s);
                Bitboard levers     = opp_pawns & Attack[s];
                Bitboard escapes    = opp_pawns & Attack[s+pawn_push(Own)]; // Push levers

                bool blocked = contains(own_pawns, s-pawn_push(Own));
                bool opposed = 0 != (opp_pawns & front_squares_bb(Own, s));

                // A pawn is passed if one of the three following conditions is true:
                // - there is no stoppers except some levers
                // - the only stoppers are the escapes, but we outnumber them
                // - there is only one front stopper which can be levered.
                // Passed pawns will be properly scored later in evaluation when we have full attack info.
                if (   (stoppers == levers)
                    || (   stoppers == (levers | escapes)
                        && pop_count(phalanxes) >= pop_count(escapes))
                    || (   R_4 < r
                        && stoppers == square_bb(s + pawn_push(Own))
                        && (  pawn_sgl_pushes_bb(Own, supporters)
                            & ~(opp_pawns | opp_pawn_dbl_att)) != 0))
                {
                    e->passers[Own] |= s;
                }

                if (0 != (supporters | phalanxes))
                {
                    i32 v = Connected[r] * (phalanxes ? 3 : 2) / (opposed ? 2 : 1)
                          + 17 * pop_count(supporters);
                    score += make_score(v, v * (r - R_3) / 4);
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
                // A pawn is backward when it is behind all pawns of the same color on the adjacent files and cannot be safely advanced.
                if (   0 == (neighbours & front_rank_bb(Opp, s))
                    && 0 != (stoppers & (escapes | (s+pawn_push(Own)))))
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

            // Penalize our unsupported pawns attacked twice by enemy pawns
            score -= WeakLever * pop_count(  own_pawns
                                           & opp_pawn_dbl_att
                                           & ~pawn_sgl_attacks_bb(Own, own_pawns));

            return score;
        }
        // Explicit template instantiations
        template Score evaluate<WHITE>(Position const&, Entry*);
        template Score evaluate<BLACK>(Position const&, Entry*);
    }

    /// Entry::evaluate_safety() calculates shelter & storm for a king,
    /// looking at the king file and the two closest files.
    template<Color Own>
    Score Entry::evaluate_safety(Position const &pos, Square own_k_sq) const
    {
        auto constexpr Opp = WHITE == Own ? BLACK : WHITE;

        Bitboard front_pawns = ~front_rank_bb(Opp, own_k_sq) & pos.pieces(PAWN);
        Bitboard own_front_pawns = pos.pieces(Own) & front_pawns;
        Bitboard opp_front_pawns = pos.pieces(Opp) & front_pawns;

        Score safety = make_score(5, 5);

        auto kf = clamp(_file(own_k_sq), F_B, F_G);
        for (auto const &f : { kf - File(1), kf, kf + File(1) })
        {
            assert(F_A <= f && f <= F_H);
            Bitboard own_front_f_pawns = own_front_pawns & file_bb(f);
            auto own_r = 0 != own_front_f_pawns ?
                        rel_rank(Own, scan_frontmost_sq(Opp, own_front_f_pawns)) : R_1;
            Bitboard opp_front_f_pawns = opp_front_pawns & file_bb(f);
            auto opp_r = 0 != opp_front_f_pawns ?
                        rel_rank(Own, scan_frontmost_sq(Opp, opp_front_f_pawns)) : R_1;
            assert((R_1 == own_r
                 && R_1 == opp_r)
                || (own_r != opp_r));

            auto ff = std::min(f, ~f);
            assert(ff < F_E);
            safety += make_score(Shelter[ff][own_r], 0);

            if (   R_1 != own_r
                && (own_r + 1) == opp_r)
            {
                if (opp_r == R_3)
                {
                    safety -= make_score(82, 82);
                }
            }
            else
            {
                safety -= make_score(Storm[ff][opp_r], 0);
            }
        }

        return safety;
    }
    // Explicit template instantiations
    template Score Entry::evaluate_safety<WHITE>(Position const&, Square) const;
    template Score Entry::evaluate_safety<BLACK>(Position const&, Square) const;

    /// Pawns::probe() looks up a current position's pawn configuration in the pawn hash table
    /// and returns a pointer to it if found, otherwise a new Entry is computed and stored there.
    Entry* probe(Position const &pos)
    {
        auto *e = pos.thread->pawn_table[pos.si->pawn_key];

        if (e->key == pos.si->pawn_key)
        {
            return e;
        }

        e->key = pos.si->pawn_key;
        e->scores[WHITE] = evaluate<WHITE>(pos, e);
        e->scores[BLACK] = evaluate<BLACK>(pos, e);

        return e;
    }
}
