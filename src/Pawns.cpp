#include "Pawns.h"

#include "BitBoard.h"
#include "Thread.h"

namespace Pawns {

    using namespace std;
    using namespace BitBoard;

    namespace {

        constexpr i32 Seeds[R_NO] = { 0, 13, 24, 18, 65, 100, 175, 330 };

#   define V(v) Value(v)

        constexpr Value Shelter[F_NO/2][R_NO] =
        {
            { V( -3), V( 81), V( 93), V( 58), V( 39), V( 18), V(  25), V(0) },
            { V(-40), V( 61), V( 35), V(-49), V(-29), V(-11), V( -63), V(0) },
            { V( -7), V( 75), V( 23), V( -2), V( 32), V(  3), V( -45), V(0) },
            { V(-36), V(-13), V(-29), V(-52), V(-48), V(-67), V(-166), V(0) }
        };

        constexpr Value Storm[F_NO/2+1][R_NO] =
        {
            { V( 89), V(107), V(123), V( 93), V( 57), V( 45), V(  51), V(0) },
            { V( 44), V(-18), V(123), V( 46), V( 39), V( -7), V(  23), V(0) },
            { V(  4), V( 52), V(162), V( 37), V(  7), V(-14), V(  -2), V(0) },
            { V(-10), V(-14), V( 90), V( 15), V(  2), V( -7), V( -16), V(0) },
            { V(  0), V(  0), V( 66), V(  6), V(  5), V(  1), V(  15), V(0) }
        };

#   undef V

#   define S(mg, eg) mk_score(mg, eg)

        constexpr Score Isolated = S( 5,15);
        constexpr Score Backward = S( 9,24);
        constexpr Score Blocked =  S(11,56);

#   undef S

        // Bonus for connected pawn indexed by [opposed][phalanx][twice supported][rank]
        Score Connected[2][2][3][R_NO];

        template<Color Own>
        Score evaluate (const Position &pos, Entry *e)
        {
            constexpr auto Opp = WHITE == Own ? BLACK : WHITE;

            Bitboard own_pawns = pos.pieces (Own, PAWN);
            Bitboard opp_pawns = pos.pieces (Opp, PAWN);

            Bitboard latt = pawn_lattacks_bb (Own, own_pawns);
            Bitboard ratt = pawn_rattacks_bb (Own, own_pawns);

            e->any_attacks[Own] = latt | ratt;
            e->dbl_attacks[Own] = latt & ratt;
            e->attack_span[Own] = 0;
            e->passers[Own] = 0;
            e->weak_unopposed[Own] = 0;
            e->semiopens[Own] = u08(0xFF);
            e->color_count[Own][WHITE] = pop_count (own_pawns & Color_bb[WHITE]);
            e->color_count[Own][BLACK] = pop_count (own_pawns & Color_bb[BLACK]);
            e->index[Own] = 0;
            std::fill_n (e->king_square[Own], MaxCache, SQ_NO);
            std::fill_n (e->king_safety[Own], MaxCache, VALUE_ZERO);
            std::fill_n (e->king_pawn_dist[Own], MaxCache, 0);

            e->king_safety_on<Own> (pos, rel_sq (Own, SQ_G1));
            e->king_safety_on<Own> (pos, rel_sq (Own, SQ_C1));

            auto score = SCORE_ZERO;

            Bitboard b;
            for (auto s : pos.squares[Own][PAWN])
            {
                assert(pos[s] == (Own|PAWN));

                auto f = _file (s);
                e->semiopens[Own] &= u08(~(1 << f));
                e->attack_span[Own] |= pawn_attack_span (Own, s);

                Bitboard neighbours = own_pawns & adj_file_bb (f);
                Bitboard supporters = neighbours & rank_bb (s-pawn_push (Own));
                Bitboard phalanxes  = neighbours & rank_bb (s);
                Bitboard stoppers   = opp_pawns & pawn_pass_span (Own, s);
                Bitboard levers     = opp_pawns & PawnAttacks[Own][s];
                Bitboard escapes    = opp_pawns & PawnAttacks[Own][s+pawn_push (Own)];

                bool blocked = contains (own_pawns, s-pawn_push (Own));
                bool opposed = 0 != (opp_pawns & front_line_bb (Own, s));

                // A pawn is backward when it is behind all pawns of the same color on the adjacent files and cannot be safely advanced.
                bool backward = 0 == (own_pawns & pawn_attack_span (Opp, s+pawn_push (Own)))
                             && 0 != (stoppers & (escapes | (s+pawn_push (Own))));

                assert(!backward
                    || 0 == (pawn_attack_span (Opp, s+pawn_push (Own)) & neighbours));

                // Include also not passed pawns which could become passed
                // after one or two pawn pushes when are not attacked more times than defended.
                // Passed pawns will be properly scored in evaluation because complete attack info needed to evaluate them.
                if (   stoppers == (levers | escapes)
                    && pop_count (supporters) >= pop_count (levers) - 1
                    && pop_count (phalanxes) >= pop_count (escapes))
                {
                    e->passers[Own] |= s;
                }
                else
                if (   stoppers == square_bb (s+pawn_push (Own))
                    && R_4 < rel_rank (Own, s))
                {
                    b = pawn_pushes_bb (Own, supporters) & ~opp_pawns;
                    while (0 != b)
                    {
                        if (!more_than_one (opp_pawns & PawnAttacks[Own][pop_lsq (b)]))
                        {
                            e->passers[Own] |= s;
                            break;
                        }
                    }
                }

                if (   0 != supporters
                    || 0 != phalanxes)
                {
                    score += Connected[opposed ? 1 : 0]
                                      [0 != phalanxes ? 1 : 0]
                                      [pop_count (supporters)]
                                      [rel_rank (Own, s)];
                }
                else
                if (   0 == neighbours
                    || backward)
                {
                    score -= 0 == neighbours ? Isolated : Backward;
                    if (!opposed)
                    {
                        e->weak_unopposed[Own] |= s;
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
    Value Entry::evaluate_safety (const Position &pos, Square fk_sq) const
    {
        constexpr auto Opp = WHITE == Own ? BLACK : WHITE;
        constexpr Bitboard BlockSquares = (WHITE == Own ? R1_bb | R2_bb : R8_bb | R7_bb) & (FA_bb | FH_bb);

        Bitboard front_ranks = ~front_rank_bb (Opp, fk_sq);
        Bitboard own_front_pawns = pos.pieces (Own, PAWN) & front_ranks;
        Bitboard opp_front_pawns = pos.pieces (Opp, PAWN) & front_ranks;

        auto value = Value(0 != (own_front_pawns & file_bb (fk_sq)) ? +5 : -5);

        if (contains (pawn_pushes_bb (Opp, opp_front_pawns) & BlockSquares, fk_sq))
        {
            value += Value(374);
        }

        auto kf = std::min (F_G, std::max (F_B, _file (fk_sq)));
        for (auto f : { kf - File(1), kf, kf + File(1) })
        {
            assert(F_A <= f && f <= F_H);
            Bitboard f_own_front_pawns = own_front_pawns & file_bb (f);
            auto own_r = 0 != f_own_front_pawns ? rel_rank (Own, scan_backmost_sq (Own, f_own_front_pawns)) : R_1;
            Bitboard f_opp_front_pawns = opp_front_pawns & file_bb (f);
            auto opp_r = 0 != f_opp_front_pawns ? rel_rank (Own, scan_frntmost_sq (Opp, f_opp_front_pawns)) : R_1;
            assert((R_1 == own_r
                 && R_1 == opp_r)
                || (own_r != opp_r));

            auto ff = std::min (f, ~f);
            assert(ff < 4);
            value += Shelter[ff][own_r];
            value -= Storm[R_1 != own_r && (own_r + 1 == opp_r) ? 4 : ff][opp_r];
        }

        return value;
    }
    // Explicit template instantiations
    template Value Entry::evaluate_safety<WHITE> (const Position&, Square) const;
    template Value Entry::evaluate_safety<BLACK> (const Position&, Square) const;

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
        e->open_count = u08(pop_count ((e->semiopens[WHITE] & e->semiopens[BLACK])));
        e->asymmetry  = u08(pop_count ((e->passers  [WHITE] | e->passers  [BLACK])
                                     | (e->semiopens[WHITE] ^ e->semiopens[BLACK])));
        return e;
    }

    /// Pawns::initialize() initializes pawn lookup tables.
    void initialize ()
    {
        for (i08 opposed = 0; opposed < 2; ++opposed)
        {
            for (i08 phalanx = 0; phalanx < 2; ++phalanx)
            {
                for (i08 support = 0; support < 3; ++support)
                {
                    for (auto r : { R_2, R_3, R_4, R_5, R_6, R_7 })
                    {
                        i32 v = 17 * support + ((Seeds[r] + (0 != phalanx ? (Seeds[r + 1] - Seeds[r]) / 2 : 0)) >> opposed);
                        Connected[opposed][phalanx][support][r] = mk_score (v, v * (r-2) / 4);
                    }
                }
            }
        }
    }
}
