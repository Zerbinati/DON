#include "Pawns.h"

#include "BitBoard.h"
#include "Thread.h"

namespace Pawns {

    using namespace std;
    using namespace BitBoard;

    namespace {

        constexpr i32 Seeds[R_NO] = { 0, 13, 24, 18, 65, 100, 175, 330 };

    #define V(v) Value(v)

        // Shelter of friend pawns for friend king by [distance from edge][rank].
        // RANK_1 = 0 is used for files where no friend pawn, or friend pawn is behind friend king.
        constexpr Value Shelter[F_NO/2][R_NO] =
        {
            { V( 16), V( 82), V( 83), V( 47), V( 19), V( 44), V(  4), V(0) },
            { V(-51), V( 56), V( 33), V(-58), V(-57), V(-50), V(-39), V(0) },
            { V(-20), V( 71), V( 16), V(-10), V( 13), V( 19), V(-30), V(0) },
            { V(-29), V( 12), V(-21), V(-40), V(-15), V(-77), V(-91), V(0) }
        };

        // Storm of blocked enemy pawns moving toward friend king, indexed by [rank]
        constexpr Value BlockedStorm[R_NO] =
        {
            V(  0), V(  0), V( 81), V( -9), V( -5), V( -1), V( 26), V(0)
        };
        // Storm of unblocked enemy pawns moving toward friend king, indexed by [distance from edge][rank].
        // RANK_1 = 0 is used for files where no enemy pawn, or enemy pawn is behind friend king.
        constexpr Value UnblockedStorm[F_NO/2][R_NO] =
        {
            { V( 54), V( 48), V( 99), V( 91), V( 42), V( 32), V( 31), V(0) },
            { V( 34), V( 27), V(105), V( 38), V( 32), V(-19), V(  3), V(0) },
            { V( -4), V( 28), V( 87), V( 18), V( -3), V(-14), V(-11), V(0) },
            { V( -5), V( 22), V( 75), V( 14), V(  2), V( -5), V(-19), V(0) }
        };

    #undef V

    #define S(mg, eg) mk_score(mg, eg)

        // Penalty for isolated pawn
        constexpr Score Isolated = S( 4,20);
        // Penalty for backward pawn
        constexpr Score Backward = S(21,22);
        // Penalty for blocked pawn
        constexpr Score Blocked =  S(12,54);

    #undef S

        // Bonus for connected pawn indexed by [opposed][phalanx][twice supported][rank]
        Score Connected[2][2][3][R_NO];

        template<Color Own>
        Score evaluate (const Position &pos, Entry *e)
        {
            constexpr auto Opp = WHITE == Own ? BLACK : WHITE;
            constexpr auto Push = WHITE == Own ? DEL_N : DEL_S;
            const auto PawnAtt = PawnAttacks[Own];

            Bitboard own_pawns = pos.pieces (Own, PAWN);
            Bitboard opp_pawns = pos.pieces (Opp, PAWN);

            Bitboard ul = shift<WHITE == Own ? DEL_NW : DEL_SE> (own_pawns);
            Bitboard ur = shift<WHITE == Own ? DEL_NE : DEL_SW> (own_pawns);

            e->any_attacks[Own] = ul | ur;
            e->dbl_attacks[Own] = ul & ur;
            e->attack_span[Own] = 0;
            e->passers[Own] = 0;
            e->weak_unopposed[Own] = 0;
            e->semiopens[Own] = u08(0xFF);
            e->color_count[Own][WHITE] = u08(pop_count (own_pawns & Color_bb[WHITE]));
            e->color_count[Own][BLACK] = u08(pop_count (own_pawns & Color_bb[BLACK]));
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
                Bitboard supporters = neighbours & rank_bb (s-Push);
                Bitboard phalanxes  = neighbours & rank_bb (s);
                Bitboard stoppers   = opp_pawns & pawn_pass_span (Own, s);
                Bitboard levers     = opp_pawns & PawnAtt[s];
                Bitboard escapes    = opp_pawns & PawnAtt[s+Push];

                bool blocked = contains (own_pawns, s-Push);
                bool opposed = 0 != (opp_pawns & front_line_bb (Own, s));

                // A pawn is backward when it is behind all pawns of the same color on the adjacent files and cannot be safely advanced.
                bool backward = 0 == (own_pawns & pawn_attack_span (Opp, s + Push))
                             && 0 != (stoppers & (escapes | (s + Push)));

                assert(!backward
                    || 0 == (pawn_attack_span (Opp, s+Push) & neighbours));

                // Include also not passed pawns which could become passed
                // after one or two pawn pushes when are not attacked more times than defended.
                // Passed pawns will be properly scored in evaluation because complete attack info needed to evaluate them.
                if (   stoppers == (levers | escapes)
                    && 0 == (own_pawns & front_line_bb (Own, s))
                    && pop_count (supporters) >= pop_count (levers) - 1
                    && pop_count (phalanxes) >= pop_count (escapes))
                {
                    e->passers[Own] |= s;
                }
                else
                if (   stoppers == square_bb (s+Push)
                    && R_4 < rel_rank (Own, s))
                {
                    b = shift<Push> (supporters) & ~opp_pawns;
                    while (0 != b)
                    {
                        if (!more_than_one (opp_pawns & PawnAtt[pop_lsq (b)]))
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
        constexpr auto Pull = WHITE == Own ? DEL_S : DEL_N;
        constexpr Bitboard BlockSquares = (WHITE == Own ? R1_bb | R2_bb : R8_bb | R7_bb) & (FA_bb | FH_bb);

        Bitboard front_ranks = rank_bb (fk_sq)
                             | front_rank_bb (Own, fk_sq);
        Bitboard own_front_pawns = pos.pieces (Own, PAWN) & front_ranks;
        Bitboard opp_front_pawns = pos.pieces (Opp, PAWN) & front_ranks;

        auto value = Value(0 != (own_front_pawns & file_bb (fk_sq)) ? +5 : -5);

        if (contains (shift<Pull> (opp_front_pawns) & BlockSquares, fk_sq))
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
            value += Shelter[ff][own_r];
            value -= R_1 != own_r && (own_r == opp_r - 1) ?
                        BlockedStorm[opp_r] :
                        UnblockedStorm[ff][opp_r];
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
        auto key = pos.si->pawn_key;
        auto *e = pos.thread->pawn_table[key];

        if (e->key == key)
        {
            return e;
        }

        e->key = key;
        e->scores[WHITE] = evaluate<WHITE> (pos, e);
        e->scores[BLACK] = evaluate<BLACK> (pos, e);
        e->open_count = u08(pop_count ((e->semiopens[WHITE] & e->semiopens[BLACK])));
        e->asymmetry  = u08(pop_count ((e->passers  [WHITE] | e->passers  [BLACK])
                                     | (e->semiopens[WHITE] ^ e->semiopens[BLACK])));
        return e;
    }

    /// Pawns::initialize() initializes lookup tables at startup.
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
                        auto v = 17 * support + ((Seeds[r] + (0 != phalanx ? (Seeds[r + 1] - Seeds[r]) / 2 : 0)) >> opposed);
                        Connected[opposed][phalanx][support][r] = mk_score (v, v * (r-2) / 4);
                    }
                }
            }
        }
    }
}
