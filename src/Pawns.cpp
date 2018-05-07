#include "Pawns.h"

#include "BitBoard.h"
#include "Thread.h"

namespace Pawns {

    using namespace std;
    using namespace BitBoard;

    namespace {

    #define V(v) Value(v)

        // Shelter of pawn for our king by [distance from edge][rank].
        // RANK_1 = 0 is used for files where we have no pawn, or pawn is behind our king.
        constexpr Value Shelter[F_NO/2][R_NO] =
        {
            { V( -9), V(64), V(77), V( 44), V( 4), V( -1), V(-11), V(0) },
            { V(-15), V(83), V(51), V(-10), V( 1), V(-10), V(-28), V(0) },
            { V(-18), V(84), V(27), V(-12), V(21), V( -7), V(-36), V(0) },
            { V( 12), V(79), V(25), V( 19), V( 9), V( -6), V(-33), V(0) }
        };
        // Strom of enemy pawns moving toward the friend king, indexed by [block-type][distance from edge][rank]
        // For the unopposed and unblocked cases, R_1 = 0 is used when opponent has no pawn on the given file, or their pawn is behind our king.
        constexpr Value Strom[3][F_NO/2][R_NO] =
        {
            { // Unopposed
                { V( 4), V(73), V(132), V(46), V(31), V(0), V(0), V(0) },
                { V( 1), V(64), V(143), V(26), V(13), V(0), V(0), V(0) },
                { V( 1), V(47), V(110), V(44), V(24), V(0), V(0), V(0) },
                { V( 0), V(72), V(127), V(50), V(31), V(0), V(0), V(0) }
            },
            { // Blocked By Pawn
                { V( 0), V( 0), V( 19), V(23), V( 1), V(0), V(0), V(0) },
                { V( 0), V( 0), V( 88), V(27), V( 2), V(0), V(0), V(0) },
                { V( 0), V( 0), V(101), V(16), V( 1), V(0), V(0), V(0) },
                { V( 0), V( 0), V(111), V(22), V(15), V(0), V(0), V(0) }
            },
            { // Unblocked
                { V(22), V(45), V(104), V(62), V( 6), V(0), V(0), V(0) },
                { V(31), V(30), V( 99), V(39), V(19), V(0), V(0), V(0) },
                { V(23), V(29), V( 96), V(41), V(15), V(0), V(0), V(0) },
                { V(21), V(23), V(116), V(41), V(15), V(0), V(0), V(0) }
            }
        };

    #undef V

    #define S(mg, eg) mk_score(mg, eg)

        // Isolated pawn penalty
        constexpr Score Isolated = S(13,18);
        // Backward pawn penalty
        constexpr Score Backward = S(24,12);
        // Blocked pawn penalty
        constexpr Score Blocked =  S(18,38);

    #undef S

        // Connected pawn bonus indexed by [opposed][phalanx][twice supported][rank]
        Score Connected[2][2][3][R_NO];

        template<Color Own>
        Score evaluate (const Position &pos, Entry *e)
        {
            constexpr auto Opp = WHITE == Own ? BLACK : WHITE;
            constexpr auto Push = WHITE == Own ? DEL_N : DEL_S;
            const auto PawnAtt = PawnAttacks[Own];

            const Bitboard own_pawns = pos.pieces (Own, PAWN);
            const Bitboard opp_pawns = pos.pieces (Opp, PAWN);

            const Bitboard ul = shift<WHITE == Own ? DEL_NW : DEL_SE> (own_pawns);
            const Bitboard ur = shift<WHITE == Own ? DEL_NE : DEL_SW> (own_pawns);

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
                bool backward = 0 == levers
                             && 0 == (own_pawns & pawn_attack_span (Opp, s + Push))
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

        const Bitboard front_pawns = pos.pieces (PAWN)
                                   & (  rank_bb (fk_sq)
                                      | front_rank_bb (Own, fk_sq));
        const Bitboard own_front_pawns = pos.pieces (Own) & front_pawns;
        const Bitboard opp_front_pawns = pos.pieces (Opp) & front_pawns;

        auto value = Value(0 != (own_front_pawns & file_bb (fk_sq)) ? +5 : -5);

        if (contains (shift<Pull> (opp_front_pawns) & BlockSquares, fk_sq))
        {
            value += 374;
        }

        auto kf = std::min (F_G, std::max (F_B, _file (fk_sq)));
        for (auto f : { kf - File(1), kf, kf + File(1) })
        {
            assert(F_A <= f && f <= F_H);
            Bitboard file_front_pawns;
            file_front_pawns = own_front_pawns & file_bb (f);
            auto own_r = 0 != file_front_pawns ? rel_rank (Own, scan_backmost_sq (Own, file_front_pawns)) : R_1;
            file_front_pawns = opp_front_pawns & file_bb (f);
            auto opp_r = 0 != file_front_pawns ? rel_rank (Own, scan_frntmost_sq (Opp, file_front_pawns)) : R_1;
            assert((R_1 == own_r
                 && R_1 == opp_r)
                || (own_r != opp_r));

            auto ff = std::min (f, ~f);
            value += Shelter[ff][own_r]
                   - Strom[own_r == R_1       ? 0 : // Unopposed
                           own_r == opp_r - 1 ? 1 : // Blocked By Pawn
                                                2]  // Unblocked
                          [ff][opp_r];
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
        auto *e = pos.thread->pawn_table.get (pos.si->pawn_key);

        if (e->key == pos.si->pawn_key)
        {
            return e;
        }

        e->key = pos.si->pawn_key;
        e->scores[WHITE] = evaluate<WHITE> (pos, e);
        e->scores[BLACK] = evaluate<BLACK> (pos, e);
        e->open_count = u08(pop_count (e->semiopens[WHITE] & e->semiopens[BLACK]));
        e->asymmetry  = u08(pop_count (  (e->passers  [WHITE] | e->passers  [BLACK])
                                       | (e->semiopens[WHITE] ^ e->semiopens[BLACK])));
        return e;
    }

    /// Pawns::initialize() initializes lookup tables at startup.
    void initialize ()
    {
        const i32 Seeds[R_NO] = { 0, 13, 24, 18, 65, 100, 175, 330 };

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
