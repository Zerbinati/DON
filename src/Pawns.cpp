#include "Pawns.h"

#include "BitBoard.h"
#include "Thread.h"

namespace Pawns {

    using namespace std;
    using namespace BitBoard;

    namespace {

    #define V(v) Value(v)

        // Weakness of friend pawn shelter in front of the friend king, indexed by [is king-file][distance from edge][rank]
        // R_1 = 0 is used for files where we have no pawns or pawn is behind our king.
        const Value ShelterWeak[2][F_NO/2][R_NO] =
        {
            {// Not On King file
                { V( 98), V(20), V(11), V(42), V( 83), V( 84), V(101), V(0) },
                { V(103), V( 8), V(33), V(86), V( 87), V(105), V(113), V(0) },
                { V(100), V( 2), V(65), V(95), V( 59), V( 89), V(115), V(0) },
                { V( 72), V( 6), V(52), V(74), V( 83), V( 84), V(112), V(0) }
            },
            {// On King file
                { V(105), V(19), V( 3), V(27), V( 85), V( 93), V( 84), V(0) },
                { V(121), V( 7), V(33), V(95), V(112), V( 86), V( 72), V(0) },
                { V(121), V(26), V(65), V(90), V( 65), V( 76), V(117), V(0) },
                { V( 79), V( 0), V(45), V(65), V( 94), V( 92), V(105), V(0) }
            }
        };
        // Dangerousness of enemy pawns moving toward the friend king, indexed by [block-type][distance from edge][rank]
        // For the unopposed and unblocked cases, R_1 = 0 is used when opponent has no pawn on the given file, or their pawn is behind our king.
        const Value StromDanger[4][F_NO/2][R_NO] =
        {
            {// BlockedByKing
                { V( 0), V(-290), V(-274), V(57), V(41), V(0), V(0), V(0) },
                { V( 0), V(  60), V( 144), V(39), V(13), V(0), V(0), V(0) },
                { V( 0), V(  65), V( 141), V(41), V(34), V(0), V(0), V(0) },
                { V( 0), V(  53), V( 127), V(56), V(14), V(0), V(0), V(0) }
            },
            {// Unopposed
                { V( 4), V(  73), V(132), V(46), V(31), V(0), V(0), V(0) },
                { V( 1), V(  64), V(143), V(26), V(13), V(0), V(0), V(0) },
                { V( 1), V(  47), V(110), V(44), V(24), V(0), V(0), V(0) },
                { V( 0), V(  72), V(127), V(50), V(31), V(0), V(0), V(0) }
            },
            {// BlockedByPawn
                { V( 0), V(   0), V( 19), V(23), V( 1), V(0), V(0), V(0) },
                { V( 0), V(   0), V( 88), V(27), V( 2), V(0), V(0), V(0) },
                { V( 0), V(   0), V(101), V(16), V( 1), V(0), V(0), V(0) },
                { V( 0), V(   0), V(111), V(22), V(15), V(0), V(0), V(0) }
            },
            {// Unblocked
                { V(22), V(  45), V(104), V(62), V( 6), V(0), V(0), V(0) },
                { V(31), V(  30), V( 99), V(39), V(19), V(0), V(0), V(0) },
                { V(23), V(  29), V( 96), V(41), V(15), V(0), V(0), V(0) },
                { V(21), V(  23), V(116), V(41), V(15), V(0), V(0), V(0) }
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
                // The pawn is backward when it cannot safely progress to next rank:
                // either there is a stoppers in the way on next rank
                // or there is a stoppers on adjacent file which controls the way to next rank.
                bool backward = 0 == levers
                             && 0 != stoppers
                             && 0 != neighbours
                             && R_6 > rel_rank (Own, s)
                                // Find the back most rank with neighbours or stoppers
                             && 0 != (b = rank_bb (scan_backmost_sq (Own, neighbours | stoppers)))
                                // If have an enemy pawn in the same or next rank, the pawn is
                                // backward because it cannot advance without being captured.
                             && 0 != (stoppers & (b | shift<Push> (b & adj_file_bb (f))));

                assert(!backward
                    || 0 == (pawn_attack_span (Opp, s+Push) & neighbours));

                // Include also not passed pawns which could become passed
                // after one or two pawn pushes when are not attacked more times than defended.
                // Passed pawns will be properly scored in evaluation because complete attack info needed to evaluate them.
                if (   0 == (own_pawns & front_line_bb (Own, s))
                    && (   (   stoppers == (levers | escapes)
                            && pop_count (supporters) > pop_count (levers)
                            && pop_count (phalanxes) >= pop_count (escapes))
                        || (   stoppers == square_bb (s+Push)
                            && R_4 < rel_rank (Own, s)
                            && 0 != (b = shift<Push> (supporters) & ~opp_pawns)
                            && pop_count (b) > pop_count (  (opp_pawns ^ stoppers)
                                                          & pawn_attacks_bb (Own, b)))))
                {
                    e->passers[Own] |= s;
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

    /// Calculates shelter and storm penalties.
    /// For the king file, as well as the two closest files.
    template<Color Own>
    Value Entry::pawn_shelter_storm (const Position &pos, Square fk_sq) const
    {
        constexpr auto Opp = WHITE == Own ? BLACK : WHITE;
        // Max Safety corresponds to start position with all the pawns in front of the king and no enemy pawn on the horizon.

        auto value = Value(258);

        auto kf = std::min (F_G, std::max (F_B, _file (fk_sq)));

        const Bitboard front_pawns = pos.pieces (PAWN)
                                   & (  rank_bb (fk_sq)
                                      | front_rank_bb (Own, fk_sq))
                                   & (  file_bb (kf)
                                      | adj_file_bb (kf));
        const Bitboard own_front_pawns = pos.pieces (Own) & front_pawns;
        const Bitboard opp_front_pawns = pos.pieces (Opp) & front_pawns;

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

            auto ff = std::min (f, F_H - f);
            value -= ShelterWeak[f == _file (fk_sq) ? 1 : 0][ff][own_r]
                   + StromDanger[f == _file (fk_sq)
                              && opp_r == rel_rank (Own, fk_sq) + 1 ? 0 : // BlockedByKing
                                 own_r == R_1                       ? 1 : // Unopposed
                                 opp_r == own_r + 1                 ? 2 : // BlockedByPawn
                                                                      3]  // Unblocked
                                [ff][opp_r];
        }

        return value;
    }
    // Explicit template instantiations
    template Value Entry::pawn_shelter_storm<WHITE> (const Position&, Square) const;
    template Value Entry::pawn_shelter_storm<BLACK> (const Position&, Square) const;

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
        const i32 Seeds[R_NO] = { 0, 13, 24, 18, 76, 100, 175, 330 };

        for (auto opposed : { 0, 1 })
        {
            for (auto phalanx : { 0, 1 })
            {
                for (auto support : { 0, 1, 2 })
                {
                    for (auto r : { R_2, R_3, R_4, R_5, R_6, R_7 })
                    {
                        const i32 v = 17 * support + ((Seeds[r] + (phalanx ? (Seeds[r + 1] - Seeds[r]) / 2 : 0)) >> opposed);
                        Connected[opposed][phalanx][support][r] = mk_score (v, v * (r-2) / 4);
                    }
                }
            }
        }
    }
}
