#include "Pawns.h"

#include "BitBoard.h"
#include "Thread.h"

namespace Pawns {

    using namespace std;
    using namespace BitBoard;

    namespace {

    #define V(v) Value(v)

        // Weakness of friendly pawn shelter in front of the friendly king indexed by [distance from edge][rank]
        const Value ShelterWeak[F_NO/2][R_NO] =
        {
            { V( 97), V(21), V(26), V(51), V(87), V( 89), V( 99), V( 0) }, // => A and H file
            { V(120), V( 0), V(28), V(76), V(88), V(103), V(104), V( 0) }, // => B and G file
            { V(101), V( 7), V(54), V(78), V(77), V( 92), V(101), V( 0) }, // => C and F file
            { V( 80), V(11), V(44), V(68), V(87), V( 90), V(119), V( 0) }  // => D and E file
        };

        enum BlockType : u08
        {
            NO_FRIENDLY_PAWN,
            BLOCKED_NONE,
            BLOCKED_BY_PAWN,
            BLOCKED_BY_KING,
        };
        // Dangerousness of enemy pawns moving toward the friendly king indexed by [block-type][distance from edge][rank]
        const Value StromDanger[4][F_NO/2][R_NO] =
        {
            {
                { V( 0), V(  67), V(134), V(38), V(32), V( 0), V( 0), V( 0) },
                { V( 0), V(  57), V(139), V(37), V(22), V( 0), V( 0), V( 0) },
                { V( 0), V(  43), V(115), V(43), V(27), V( 0), V( 0), V( 0) },
                { V( 0), V(  68), V(124), V(57), V(32), V( 0), V( 0), V( 0) }
            },
            {
                { V(20), V(  43), V(100), V(56), V(20), V( 0), V( 0), V( 0) },
                { V(23), V(  20), V( 98), V(40), V(15), V( 0), V( 0), V( 0) },
                { V(23), V(  39), V(103), V(36), V(18), V( 0), V( 0), V( 0) },
                { V(28), V(  19), V(108), V(42), V(26), V( 0), V( 0), V( 0) }
            },
            {
                { V( 0), V(   0), V( 75), V(14), V( 2), V( 0), V( 0), V( 0) },
                { V( 0), V(   0), V(150), V(30), V( 4), V( 0), V( 0), V( 0) },
                { V( 0), V(   0), V(160), V(22), V( 5), V( 0), V( 0), V( 0) },
                { V( 0), V(   0), V(166), V(24), V(13), V( 0), V( 0), V( 0) }
            },
            {
                { V( 0), V(-283), V(-281), V(57), V(31), V( 0), V( 0), V( 0) },
                { V( 0), V(  58), V( 141), V(39), V(18), V( 0), V( 0), V( 0) },
                { V( 0), V(  65), V( 142), V(48), V(32), V( 0), V( 0), V( 0) },
                { V( 0), V(  60), V( 126), V(51), V(19), V( 0), V( 0), V( 0) }
            }
        };

        // Max bonus for king safety by pawns.
        // Corresponds position with all the pawns in front of the king and no enemy pawn on the horizon.
        const Value MaxSafety = V(258);

    #undef V

    #define S(mg, eg) mk_score(mg, eg)
        
        // Isolated pawn penalty by [opposed]
        const Score Isolated[2]     = { S(45,40), S(30,27) };
        // Backward pawn penalty by [opposed]
        const Score Backward[2]     = { S(56,33), S(41,19) };
        // Unsupported pawn penalty for pawns which are neither isolated or backward, by number of pawns it supports [0, 1, 2].
        const Score Unsupported[3]  = { S(17, 8), S(18, 9), S(21,12) };
        // Levered pawn bonus by [rank]
        const Score Levered[R_NO]   = { S( 0, 0), S( 0, 0), S( 0, 0), S( 0, 0), S(17,16), S(33,32), S( 0, 0), S( 0, 0) };
        // Blocked pawn penalty
        const Score Blocked         = S(18, 38);
        // Unstopped pawn bonus for pawns going to promote
        const Score Unstopped       = S(0, 20);

    #undef S

        // Connected pawn bonus by [opposed][phalanx][twice supported][rank] (by formula)
        Score Connected[2][2][2][R_NO];

        template<Color Own>
        Score evaluate (const Position &pos, Entry *e)
        {
            const auto Opp  = Own == WHITE ? BLACK  : WHITE;
            const auto Push = Own == WHITE ? DEL_N  : DEL_S;
            const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
            const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;

            const Bitboard own_pawns = pos.pieces (Own, PAWN);
            const Bitboard opp_pawns = pos.pieces (Opp, PAWN);

            e->pawn_attacks    [Own] = shift_bb<LCap> (own_pawns)
                                     | shift_bb<RCap> (own_pawns);
            e->passed_pawns    [Own] = 0;
            e->pawn_attack_span[Own] = 0;
            e->semiopen_files  [Own] = u08(0xFF);
            e->king_sq         [Own] = SQ_NO;
            e->pawns_on_sqrs   [Own][WHITE] = u08(pop_count (own_pawns & Liht_bb));
            e->pawns_on_sqrs   [Own][BLACK] = u08(pop_count (own_pawns & Dark_bb));

            auto pawn_score = SCORE_ZERO;

            Bitboard b;
            for (auto s : pos.squares<PAWN> (Own))
            {
                assert(pos[s] == (Own|PAWN));

                auto f = _file (s);
                e->semiopen_files[Own] &= u08(~(1 << f));
                e->pawn_attack_span[Own] |= pawn_attack_span (Own, s);

                Bitboard neighbours = own_pawns & adj_file_bb (f);
                Bitboard supporters = neighbours & PawnAttacks[Opp][s];
                Bitboard stoppers   = opp_pawns & pawn_pass_span (Own, s);

                bool opposed        = (opp_pawns & front_sqrs_bb (Own, s)) != 0;
                bool blocked        = (own_pawns & (s+Push)) != 0;
                bool phalanxed      = (neighbours & rank_bb (s)) != 0;
                bool connected      = phalanxed || supporters != 0;
                bool levered        = (opp_pawns & PawnAttacks[Own][s]) != 0;
                // A pawn is backward when it is behind all pawns of the same color on the adjacent files and cannot be safely advanced.
                // The pawn is backward when it cannot safely progress to next rank:
                // either there is a stoppers in the way on next rank
                // or there is a stoppers on adjacent file which controls the way to next rank.
                bool backward       =  !levered
                                    && stoppers != 0
                                    && neighbours != 0
                                    && rel_rank (Own, s) < R_6
                                    // Find the backmost rank with neighbours or stoppers
                                    && (b = rank_bb (scan_backmost_sq (Own, neighbours | stoppers))) != 0
                                    // If have an enemy pawn in the same or next rank, the pawn is
                                    // backward because it cannot advance without being captured.
                                    && (stoppers & (b | shift_bb<Push> (b & adj_file_bb (f)))) != 0;

                // Passed pawns will be properly scored in evaluation because complete attack info needed to evaluate them.
                if (   stoppers == 0
                    && (own_pawns & front_sqrs_bb (Own, s)) == 0)
                {
                    e->passed_pawns[Own] += s;
                }

                auto score = SCORE_ZERO;

                if (neighbours == 0)
                {
                    score -= Isolated[opposed ? 1 : 0];
                }
                else
                if (backward)
                {
                    score -= Backward[opposed ? 1 : 0];
                }
                else
                if (supporters == 0)
                {
                    b = neighbours & PawnAttacks[Own][s];
                    score -= Unsupported[b != 0 ? more_than_one (b) ? 2 : 1 : 0];
                }

                if (connected)
                {
                    score += Connected[opposed ? 1 : 0]
                                      [phalanxed ? 1 : 0]
                                      [more_than_one (supporters) ? 1 : 0]
                                      [rel_rank (Own, s)];
                }

                if (levered)
                {
                    score += Levered[rel_rank (Own, s)];
                }

                if (blocked)
                {
                    score -= Blocked;
                }

//#           if !defined(NDEBUG)
                //std::cout << to_string (s) << " : " << mg_value (score) << ", " << eg_value (score) << std::endl;
//#           endif
                pawn_score += score;
            }

            b = e->semiopen_files[Own] ^ u08(0xFF);
            e->pawn_span[Own] = u08(b != 0 ? scan_msq (b) - scan_lsq (b) : 0);

            return pawn_score;
        }
        // Explicit template instantiations
        template Score evaluate<WHITE> (const Position&, Entry*);
        template Score evaluate<BLACK> (const Position&, Entry*);
    }

    template<Color Own>
    Value Entry::pawn_shelter_storm (const Position &pos, Square k_sq) const
    {
        const auto Opp = Own == WHITE ? BLACK : WHITE;

        auto value = MaxSafety;

        Bitboard front_pawns =
              pos.pieces (PAWN)
            & (  rank_bb (k_sq)
               | front_rank_bb (Own, k_sq));
        Bitboard own_front_pawns = pos.pieces (Own) & front_pawns;
        Bitboard opp_front_pawns = pos.pieces (Opp) & front_pawns;

        auto kf = std::min (std::max (_file (k_sq), F_B), F_G);
        for (auto f = kf - 1; f <= kf + 1; ++f)
        {
            assert(F_A <= f && f <= F_H);

            Bitboard mid_pawns;
            mid_pawns = own_front_pawns & file_bb (f);
            auto own_r = mid_pawns != 0 ? rel_rank (Own, scan_backmost_sq (Own, mid_pawns)) : R_1;

            mid_pawns = opp_front_pawns & file_bb (f);
            auto opp_r = mid_pawns != 0 ? rel_rank (Own, scan_frntmost_sq (Opp, mid_pawns)) : R_1;

            value -= ShelterWeak[std::min (f, F_H - f)][own_r]
                   + StromDanger[   f == _file (k_sq)
                                 && opp_r == rel_rank (Own, k_sq) + 1 ? BLOCKED_BY_KING  :
                                    own_r == R_1                      ? NO_FRIENDLY_PAWN :
                                    opp_r == own_r + 1                ? BLOCKED_BY_PAWN  : BLOCKED_NONE]
                                [std::min (f, F_H - f)][opp_r];
        }

        return value;
    }
    // Explicit template instantiations
    template Value Entry::pawn_shelter_storm<WHITE> (const Position&, Square) const;
    template Value Entry::pawn_shelter_storm<BLACK> (const Position&, Square) const;

    template<Color Own>
    Score Entry::evaluate_unstoppable_pawns () const
    {
        return passed_pawns[Own] != 0 ?
                    Unstopped * i32(rel_rank (Own, scan_frntmost_sq (Own, passed_pawns[Own]))) :
                    SCORE_ZERO;
    }
    // Explicit template instantiations
    template Score Entry::evaluate_unstoppable_pawns<WHITE> () const;
    template Score Entry::evaluate_unstoppable_pawns<BLACK> () const;

    Entry* probe (const Position &pos)
    {
        auto pawn_key = pos.pawn_key ();
        auto *e = pos.thread ()->pawn_table[pawn_key];
        // If pawn key matches the position's pawn hash key,
        // it means that have analysed this pawn configuration before,
        // and can simply return the information found instead of recomputing it.
        if (   !e->used
            || e->pawn_key != pawn_key)
        {
            e->used = true;
            e->pawn_key = pawn_key;
            e->pawn_score =
                + evaluate<WHITE> (pos, e)
                - evaluate<BLACK> (pos, e);
            e->asymmetry = pop_count (e->semiopen_files[WHITE] ^ e->semiopen_files[BLACK]);
        }
        return e;
    }

    void initialize ()
    {
        static const i32 Seeds[R_NO] = { 0, 8, 19, 13, 71, 94, 169, 324 };

        for (u08 opposed = 0; opposed <= 1; ++opposed)
        {
            for (u08 phalanx = 0; phalanx <= 1; ++phalanx)
            {
                for (u08 apex = 0; apex <= 1; ++apex)
                {
                    for (auto r = R_2; r < R_8; ++r)
                    {
                        auto v = i32((i32(Seeds[r] + (Seeds[r + 1] - Seeds[r])*0.5*phalanx) >> opposed) * (1.0 + 0.5*apex));
                        Connected[opposed][phalanx][apex][r] = mk_score (v * 1 / 1, v * 5 / 8);
                    }
                }
            }
        }
    }

}
