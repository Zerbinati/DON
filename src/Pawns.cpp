#include "Pawns.h"

#include "BitBoard.h"
#include "Thread.h"

namespace Pawns {

    using namespace std;
    using namespace BitBoard;

    namespace {

    #define V(v) Value(v)

        // Weakness of friend pawn shelter in front of the friend king,
        // indexed by [distance from edge][rank]
        const Value ShelterWeak[F_NO/2][R_NO] =
        {
            { V(100), V(20), V(10), V(46), V(82), V( 86), V( 98), V(0) }, // => A and H file
            { V(116), V( 4), V(28), V(87), V(94), V(108), V(110), V(0) }, // => B and G file
            { V(109), V( 1), V(59), V(87), V(62), V( 91), V(116), V(0) }, // => C and F file
            { V( 75), V(12), V(43), V(59), V(90), V( 94), V(112), V(0) }  // => D and E file
        };

        enum BlockType : u08
        {
            BLOCKED_NO,
            BLOCKED_BY_NONE,
            BLOCKED_BY_PAWN,
            BLOCKED_BY_KING,
        };
        // Dangerousness of enemy pawns moving toward the friend king,
        // indexed by [block-type][distance from edge][rank]
        const Value StromDanger[4][F_NO/2][R_NO] =
        {
            {
                { V( 4), V(  73), V(132), V(46), V(31), V(0), V(0), V(0) },
                { V( 1), V(  64), V(143), V(26), V(13), V(0), V(0), V(0) },
                { V( 1), V(  47), V(110), V(44), V(24), V(0), V(0), V(0) },
                { V( 0), V(  72), V(127), V(50), V(31), V(0), V(0), V(0) }
            },
            {
                { V(22), V(  45), V(104), V(62), V( 6), V(0), V(0), V(0) },
                { V(31), V(  30), V( 99), V(39), V(19), V(0), V(0), V(0) },
                { V(23), V(  29), V( 96), V(41), V(15), V(0), V(0), V(0) },
                { V(21), V(  23), V(116), V(41), V(15), V(0), V(0), V(0) }
            },
            {
                { V( 0), V(   0), V( 79), V(23), V( 1), V(0), V(0), V(0) },
                { V( 0), V(   0), V(148), V(27), V( 2), V(0), V(0), V(0) },
                { V( 0), V(   0), V(161), V(16), V( 1), V(0), V(0), V(0) },
                { V( 0), V(   0), V(171), V(22), V(15), V(0), V(0), V(0) }
            },
            {
                { V( 0), V(-290), V(-274), V(57), V(41), V(0), V(0), V(0) },
                { V( 0), V(  60), V( 144), V(39), V(13), V(0), V(0), V(0) },
                { V( 0), V(  65), V( 141), V(41), V(34), V(0), V(0), V(0) },
                { V( 0), V(  53), V( 127), V(56), V(14), V(0), V(0), V(0) }
            }
        };

    #undef V

    #define S(mg, eg) mk_score(mg, eg)
        
        // Isolated pawn penalty indexed by [opposed]
        const Score Isolated[2]     = { S(45,40), S(30,27) };
        // Backward pawn penalty indexed by [opposed]
        const Score Backward[2]     = { S(56,33), S(41,19) };
        // Levered pawn bonus indexed by [rank]
        const Score Levered[R_NO]   = { S( 0, 0), S( 0, 0), S( 0, 0), S( 0, 0), S(17,16), S(33,32), S( 0, 0), S( 0, 0) };
        // Unsupported pawn penalty
        const Score Unsupported     = S(17, 8);
        // Blocked pawn penalty
        const Score Blocked         = S(18,38);
        // Unstopped pawn bonus for pawns going to promote
        const Score Unstopped       = S( 0,20);

    #undef S

        // Connected pawn bonus indexed by [opposed][phalanx][twice supported][rank]
        Score Connected[2][2][2][R_NO];

        template<Color Own>
        Score evaluate (const Position &pos, Entry *e)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N  : DEL_S;
            static const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
            static const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;
            static const auto PAtt = PawnAttacks[Own];

            const Bitboard own_pawns = pos.pieces (Own, PAWN);
            const Bitboard opp_pawns = pos.pieces (Opp, PAWN);

            e->attacks    [Own] = shift<LCap> (own_pawns)
                                | shift<RCap> (own_pawns);
            e->passers    [Own] = 0;
            e->attack_span[Own] = 0;
            e->semiopens  [Own] = u08(0xFF);
            e->color_count[Own][WHITE] = u08(pop_count (own_pawns & Color_bb[WHITE]));
            e->color_count[Own][BLACK] = u08(pop_count (own_pawns & Color_bb[BLACK]));
            e->index[Own] = 0;
            std::fill_n (e->king_square[Own], MaxCache, SQ_NO);
            e->do_king_safety<Own> (pos, rel_sq (Own, SQ_G1));
            e->do_king_safety<Own> (pos, rel_sq (Own, SQ_C1));

            auto score = SCORE_ZERO;

            File f;
            Bitboard b, neighbours, supporters, stoppers;
            bool opposed, blocked, phalanxed, connected, levered, backward;
            for (auto s : pos.squares[Own][PAWN])
            {
                assert(pos[s] == (Own|PAWN));

                f = _file (s);
                e->semiopens[Own] &= u08(~(0x01 << f));
                e->attack_span[Own] |= pawn_attack_span (Own, s);

                neighbours = own_pawns & adj_file_bb (f);
                supporters = neighbours & rank_bb (s-Push);
                stoppers   = opp_pawns & pawn_pass_span (Own, s);

                opposed    = (opp_pawns & front_sqrs_bb (Own, s)) != 0;
                blocked    = (own_pawns & (s+Push)) != 0;
                phalanxed  = (neighbours & rank_bb (s)) != 0;
                connected  = phalanxed || supporters != 0;
                levered    = (opp_pawns & PAtt[s]) != 0;
                // A pawn is backward when it is behind all pawns of the same color on the adjacent files and cannot be safely advanced.
                // The pawn is backward when it cannot safely progress to next rank:
                // either there is a stoppers in the way on next rank
                // or there is a stoppers on adjacent file which controls the way to next rank.
                backward   = !levered
                          && stoppers != 0
                          && neighbours != 0
                          && rel_rank (Own, s) < R_6
                            // Find the backmost rank with neighbours or stoppers
                          && (b = rank_bb (scan_backmost_sq (Own, neighbours | stoppers))) != 0
                            // If have an enemy pawn in the same or next rank, the pawn is
                            // backward because it cannot advance without being captured.
                          && (stoppers & (b | shift<Push> (b & adj_file_bb (f)))) != 0;

                // Passed pawns will be properly scored in evaluation because complete attack info needed to evaluate them.
                if (   stoppers == 0
                    && (own_pawns & front_sqrs_bb (Own, s)) == 0)
                {
                    e->passers[Own] |= s;
                }

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
                    score -= Unsupported;
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
            }

            return score;
        }
        // Explicit template instantiations
        template Score evaluate<WHITE> (const Position&, Entry*);
        template Score evaluate<BLACK> (const Position&, Entry*);
    }

    // Calculates shelter and storm penalties.
    template<Color Own>
    Value Entry::pawn_shelter_storm (const Position &pos, Square fk_sq) const
    {
        static const auto Opp = Own == WHITE ? BLACK : WHITE;
        // Max bonus for king safety by pawns.
        auto value = Value(258);
        Bitboard front_pawns =
              pos.pieces (PAWN)
            & (  rank_bb (fk_sq)
               | front_rank_bb (Own, fk_sq));
        Bitboard own_front_pawns = pos.pieces (Own) & front_pawns;
        Bitboard opp_front_pawns = pos.pieces (Opp) & front_pawns;

        auto kf = std::min (std::max (_file (fk_sq), F_B), F_G);
        for (auto f = kf - 1; f <= kf + 1; ++f)
        {
            assert(F_A <= f && f <= F_H);
            Bitboard file_front_pawns;
            file_front_pawns = own_front_pawns & file_bb (f);
            auto own_r = file_front_pawns != 0 ? rel_rank (Own, scan_backmost_sq (Own, file_front_pawns)) : R_1;
            file_front_pawns = opp_front_pawns & file_bb (f);
            auto opp_r = file_front_pawns != 0 ? rel_rank (Own, scan_frntmost_sq (Opp, file_front_pawns)) : R_1;
            assert((   own_r == R_1
                    && opp_r == R_1)
                || (own_r != opp_r));
            value -= ShelterWeak[std::min (f, F_H - f)][own_r]
                   + StromDanger[   f == _file (fk_sq)
                                 && opp_r == rel_rank (Own, fk_sq) + 1 ? BLOCKED_BY_KING :
                                    own_r == R_1                       ? BLOCKED_NO      :
                                    opp_r == own_r + 1                 ? BLOCKED_BY_PAWN :
                                                                         BLOCKED_BY_NONE]
                                [std::min (f, F_H - f)][opp_r];
        }
        return value;
    }
    // Explicit template instantiations
    template Value Entry::pawn_shelter_storm<WHITE> (const Position&, Square) const;
    template Value Entry::pawn_shelter_storm<BLACK> (const Position&, Square) const;

    // Scores the most advanced passed pawns.
    template<Color Own>
    Score Entry::evaluate_unstoppable_pawns () const
    {
        return passers[Own] != 0 ?
                Unstopped * i32(rel_rank (Own, scan_frntmost_sq (Own, passers[Own]))) :
                SCORE_ZERO;
    }
    // Explicit template instantiations
    template Score Entry::evaluate_unstoppable_pawns<WHITE> () const;
    template Score Entry::evaluate_unstoppable_pawns<BLACK> () const;

    // Looks up a PawnEntry object, and returns a pointer to it.
    // The pointer is also stored in a hash table.
    Entry* probe (const Position &pos)
    {
        auto pawn_key = pos.si->pawn_key;
        auto *e = pos.thread->pawn_table[pawn_key];

        if (e->key == pawn_key)
        {
            return e;
        }

        e->key = pawn_key;
        e->score =
            + evaluate<WHITE> (pos, e)
            - evaluate<BLACK> (pos, e);
        e->asymmetry =
            u08(pop_count (  e->semiopens[WHITE]
                           ^ e->semiopens[BLACK]));
        e->open_count =
            u08(pop_count (  e->semiopens[WHITE]
                           & e->semiopens[BLACK]));
        return e;
    }

    // Initialize lookup tables during startup
    void initialize ()
    {
        static const i32 Seeds[R_NO] = { 0, 8, 19, 13, 71, 94, 169, 324 };

        for (u08 opposed = 0; opposed < 2; ++opposed)
        {
            for (u08 phalanx = 0; phalanx < 2; ++phalanx)
            {
                for (u08 apex = 0; apex < 2; ++apex)
                {
                    for (auto r = R_2; r < R_8; ++r)
                    {
                        auto v = i32((i32(Seeds[r] + (Seeds[r+1] - Seeds[r])*0.5*phalanx) >> opposed) * (1.0 + 0.5*apex));
                        Connected[opposed][phalanx][apex][r] = mk_score (v, v * i32(r-R_3) / 4);
                    }
                }
            }
        }
    }
}
