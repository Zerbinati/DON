#include "Pawns.h"

#include <algorithm>
#include <cassert>

#include "BitBoard.h"
#include "Helper.h"
#include "Thread.h"

namespace Pawns {

    namespace {
        // Connected pawn bonus
        constexpr Array<i32, RANKS> Connected{ 0, 7, 8, 12, 29, 48, 86, 0 };

    #define S(mg, eg) makeScore(mg, eg)
        // Safety of friend pawns shelter for our king by [distance from edge][rank].
        // RANK_1 is used for files where we have no pawn, or pawn is behind our king.
        constexpr Array<Score, FILES/2, RANKS> Shelter
        {{
            { S( -6, 0), S( 81, 0), S( 93, 0), S( 58, 0), S( 39, 0), S( 18, 0), S(  25, 0), S(0, 0) },
            { S(-43, 0), S( 61, 0), S( 35, 0), S(-49, 0), S(-29, 0), S(-11, 0), S( -63, 0), S(0, 0) },
            { S(-10, 0), S( 75, 0), S( 23, 0), S( -2, 0), S( 32, 0), S(  3, 0), S( -45, 0), S(0, 0) },
            { S(-39, 0), S(-13, 0), S(-29, 0), S(-52, 0), S(-48, 0), S(-67, 0), S(-166, 0), S(0, 0) }
        }};

        // Danger of unblocked enemy pawns storm toward our king by [distance from edge][rank].
        // RANK_1 is used for files where the enemy has no pawn, or their pawn is behind our king.
        // [0][1 - 2] accommodate opponent pawn on edge (likely blocked by king)
        constexpr Array<Score, FILES/2, RANKS> Storm
        {{
            { S( 85, 0), S(-289, 0), S(-166, 0), S( 97, 0), S( 50, 0), S( 45, 0), S( 50, 0), S(0, 0) },
            { S( 46, 0), S( -25, 0), S( 122, 0), S( 45, 0), S( 37, 0), S(-10, 0), S( 20, 0), S(0, 0) },
            { S( -6, 0), S(  51, 0), S( 168, 0), S( 34, 0), S( -2, 0), S(-22, 0), S(-14, 0), S(0, 0) },
            { S(-15, 0), S( -11, 0), S( 101, 0), S(  4, 0), S( 11, 0), S(-15, 0), S(-29, 0), S(0, 0) }
        }};

        constexpr Score BlockedStorm  { S(82,82) };

        constexpr Score Basic         { S( 5, 5) };
        constexpr Score Backward      { S( 9,24) };
        constexpr Score Isolated      { S( 5,15) };
        constexpr Score Unopposed     { S(13,27) };
        constexpr Score WeakDoubled   { S(11,56) };
        constexpr Score WeakTwiceLever{ S( 0,56) };

    #undef S

        /// evaluateSafetyOn() calculates shelter & storm for king,
        /// looking at the king file and the two closest files.
        template<Color Own>
        Score evaluateSafetyOn(Position const &pos, Square kSq) {
            constexpr auto Opp{ ~Own };

            Bitboard frontPawns{ ~frontRanksBB(Opp, kSq) & pos.pieces(PAWN) };
            Bitboard ownFrontPawns{ pos.pieces(Own) & frontPawns };
            Bitboard oppFrontPawns{ pos.pieces(Opp) & frontPawns };

            Score safety{ Basic };

            auto kF{ clamp(SFile[kSq], FILE_B, FILE_G) };
            for (File f = File(kF - 1); f <= File(kF + 1); ++f) {
                assert(FILE_A <= f && f <= FILE_H);
                Bitboard ownFrontFilePawns = ownFrontPawns & FileBB[f];
                auto ownR{ 0 != ownFrontFilePawns ?
                            relativeRank(Own, scanFrontMostSq(Opp, ownFrontFilePawns)) : RANK_1 };
                Bitboard oppFrontFilePawns = oppFrontPawns & FileBB[f];
                auto oppR{ 0 != oppFrontFilePawns ?
                            relativeRank(Own, scanFrontMostSq(Opp, oppFrontFilePawns)) : RANK_1 };
                assert((ownR != oppR)
                    || (RANK_1 == ownR
                     && RANK_1 == oppR));

                safety += Shelter[foldFile(f)][ownR];
                safety -=
                       (RANK_1 != ownR)
                    && (ownR + 1) == oppR ?
                          BlockedStorm * (RANK_3 == oppR) :
                          Storm[foldFile(f)][oppR];

            }

            return safety;
        }
        // Explicit template instantiations
        template Score evaluateSafetyOn<WHITE>(Position const&, Square);
        template Score evaluateSafetyOn<BLACK>(Position const&, Square);

    }

    i32 Entry::passedCount() const {
        return popCount(passers[WHITE] | passers[BLACK]);
    }

    /// Entry::evaluateKingSafety()
    template<Color Own>
    Score Entry::evaluateKingSafety(Position const &pos, Bitboard attacks) {
        auto kSq{ pos.square(Own|KING) };

        // Find King path
        Array<Bitboard, CASTLE_SIDES> kPaths
        {
            pos.castleKingPath(Own, CS_KING) * (pos.canCastle(Own, CS_KING) && pos.castleExpeded(Own, CS_KING)),
            pos.castleKingPath(Own, CS_QUEN) * (pos.canCastle(Own, CS_QUEN) && pos.castleExpeded(Own, CS_QUEN))
        };
        if (0 != (kPaths[CS_KING] & attacks)) {
            kPaths[CS_KING] = 0;
        }
        if (0 != (kPaths[CS_QUEN] & attacks)) {
            kPaths[CS_QUEN] = 0;
        }

        Bitboard kPath{ kPaths[CS_KING]
                      | kPaths[CS_QUEN] };

        if (kingSq[Own]   != kSq
         || kingPath[Own] != kPath) {

            auto safety{ evaluateSafetyOn<Own>(pos, kSq) };
            if (0 != kPaths[CS_KING]) {
                safety = std::max(evaluateSafetyOn<Own>(pos, relativeSq(Own, SQ_G1)), safety,
                                  [](Score s1, Score s2) {
                                    return mgValue(s1) < mgValue(s2);
                                  });
            }
            if (0 != kPaths[CS_QUEN]) {
                safety = std::max(evaluateSafetyOn<Own>(pos, relativeSq(Own, SQ_C1)), safety,
                                  [](Score s1, Score s2) {
                                    return mgValue(s1) < mgValue(s2);
                                  });
            }

            kingSafety[Own] = safety;

            if (kingSq[Own] != kSq) {
                // In endgame, king near to closest pawn
                i32 kDist{ 0 };
                Bitboard pawns{ pos.pieces(Own, PAWN) };
                if (0 != pawns) {
                    if (0 != (pawns & PieceAttackBB[KING][kSq])) {
                        kDist = 1;
                    }
                    else {
                        kDist = 8;
                        while (0 != pawns) {
                            kDist = std::min(distance(kSq, popLSq(pawns)), kDist);
                        }
                    }
                }
                kingDist[Own] = makeScore(0, 16 * kDist);
            }

            kingSq[Own] = kSq;
            kingPath[Own] = kPath;
        }

        return (kingSafety[Own] - kingDist[Own]);
    }
    // Explicit template instantiations
    template Score Entry::evaluateKingSafety<WHITE>(Position const&, Bitboard);
    template Score Entry::evaluateKingSafety<BLACK>(Position const&, Bitboard);

    /// Entry::evaluate()
    template<Color Own>
    void Entry::evaluate(Position const &pos) {
        constexpr auto Opp{ ~Own };

        Bitboard pawns{ pos.pieces(PAWN) };
        Bitboard ownPawns{ pos.pieces(Own) & pawns };
        Bitboard oppPawns{ pos.pieces(Opp) & pawns };

        kingSq    [Own] = SQ_NONE;
        //kingPath  [Own] = 0;
        //kingSafety[Own] = SCORE_ZERO;
        //kingDist  [Own] = SCORE_ZERO;
        attackSpan[Own] = pawnSglAttackBB<Own>(ownPawns);
        passers   [Own] = 0;
        score     [Own] = SCORE_ZERO;
        for (Square s : pos.squares(Own|PAWN)) {
            assert((Own|PAWN) == pos[s]);

            auto r{ relativeRank(Own, s) };
            assert(RANK_2 <= r && r <= RANK_7);

            Bitboard neighbours { ownPawns & adjacentFilesBB(s) };
            Bitboard supporters { neighbours & rankBB(s - PawnPush[Own]) };
            Bitboard phalanxes  { neighbours & rankBB(s) };
            Bitboard stoppers   { oppPawns & pawnPassSpan(Own, s) };
            Bitboard blocker    { stoppers & (s + PawnPush[Own]) };
            Bitboard levers     { stoppers & PawnAttackBB[Own][s] };
            Bitboard sentres    { stoppers & PawnAttackBB[Own][s + PawnPush[Own]] }; // push levers

            bool opposed { 0 != (stoppers & frontSquaresBB(Own, s)) };
            // Backward: A pawn is backward when it is behind all pawns of the same color
            // on the adjacent files and cannot be safely advanced.
            bool backward{ 0 == (neighbours & frontRanksBB(Opp, s + PawnPush[Own]))
                        && 0 != (blocker | sentres) };

            // Compute additional span if pawn is not blocked nor backward
            if (!backward
             && !blocker) {
                attackSpan[Own] |= pawnAttackSpan(Own, s /*+ PawnPush[Own]*/);
            }

            // A pawn is passed if one of the three following conditions is true:
            // - Lever there is no stoppers except the levers
            // - Sentry there is no stoppers except the sentres, but we outnumber them
            // - Sneaker there is only one front stopper which can be levered.
            if (// Lever
                (stoppers == levers)
                // Lever + Sentry
             || (stoppers == (levers | sentres)
              && popCount(phalanxes) >= popCount(sentres))
                // Sneaker => Blocked pawn
             || (stoppers == blocker
              && RANK_5 <= r
              && 0 != ( pawnSglPushBB<Own>(supporters)
                     & ~(oppPawns | pawnDblAttackBB<Opp>(oppPawns))))) {
                // Passed pawns will be properly scored later in evaluation when we have full attack info.
                passers[Own] |= s;
            }

            Score sp{ SCORE_ZERO };

            if (0 != supporters
             || 0 != phalanxes) {
                i32 v{ Connected[r] * (2 + (0 != phalanxes) - opposed)
                     + 21 * popCount(supporters) };
                sp += makeScore(v, v * (r - RANK_3) / 4);
            }
            else
            if (0 == neighbours) {
                sp -= Isolated
                    + Unopposed * !opposed;
            }
            else
            if (backward) {
                sp -= Backward
                    + Unopposed * !opposed;
            }

            if (0 == supporters) {
                sp -= WeakDoubled * contains(ownPawns, s - PawnPush[Own])
                    // Attacked twice by enemy pawns
                    + WeakTwiceLever * moreThanOne(levers);
            }

            score[Own] += sp;
        }
    }
    // Explicit template instantiations
    template void Entry::evaluate<WHITE>(Position const&);
    template void Entry::evaluate<BLACK>(Position const&);

    /// Pawns::probe() looks up a current position's pawn configuration in the pawn hash table
    /// and returns a pointer to it if found, otherwise a new Entry is computed and stored there.
    Entry* probe(Position const &pos) {
        Key pawnKey{ pos.pawnKey() };
        auto *e{ pos.thread()->pawnHash[pawnKey] };

        if (e->key == pawnKey) {
            return e;
        }

        e->key = pawnKey;
        e->evaluate<WHITE>(pos),
        e->evaluate<BLACK>(pos);

        return e;
    }

}
