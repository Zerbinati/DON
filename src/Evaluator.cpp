#include "Evaluator.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>

#include "BitBoard.h"
#include "Helper.h"
#include "King.h"
#include "Material.h"
#include "Pawns.h"
#include "Notation.h"
#include "Thread.h"
#include "UCI.h"

namespace Evaluator {

    namespace {

        // PAWN, NIHT, BSHP, ROOK, QUEN, KING,
        enum Term : u08 { MATERIAL = 8, IMBALANCE, MOBILITY, THREAT, PASSER, SPACE, INITIATIVE, TOTAL, TERM_NO = 16 };

        class Tracer {

        private:
            static Table<Score, TERM_NO, COLORS> Scores;

        public:
            static void clear() {
                Scores.fill(SCORE_ZERO);
            }

            static void write(Term t, Color c, Score s) {
                Scores[t][c] = s;
            }
            static void write(Term t, Score sW, Score sB = SCORE_ZERO) {
                write(t, WHITE, sW);
                write(t, BLACK, sB);
            }

            friend std::ostream& operator<<(std::ostream &, Term);


        };

        Table<Score, TERM_NO, COLORS> Tracer::Scores;

        std::ostream& operator<<(std::ostream &os, Term t) {
            if (t == MATERIAL
             || t == IMBALANCE
             || t == INITIATIVE
             || t == TOTAL) {
                os << " | ----- -----" << " | ----- -----";
            }
            else {
                os << " | " << Tracer::Scores[t][WHITE] << " | " << Tracer::Scores[t][BLACK];
            }
            os << " | " << Tracer::Scores[t][WHITE] - Tracer::Scores[t][BLACK] << " |\n";
            return os;
        }

        constexpr Bitboard CenterBB{ (FileBB[FILE_D]|FileBB[FILE_E]) & (RankBB[RANK_4]|RankBB[RANK_5]) };

        constexpr Array<Bitboard, COLORS> LowRankBB
        {
            RankBB[RANK_2]|RankBB[RANK_3],
            RankBB[RANK_7]|RankBB[RANK_6]
        };

        constexpr Array<Bitboard, COLORS> CampBB
        {
            RankBB[RANK_1]|RankBB[RANK_2]|RankBB[RANK_3]|RankBB[RANK_4]|RankBB[RANK_5],
            RankBB[RANK_8]|RankBB[RANK_7]|RankBB[RANK_6]|RankBB[RANK_5]|RankBB[RANK_4]
        };

        constexpr Array<Bitboard, FILES> KingFlankBB
        {
            SlotFileBB[CS_QUEN] ^ FileBB[FILE_D],
            SlotFileBB[CS_QUEN],
            SlotFileBB[CS_QUEN],
            SlotFileBB[CS_CENTRE],
            SlotFileBB[CS_CENTRE],
            SlotFileBB[CS_KING],
            SlotFileBB[CS_KING],
            SlotFileBB[CS_KING] ^ FileBB[FILE_E]
        };

        constexpr Array<Bitboard, COLORS> OutpostBB
        {
            RankBB[RANK_4]|RankBB[RANK_5]|RankBB[RANK_6],
            RankBB[RANK_5]|RankBB[RANK_4]|RankBB[RANK_3]
        };


    #define S(mg, eg) makeScore(mg, eg)

        constexpr Array<Score, PIECE_TYPES, 28> Mobility
        {{
            {}, {},
            { // Knight
                S(-62,-81), S(-53,-56), S(-12,-30), S( -4,-14), S(  3,  8), S( 13, 15),
                S( 22, 23), S( 28, 27), S( 33, 33)
            },
            { // Bishop
                S(-48,-59), S(-20,-23), S( 16, -3), S( 26, 13), S( 38, 24), S( 51, 42),
                S( 55, 54), S( 63, 57), S( 63, 65), S( 68, 73), S( 81, 78), S( 81, 86),
                S( 91, 88), S( 98, 97)
            },
            { // Rook
                S(-58,-76), S(-27,-18), S(-15, 28), S(-10, 55), S( -5, 69), S( -2, 82),
                S(  9,112), S( 16,118), S( 30,132), S( 29,142), S( 32,155), S( 38,165),
                S( 46,166), S( 48,169), S( 58,171)
            },
            { // Queen
                S(-39,-36), S(-21,-15), S(  3,  8), S(  3, 18), S( 14, 34), S( 22, 54),
                S( 28, 61), S( 41, 73), S( 43, 79), S( 48, 92), S( 56, 94), S( 60,104),
                S( 60,113), S( 66,120), S( 67,123), S( 70,126), S( 71,133), S( 73,136),
                S( 79,140), S( 88,143), S( 88,148), S( 99,166), S(102,170), S(102,175),
                S(106,184), S(109,191), S(113,206), S(116,212)
            }
        }};

        constexpr Array<Score, 2> RookOnFile
        {
            S(21, 4), S(47,25)
        };

        constexpr Array<Score, PIECE_TYPES> MinorThreat
        {
            S( 0, 0), S( 5,32), S(57,41), S(77,56), S(88,119), S(79,161), S( 0, 0)
        };
        constexpr Array<Score, PIECE_TYPES> MajorThreat
        {
            S( 0, 0), S( 2,44), S(36,71), S(36,61), S( 0, 38), S(51, 38), S( 0, 0)
        };

        constexpr Array<Score, RANKS> PasserRank
        {
            S( 0, 0), S(10,28), S(17,33), S(15,41), S(62,72), S(168,177), S(276,260), S( 0, 0)
        };

        constexpr Score MinorBehindPawn { S( 18,  3) };
        constexpr Score MinorKingProtect{ S(  7,  8) };
        constexpr Score KnightOutpost   { S( 30, 21) };
        constexpr Score BishopOutpost   { S( 30, 21) };
        constexpr Score BishopOnDiagonal{ S( 45,  0) };
        constexpr Score BishopPawns     { S(  3,  7) };
        constexpr Score BishopTrapped   { S( 50, 50) };
        constexpr Score RookOnQueenFile { S(  7,  6) };
        constexpr Score RookTrapped     { S( 52, 10) };
        constexpr Score QueenAttacked   { S( 49, 15) };
        constexpr Score PawnLessFlank   { S( 17, 95) };
        constexpr Score PasserFile      { S( 11,  8) };
        constexpr Score KingFlankAttacks{ S(  8,  0) };
        constexpr Score PieceRestricted { S(  7,  7) };
        constexpr Score PieceHanged     { S( 69, 36) };
        constexpr Score QueenProtected  { S( 14,  0) };
        constexpr Score PawnThreat      { S(173, 94) };
        constexpr Score PawnPushThreat  { S( 48, 39) };
        constexpr Score KingThreat      { S( 24, 89) };
        constexpr Score KnightOnQueen   { S( 16, 12) };
        constexpr Score SliderOnQueen   { S( 59, 18) };

    #undef S

        constexpr Array<i32, PIECE_TYPES> SafeCheckWeight   { 0, 0, 790, 635, 1080, 780, 0 };
        constexpr Array<i32, PIECE_TYPES> KingAttackerWeight{ 0, 0,  81,  52,   44,  10, 0 };

        // Evaluator class contains various evaluation functions.
        template<bool Trace>
        class Evaluation {
        private:
            Position const &pos;

            King    ::Entry *kingEntry{ nullptr };
            Material::Entry *matlEntry{ nullptr };
            Pawns   ::Entry *pawnEntry{ nullptr };

            // Contains all squares attacked by the color and piece type.
            Array<Bitboard, COLORS> fulAttacks;
            // Contains all squares attacked by the color and piece type with pinned removed.
            Array<Bitboard, COLORS, PIECE_TYPES> sqlAttacks;
            // Contains all squares attacked by more than one pieces of a color, possibly via x-ray or by one pawn and one piece.
            Array<Bitboard, COLORS> dblAttacks;
            // Contains all squares from which queen can be attacked
            Array<Bitboard, COLORS, 3> queenAttacked;

            Array<Bitboard, COLORS> mobArea;
            Array<Score, COLORS> mobility;

            // The squares adjacent to the king plus some other very near squares, depending on king position.
            Array<Bitboard, COLORS> kingRing;
            // Number of pieces of the color, which attack a square in the kingRing of the enemy king.
            Array<i32, COLORS> kingAttackersCount;
            // Sum of the "weight" of the pieces of the color which attack a square in the kingRing of the enemy king.
            // The weights of the individual piece types are given by the KingAttackerWeight[piece-type]
            Array<i32, COLORS> kingAttackersWeight;
            // Number of attacks by the color to squares directly adjacent to the enemy king.
            // Pieces which attack more than one square are counted multiple times.
            // For instance, if there is a white knight on g5 and black's king is on g8, this white knight adds 2 to kingAttacksCount[WHITE]
            Array<i32, COLORS> kingAttacksCount;

            Array<bool, COLORS> canCastle;

            template<Color> void initialize();
            template<Color, PieceType> Score pieces();
            template<Color> Score king() const;
            template<Color> Score threats() const;
            template<Color> Score passPawn() const;
            template<Color> Score space() const;

            Score initiative(Score) const;
            Scale scaleFactor(Value) const;

        public:

            Evaluation() = delete;
            Evaluation(Evaluation const&) = delete;
            Evaluation& operator=(Evaluation const&) = delete;

            Evaluation(Position const&);

            Value value();
        };

        template<bool Trace>
        Evaluation<Trace>::Evaluation(Position const &p) :
            pos{ p }
        {}

        /// initialize() computes pawn and king attacks also mobility and the king ring
        template<bool Trace> template<Color Own>
        void Evaluation<Trace>::initialize() {
            constexpr auto Opp{ ~Own };

            const auto kSq{ pos.square(Own|KING) };

            sqlAttacks[Own].fill(0);
            sqlAttacks[Own][PAWN] = pawnEntry->sglAttacks[Own];
            sqlAttacks[Own][KING] = PieceAttackBB[KING][kSq];

            fulAttacks[Own] =
            sqlAttacks[Own][NONE] = sqlAttacks[Own][PAWN]
                                  | sqlAttacks[Own][KING];

            dblAttacks[Own] = pawnEntry->dblAttacks[Own]
                            | (sqlAttacks[Own][PAWN]
                             & sqlAttacks[Own][KING]);

            queenAttacked[Own].fill(0);

            // Mobility area: Exclude followings
            mobArea[Own] = ~(// Squares protected by enemy pawns
                             pawnEntry->sglAttacks[Opp]
                             // Squares occupied by friend Queen and King
                           | pos.pieces(Own, QUEN, KING)
                             // Squares occupied by friend King blockers
                           | pos.kingBlockers(Own)
                             // Squares occupied by block pawns (pawns on ranks 2-3/blocked)
                           | (pos.pieces(Own, PAWN)
                            & (LowRankBB[Own]
                             | pawnSglPushBB<Opp>(pos.pieces()))));

            mobility[Own] = SCORE_ZERO;

            // King safety tables
            auto sq{ makeSquare(clamp(sFile(kSq), FILE_B, FILE_G),
                                clamp(sRank(kSq), RANK_2, RANK_7)) };
            kingRing[Own] = PieceAttackBB[KING][sq] | sq;

            kingAttackersCount [Opp] = popCount(kingRing[Own] & pawnEntry->sglAttacks[Opp]);
            kingAttackersWeight[Opp] = 0;
            kingAttacksCount   [Opp] = 0;

            // Remove from kingRing the squares defended by two pawns
            kingRing[Own] &= ~pawnEntry->dblAttacks[Own];
            canCastle[Own] = pos.canCastle(Own)
                          && ((pos.canCastle(Own, CS_KING)
                            && pos.castleExpeded(Own, CS_KING))
                           || (pos.canCastle(Own, CS_QUEN)
                            && pos.castleExpeded(Own, CS_QUEN)));
        }

        /// pieces() evaluates the pieces of the color and type
        template<bool Trace> template<Color Own, PieceType PT>
        Score Evaluation<Trace>::pieces() {
            static_assert (NIHT <= PT && PT <= QUEN, "PT incorrect");
            constexpr auto Opp{ ~Own };

            auto kSq{ pos.square(Own|KING) };
            Bitboard kingBlockers{ pos.kingBlockers(Own) };

            Score score{ SCORE_ZERO };

            for (Square s : pos.squares(Own|PT)) {
                assert(pos[s] == (Own|PT));

                Bitboard action{
                    contains(kingBlockers, s) ?
                        LineBB[kSq][s] : BoardBB };

                // Find attacked squares, including x-ray attacks for Bishops, Rooks and Queens
                Bitboard attacks{
                    PT == NIHT ? PieceAttackBB[NIHT][s] & action :
                    PT == BSHP ? attacksBB<BSHP>(s, pos.pieces() ^ ((pos.pieces(Own, QUEN, BSHP) & ~kingBlockers) | pos.pieces(Opp, QUEN))) & action :
                    PT == ROOK ? attacksBB<ROOK>(s, pos.pieces() ^ ((pos.pieces(Own, QUEN, ROOK) & ~kingBlockers) | pos.pieces(Opp, QUEN))) & action :
                                 attacksBB<QUEN>(s, pos.pieces() ^ ((pos.pieces(Own, QUEN)       & ~kingBlockers))) & action };

                auto mob = popCount(attacks & mobArea[Own]);
                assert(0 <= mob && mob <= 27);

                // Bonus for piece mobility
                mobility[Own] += Mobility[PT][mob];

                Bitboard b;
                // Special evaluation for pieces
                if (PT == NIHT
                 || PT == BSHP) {

                    dblAttacks[Own] |= sqlAttacks[Own][NONE] & attacks;

                    // Bonus for minor shielded by pawn
                    score += MinorBehindPawn * (relativeRank(Own, s) <= RANK_6
                                             && contains(pos.pieces(PAWN), s + PawnPush[Own]));

                    // Penalty for distance from the friend king
                    score -= MinorKingProtect * distance(kSq, s);

                    b =  OutpostBB[Own]
                      &  sqlAttacks[Own][PAWN]
                      & ~pawnEntry->attacksSpan[Opp];

                    if (PT == NIHT) {

                        // Bonus for knight outpost squares
                        if (contains(b, s)) {
                            score += KnightOutpost * 2;
                        }
                        else
                        if ((b
                           & attacks
                           & ~pos.pieces(Own)) != 0) {
                            score += KnightOutpost * 1;
                        }
                    }
                    if (PT == BSHP) {

                        // Bonus for bishop outpost squares
                        if (contains(b, s)) {
                            score += BishopOutpost * 1;
                        }

                        // Penalty for pawns on the same color square as the bishop,
                        // less when the bishop is protected by pawn
                        // more when the center files are blocked with pawns.
                        score -= BishopPawns
                               * popCount(pos.pawnsOnSqColor(Own, sColor(s)))
                               * (!contains(sqlAttacks[Own][PAWN], s)
                                + popCount(pos.pieces(Own, PAWN)
                                         & SlotFileBB[CS_CENTRE]
                                         & pawnSglPushBB<Opp>(pos.pieces())));

                        // Bonus for bishop on a long diagonal which can "see" both center squares
                        score += BishopOnDiagonal
                               * moreThanOne(attacksBB<BSHP>(s, pos.pieces(PAWN)) & CenterBB);

                        // An important Chess960 pattern: A cornered bishop blocked by a friend pawn diagonally in front of it.
                        // It is a very serious problem, especially when that pawn is also blocked.
                        // Bishop (white or black) on a1/h1 or a8/h8 which is trapped by own pawn on b2/g2 or b7/g7.
                        if (mob <= 1
                         && Options["UCI_Chess960"]
                         && (relativeSq(Own, s) == SQ_A1
                          || relativeSq(Own, s) == SQ_H1)) {
                            auto Push{ PawnPush[Own] };
                            auto del{ Push + sign(FILE_E - sFile(s)) * EAST };
                            if (contains(pos.pieces(Own, PAWN), s + del)) {
                                score -= BishopTrapped
                                        * (!contains(pos.pieces(), s + del + Push) ?
                                                !contains(pos.pieces(Own, PAWN), s + del + del) ?
                                                    1 : 2 : 4);
                            }
                        }
                    }
                }
                if (PT == ROOK) {

                    dblAttacks[Own] |= sqlAttacks[Own][NONE] & attacks;

                    // Bonus for rook on the same file as a queen
                    if ((pos.pieces(QUEN) & fileBB(s)) != 0) {
                        score += RookOnQueenFile;
                    }

                    // Bonus for rook when on an open or semi-open file
                    if (pos.semiopenFileOn(Own, s)) {
                        score += RookOnFile[pos.semiopenFileOn(Opp, s)];
                    }
                    else
                    // Penalty for rook when trapped by the king, even more if the king can't castle
                    if (mob <= 3
                     && relativeRank(Own, s) < RANK_4
                     && (pos.pieces(Own, PAWN) & frontSquaresBB(Own, s)) != 0) {
                        auto kF{ sFile(kSq) };
                        if (((kF < FILE_E) && (sFile(s) < kF))
                         || ((kF > FILE_D) && (sFile(s) > kF))) {
                            score -= RookTrapped * (1 + !pos.canCastle(Own));
                        }
                    }
                }
                if (PT == QUEN) {

                    Bitboard pc{  pos.pieces(Own)
                               & ~kingBlockers };
                    dblAttacks[Own] |= sqlAttacks[Own][NONE]
                                     & (attacks
                                      | (attacksBB<BSHP>(s, pos.pieces() ^ (pc & pos.pieces(BSHP) & PieceAttackBB[BSHP][s])) & action)
                                      | (attacksBB<ROOK>(s, pos.pieces() ^ (pc & pos.pieces(ROOK) & PieceAttackBB[ROOK][s])) & action));

                    queenAttacked[Own][0] |= pos.attacksFrom(NIHT, s);
                    queenAttacked[Own][1] |= pos.attacksFrom(BSHP, s);
                    queenAttacked[Own][2] |= pos.attacksFrom(ROOK, s);

                    // Penalty for pin or discover attack on the queen
                    b = 0; // Queen attackers
                    Bitboard queenBlockers{  pos.sliderBlockersAt(s, pos.pieces(Opp, BSHP, ROOK), b, b)
                                          & ~(  pos.kingBlockers(Opp)
                                            | ( pos.pieces(Opp, PAWN)
                                             &  fileBB(s)
                                             & ~pawnSglAttackBB<Own>(pos.pieces(Own)))) };
                    if (queenBlockers != 0) {
                        score -= QueenAttacked;
                    }
                }

                sqlAttacks[Own][PT]   |= attacks;
                sqlAttacks[Own][NONE] |= attacks;
                if (canCastle[Opp]) {
                fulAttacks[Own]       |= pos.attacksFrom(PT, s);
                }

                if ((attacks & kingRing[Opp]) != 0) {
                    kingAttackersCount [Own]++;
                    kingAttackersWeight[Own] += KingAttackerWeight[PT];
                    kingAttacksCount   [Own] += popCount(attacks
                                                       & sqlAttacks[Opp][KING]);
                }
            }

            if (Trace) {
                Tracer::write(Term(PT), Own, score);
            }

            return score;
        }

        /// king() evaluates the king of the color
        template<bool Trace> template<Color Own>
        Score Evaluation<Trace>::king() const {
            constexpr auto Opp{ ~Own };

            auto kSq{ pos.square(Own|KING) };

            // Main king safety evaluation
            i32 kingDanger{ 0 };

            // Attacked squares defended at most once by friend queen or king
            Bitboard weakArea{ sqlAttacks[Opp][NONE]
                             & ~dblAttacks[Own]
                             & (~sqlAttacks[Own][NONE]
                              |  sqlAttacks[Own][QUEN]
                              |  sqlAttacks[Own][KING]) };

            // Safe squares where enemy's safe checks are possible on next move
            Bitboard safeArea{ ~pos.pieces(Opp)
                             & (~sqlAttacks[Own][NONE]
                              | (weakArea
                               & dblAttacks[Opp])) };

            Bitboard unsafeCheck{ 0 };

            Bitboard rookPins{ attacksBB<ROOK>(kSq, pos.pieces() ^ pos.pieces(Own, QUEN)) };
            Bitboard bshpPins{ attacksBB<BSHP>(kSq, pos.pieces() ^ pos.pieces(Own, QUEN)) };

            // Enemy rooks checks
            Bitboard rookSafeChecks{  rookPins
                                   &  sqlAttacks[Opp][ROOK]
                                   &  safeArea};
            if (rookSafeChecks != 0) {
                kingDanger += SafeCheckWeight[ROOK];
            }
            else {
                unsafeCheck |= rookPins
                             & sqlAttacks[Opp][ROOK];
            }

            // Enemy queens checks
            Bitboard quenSafeChecks{ (rookPins | bshpPins)
                                   &  sqlAttacks[Opp][QUEN]
                                   &  safeArea
                                   & ~sqlAttacks[Own][QUEN]
                                   & ~rookSafeChecks };
            if (quenSafeChecks != 0) {
                kingDanger += SafeCheckWeight[QUEN];
            }

            // Enemy bishops checks
            Bitboard bshpSafeChecks{  bshpPins
                                   &  sqlAttacks[Opp][BSHP]
                                   &  safeArea
                                   & ~quenSafeChecks };
            if (bshpSafeChecks != 0) {
                kingDanger += SafeCheckWeight[BSHP];
            }
            else {
                unsafeCheck |= bshpPins
                             & sqlAttacks[Opp][BSHP];
            }

            // Enemy knights checks
            Bitboard nihtSafeChecks{  PieceAttackBB[NIHT][kSq]
                                   &  sqlAttacks[Opp][NIHT]
                                   &  safeArea };
            if (nihtSafeChecks != 0) {
                kingDanger += SafeCheckWeight[NIHT];
            }
            else {
                unsafeCheck |= PieceAttackBB[NIHT][kSq]
                             & sqlAttacks[Opp][NIHT];
            }

            Bitboard b;

            b =  KingFlankBB[sFile(kSq)]
              &  CampBB[Own]
              &  sqlAttacks[Opp][NONE];
            // Friend king flank attack count
            i32 kingFlankAttack = popCount(b)                     // Squares attacked by enemy in friend king flank
                          + popCount(b & dblAttacks[Opp]);  // Squares attacked by enemy twice in friend king flank.
            // Friend king flank defense count
            b =  KingFlankBB[sFile(kSq)]
              &  CampBB[Own]
              &  sqlAttacks[Own][NONE];
            i32 kingFlankDefense = popCount(b);

            // King Safety:
            Score score{ kingEntry->evaluateSafety<Own>(pos, fulAttacks[Opp]) };

            kingDanger +=   1 * (kingAttackersCount[Opp] * kingAttackersWeight[Opp])
                        +  69 * kingAttacksCount[Opp]
                        + 185 * popCount(kingRing[Own] & weakArea)
                        + 148 * popCount(unsafeCheck)
                        +  98 * popCount(pos.kingBlockers(Own))
                        +   3 * nSqr(kingFlankAttack) / 8
                        // Enemy queen is gone
                        - 873 * (pos.pieces(Opp, QUEN) == 0)
                        // Friend knight is near by to defend king
                        - 100 * (( sqlAttacks[Own][NIHT]
                                & (sqlAttacks[Own][KING] | kSq)) != 0)
                        // Mobility
                        -   1 * (mgValue(mobility[Own] - mobility[Opp]))
                        -   4 * kingFlankDefense
                        // Pawn Safety quality
                        -   3 * mgValue(score) / 4
                        +  37;

            // Transform the king danger into a score
            if (kingDanger > 100) {
                score -= makeScore(nSqr(kingDanger) / 0x1000, kingDanger / 0x10);
            }

            // Penalty for king on a pawn less flank
            score -= PawnLessFlank * ((pos.pieces(PAWN) & KingFlankBB[sFile(kSq)]) == 0);

            // King tropism: Penalty for slow motion attacks moving towards friend king zone
            score -= KingFlankAttacks * kingFlankAttack;

            if (Trace) {
                Tracer::write(Term(KING), Own, score);
            }

            return score;
        }

        /// threats() evaluates the threats of the color
        template<bool Trace> template<Color Own>
        Score Evaluation<Trace>::threats() const {
            constexpr auto Opp{ ~Own };

            Score score{ SCORE_ZERO };

            // Squares defended by the opponent,
            // - defended the square with a pawn
            // - defended the square twice and not attacked twice.
            Bitboard defendedArea =
                sqlAttacks[Opp][PAWN]
              | ( dblAttacks[Opp]
               & ~dblAttacks[Own]);
            // Enemy non-pawns
            Bitboard nonPawnsEnemies =
                pos.pieces(Opp)
             & ~pos.pieces(PAWN);
            // Enemy not defended and under attacked by any friend piece
            Bitboard attackedUndefendedEnemies =
                pos.pieces(Opp)
             & ~defendedArea
             &  sqlAttacks[Own][NONE];
            // Non-pawn enemies, defended by enemies
            Bitboard defendedNonPawnsEnemies =
                nonPawnsEnemies
              & defendedArea;

            Bitboard b;

            if ((attackedUndefendedEnemies
               | defendedNonPawnsEnemies) != 0) {
                // Bonus according to the type of attacking pieces

                // Enemies attacked by minors
                b =  (attackedUndefendedEnemies
                    | defendedNonPawnsEnemies)
                  &  (sqlAttacks[Own][NIHT]
                    | sqlAttacks[Own][BSHP]);
                while (b != 0) {
                    score += MinorThreat[pType(pos[popLSq(b)])];
                }

                if (attackedUndefendedEnemies != 0) {
                    // Enemies attacked by majors
                    b =  attackedUndefendedEnemies
                      &  sqlAttacks[Own][ROOK];
                    while (b != 0) {
                        score += MajorThreat[pType(pos[popLSq(b)])];
                    }

                    // Enemies attacked by king
                    b =  attackedUndefendedEnemies
                      &  sqlAttacks[Own][KING];
                    if (b != 0) {
                        score += KingThreat;
                    }

                    // Enemies attacked are hanging
                    b =  attackedUndefendedEnemies
                      &  (~sqlAttacks[Opp][NONE]
                        | (nonPawnsEnemies
                         & dblAttacks[Own]));
                    score += PieceHanged * popCount(b);

                    // Additional bonus if weak piece is only protected by a queen
                    b =  attackedUndefendedEnemies
                      &  sqlAttacks[Opp][QUEN];
                    score += QueenProtected * popCount(b);
                }
            }

            // Bonus for restricting their piece moves
            b = ~defendedArea
              &  sqlAttacks[Opp][NONE]
              &  sqlAttacks[Own][NONE];
            score += PieceRestricted * popCount(b);

            // Defended or Unattacked squares
            Bitboard safeArea =
                sqlAttacks[Own][NONE]
             | ~sqlAttacks[Opp][NONE];
            // Safe friend pawns
            b =  safeArea
              &  pos.pieces(Own, PAWN);
            // Safe friend pawns attacks on non-pawn enemies
            b =  pawnSglAttackBB<Own>(b)
              &  nonPawnsEnemies;
            score += PawnThreat * popCount(b);

            // Friend pawns who can push on the next move
            b =  pos.pieces(Own, PAWN)
              & ~pos.kingBlockers(Own);
            // Friend pawns push (squares where friend pawns can push on the next move)
            b =  pawnSglPushBB<Own>(b)
              & ~pos.pieces();
            b |= pawnSglPushBB<Own>(b & RankBB[relativeRank(Own, RANK_3)])
              & ~pos.pieces();
            // Friend pawns push safe (only the squares which are relatively safe)
            b &= safeArea
              & ~sqlAttacks[Opp][PAWN];
            // Friend pawns push safe attacks an enemies
            b =  nonPawnsEnemies
              &  pawnSglAttackBB<Own>(b);
            score += PawnPushThreat * popCount(b);

            // Bonus for threats on the next moves against enemy queens
            if (pos.pieces(Opp, QUEN) != 0) {
                safeArea =  mobArea[Own]
                         & ~defendedArea;
                b = safeArea
                  & (sqlAttacks[Own][NIHT] & queenAttacked[Opp][0]);
                score += KnightOnQueen * popCount(b);

                b = safeArea
                  & ((sqlAttacks[Own][BSHP] & queenAttacked[Opp][1])
                   | (sqlAttacks[Own][ROOK] & queenAttacked[Opp][2]))
                  & dblAttacks[Own];
                score += SliderOnQueen * popCount(b);
            }

            if (Trace) {
                Tracer::write(THREAT, Own, score);
            }

            return score;
        }

        /// passPawn() evaluates the passed pawns of the color
        template<bool Trace> template<Color Own>
        Score Evaluation<Trace>::passPawn() const {
            constexpr auto Opp{ ~Own };
            constexpr auto Push{ PawnPush[Own] };

            auto kingProximity = [&](Color c, Square s) {
                return std::min(distance(pos.square(c|KING), s), 5);
            };

            Score score{ SCORE_ZERO };

            Bitboard passPawns{ pawnEntry->passPawns[Own] };
            while (passPawns != 0) {
                auto s{ popLSq(passPawns) };
                assert((frontSquaresBB(Own, s) & pos.pieces(Own, PAWN)) == 0
                    && (pos.pieces(Opp, PAWN)
                      & (pawnSglPushBB<Own>(frontSquaresBB(Own, s))
                       | ( pawnPassSpan(Own, s + Push)
                        & ~PawnAttackBB[Own][s + Push]))) == 0);

                i32 r{ relativeRank(Own, s) };
                // Base bonus depending on rank.
                Score bonus{ PasserRank[r] };

                auto pushSq{ s + Push };

                if (r > RANK_3) {
                    i32 w{ 5 * r - 13 };

                    // Adjust bonus based on the king's proximity
                    bonus += makeScore(0, i32(+4.75*w*kingProximity(Opp, pushSq)
                                              -2.00*w*kingProximity(Own, pushSq)));
                    // If pushSq is not the queening square then consider also a second push.
                    if (r < RANK_7) {
                        bonus += makeScore(0, -1*w*kingProximity(Own, pushSq + Push));
                    }

                    // If the pawn is free to advance.
                    if (pos.empty(pushSq)) {
                        Bitboard behindMajors{ frontSquaresBB(Opp, s)
                                             & pos.pieces(ROOK, QUEN) };

                        Bitboard attackedSquares{ pawnPassSpan(Own, s) };
                        if ((behindMajors & pos.pieces(Opp)) == 0) {
                            attackedSquares &= sqlAttacks[Opp][NONE];
                        }

                        i32 k{ 0 };
                        // Bonus according to attacked squares
                        k += ((attackedSquares) == 0                           ? 35 :
                              (attackedSquares & frontSquaresBB(Own, s)) == 0  ? 20 :
                              !contains(attackedSquares, pushSq)               ?  9 : 0);
                        // Bonus according to defended squares
                        k += 5 * ((behindMajors & pos.pieces(Own)) != 0
                               || contains(sqlAttacks[Own][NONE], pushSq));

                        bonus += makeScore(k*w, k*w);
                    }
                }

                // Scale down bonus for candidate passPawns
                // - need more than one pawn push to become passPawns
                // - have a pawn in front of it
                if (!pos.pawnPassedAt(Own, pushSq)
                 || contains(pos.pieces(PAWN), pushSq)) {
                    bonus = bonus / 2;
                }
                // Rank bonus + File bonus
                score += bonus
                       - PasserFile * foldFile(sFile(s));
            }

            if (Trace) {
                Tracer::write(PASSER, Own, score);
            }

            return score;
        }

        /// space() evaluates the space of the color
        /// The space evaluation is a simple bonus based on the number of safe squares
        /// available for minor pieces on the central four files on ranks 2-4
        /// Safe squares one, two or three squares behind a friend pawn are counted twice
        /// The aim is to improve play on opening
        template<bool Trace> template<Color Own>
        Score Evaluation<Trace>::space() const {
            constexpr auto Opp{ ~Own };

            // Safe squares for friend pieces inside the area defined by SpaceMask.
            Bitboard safeSpace{  SlotFileBB[CS_CENTRE]
                              &  PawnSideBB[Own]
                              & ~pos.pieces(Own, PAWN)
                              & ~sqlAttacks[Opp][PAWN] };
            // Find all squares which are at most three squares behind some friend pawn
            Bitboard behind{ pos.pieces(Own, PAWN) };
            behind |= pawnSglPushBB<Opp>(behind);
            behind |= pawnDblPushBB<Opp>(behind);

            i32 bonus{ popCount(safeSpace)
                     + popCount( behind
                              &  safeSpace
                              & ~sqlAttacks[Opp][NONE]) };
            i32 weight{ pos.count(Own) - 1 };
            Score score{ makeScore(bonus * weight * weight / 16, 0) };

            if (Trace) {
                Tracer::write(SPACE, Own, score);
            }

            return score;
        }

        /// initiative() evaluates the initiative correction value for the position
        /// i.e. second order bonus/malus based on the known attacking/defending status of the players
        template<bool Trace>
        Score Evaluation<Trace>::initiative(Score s) const {
            auto wkSq{ pos.square(W_KING) };
            auto bkSq{ pos.square(B_KING) };

            i32 outflanking{ distance<File>(wkSq, bkSq)
                           - distance<Rank>(wkSq, bkSq) };

            // Compute the initiative bonus for the attacking side
            i32 complexity =  1 * pawnEntry->complexity
                           +  9 * outflanking
                            // King infiltration
                           + 24 * (sRank(wkSq) > RANK_4
                                || sRank(bkSq) < RANK_5)
                           + 51 * (pos.nonPawnMaterial() == VALUE_ZERO)
                            // Pawn not on both flanks
                           - 21 * (pawnEntry->pawnNotBothFlank)
                            // Almost Unwinnable
                           - 43 * (pawnEntry->pawnNotBothFlank
                                && outflanking < 0)
                           - 89;

            auto mg{ mgValue(s) };
            auto eg{ egValue(s) };
            // Now apply the bonus: note that we find the attacking side by extracting the
            // sign of the midgame or endgame values, and that we carefully cap the bonus
            // so that the midgame and endgame scores do not change sign after the bonus.
            Score score{ makeScore(sign(mg) * clamp(complexity + 50, -abs(mg), 0),
                                   sign(eg) * std::max(complexity, -abs(eg))) };

            if (Trace) {
                Tracer::write(INITIATIVE, score);
            }

            return score;
        }

        /// scaleFactor() evaluates the scaleFactor for the position
        template<bool Trace>
        Scale Evaluation<Trace>::scaleFactor(Value eg) const {
            auto stngColor{ eg >= VALUE_ZERO ? WHITE : BLACK };

            Scale scale{ matlEntry->scalingFunc[stngColor] != nullptr ?
                        (*matlEntry->scalingFunc[stngColor])(pos) : SCALE_NONE };
            if (scale == SCALE_NONE) {
                scale = matlEntry->scaleFactor[stngColor];
            }

            // If scaleFactor is not already specific, scaleFactor down the endgame via general heuristics
            if (scale == SCALE_NORMAL) {

                scale = pos.bishopOpposed()
                     && pos.nonPawnMaterial() == 2 * VALUE_MG_BSHP ?
                        Scale(22) :
                        std::min(Scale(36 + (7 - 5 * pos.bishopOpposed()) * pos.count(stngColor|PAWN)), SCALE_NORMAL);

                // Scale down endgame factor when shuffling
                scale = std::max(Scale(scale + 3 - pos.clockPly() / 4), SCALE_DRAW);
            }
            return scale;
        }

        /// value() computes the various parts of the evaluation and
        /// returns the value of the position from the point of view of the side to move.
        template<bool Trace>
        Value Evaluation<Trace>::value() {
            assert(pos.checkers() == 0);

            // Probe the material hash table
            matlEntry = Material::probe(pos);
            // If have a specialized evaluation function for the material configuration
            if (matlEntry->evaluationFunc != nullptr) {
                return (*matlEntry->evaluationFunc)(pos);
            }

            // Probe the pawn hash table
            pawnEntry = Pawns::probe(pos);

            // Score is computed internally from the white point of view.
            // Initialize by
            // - incrementally updated scores (material + piece square tables)
            // - material imbalance
            // - pawn score
            // - dynamic contempt
            Score score{ pos.psqScore()
                       + matlEntry->imbalance
                       + (pawnEntry->score[WHITE] - pawnEntry->score[BLACK])
                       + pos.thread()->contempt };

            // Early exit if score is high
            Value v{ (mgValue(score) + egValue(score)) / 2 };
            if (abs(v) > (VALUE_LAZY_THRESHOLD
                        + pos.nonPawnMaterial() / 64)) {
                return pos.activeSide() == WHITE ? +v : -v;
            }

            // Probe the king hash table
            kingEntry = King::probe(pos);

            if (Trace) {
                Tracer::clear();
            }

            initialize<WHITE>();
            initialize<BLACK>();

            // Pieces should be evaluated first (populate attack information)
            score += pieces<WHITE, NIHT>() - pieces<BLACK, NIHT>();
            score += pieces<WHITE, BSHP>() - pieces<BLACK, BSHP>();
            score += pieces<WHITE, ROOK>() - pieces<BLACK, ROOK>();
            score += pieces<WHITE, QUEN>() - pieces<BLACK, QUEN>();

            assert((sqlAttacks[WHITE][NONE] & dblAttacks[WHITE]) == dblAttacks[WHITE]);
            assert((sqlAttacks[BLACK][NONE] & dblAttacks[BLACK]) == dblAttacks[BLACK]);

            score += mobility[WHITE]   - mobility[BLACK]
                   + king    <WHITE>() - king    <BLACK>()
                   + threats <WHITE>() - threats <BLACK>()
                   + passPawn<WHITE>() - passPawn<BLACK>();
            if (pos.nonPawnMaterial() >= VALUE_SPACE_THRESHOLD) {
            score += space   <WHITE>() - space   <BLACK>();
            }

            score += initiative(score);

            assert(-VALUE_INFINITE < mgValue(score) && mgValue(score) < +VALUE_INFINITE);
            assert(-VALUE_INFINITE < egValue(score) && egValue(score) < +VALUE_INFINITE);
            assert(0 <= matlEntry->phase && matlEntry->phase <= Material::PhaseResolution);

            // Interpolates between midgame and scaled endgame values (scaled by 'scaleFactor(egValue(score))').
            v = mgValue(score) * (matlEntry->phase)
              + egValue(score) * (Material::PhaseResolution - matlEntry->phase) * scaleFactor(egValue(score)) / SCALE_NORMAL;
            v /= Material::PhaseResolution;

            if (Trace) {
                // Write remaining evaluation terms
                Tracer::write(Term(PAWN), pawnEntry->score[WHITE], pawnEntry->score[BLACK]);
                Tracer::write(MATERIAL  , pos.psqScore());
                Tracer::write(IMBALANCE , matlEntry->imbalance);
                Tracer::write(MOBILITY  , mobility[WHITE], mobility[BLACK]);
                Tracer::write(TOTAL     , score);
            }

            // Active side's point of view
            return (pos.activeSide() == WHITE ? +v : -v) + VALUE_TEMPO;
        }
    }

    /// evaluate() returns a static evaluation of the position from the point of view of the side to move.
    Value evaluate(Position const &pos) {
        return Evaluation<false>(pos).value();
    }

    /// trace() returns a string (suitable for outputting to stdout for debugging)
    /// that contains the detailed descriptions and values of each evaluation term.
    std::string trace(Position const &pos)
    {
        if (pos.checkers() != 0) {
            return "Evaluation: none (in check)\n";
        }
        // Reset any dynamic contempt
        auto contempt = pos.thread()->contempt;
        pos.thread()->contempt = SCORE_ZERO;
        auto value{ Evaluation<true>(pos).value() };
        pos.thread()->contempt = contempt;

        // Trace scores are from White's point of view
        value = pos.activeSide() == WHITE ? +value : -value;

        std::ostringstream oss;

        oss << "      Eval Term |    White    |    Black    |    Total     \n"
            << "                |   MG    EG  |   MG    EG  |   MG    EG   \n"
            << "----------------+-------------+-------------+--------------\n"
            << "       Material" << Term(MATERIAL)
            << "      Imbalance" << Term(IMBALANCE)
            << "           Pawn" << Term(PAWN)
            << "         Knight" << Term(NIHT)
            << "         Bishop" << Term(BSHP)
            << "           Rook" << Term(ROOK)
            << "          Queen" << Term(QUEN)
            << "       Mobility" << Term(MOBILITY)
            << "           King" << Term(KING)
            << "         Threat" << Term(THREAT)
            << "         Passer" << Term(PASSER)
            << "          Space" << Term(SPACE)
            << "     Initiative" << Term(INITIATIVE)
            << "----------------+-------------+-------------+--------------\n"
            << "          Total" << Term(TOTAL)
            << std::showpos << std::showpoint << std::fixed << std::setprecision(2)
            << "\nEvaluation: " << toCP(value) / 100 << " (white side)\n";

        return oss.str();
    }
}
