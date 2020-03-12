#include "Evaluator.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>

#include "BitBoard.h"
#include "Helper.h"
#include "Material.h"
#include "Notation.h"
#include "Pawns.h"
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

            friend std::ostream& operator<<(std::ostream &os, Term t) {
                switch (t) {
                case MATERIAL:
                case IMBALANCE:
                case INITIATIVE:
                case TOTAL:
                    os << " | ----- -----" << " | ----- -----";
                    break;
                default:
                    os << " | " << Scores[t][WHITE] << " | " << Scores[t][BLACK];
                    break;
                }
                os << " | " << Scores[t][WHITE] - Scores[t][BLACK] << " |\n";
                return os;
            }

        };

        Table<Score, TERM_NO, COLORS> Tracer::Scores;


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

        constexpr Array<Bitboard, 3> SlotFileBB
        {
            FileBB[FILE_E]|FileBB[FILE_F]|FileBB[FILE_G]|FileBB[FILE_H],    // K-File
            FileBB[FILE_A]|FileBB[FILE_B]|FileBB[FILE_C]|FileBB[FILE_D],    // Q-File
            FileBB[FILE_C]|FileBB[FILE_D]|FileBB[FILE_E]|FileBB[FILE_F]     // C-File
        };

        constexpr Array<Bitboard, FILES> KingFlankBB
        {
            SlotFileBB[CS_QUEN] ^ FileBB[FILE_D],
            SlotFileBB[CS_QUEN],
            SlotFileBB[CS_QUEN],
            SlotFileBB[CS_NONE],
            SlotFileBB[CS_NONE],
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
        constexpr Score QueenWeaken     { S( 49, 15) };
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

            Pawns   ::Entry *pawnEntry{ nullptr };
            Material::Entry *matlEntry{ nullptr };

            // Contains all squares attacked by the color and piece type.
            Array<Bitboard, COLORS> fulAttacks;
            // Contains all squares attacked by the color and piece type with pinned removed.
            Array<Bitboard, COLORS, PIECE_TYPES> sqlAttacks;
            // Contains all squares attacked by more than one pieces of a color, possibly via x-ray or by one pawn and one piece.
            Array<Bitboard, COLORS> pawnsDblAttacks;
            Array<Bitboard, COLORS> dblAttacks;
            // Contains all squares from which queen can be attacked
            Array<Bitboard, COLORS, 3>  queenAttacked;

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

            template<Color> void initAttacks();
            template<Color> void initMobility();
            template<Color, PieceType> Score pieces();
            template<Color> Score king() const;
            template<Color> Score threats() const;
            template<Color> Score passers() const;
            template<Color> Score space() const;

            Score initiative(Score) const;
            Scale scale(Value) const;

        public:

            Evaluation() = delete;
            Evaluation(Evaluation const&) = delete;
            Evaluation& operator=(Evaluation const&) = delete;

            Evaluation(Position const &p) :
                pos{ p }
            {}

            Value value();
        };

        /// initAttacks() computes pawn and king attacks.
        template<bool Trace> template<Color Own>
        void Evaluation<Trace>::initAttacks() {
            Bitboard pawns{ pos.pieces(Own, PAWN) };

            auto kSq{ pos.square(Own|KING) };

            sqlAttacks[Own].fill(0);
            sqlAttacks[Own][PAWN] =  pawnSglAttackBB<Own>(pawns & ~pos.kingBlockers(Own))
                                  | (pawnSglAttackBB<Own>(pawns &  pos.kingBlockers(Own)) & PieceAttackBB[BSHP][kSq]);
            sqlAttacks[Own][KING] = PieceAttackBB[KING][kSq];
            sqlAttacks[Own][NONE] = sqlAttacks[Own][PAWN]
                                  | sqlAttacks[Own][KING];
            //
            fulAttacks[Own] = pawnSglAttackBB<Own>(pawns)
                            | sqlAttacks[Own][KING];
            //
            pawnsDblAttacks[Own] = pawnDblAttackBB<Own>(pawns)
                                 & sqlAttacks[Own][PAWN];
            dblAttacks[Own] = pawnsDblAttacks[Own]
                            | (sqlAttacks[Own][PAWN]
                             & sqlAttacks[Own][KING]);
            //
            queenAttacked[Own].fill(0);
        }
        /// initMobility() computes mobility and king-ring.
        template<bool Trace> template<Color Own>
        void Evaluation<Trace>::initMobility() {
            constexpr auto Opp{ ~Own };

            // Mobility area: Exclude followings
            mobArea[Own] = ~(// Squares protected by enemy pawns
                             sqlAttacks[Opp][PAWN]
                             // Squares occupied by friend Queen and King
                           | pos.pieces(Own, QUEN, KING)
                            // Squares occupied by friend King blockers
                           | (pos.kingBlockers(Own) /*& pos.pieces(Own)*/)
                            // Squares occupied by block pawns (pawns on ranks 2-3/blocked)
                           | (pos.pieces(Own, PAWN)
                            & (LowRankBB[Own]
                             | pawnSglPushBB<Opp>(pos.pieces()))));

            mobility[Own] = SCORE_ZERO;

            auto kSq{ pos.square(Own|KING) };
            // King safety tables
            auto sq{ makeSquare(clamp(sFile(kSq), FILE_B, FILE_G),
                                clamp(sRank(kSq), RANK_2, RANK_7)) };
            kingRing[Own] = PieceAttackBB[KING][sq] | sq;

            kingAttackersCount [Opp] = popCount(kingRing[Own] & sqlAttacks[Opp][PAWN]);
            kingAttackersWeight[Opp] = 0;
            kingAttacksCount   [Opp] = 0;

            // Remove from kingRing the squares defended by two pawns
            kingRing[Own] &= ~pawnsDblAttacks[Own];
        }

        /// pieces() evaluates the pieces of the color and type.
        template<bool Trace> template<Color Own, PieceType PT>
        Score Evaluation<Trace>::pieces() {
            static_assert (NIHT <= PT && PT <= QUEN, "PT incorrect");

            constexpr auto Opp{ ~Own };

            Score score{ SCORE_ZERO };

            for (Square s : pos.squares(Own|PT)) {
                assert((Own|PT) == pos[s]);

                // Find attacked squares, including x-ray attacks for Bishops, Rooks and Queens
                Bitboard attacks;
                switch (PT) {
                case BSHP: attacks = attacksBB<BSHP>(s, pos.pieces() ^ ((pos.pieces(Own, QUEN, BSHP) & ~pos.kingBlockers(Own)) | pos.pieces(Opp, QUEN))); break;
                case ROOK: attacks = attacksBB<ROOK>(s, pos.pieces() ^ ((pos.pieces(Own, QUEN, ROOK) & ~pos.kingBlockers(Own)) | pos.pieces(Opp, QUEN))); break;
                case QUEN: attacks = attacksBB<QUEN>(s, pos.pieces() ^ ((pos.pieces(Own, QUEN)       & ~pos.kingBlockers(Own))));                         break;
                case NIHT: default: attacks = PieceAttackBB[NIHT][s];                                                                                      break;
                }

                Bitboard action{
                    contains(pos.kingBlockers(Own), s) ?
                        LineBB[pos.square(Own|KING)][s] : BoardBB };

                attacks &= action;

                switch (PT) {
                case BSHP: {
                    Bitboard pc =  pos.pieces(Own)
                                & ~pos.kingBlockers(Own);
                    dblAttacks[Own] |= sqlAttacks[Own][NONE]
                                     & (attacks
                                      | (pawnSglAttackBB<Own>(pc & pos.pieces(PAWN) & frontRanksBB(Own, s)) & PieceAttackBB[BSHP][s] & action));
                }
                    break;
                case QUEN: {
                    Bitboard pc =  pos.pieces(Own)
                                & ~pos.kingBlockers(Own);
                    dblAttacks[Own] |= sqlAttacks[Own][NONE]
                                     & (attacks
                                      | (pawnSglAttackBB<Own>(pc & pos.pieces(PAWN) & frontRanksBB(Own, s)) & PieceAttackBB[BSHP][s] & action)
                                      | (attacksBB<BSHP>(s, pos.pieces() ^ (pc & pos.pieces(BSHP) & PieceAttackBB[BSHP][s])) & action)
                                      | (attacksBB<ROOK>(s, pos.pieces() ^ (pc & pos.pieces(ROOK) & PieceAttackBB[ROOK][s])) & action));
                }
                    break;
                default:
                    dblAttacks[Own] |= sqlAttacks[Own][NONE]
                                     & attacks;
                    break;
                }

                sqlAttacks[Own][PT]   |= attacks;
                sqlAttacks[Own][NONE] |= attacks;
                fulAttacks[Own]       |= pos.pieceAttacksFrom(PT, s);

                if ((attacks & kingRing[Opp]) != 0) {
                    kingAttackersCount [Own]++;
                    kingAttackersWeight[Own] += KingAttackerWeight[PT];
                    kingAttacksCount   [Own] += popCount(attacks & sqlAttacks[Opp][KING]);
                }

                auto mob = popCount(attacks & mobArea[Own]);
                assert(0 <= mob && mob <= 27);

                // Bonus for piece mobility
                mobility[Own] += Mobility[PT][mob];

                Bitboard b;
                // Special extra evaluation for pieces
                switch (PT) {
                case NIHT:
                case BSHP: {
                    // Bonus for minor behind a pawn
                    score += MinorBehindPawn * contains(pawnSglPushBB<Opp>(pos.pieces(PAWN)), s);

                    // Penalty for distance from the friend king
                    score -= MinorKingProtect * distance(s, pos.square(Own|KING));

                    b = OutpostBB[Own]
                      & ~pawnEntry->attackSpan[Opp]
                      & sqlAttacks[Own][PAWN];

                    if (NIHT == PT) {

                        // Bonus for knight outpost squares
                        if (contains(b, s)) {
                            score += KnightOutpost * 2;
                        }
                        else
                        if (0 != (b & attacks & ~pos.pieces(Own))) {
                            score += KnightOutpost * 1;
                        }

                    }
                    else
                    if (BSHP == PT) {

                        // Bonus for bishop outpost squares
                        if (contains(b, s)) {
                            score += BishopOutpost * 1;
                        }

                        // Penalty for pawns on the same color square as the bishop,
                        // more when the center files are blocked with pawns.
                        score -= BishopPawns
                               * popCount(pos.pieces(Own, PAWN)
                                        & ColorBB[sColor(s)])
                               * (1 + popCount(pos.pieces(Own, PAWN)
                                             & SlotFileBB[CS_NONE]
                                             & pawnSglPushBB<Opp>(pos.pieces())));

                        // Bonus for bishop on a long diagonal which can "see" both center squares
                        score += BishopOnDiagonal
                               * moreThanOne(attacksBB<BSHP>(s, pos.pieces(PAWN)) & CenterBB);

                        // An important Chess960 pattern: A cornered bishop blocked by a friend pawn diagonally in front of it.
                        // It is a very serious problem, especially when that pawn is also blocked.
                        // Bishop (white or black) on a1/h1 or a8/h8 which is trapped by own pawn on b2/g2 or b7/g7.
                        if (1 >= mob
                         && Options["UCI_Chess960"]) {
                            auto relSq = relativeSq(Own, s);
                            if (SQ_A1 == relSq
                             || SQ_H1 == relSq) {

                                auto del{ PawnPush[Own] + sign(FILE_E - sFile(s)) * EAST };
                                if (contains(pos.pieces(Own, PAWN), s + del)) {
                                    score -= BishopTrapped
                                           * (!contains(pos.pieces(), s + del + PawnPush[Own]) ?
                                                 !contains(pos.pieces(Own, PAWN), s + del + del) ?
                                                     1 : 2 : 4);
                                }
                            }
                        }
                    }
                }
                    break;
                case ROOK: {

                    // Bonus for rook on the same file as a queen
                    if (0 != (fileBB(s) & pos.pieces(QUEN))) {
                        score += RookOnQueenFile;
                    }

                    // Bonus for rook when on an open or semi-open file
                    if (pos.semiopenFileOn(Own, s)) {
                        score += RookOnFile[pos.semiopenFileOn(Opp, s)];
                    }
                    else
                    // Penalty for rook when trapped by the king, even more if the king can't castle
                    if (3 >= mob
                     && RANK_4 > relativeRank(Own, s)
                     && 0 != (frontSquaresBB(Own, s) & pos.pieces(Own, PAWN))) {
                        auto kF = sFile(pos.square(Own|KING));
                        if (((FILE_E > kF) && (kF > sFile(s)))
                         || ((FILE_D < kF) && (kF < sFile(s)))) {
                            score -= RookTrapped * (1 + !pos.canCastle(Own));
                        }
                    }
                }
                    break;
                case QUEN: {

                    queenAttacked[Own][0] |= pos.pieceAttacksFrom(NIHT, s);
                    queenAttacked[Own][1] |= pos.pieceAttacksFrom(BSHP, s);
                    queenAttacked[Own][2] |= pos.pieceAttacksFrom(ROOK, s);

                    // Penalty for pin or discover attack on the queen
                    b = 0; // Queen attackers
                    if (( pos.sliderBlockersAt(s, pos.pieces(Opp, BSHP, ROOK), b, b)
                       & ~pos.kingBlockers(Opp)
                       & ~( fileBB(s)
                         &  pos.pieces(Opp, PAWN)
                         & ~pawnSglAttackBB<Own>(pos.pieces(Own)))) != 0) {
                        score -= QueenWeaken;
                    }
                }
                    break;
                }
            }

            if (Trace) {
                Tracer::write(Term(PT), Own, score);
            }

            return score;
        }

        /// king() evaluates the king of the color.
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
            if (0 != rookSafeChecks) {
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
            if (0 != quenSafeChecks) {
                kingDanger += SafeCheckWeight[QUEN];
            }

            // Enemy bishops checks
            Bitboard bshpSafeChecks{  bshpPins
                                   &  sqlAttacks[Opp][BSHP]
                                   &  safeArea
                                   & ~quenSafeChecks };
            if (0!= bshpSafeChecks) {
                kingDanger += SafeCheckWeight[BSHP];
            }
            else {
                unsafeCheck |= bshpPins
                             & sqlAttacks[Opp][BSHP];
            }

            // Enemy knights checks
            Bitboard nihtChecks{  PieceAttackBB[NIHT][kSq]
                               &  sqlAttacks[Opp][NIHT]};
            if (0 != (nihtChecks & safeArea)) {
                kingDanger += SafeCheckWeight[NIHT];
            }
            else {
                unsafeCheck |= nihtChecks;
            }

            Bitboard b;

            b =  KingFlankBB[sFile(kSq)]
              &  CampBB[Own]
              &  sqlAttacks[Opp][NONE];
            // Friend king flank attack count
            i32 kfAttacks = popCount(b)                     // Squares attacked by enemy in friend king flank
                          + popCount(b & dblAttacks[Opp]);  // Squares attacked by enemy twice in friend king flank.
            // Friend king flank defense count
            b =  KingFlankBB[sFile(kSq)]
              &  CampBB[Own]
              &  sqlAttacks[Own][NONE];
            i32 kfDefense = popCount(b);

            // King Safety:
            Score score{ pawnEntry->evaluateKingSafety<Own>(pos, fulAttacks[Opp]) };

            kingDanger +=   1 * (kingAttackersCount[Opp] * kingAttackersWeight[Opp])
                        +  69 * kingAttacksCount[Opp]
                        + 185 * popCount(kingRing[Own] & weakArea)
                        + 148 * popCount(unsafeCheck)
                        +  98 * popCount(pos.kingBlockers(Own))
                        +   3 * kfAttacks * kfAttacks / 8
                        // Enemy queen is gone
                        - 873 * (0 == pos.pieces(Opp, QUEN))
                        // Friend knight is near by to defend king
                        - 100 * (0 != ( sqlAttacks[Own][NIHT]
                                     & (sqlAttacks[Own][KING] | kSq)))
                        // Mobility
                        -   1 * (mgValue(mobility[Own] - mobility[Opp]))
                        -   4 * kfDefense
                        // Pawn Safety quality
                        -   3 * mgValue(score) / 4
                        +  37;

            // Transform the king danger into a score
            if (100 < kingDanger) {
                score -= makeScore(kingDanger * kingDanger / 0x1000, kingDanger / 0x10);
            }

            // Penalty for king on a pawn less flank
            score -= PawnLessFlank * (0 == (pos.pieces(PAWN) & KingFlankBB[sFile(kSq)]));

            // King tropism: Penalty for slow motion attacks moving towards friend king zone
            score -= KingFlankAttacks * kfAttacks;

            if (Trace) {
                Tracer::write(Term(KING), Own, score);
            }

            return score;
        }

        /// threats() evaluates the threats of the color.
        template<bool Trace> template<Color Own>
        Score Evaluation<Trace>::threats() const {
            constexpr auto Opp{ ~Own };

            Score score{ SCORE_ZERO };

            // Squares defended by the opponent,
            // - defended the square with a pawn
            // - defended the square twice and not attacked twice.
            Bitboard defendedArea = sqlAttacks[Opp][PAWN]
                                  | ( dblAttacks[Opp]
                                   & ~dblAttacks[Own]);
            // Enemy non-pawns
            Bitboard nonPawnsEnemies =  pos.pieces(Opp)
                                     & ~pos.pieces(PAWN);
            // Enemy not defended and under attacked by any friend piece
            Bitboard attackedUndefendedEnemies =  pos.pieces(Opp)
                                               & ~defendedArea
                                               &  sqlAttacks[Own][NONE];
            // Non-pawn enemies, defended by enemies
            Bitboard defendedNonPawnsEnemies = nonPawnsEnemies
                                             & defendedArea;

            Bitboard b;

            if (0 != (attackedUndefendedEnemies
                    | defendedNonPawnsEnemies)) {
                // Bonus according to the type of attacking pieces

                // Enemies attacked by minors
                b =  (attackedUndefendedEnemies
                    | defendedNonPawnsEnemies)
                  &  (sqlAttacks[Own][NIHT]
                    | sqlAttacks[Own][BSHP]);
                while (0 != b) {
                    score += MinorThreat[pType(pos[popLSq(b)])];
                }

                if (0 != attackedUndefendedEnemies) {
                    // Enemies attacked by majors
                    b =  attackedUndefendedEnemies
                      &  sqlAttacks[Own][ROOK];
                    while (0 != b) {
                        score += MajorThreat[pType(pos[popLSq(b)])];
                    }

                    // Enemies attacked by king
                    b =  attackedUndefendedEnemies
                      &  sqlAttacks[Own][KING];
                    if (0 != b) {
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

            Bitboard safeArea;

            // Defended or Unattacked squares
            safeArea =  sqlAttacks[Own][NONE]
                     | ~sqlAttacks[Opp][NONE];
            // Safe friend pawns
            b =  safeArea
              &  pos.pieces(Own, PAWN);
            // Safe friend pawns attacks on non-pawn enemies
            b =  nonPawnsEnemies
              &  pawnSglAttackBB<Own>(b)
              &  sqlAttacks[Own][PAWN];
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
            if (0 != pos.pieces(Opp, QUEN)) {
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

        /// passers() evaluates the passed pawns of the color.
        template<bool Trace> template<Color Own>
        Score Evaluation<Trace>::passers() const {
            constexpr auto Opp{ ~Own };

            auto kingProximity = [&](Color c, Square s) {
                return std::min(distance(pos.square(c|KING), s), 5);
            };

            Score score{ SCORE_ZERO };

            Bitboard psr{ pawnEntry->passers[Own] };
            while (0 != psr) {
                auto s{ popLSq(psr) };
                assert(0 == ((pawnSglPushBB<Own>(frontSquaresBB(Own, s))
                            | ( pawnPassSpan(Own, s + PawnPush[Own])
                             & ~pos.pawnAttacksFrom(Own, s + PawnPush[Own])))
                           & pos.pieces(Opp, PAWN)));

                i32 r{ relativeRank(Own, s) };
                // Base bonus depending on rank.
                Score bonus{ PasserRank[r] };

                auto pushSq{ s + PawnPush[Own] };

                if (RANK_3 < r) {
                    i32 w{ 5 * r - 13 };

                    // Adjust bonus based on the king's proximity
                    bonus += makeScore(0, i32(+4.75*w*kingProximity(Opp, pushSq)
                                            -2.00*w*kingProximity(Own, pushSq)));
                    // If block square is not the queening square then consider also a second push.
                    if (RANK_7 != r) {
                        bonus += makeScore(0, -1*w*kingProximity(Own, pushSq + PawnPush[Own]));
                    }

                    // If the pawn is free to advance.
                    if (pos.empty(pushSq)) {
                        Bitboard attackedSquares{ pawnPassSpan(Own, s) };

                        Bitboard behindMajors{ frontSquaresBB(Opp, s)
                                             & pos.pieces(ROOK, QUEN) };
                        if (0 == (pos.pieces(Opp) & behindMajors)) {
                            attackedSquares &= sqlAttacks[Opp][NONE];
                        }

                        // Bonus according to attacked squares.
                        i32 k{ 0 == attackedSquares               ? 35 :
                               0 == (attackedSquares
                                   & frontSquaresBB(Own, s))      ? 20 :
                               !contains(attackedSquares, pushSq) ?  9 : 0 };

                        // Bonus according to defended squares.
                        if (0 != (pos.pieces(Own) & behindMajors)
                         || contains(sqlAttacks[Own][NONE], pushSq)) {
                            k += 5;
                        }

                        bonus += makeScore(k*w, k*w);
                    }
                }

                // Scale down bonus for candidate passers
                // - have a pawn in front of it.
                // - need more than one pawn push to become passer.
                if (contains(pos.pieces(PAWN), pushSq)
                 || !pos.pawnPassedAt(Own, pushSq)) {
                    bonus /= 2;
                }

                score += bonus
                    - PasserFile * foldFile(sFile(s));
            }

            if (Trace) {
                Tracer::write(PASSER, Own, score);
            }

            return score;
        }

        /// space() evaluates the space of the color.
        /// The space evaluation is a simple bonus based on the number of safe squares
        /// available for minor pieces on the central four files on ranks 2-4
        /// Safe squares one, two or three squares behind a friend pawn are counted twice
        /// The aim is to improve play on opening
        template<bool Trace> template<Color Own>
        Score Evaluation<Trace>::space() const {
            constexpr auto Opp{ ~Own };

            // Find all squares which are at most three squares behind some friend pawn
            Bitboard behind{ pos.pieces(Own, PAWN) };
            behind |= pawnSglPushBB<Opp>(behind);
            behind |= pawnSglPushBB<Opp>(behind);

            // Safe squares for friend pieces inside the area defined by SpaceMask.
            Bitboard safeSpace{  PawnSideBB[Own]
                              &  SlotFileBB[CS_NONE]
                              & ~pos.pieces(Own, PAWN)
                              & ~sqlAttacks[Opp][PAWN] };

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
            i32 outflanking = distance<File>(pos.square(WHITE|KING), pos.square(BLACK|KING))
                            - distance<Rank>(pos.square(WHITE|KING), pos.square(BLACK|KING));
            // Compute the initiative bonus for the attacking side
            i32 complexity = 11 * pos.count(PAWN)
                           +  9 * pawnEntry->passedCount()
                           +  9 * outflanking
                            // King infiltration
                           + 24 * (sRank(pos.square(WHITE|KING)) > RANK_4
                                || sRank(pos.square(BLACK|KING)) < RANK_5)
                           + 51 * (VALUE_ZERO == pos.nonPawnMaterial())
                           - 110;

            // Pawn on both flanks
            if (0 != (pos.pieces(PAWN) & SlotFileBB[CS_KING])
             && 0 != (pos.pieces(PAWN) & SlotFileBB[CS_QUEN])) {
                complexity += 21;
            }
            else
            // Almost Unwinnable
            if (0 > outflanking
             && 0 == pawnEntry->passedCount()) {
                complexity -= 43;
            }

            auto mg{ mgValue(s) };
            auto eg{ egValue(s) };
            // Now apply the bonus: note that we find the attacking side by extracting the
            // sign of the midgame or endgame values, and that we carefully cap the bonus
            // so that the midgame and endgame scores do not change sign after the bonus.
            Score score{ makeScore(sign(mg) * clamp(complexity + 50, -abs(mg), 0),
                                   sign(eg) * std::max(complexity  , -abs(eg))) };

            if (Trace) {
                Tracer::write(INITIATIVE, score);
            }

            return score;
        }

        /// scale() evaluates the scale for the position
        template<bool Trace>
        Scale Evaluation<Trace>::scale(Value eg) const {
            auto stngColor{ eg >= VALUE_ZERO ? WHITE : BLACK };

            auto scl{ nullptr != matlEntry->scalingFunc[stngColor] ?
                        (*matlEntry->scalingFunc[stngColor])(pos) : SCALE_NONE };
            if (SCALE_NONE == scl) {
                scl = matlEntry->scale[stngColor];
            }
            assert(SCALE_NONE != scl);

            // If scale is not already specific, scale down the endgame via general heuristics
            if (SCALE_NORMAL == scl) {

                scl = pos.bishopOpposed()
                   && 2 * VALUE_MG_BSHP == pos.nonPawnMaterial() ?
                        Scale(22) :
                        std::min(Scale(36 + (7 - 5 * pos.bishopOpposed()) * pos.count(stngColor|PAWN)), SCALE_NORMAL);

                // Scale down endgame factor when shuffling
                scl = std::max(Scale(scl + 3 - pos.clockPly() / 4), SCALE_DRAW);
            }
            return scl;
        }

        /// value() computes the various parts of the evaluation and
        /// returns the value of the position from the point of view of the side to move.
        template<bool Trace>
        Value Evaluation<Trace>::value() {
            assert(0 == pos.checkers());

            // Probe the material hash table
            matlEntry = Material::probe(pos);
            // If have a specialized evaluation function for the material configuration
            if (nullptr != matlEntry->evaluationFunc) {
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
                       + (pawnEntry->score[WHITE]
                        - pawnEntry->score[BLACK])
                       + pos.thread()->contempt };

            // Early exit if score is high
            Value v{ (mgValue(score) + egValue(score)) / 2 };
            if (abs(v) > VALUE_LAZY_THRESHOLD
                       + pos.nonPawnMaterial() / 64) {
                return WHITE == pos.activeSide() ? +v : -v;
            }

            if (Trace) {
                Tracer::clear();
            }

            initAttacks <WHITE>(), initAttacks <BLACK>();
            initMobility<WHITE>(), initMobility<BLACK>();

            // Pieces should be evaluated first (populate attack information)
            score += pieces<WHITE, NIHT>() - pieces<BLACK, NIHT>();
            score += pieces<WHITE, BSHP>() - pieces<BLACK, BSHP>();
            score += pieces<WHITE, ROOK>() - pieces<BLACK, ROOK>();
            score += pieces<WHITE, QUEN>() - pieces<BLACK, QUEN>();

            assert((sqlAttacks[WHITE][NONE] & dblAttacks[WHITE]) == dblAttacks[WHITE]);
            assert((sqlAttacks[BLACK][NONE] & dblAttacks[BLACK]) == dblAttacks[BLACK]);

            score += mobility[WHITE]  - mobility[BLACK]
                   + king   <WHITE>() - king   <BLACK>()
                   + threats<WHITE>() - threats<BLACK>()
                   + passers<WHITE>() - passers<BLACK>();
            if (pos.nonPawnMaterial() >= VALUE_SPACE_THRESHOLD) {
            score += space  <WHITE>() - space  <BLACK>();
            }

            score += initiative(score);

            assert(-VALUE_INFINITE < mgValue(score) && mgValue(score) < +VALUE_INFINITE);
            assert(-VALUE_INFINITE < egValue(score) && egValue(score) < +VALUE_INFINITE);
            assert(0 <= matlEntry->phase && matlEntry->phase <= Material::PhaseResolution);

            // Interpolates between midgame and scaled endgame values (scaled by 'scale(egValue(score))').
            v = mgValue(score) * (matlEntry->phase)
              + egValue(score) * (Material::PhaseResolution - matlEntry->phase) * scale(egValue(score)) / SCALE_NORMAL;
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
            return (WHITE == pos.activeSide() ? +v : -v) + VALUE_TEMPO;
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
        if (0 != pos.checkers()) {
            return "Evaluation: none (in check)\n";
        }
        // Reset any dynamic contempt
        auto contempt = pos.thread()->contempt;
        pos.thread()->contempt = SCORE_ZERO;
        auto value{ Evaluation<true>(pos).value() };
        pos.thread()->contempt = contempt;

        // Trace scores are from White's point of view
        value = WHITE == pos.activeSide() ? +value : -value;

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
            << "\nEvaluation: " << toCP(value) / 100.0 << " (white side)\n";

        return oss.str();
    }
}
