#include "Evaluator.h"

#include <iomanip>
#include <ostream>

#include "Pawns.h"
#include "Material.h"
#include "MoveGenerator.h"
#include "Thread.h"

namespace Evaluator {

    using namespace std;
    using namespace BitBoard;
    using namespace MoveGen;

    // Tempo bonus
    const Value Tempo = Value(20);

    namespace {

        namespace Tracer {

            enum Term : u08
            {
                // The first 6 entries are for PieceType
                MATERIAL = NONE,
                IMBALANCE,
                MOBILITY,
                THREAT,
                PASSED_PAWN,
                SPACE_ACTIVITY,
                TOTAL,
            };

            double cp[TOTAL+1][CLR_NO][PH_NO];

            void write (u08 term, Color c, Score score)
            {
                cp[term][c][MG] = value_to_cp (mg_value (score));
                cp[term][c][EG] = value_to_cp (eg_value (score));
            }
            void write (u08 term, Score wscore, Score bscore = SCORE_ZERO)
            {
                write (term, WHITE, wscore);
                write (term, BLACK, bscore);
            }

            ostream& operator<< (ostream &os, Term term)
            {
                if (   term == Term(PAWN)
                    || term == MATERIAL
                    || term == IMBALANCE
                    || term == TOTAL)
                {
                    os << " | ----- ----- | ----- ----- | ";
                }
                else
                {
                    os << " | " << std::setw (5) << cp[term][WHITE][MG]
                       << " "   << std::setw (5) << cp[term][WHITE][EG]
                       << " | " << std::setw (5) << cp[term][BLACK][MG]
                       << " "   << std::setw (5) << cp[term][BLACK][EG]
                       << " | ";
                }
                os << std::setw (5) << cp[term][WHITE][MG] - cp[term][BLACK][MG] << " "
                   << std::setw (5) << cp[term][WHITE][EG] - cp[term][BLACK][EG]
                   << std::endl;
                return os;
            }
        }

        using namespace Tracer;

        // Struct EvalInfo contains various information computed and collected
        // by the evaluation functions.
        struct EvalInfo
        {
            // ful_attacked_by[color][piece-type] contains all squares attacked by the given color and piece type,
            // ful_attacked_by[color][NONE] contains all squares attacked by the given color.
            Bitboard ful_attacked_by[CLR_NO][MAX_PTYPE];
            // pin_attacked_by[color][piece-type] contains all squares attacked by the given color and piece type with pinned removed,
            // pin_attacked_by[color][NONE] contains all squares attacked by the given color with pinned removed.
            Bitboard pin_attacked_by[CLR_NO][MAX_PTYPE];

            // pinneds[color] contains all the pinned pieces
            Bitboard pinneds[CLR_NO];

            // king_ring[color] is the zone around the king which is considered
            // by the king safety evaluation. This consists of the squares directly
            // adjacent to the king, and the three (or two, for a king on an edge file)
            // squares two ranks in front of the king. For instance, if black's king
            // is on g8, king_ring[BLACK] is a bitboard containing the squares f8, h8,
            // f7, g7, h7, f6, g6 and h6.
            Bitboard king_ring[CLR_NO];

            // king_ring_attackers_count[color] is the number of pieces of the given color
            // which attack a square in the king_ring of the enemy king.
            u08 king_ring_attackers_count[CLR_NO];

            // king_ring_attackers_weight[color] is the sum of the "weight" of the pieces
            // of the given color which attack a square in the king_ring of the enemy king.
            // The weights of the individual piece types are given by the KingAttackWeights[piece-type]
            i32 king_ring_attackers_weight[CLR_NO];

            // king_zone_attacks_count[color] is the number of attacks by
            // the given color to squares directly adjacent to the enemy king.
            // Pieces which attack more than one square are counted multiple times.
            // e.g, if Black's King is on g8 and there's a White knight on g5,
            // this knight adds 2 to king_zone_attacks_count[WHITE].
            u08 king_zone_attacks_count[CLR_NO];

            // Pointers to pawn and material hash table entries
            Pawns   ::Entry *pe = nullptr;
            Material::Entry *me = nullptr;

            EvalInfo () = default;
            EvalInfo (const EvalInfo&) = delete;
            EvalInfo& operator= (const EvalInfo&) = delete;
        };

    #define S(mg, eg) mk_score (mg, eg)

        // PieceMobility[piece-type][attacks] contains bonuses for mobility,
        // indexed by piece type and number of attacked squares in the mobility area.
        const Score PieceMobility[NONE][28] =
        {
            {},
            { // Knight
                S(-75,-76), S(-56,-54), S(- 9,-26), S( -2,-10), S(  6,  5), S( 15, 11),
                S( 22, 26), S( 30, 28), S( 36, 29)
            },
            { // Bishop
                S(-48,-58), S(-21,-19), S( 16, -2), S( 26, 12), S( 37, 22), S( 51, 42),
                S( 54, 54), S( 63, 58), S( 65, 63), S( 71, 70), S( 79, 74), S( 81, 86),
                S( 92, 90), S( 97, 94)
            },
            { // Rook
                S(-56,-78), S(-25,-18), S(-11, 26), S( -5, 55), S( -4, 70), S( -1, 81),
                S(  8,109), S( 14,120), S( 21,128), S( 23,143), S( 31,154), S( 32,160),
                S( 43,165), S( 49,168), S( 59,169)
            },
            { // Queen
                S(-40,-35), S(-25,-12), S(  2,  7), S(  4, 19), S( 14, 37), S( 24, 55),
                S( 25, 62), S( 40, 76), S( 43, 79), S( 47, 87), S( 54, 94), S( 56,102),
                S( 60,111), S( 70,116), S( 72,118), S( 73,122), S( 75,128), S( 77,130),
                S( 85,133), S( 94,136), S( 99,140), S(108,157), S(112,158), S(113,161),
                S(118,174), S(119,177), S(123,191), S(128,199)
            },
            {}
        };

        // Outpost[supported by pawn] contains bonuses for knights and bishops outposts.
        const Score KnightOutpost[2] = { S(43,11), S(65,20) };
        const Score BishopOutpost[2] = { S(20, 3), S(29, 8) };

        // ReachableOutpost[supported by pawn] contains bonuses for knights and bishops
        // which can reach a outpost square in one move.
        const Score KnightReachableOutpost[2] = { S(21, 5), S(35, 8) };
        const Score BishopReachableOutpost[2] = { S( 8, 0), S(14, 4) };

        // RookOnFile[semiopen/open] contains bonuses for rooks when there is no
        // friendly pawn on the rook file.
        const Score RookOnFile[2] = { S(20, 7), S(45,20) };

        // SafePawnThreat[piece-type] contains bonuses according to which piece type is attacked by pawn
        // which is protected or is not attacked.
        const Score SafePawnThreat[NONE] = { S(  0,  0), S(176,139), S(131,127), S(217,218), S(203,215), S(  0,  0) };

        enum PieceCategory : u08
        {
            MINOR,
            MAJOR,
        };
        // PieceThreat[attacker category][attacked type] contains
        // bonuses according to which piece type attacks which one.
        // Attacks on lesser pieces which are pawn-defended are not considered.
        const Score PieceThreat[2][NONE] =
        {
            { S( 0, 33), S(45, 43), S(46, 47), S(72,107), S(48,118), S( 0, 0) },  // Minor attackers
            { S( 0, 25), S(40, 62), S(40, 59), S( 0, 34), S(35, 48), S( 0, 0) },  // Major attackers
        };
        // KingThreat[one/more] contains bonuses for King attacks on
        // pawns or pieces which are not pawn-defended.
        const Score KingThreat[2] = { S( 3, 62), S( 9,138) };

        const Score HangingPawnThreat = S(71,61);
        const Score PawnPushThreat    = S(38,22);

        const Score PieceLoosed       = S( 0,25); // Bonus for each loosed piece
        const Score PieceHanged       = S(48,27); // Bonus for each hanged piece

        const Score MinorBehindPawn   = S(16, 0); // Bonus for minor behind a pawn

        const Score BishopPawns       = S( 8,12); // Penalty for bishop with pawns on same color
        const Score BishopTrapped     = S(50,50); // Penalty for bishop trapped with pawns (Chess960)

        const Score RookOnPawns       = S( 8,24); // Bonus for rook on pawns
        const Score RookTrapped       = S(92, 0); // Penalty for rook trapped

        const Score PieceSafeCheck    = S(20,20);
        const Score PieceOtherCheck   = S(10,10);

        // PawnPassedScore[file] contains a bonus for passed pawns according to the file of the pawn.
        const Score PawnPassedScore[F_NO/2] = { S( 9, 10), S( 2, 10), S( 1, -8), S(-20,-12) };

    #undef S

    #define V(v) Value(v)
        // PawnPassedValue[phase][rank] contains bonuses for passed pawns according to the rank of the pawn.
        // Don't use a Score because the two components processed independently.
        const Value PawnPassedValue[PH_NO][R_NO] =
        {
            { V(0), V(5), V( 5), V(31), V(73), V(166), V(252), V(0) },
            { V(0), V(7), V(14), V(38), V(73), V(166), V(252), V(0) }
        };
    #undef V

        const i32 MaxAttackUnits = 400;
        // Various little "meta-bonuses" measuring the strength of
        // the enemy attack are added up into an integer, which
        // is used as an index to KingDanger[].
        Score KingDanger[MaxAttackUnits];

        // KingAttackWeights[piece-type] contains king attack weights by piece type
        const i32 KingAttackWeights [NONE] = { 1, 14, 10,  8,  2,  0 };

        // Penalties for enemy's piece safe checks
        const i32 PieceSafeCheckUnit[NONE] = { 0, 17,  5, 45, 52 , 0 };

        // Penalty for enemy's queen contact checks
        const i32 QueenContactCheckUnit = 89;

        //  --- init evaluation info --->
        template<Color Own>
        // init_evaluation<>() initializes king ring bitboards for the given color
        // To be done at the beginning of the evaluation.
        void init_evaluation (const Position &pos, EvalInfo &ei)
        {
            const auto Opp  = Own == WHITE ? BLACK : WHITE;
            const auto Push = Own == WHITE ? DEL_N : DEL_S;
            const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
            const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;

            ei.ful_attacked_by[Own][NONE] |=
            ei.ful_attacked_by[Own][PAWN]  = ei.pe->pawn_attacks[Own];

            Bitboard pinneds = ei.pinneds[Own] = pos.pinneds (Own);
            Bitboard pinned_pawns = pinneds & pos.pieces (Own, PAWN);
            if (pinned_pawns != 0)
            {
                Bitboard loosed_pawns = ~pinned_pawns & pos.pieces (Own, PAWN);
                Bitboard pawn_attacks = shift_bb<LCap> (loosed_pawns) | shift_bb<RCap> (loosed_pawns);
                while (pinned_pawns != 0)
                {
                    auto s = pop_lsq (pinned_pawns);
                    pawn_attacks |= PawnAttacks[Own][s] & RayLine_bb[pos.square<KING> (Own)][s];
                }
                ei.pin_attacked_by[Own][NONE] |=
                ei.pin_attacked_by[Own][PAWN]  = pawn_attacks;
            }
            else
            {
                ei.pin_attacked_by[Own][NONE] |=
                ei.pin_attacked_by[Own][PAWN]  = ei.pe->pawn_attacks[Own];
            }

            Bitboard king_attacks         =
            ei.ful_attacked_by[Opp][KING] =
            ei.pin_attacked_by[Opp][KING] = PieceAttacks[KING][pos.square<KING> (Opp)];

            ei.king_ring_attackers_count [Own] = 0;
            ei.king_ring_attackers_weight[Own] = 0;
            ei.king_zone_attacks_count   [Own] = 0;
            ei.king_ring                 [Opp] = 0;

            // Init king safety tables only if going to use them
            // Do not evaluate king safety when you are close to the endgame so the weight of king safety is small
            if (pos.non_pawn_material (Own) >= VALUE_MG_QUEN)
            {
                ei.king_ring[Opp] = king_attacks|(DistRings_bb[pos.square<KING> (Opp)][1] &
                                                        (rel_rank (Opp, pos.square<KING> (Opp)) < R_5 ? PawnPassSpan[Opp][pos.square<KING> (Opp)] :
                                                         rel_rank (Opp, pos.square<KING> (Opp)) < R_7 ? PawnPassSpan[Opp][pos.square<KING> (Opp)]|PawnPassSpan[Own][pos.square<KING> (Opp)] :
                                                                                                        PawnPassSpan[Own][pos.square<KING> (Opp)]));
                if ((king_attacks & ei.pin_attacked_by[Own][PAWN]) != 0)
                {
                    Bitboard attackers = pos.pieces (Own, PAWN) & (king_attacks|(DistRings_bb[pos.square<KING> (Opp)][1] & (rank_bb (pos.square<KING> (Opp)-Push)|rank_bb (pos.square<KING> (Opp)))));
                    ei.king_ring_attackers_count [Own] = u08(pop_count (attackers));
                    ei.king_ring_attackers_weight[Own] = ei.king_ring_attackers_count [Own]*KingAttackWeights[PAWN];
                }
            }
        }

        template<Color Own, PieceType PT, bool Trace>
        // evaluate_pieces<>() assigns bonuses and penalties to the pieces of the given color and type
        Score evaluate_pieces (const Position &pos, EvalInfo &ei, const Bitboard mobility_area, Score &mobility)
        {
            assert(PT == NIHT || PT == BSHP || PT == ROOK || PT == QUEN);

            const auto Opp  = Own == WHITE ? BLACK : WHITE;
            const auto Push = Own == WHITE ? DEL_N : DEL_S;

            auto score = SCORE_ZERO;

            ei.ful_attacked_by[Own][PT] = 0;
            ei.pin_attacked_by[Own][PT] = 0;

            for (Square s : pos.squares<PT> (Own))
            {
                // Find attacked squares, including x-ray attacks for bishops and rooks
                Bitboard attacks =
                    PT == BSHP ? attacks_bb<BSHP> (s, (pos.pieces () ^ pos.pieces (Own, QUEN, BSHP)) | ei.pinneds[Own]) :
                    PT == ROOK ? attacks_bb<ROOK> (s, (pos.pieces () ^ pos.pieces (Own, QUEN, ROOK)) | ei.pinneds[Own]) :
                    PT == QUEN ? attacks_bb<QUEN> (s, (pos.pieces () ^ pos.pieces (Own, QUEN)) | ei.pinneds[Own]) :
                    /*PT == NIHT*/PieceAttacks[PT][s];

                ei.ful_attacked_by[Own][NONE] |= ei.ful_attacked_by[Own][PT] |= attacks;

                if ((ei.pinneds[Own] & s) != 0)
                {
                    attacks &= RayLine_bb[pos.square<KING> (Own)][s];
                }
                ei.pin_attacked_by[Own][NONE] |= ei.pin_attacked_by[Own][PT] |= attacks;

                if ((ei.king_ring[Opp] & attacks) != 0)
                {
                    ei.king_ring_attackers_count [Own]++;
                    ei.king_ring_attackers_weight[Own] += KingAttackWeights[PT];
                    Bitboard zone_attacks = ei.ful_attacked_by[Opp][KING] & attacks;
                    ei.king_zone_attacks_count[Own] += u08(pop_count (zone_attacks));
                }

                if (PT == QUEN)
                {
                    attacks &= ~(  ei.pin_attacked_by[Opp][NIHT]
                                 | ei.pin_attacked_by[Opp][BSHP]
                                 | ei.pin_attacked_by[Opp][ROOK]);
                }

                i32 mob = pop_count (attacks & mobility_area);
                mobility += PieceMobility[PT][mob];

                // Special extra evaluation for pieces

                if (PT == NIHT || PT == BSHP)
                {
                    const Bitboard OutpostMask = Own == WHITE ? // Mask of allowed outpost squares
                        R4_bb|R5_bb|R6_bb :
                        R5_bb|R4_bb|R3_bb;

                    // Bonus for minors (bishop or knight) when behind a pawn
                    if (   rel_rank (Own, s) < R_5
                        && (pos.pieces (PAWN) & (s+Push)) != 0)
                    {
                        score += MinorBehindPawn;
                    }

                    if (PT == NIHT)
                    {
                        // Bonus for knight outpost square
                        Bitboard bb = OutpostMask & ~ei.pe->pawn_attack_span[Opp];
                        if ((bb & s) != 0)
                        {
                            score += KnightOutpost[(ei.pin_attacked_by[Own][PAWN] & s) != 0 ? 1 : 0];
                        }
                        else
                        {
                            bb &= attacks & ~pos.pieces (Own);
                            if (bb != 0)
                            {
                                score += KnightReachableOutpost[(ei.pin_attacked_by[Own][PAWN] & bb) != 0 ? 1 : 0];
                            }
                        }
                    }
                    else
                    if (PT == BSHP)
                    {
                        // Bonus for bishop outpost square
                        Bitboard bb = OutpostMask & ~ei.pe->pawn_attack_span[Opp];
                        if ((bb & s) != 0)
                        {
                            score += BishopOutpost[(ei.pin_attacked_by[Own][PAWN] & s) != 0 ? 1 : 0];
                        }
                        else
                        {
                            bb &= attacks & ~pos.pieces (Own);
                            if (bb != 0)
                            {
                                score += BishopReachableOutpost[(ei.pin_attacked_by[Own][PAWN] & bb) != 0 ? 1 : 0];
                            }
                        }

                        // Penalty for pawns on the same color square as the bishop
                        score -= BishopPawns * ei.pe->pawns_on_squarecolor<Own> (s);

                        if (   rel_sq (Own, s) == SQ_A8
                            || rel_sq (Own, s) == SQ_H8)
                        {
                            auto del = (F_A == _file (s) ? DEL_E : DEL_W)-Push;
                            if (pos[s + del] == (Own|PAWN))
                            {
                                score -= BishopTrapped;
                            }
                        }

                        if (pos.chess960 ())
                        {
                            // An important Chess960 pattern: A cornered bishop blocked by a friendly pawn diagonally in front of it.
                            // It is a very serious problem, especially when that pawn is also blocked.
                            // Bishop on a1/h1 or a8/h8 (white or black) which is trapped by own pawn on b2/g2 or b7/g7 (white or black).
                            if (   rel_sq (Own, s) == SQ_A1
                                || rel_sq (Own, s) == SQ_H1)
                            {
                                auto del = (F_A == _file (s) ? DEL_E : DEL_W)+Push;
                                if (pos[s+del] == (Own|PAWN))
                                {
                                    score -= BishopTrapped *
                                            (  !pos.empty (s+del+Push) ? 4 :
                                                pos[s+del+del] == (Own|PAWN) ? 2 : 1);
                                }
                            }
                        }
                    }
                }
                else
                if (PT == ROOK)
                {
                    // Bonus for rook aligning with enemy pawns on the same rank/file
                    if (rel_rank (Own, s) > R_4)
                    {
                        score += RookOnPawns * pop_count (pos.pieces (Opp, PAWN) & PieceAttacks[ROOK][s]);
                    }

                    // Bonus for rook when on an open or semi-open file
                    if (ei.pe->file_semiopen<Own> (_file (s)))
                    {
                        score += RookOnFile[ei.pe->file_semiopen<Opp> (_file (s)) ? 1 : 0];
                    }
                    else
                    // Penalty for rook when trapped by the king, even more if the king can't castle
                    if (mob <= 3)
                    {
                        auto fk_sq = pos.square<KING> (Own);
                        // Rooks trapped by own king, more if the king has lost its castling capability.
                        if (   (_file (fk_sq) < F_E) == (_file (s) < _file (fk_sq))
                            && (rel_rank (Own, fk_sq) == R_1 || _rank (fk_sq) == _rank (s))
                            && !ei.pe->side_semiopen<Own> (_file (fk_sq), _file (s) < _file (fk_sq)))
                        {
                            score -= (RookTrapped - mk_score (22 * mob, 0)) * (pos.can_castle (Own) ? 1 : 2);
                        }
                    }
                }
            }

            if (Trace)
            {
                write (PT, Own, score);
            }

            return score;
        }
        //  --- init evaluation info <---

        template<Color Own, bool Trace>
        // evaluate_king<>() assigns bonuses and penalties to a king of the given color.
        Score evaluate_king (const Position &pos, const EvalInfo &ei)
        {
            const auto Opp  = Own == WHITE ? BLACK : WHITE;
            const auto Push = Own == WHITE ? DEL_N : DEL_S;

            auto fk_sq = pos.square<KING> (Own);

            // King Safety: friend pawns shelter and enemy pawns storm
            ei.pe->evaluate_king_safety<Own> (pos);

            Value value = ei.pe->king_safety[Own][CS_NO];
            // If can castle use the value after the castle if is bigger
            if (   rel_rank (Own, fk_sq) == R_1
                && pos.can_castle (Own) != CR_NONE)
            {
                if (    pos.can_castle (Castling<Own, CS_KING>::Right) != CR_NONE
                    && (pos.king_path (Castling<Own, CS_KING>::Right) & ei.ful_attacked_by[Opp][NONE]) == 0
                    && (pos.castle_path (Castling<Own, CS_KING>::Right) & pos.pieces ()) == 0)
                {
                    value = std::max (ei.pe->king_safety[Own][CS_KING], value);
                }
                if (    pos.can_castle (Castling<Own, CS_QUEN>::Right) != CR_NONE
                    && (pos.king_path (Castling<Own, CS_QUEN>::Right) & ei.ful_attacked_by[Opp][NONE]) == 0
                    && (pos.castle_path (Castling<Own, CS_QUEN>::Right) & pos.pieces ()) == 0)
                {
                    value = std::max (ei.pe->king_safety[Own][CS_QUEN], value);
                }
            }

            auto score = mk_score (value, -16 * ei.pe->king_pawn_dist[Own]);

            // Main king safety evaluation
            if (ei.king_ring_attackers_count[Opp] != 0)
            {
                // Find the attacked squares which are defended only by the king in the king zone...
                Bitboard king_zone_undef =
                      ei.ful_attacked_by[Own][KING] // King-zone
                    & ei.ful_attacked_by[Opp][NONE]
                    & ~(  ei.pin_attacked_by[Own][PAWN]
                        | ei.pin_attacked_by[Own][NIHT]
                        | ei.pin_attacked_by[Own][BSHP]
                        | ei.pin_attacked_by[Own][ROOK]
                        | ei.pin_attacked_by[Own][QUEN]);

                // ... and those which are not defended at all in the king ring
                Bitboard king_ring_undef =
                       ei.king_ring[Own]
                    &  ei.pin_attacked_by[Opp][NONE]
                    & ~ei.pin_attacked_by[Own][NONE];

                // Initialize the 'attack_units' variable, which is used later on as an
                // index to the KingDanger[] array. The initial value is based on the
                // number and types of the enemy's attacking pieces, the number of
                // attacked and undefended squares around our king, and the quality of
                // the pawn shelter (current 'mg score' value).
                i32 attack_units =
                    + std::min ((ei.king_ring_attackers_weight[Opp]*ei.king_ring_attackers_count[Opp])/2, 72)   // King-ring attacks
                    +  9 * (ei.king_zone_attacks_count[Opp])                                                    // King-zone attacks
                    + 27 * (pop_count (king_zone_undef))                                                        // King-zone undefended pieces
                    + 11 * (pop_count (ei.pinneds[Own]))                                                        // King pinned piece
                    + 11 * (pop_count (king_ring_undef))                                                        // King-ring undefended piece
                    - 64 * (pos.count<QUEN>(Opp) == 0)
                    - i32(value) / 8;

                // Undefended squares around king not occupied by enemy's
                king_zone_undef &= ~pos.pieces (Opp);
                if (king_zone_undef != 0)
                {
                    if (pos.count<QUEN> (Opp) > 0)
                    {
                        // Analyze enemy's safe queen contact checks.
                        // Undefended squares around the king attacked by enemy queen...
                        Bitboard undefended_attacked = king_zone_undef & ei.pin_attacked_by[Opp][QUEN];

                        Bitboard unsafe =
                              ei.ful_attacked_by[Opp][PAWN]
                            | ei.ful_attacked_by[Opp][NIHT]
                            | ei.ful_attacked_by[Opp][BSHP]
                            | ei.ful_attacked_by[Opp][ROOK]
                            | ei.ful_attacked_by[Opp][KING];
                        while (undefended_attacked != 0)
                        {
                            auto sq = pop_lsq (undefended_attacked);
                            if (   (unsafe & sq) != 0
                                || (  pos.count<QUEN> (Opp) > 1
                                    && more_than_one (pos.pieces (Opp, QUEN) & (PieceAttacks[QUEN][sq]))
                                    && more_than_one (pos.pieces (Opp, QUEN) & (attacks_bb<QUEN> (sq, pos.pieces () ^ pos.pieces (Opp, QUEN))))))
                            {
                                attack_units += QueenContactCheckUnit;
                            }
                        }
                    }
                }

                // Analyse the safe enemy's checks which are possible on safe area ...
                Bitboard safe_area  = ~(ei.pin_attacked_by[Own][NONE] | (pos.pieces (Opp)));
                // ... and some other potential checks, only requiring the square to be 
                // safe from pawn-attacks, and not being occupied by a blocked pawn.
                Bitboard other_area = ~(ei.pin_attacked_by[Own][PAWN] | (pos.pieces (Opp, PAWN) & shift_bb<Push> (pos.pieces (PAWN))));

                Bitboard rook_attack = attacks_bb<ROOK> (fk_sq, pos.pieces ());
                Bitboard bshp_attack = attacks_bb<BSHP> (fk_sq, pos.pieces ());
                Bitboard niht_attack = PieceAttacks[NIHT][fk_sq];

                // Queen safe checks
                if (((rook_attack | bshp_attack) & ei.pin_attacked_by[Opp][QUEN] & safe_area) != 0)
                {
                    score -= PieceSafeCheck;
                    attack_units += PieceSafeCheckUnit[QUEN];
                }

                // Rook safe and other checks
                if ((rook_attack & ei.pin_attacked_by[Opp][ROOK] & safe_area) != 0)
                {
                    score -= PieceSafeCheck;
                    attack_units += PieceSafeCheckUnit[ROOK];
                }
                else
                if ((rook_attack & ei.pin_attacked_by[Opp][ROOK] & other_area) != 0)
                {
                    score -= PieceOtherCheck;
                }

                // Bishop safe and other checks
                if ((bshp_attack & ei.pin_attacked_by[Opp][BSHP] & safe_area) != 0)
                {
                    score -= PieceSafeCheck;
                    attack_units += PieceSafeCheckUnit[BSHP];
                }
                else
                if ((bshp_attack & ei.pin_attacked_by[Opp][BSHP] & other_area) != 0)
                {
                    score -= PieceOtherCheck;
                }

                // Knight safe and other checks
                if ((niht_attack & ei.pin_attacked_by[Opp][NIHT] & safe_area) != 0)
                {
                    score -= PieceSafeCheck;
                    attack_units += PieceSafeCheckUnit[NIHT];
                }
                else
                if ((niht_attack & ei.pin_attacked_by[Opp][NIHT] & other_area) != 0)
                {
                    score -= PieceOtherCheck;
                }

                // Finally, extract the king danger score from the KingDanger[] array
                // and subtract the score from the evaluation.
                // attack_units must be in range [0, MaxAttackUnits-1]
                score -= KingDanger[std::min (std::max (attack_units, 0), MaxAttackUnits-1)];
            }

            if (Trace)
            {
                write (KING, Own, score);
            }

            return score;
        }

        template<Color Own, bool Trace>
        // evaluate_threats<>() evaluates the threats of the given color.
        // according to the type of attacking piece and the type of attacked pieces.
        Score evaluate_threats (const Position &pos, const EvalInfo &ei)
        {
            const auto Opp      = Own == WHITE ? BLACK : WHITE;
            const auto Push     = Own == WHITE ? DEL_N  : DEL_S;
            const auto LCap     = Own == WHITE ? DEL_NW : DEL_SE;
            const auto RCap     = Own == WHITE ? DEL_NE : DEL_SW;
            const auto Rank2BB  = Own == WHITE ? R2_bb : R7_bb;
            const auto Rank7BB  = Own == WHITE ? R7_bb : R2_bb;

            auto score = SCORE_ZERO;

            // Loose enemies (except Queen and King)
            Bitboard b =
                  (pos.pieces (Opp) ^ pos.pieces (Opp, QUEN, KING))
                & ~(  ei.pin_attacked_by[Own][NONE]
                    | ei.pin_attacked_by[Opp][NONE]);
            score += PieceLoosed * pop_count (b);

            // Non-pawn enemies attacked by any friendly pawn
            Bitboard weak_nonpawns =
                  (pos.pieces (Opp) ^ pos.pieces (Opp, PAWN))
                &  ei.pin_attacked_by[Own][PAWN];
            if (weak_nonpawns != 0)
            {
                // Safe Pawns
                b = pos.pieces (Own, PAWN)
                  & ( ~ei.pin_attacked_by[Opp][NONE]
                     | ei.pin_attacked_by[Own][NONE]);
                // Safe Pawn threats
                b = weak_nonpawns & (shift_bb<RCap>(b) | shift_bb<LCap>(b));
                score += HangingPawnThreat * pop_count (weak_nonpawns ^ b);
                while (b != 0)
                {
                    score += SafePawnThreat[ptype (pos[pop_lsq (b)])];
                }
            }

            // Enemies not defended by pawn and attacked by any friendly piece
            Bitboard weak_pieces =
                  pos.pieces (Opp)
                & ~ei.pin_attacked_by[Opp][PAWN]
                &  ei.pin_attacked_by[Own][NONE];

            // Non-pawn enemies defended by a pawn and attacked by any friendly piece
            Bitboard defended_nonpawns =
                  (pos.pieces (Opp) ^ pos.pieces (Opp, PAWN))
                &  ei.pin_attacked_by[Opp][PAWN]
                &  ei.pin_attacked_by[Own][NONE];

            // Add a bonus according to the kind of attacking pieces

            // Enemies attacked by minor pieces
            b = (weak_pieces | defended_nonpawns | pos.pieces (Opp, ROOK, QUEN))
              & (ei.pin_attacked_by[Own][NIHT] | ei.pin_attacked_by[Own][BSHP]);
            while (b != 0)
            {
                score += PieceThreat[MINOR][ptype (pos[pop_lsq (b)])];
            }
            // Enemies attacked by rooks
            b = (weak_pieces | pos.pieces (Opp, QUEN))
              & (ei.pin_attacked_by[Own][ROOK]);
            while (b != 0)
            {
                score += PieceThreat[MAJOR][ptype (pos[pop_lsq (b)])];
            }

            if (weak_pieces != 0)
            {
                // Weak hanging enemies attacked by any
                score += PieceHanged * pop_count (weak_pieces & ~ei.pin_attacked_by[Opp][NONE]);

                // Weak enemies attacked by king
                b = weak_pieces & ei.pin_attacked_by[Own][KING];
                if (b != 0)
                {
                    score += KingThreat[more_than_one (b) ? 1 : 0];
                }
            }

            // Bonus if some friendly pawns can safely push and attack an enemy piece
            b = pos.pieces (Own, PAWN) & ~Rank7BB;
            b = shift_bb<Push> (b | (shift_bb<Push> (b & Rank2BB) & ~pos.pieces ()));
            // Safe pawn pushes
            b &= ~pos.pieces ()
              &  ~ei.pin_attacked_by[Opp][PAWN]
              &  ( ~ei.pin_attacked_by[Opp][NONE]
                  | ei.pin_attacked_by[Own][NONE]);
            // Safe pawn pushes attacks an enemy piece
            b =  (shift_bb<LCap> (b) | shift_bb<RCap> (b))
              &   pos.pieces (Opp)
              &  ~ei.pin_attacked_by[Own][PAWN];
            score += PawnPushThreat * pop_count (b);

            if (Trace)
            {
                write (THREAT, Own, score);
            }

            return score;
        }

        template<Color Own, bool Trace>
        // evaluate_passed_pawns<>() evaluates the passed pawns of the given color.
        Score evaluate_passed_pawns (const Position &pos, const EvalInfo &ei)
        {
            const auto Opp  = Own == WHITE ? BLACK : WHITE;
            const auto Push = Own == WHITE ? DEL_N : DEL_S;

            const auto nonpawn_diff = pos.count<NONPAWN> (Own) - pos.count<NONPAWN> (Opp);

            auto score = SCORE_ZERO;

            Bitboard passed_pawns = ei.pe->passed_pawns[Own];
            while (passed_pawns != 0)
            {
                auto s = pop_lsq (passed_pawns);
                assert(pos.passed_pawn (Own, s));

                auto rank = rel_rank (Own, s);
                // Base bonus depending on rank.
                auto mg_value = PawnPassedValue[MG][rank];
                auto eg_value = PawnPassedValue[EG][rank];

                auto r  = i08(rank) - i08(R_2);
                auto rr = r*(r-1);

                if (rr != 0)
                {
                    auto block_sq = s+Push;

                    // Adjust bonus based on kings proximity.
                    eg_value +=
                        + 5*rr*dist (pos.square<KING> (Opp), block_sq)
                        - 2*rr*dist (pos.square<KING> (Own), block_sq);
                    // If block square is less then the queening square then consider also a second push.
                    if (rel_rank (Own, block_sq) < R_8)
                    {
                        eg_value -= 1*rr*dist (pos.square<KING> (Own), block_sq+Push);
                    }

                    // If the pawn is free to advance.
                    if (pos.empty (block_sq))
                    {
                        // Squares to queen
                        Bitboard front_squares = FrontSqrs_bb[Own][s];
                        Bitboard behind_majors = FrontSqrs_bb[Opp][s] & pos.pieces (ROOK, QUEN);
                        if (behind_majors != 0)
                        {
                            behind_majors &= attacks_bb<ROOK> (s, pos.pieces ());
                        }
                        Bitboard unsafe_squares = front_squares
                            ,      safe_squares = front_squares;
                        // If there is an enemy rook or queen attacking the pawn from behind,
                        // add all X-ray attacks by the rook or queen. Otherwise consider only
                        // the squares in the pawn's path attacked or occupied by the enemy.
                        if ((behind_majors & pos.pieces (Opp)) == 0)
                        {
                            unsafe_squares &= ei.pin_attacked_by[Opp][NONE] | pos.pieces (Opp);
                        }

                        if ((behind_majors & pos.pieces (Own)) == 0)
                        {
                            safe_squares &= ei.pin_attacked_by[Own][NONE];
                        }

                        // Give a big bonus if there aren't enemy attacks, otherwise
                        // a smaller bonus if the block square is not attacked.
                        i32 k = unsafe_squares != 0 ?
                                    (unsafe_squares & block_sq) != 0 ?
                                        0 : 8 : 18;

                        if (safe_squares != 0)
                        {
                            // Give a big bonus if the path to the queen is fully defended,
                            // a smaller bonus if at least the block square is defended.
                            k += safe_squares == front_squares ?
                                    6 : (safe_squares & block_sq) != 0 ?
                                        4 : 0;
                        }

                        mg_value += k*rr + 0*r + 0;
                        eg_value += k*rr + 0*r + 0;
                    }
                    else
                    // If the pawn is blocked by own pieces
                    if ((pos.pieces (Own) & block_sq) != 0)
                    {
                        mg_value += 1*rr + 2*r + 0;
                        eg_value += 1*rr + 2*r + 0;
                    }
                }
                // If non-pawn count differ.
                if (nonpawn_diff != 0)
                {
                    eg_value *= 1.0 + (double) (nonpawn_diff) / 4.0;
                }

                score += mk_score (mg_value, eg_value) + PawnPassedScore[std::min (_file (s), F_H - _file (s))];
            }

            if (Trace)
            {
                write (PASSED_PAWN, Own, score);
            }

            return score;
        }

        template<Color Own, bool Trace>
        // evaluate_space_activity<>() computes the space evaluation of the given color.
        // The space evaluation is a simple bonus based on the number of safe squares
        // available for minor pieces on the central four files on ranks 2--4.
        // Safe squares one, two or three squares behind a friendly pawn are counted twice.
        // The aim is to improve play on game opening.
        Score evaluate_space_activity (const Position &pos, const EvalInfo &ei)
        {
            const auto Opp = Own == WHITE ? BLACK : WHITE;
            // SpaceMask contains the area of the board which is considered by
            // the space evaluation. In the middle game, each side is given a bonus
            // based on how many squares inside this area are safe and available for
            // friendly minor pieces.
            const Bitboard SpaceMask = Own == WHITE ?
                (FC_bb|FD_bb|FE_bb|FF_bb) & (R2_bb|R3_bb|R4_bb) :
                (FC_bb|FD_bb|FE_bb|FF_bb) & (R7_bb|R6_bb|R5_bb);

            // Find the safe squares for our pieces inside the area defined by SpaceMask.
            // A square is unsafe:
            // if it is attacked by an enemy pawn or
            // if it is undefended and attacked by an enemy piece.
            Bitboard safe_space =
                   SpaceMask
                & ~pos.pieces (Own, PAWN)
                & ~ei.pin_attacked_by[Opp][PAWN]
                & (   ei.pin_attacked_by[Own][NONE]
                   | ~ei.pin_attacked_by[Opp][NONE]);

            // Since SpaceMask is fully on our half of the board
            assert(u32(safe_space >> (Own == WHITE ? 32 : 0)) == 0);

            // Find all squares which are at most three squares behind some friendly pawn
            Bitboard behind = pos.pieces (Own, PAWN);
            behind |= shift_bb<Own == WHITE ? DEL_S  : DEL_N > (behind);
            behind |= shift_bb<Own == WHITE ? DEL_SS : DEL_NN> (behind);

            // Count safe_space + (behind & safe_space) with a single pop_count
            auto bonus = pop_count ((Own == WHITE ? safe_space << 32 : safe_space >> 32) | (behind & safe_space));
            auto weight = pos.count<NIHT> () + pos.count<BSHP> ();

            auto score = mk_score (bonus * weight * weight * 2 / 11, 0);

            if (Trace)
            {
                write (SPACE_ACTIVITY, Own, score);
            }

            return score;
        }

        // evaluate_initiative() computes the initiative correction value for the position
        // i.e. second order bonus/malus based on the known attacking/defending status of the players.
        Score evaluate_initiative (const Position &pos, i32 asymmetry, Value eg)
        {
            auto king_dist = dist<File> (pos.square<KING> (WHITE), pos.square<KING> (BLACK))
                           - dist<Rank> (pos.square<KING> (WHITE), pos.square<KING> (BLACK));

            // Compute the initiative bonus for the attacking side
            auto initiative = 8 * (asymmetry + king_dist) + 12 * pos.count<PAWN> () - 120;

            // Now apply the bonus: note that we find the attacking side by extracting
            // the sign of the endgame value, and that we carefully cap the bonus so
            // that the endgame score will never be divided by more than two.
            return mk_score (0, sign (eg) * std::max (initiative, -abs (eg / 2)));
        }

        // evaluate_scale_factor() computes the scale factor for the winning side
        ScaleFactor evaluate_scale_factor (const Position &pos, const EvalInfo &ei, Value eg)
        {
            assert(PHASE_ENDGAME <= ei.me->game_phase && ei.me->game_phase <= PHASE_MIDGAME);

            const auto strong_side = eg >= VALUE_ZERO ? WHITE : BLACK;
            // Scale winning side if position is more drawish than it appears
            auto scale_factor = ei.me->scale_factor (pos, strong_side);

            // If don't already have an unusual scale factor, check for certain
            // types of endgames, and use a lower scale for those.
            if (   ei.me->game_phase < PHASE_MIDGAME
                && (scale_factor == SCALE_FACTOR_NORMAL || scale_factor == SCALE_FACTOR_ONEPAWN))
            {
                if (pos.opposite_bishops ())
                {
                    // Endgame with opposite-colored bishops and no other pieces (ignoring pawns)
                    // is almost a draw, in case of KBP vs KB is even more a draw.
                    if (   pos.non_pawn_material (WHITE) == VALUE_MG_BSHP
                        && pos.non_pawn_material (BLACK) == VALUE_MG_BSHP)
                    {
                        auto pawn_diff = abs (pos.count<PAWN> (WHITE) - pos.count<PAWN> (BLACK));
                        scale_factor = ScaleFactor(pawn_diff != 0 ? 12 * pawn_diff : 8);
                    }
                    // Endgame with opposite-colored bishops, but also other pieces. Still
                    // a bit drawish, but not as drawish as with only the two bishops. 
                    else
                    {
                        scale_factor = ScaleFactor(scale_factor * SCALE_FACTOR_BISHOPS/SCALE_FACTOR_NORMAL);
                    }
                }
                // Endings where weaker side can place his king in front of the strong side pawns are drawish.
                else
                if (    abs (eg) <= VALUE_EG_BSHP
                    &&  ei.pe->pawn_span[strong_side] <= 1
                    && !pos.passed_pawn (~strong_side, pos.square<KING> (~strong_side)))
                {
                    scale_factor = ei.pe->pawn_span[strong_side] != 0 ? ScaleFactor(51) : ScaleFactor(37);
                }
            }
            return scale_factor;
        }

    }

    template<bool Trace>
    // evaluate<>() is the main evaluation function.
    // It returns a static evaluation of the position from the point of view of the side to move.
    Value evaluate (const Position &pos)
    {
        assert(pos.checkers () == 0);

        EvalInfo ei;

        // Probe the material hash table
        ei.me = Material::probe (pos);

        // If have a specialized evaluation function for the current material configuration
        if (ei.me->specialized_eval_exists ())
        {
            return ei.me->evaluate (pos);
        }

        // Initialize score by reading the incrementally updated scores included
        // in the position object (material + piece square tables).
        // Score is computed internally from the white point of view.
        auto score  = pos.psq_score ();

        score += ei.me->imbalance;

        // Probe the pawn hash table
        ei.pe = Pawns::probe (pos);
        score += ei.pe->pawn_score;

        for (auto c = WHITE; c <= BLACK; ++c)
        {
            ei.ful_attacked_by[c][NONE] = 0;
            ei.pin_attacked_by[c][NONE] = 0;
        }

        // Initialize attack and king safety bitboards
        init_evaluation<WHITE> (pos, ei);
        init_evaluation<BLACK> (pos, ei);

        for (auto c = WHITE; c <= BLACK; ++c)
        {
            ei.ful_attacked_by[c][NONE] |= ei.ful_attacked_by[c][KING];
            ei.pin_attacked_by[c][NONE] |= ei.pin_attacked_by[c][KING];
        }

        // Evaluate pieces and mobility
        Score mobility[CLR_NO] = { SCORE_ZERO, SCORE_ZERO };
        // Pawns blocked or on ranks 2-3 will be excluded from the mobility area
        const Bitboard blocked_pawns[CLR_NO] =
        {
            pos.pieces (WHITE, PAWN) & (shift_bb<DEL_S> (pos.pieces ()) | R2_bb | R3_bb),
            pos.pieces (BLACK, PAWN) & (shift_bb<DEL_N> (pos.pieces ()) | R7_bb | R6_bb)
        };
        // Do not include in mobility area squares protected by enemy pawns or occupied by friend blocked pawns or king
        const Bitboard mobility_area[CLR_NO] =
        {
            ~(ei.pin_attacked_by[BLACK][PAWN] | blocked_pawns[WHITE] | pos.square<KING> (WHITE)),
            ~(ei.pin_attacked_by[WHITE][PAWN] | blocked_pawns[BLACK] | pos.square<KING> (BLACK))
        };

        // Evaluate all pieces but king and pawns
        score +=
            + evaluate_pieces<WHITE, NIHT, Trace> (pos, ei, mobility_area[WHITE], mobility[WHITE])
            - evaluate_pieces<BLACK, NIHT, Trace> (pos, ei, mobility_area[BLACK], mobility[BLACK]);
        score +=
            + evaluate_pieces<WHITE, BSHP, Trace> (pos, ei, mobility_area[WHITE], mobility[WHITE])
            - evaluate_pieces<BLACK, BSHP, Trace> (pos, ei, mobility_area[BLACK], mobility[BLACK]);
        score +=
            + evaluate_pieces<WHITE, ROOK, Trace> (pos, ei, mobility_area[WHITE], mobility[WHITE])
            - evaluate_pieces<BLACK, ROOK, Trace> (pos, ei, mobility_area[BLACK], mobility[BLACK]);
        score +=
            + evaluate_pieces<WHITE, QUEN, Trace> (pos, ei, mobility_area[WHITE], mobility[WHITE])
            - evaluate_pieces<BLACK, QUEN, Trace> (pos, ei, mobility_area[BLACK], mobility[BLACK]);

        // Evaluate piece mobility
        score +=
            + mobility[WHITE]
            - mobility[BLACK];

        // Evaluate kings after all other pieces, needed full attack information including king.
        score +=
            + evaluate_king<WHITE, Trace> (pos, ei)
            - evaluate_king<BLACK, Trace> (pos, ei);

        // Evaluate tactical threats, needed full attack information including king
        score +=
            + evaluate_threats<WHITE, Trace> (pos, ei)
            - evaluate_threats<BLACK, Trace> (pos, ei);

        // Evaluate passed pawns, needed full attack information including king
        score +=
            + evaluate_passed_pawns<WHITE, Trace> (pos, ei)
            - evaluate_passed_pawns<BLACK, Trace> (pos, ei);

        // If in the opening phase
        if (   pos.non_pawn_material (WHITE) +  pos.non_pawn_material (BLACK) >= VALUE_SPACE
            && pos.count<NIHT> () + pos.count<BSHP> () != 0)
        {
            // Evaluate space activity
            score +=
                + evaluate_space_activity<WHITE, Trace> (pos, ei)
                - evaluate_space_activity<BLACK, Trace> (pos, ei);
        }
        else
        // If both sides have only pawns
        if (   pos.non_pawn_material (WHITE) == VALUE_ZERO
            && pos.non_pawn_material (BLACK) == VALUE_ZERO)
        {
            // Evaluate potential unstoppable pawns
            score +=
                + ei.pe->evaluate_unstoppable_pawns<WHITE> ()
                - ei.pe->evaluate_unstoppable_pawns<BLACK> ();
        }

        // Evaluate position potential for the winning side
        score += evaluate_initiative (pos, ei.pe->asymmetry, eg_value (score));

        assert(-VALUE_INFINITE < mg_value (score) && mg_value (score) < +VALUE_INFINITE);
        assert(-VALUE_INFINITE < eg_value (score) && eg_value (score) < +VALUE_INFINITE);

        // Evaluate scale factor for the winning side
        auto scale_factor = evaluate_scale_factor (pos, ei, eg_value (score));

        // Interpolates between a middle game and a endgame score scaled by scale factor, based on game phase.
        auto value = Value((  mg_value (score) * i32(ei.me->game_phase)
                            + eg_value (score) * i32(PHASE_MIDGAME - ei.me->game_phase)*i32(scale_factor)/SCALE_FACTOR_NORMAL)
                            / PHASE_MIDGAME);

        if (Trace)
        {
            // Write remaining evaluation terms
            write (PAWN     , ei.pe->pawn_score);
            write (MATERIAL , pos.psq_score ());
            write (IMBALANCE, ei.me->imbalance);
            write (MOBILITY , mobility[WHITE], mobility[BLACK]);
            write (TOTAL    , score);
        }

        return (pos.active () == WHITE ? +value : -value) + Tempo;
    }
    // --------------------------------
    // Explicit template instantiations
    template Value evaluate<false> (const Position&);
    template Value evaluate<true > (const Position&);

    // trace() is like evaluate() but instead of a value returns a string
    // (suitable to be print on stdout) that contains the detailed descriptions
    // and values of each evaluation term. Used for debugging.
    string trace   (const Position &pos)
    {
        std::memset (cp, 0x00, sizeof (cp));

        auto value = pos.active () == WHITE ?  // White's point of view
            +evaluate<true> (pos) :
            -evaluate<true> (pos);

        ostringstream oss;
        oss << std::showpos << std::showpoint << std::setprecision (2) << std::fixed
            << "      Eval Term |    White    |    Black    |     Total    \n"
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
            << "    King Safety" << Term(KING)
            << "         Threat" << Term(THREAT)
            << "    Passed Pawn" << Term(PASSED_PAWN)
            << " Space Activity" << Term(SPACE_ACTIVITY)
            << "----------------+-------------+-------------+--------------\n"
            << "          Total" << Term(TOTAL)
            << "\nEvaluation: " << value_to_cp (value) << " (white side)\n"
            << std::noshowpoint << std::noshowpos;
        return oss.str ();
    }

    // initialize() init tables
    void initialize ()
    {
        auto mg = 0;
        for (auto i = 0; i < MaxAttackUnits; ++i)
        {
            //                              MaxIncrement, MaxValue
            mg = std::min (std::min (i*i - 16, mg + 322), 47410);
            KingDanger[i] = mk_score (mg * 268 / 7700, 0);
        }
    }

}
