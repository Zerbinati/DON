#include "Evaluator.h"

#include <ostream>

#include "Pawns.h"
#include "Material.h"
#include "MoveGenerator.h"
#include "Thread.h"

namespace Evaluator {

    using namespace std;
    using namespace BitBoard;
    using namespace MoveGen;
    using namespace EndGame;

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
                TERM_NO
            };

            double cp[TERM_NO][CLR_NO][PH_NO];

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
                switch (u08(term))
                {
                case PAWN:
                case MATERIAL:
                case IMBALANCE:
                case TOTAL:
                    os << " | ----- ----- | ----- ----- | ";
                    break;
                default:
                    os << " | " << std::setw (5) << cp[term][WHITE][MG]
                       << " "   << std::setw (5) << cp[term][WHITE][EG]
                       << " | " << std::setw (5) << cp[term][BLACK][MG]
                       << " "   << std::setw (5) << cp[term][BLACK][EG]
                       << " | ";
                    break;
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
        private:
            template<Color Own>
            void initialize (const Position &pos)
            {
                static const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
                static const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;
                static const auto PAtt = PawnAttacks[Own];

                auto fk_sq = pos.square (Own, KING);

                Bitboard pinned_pawns = pos.abs_blockers (Own) & pos.pieces (Own, PAWN);
                if (pinned_pawns != 0)
                {
                    Bitboard loosed_pawns = ~pinned_pawns & pos.pieces (Own, PAWN);
                    Bitboard pawn_attacks = shift<LCap> (loosed_pawns)
                                          | shift<RCap> (loosed_pawns);
                    while (pinned_pawns != 0)
                    {
                        auto s = pop_lsq (pinned_pawns);
                        pawn_attacks |= PAtt[s] & strline_bb (fk_sq, s);
                    }
                    pin_attacked_by[Own][PAWN] = pawn_attacks;
                }
                else
                {
                    pin_attacked_by[Own][PAWN] = pe->attacks[Own];
                }

                pin_attacked_by[Own][KING] = PieceAttacks[KING][fk_sq];

                ful_attacked_by[Own]       = pe->attacks[Own] | pin_attacked_by[Own][KING];
                pin_attacked_by[Own][NONE] = pin_attacked_by[Own][PAWN] | pin_attacked_by[Own][KING];

                dbl_attacked[Own] = pin_attacked_by[Own][PAWN] & pin_attacked_by[Own][KING];

                king_ring                 [Own] = 0;
                king_ring_attackers_count [Own] = 0;
                king_ring_attackers_weight[Own] = 0;
                king_zone_attacks_count   [Own] = 0;
            }

        public:
            // Pointers to pawn and material hash table entries
            Pawns   ::Entry *const &pe = nullptr;
            Material::Entry *const &me = nullptr;

            // Contains all squares attacked by the given color and piece type.
            Bitboard ful_attacked_by[CLR_NO];
            // Contains all squares attacked by the given color and piece type with pinned removed.
            Bitboard pin_attacked_by[CLR_NO][MAX_PTYPE];
            // Squares attacked by more than one pieces of a given color, possibly via x-ray or by one pawn and one piece.
            // Diagonal x-ray through pawn or squares attacked by 2 pawns are not explicitly added.
            Bitboard dbl_attacked   [CLR_NO];

            // Zone around the king which is considered by the king safety evaluation.
            // This consists of the squares directly adjacent to the king, and the three (or two, for a king on an edge file) squares two ranks in front of the king.
            // For instance, if black's king is on g8, king_ring[BLACK] is a bitboard containing the squares f8, h8, f7, g7, h7, f6, g6 and h6.
            Bitboard king_ring                 [CLR_NO];
            // Number of pieces of the given color, which attack a square in the king_ring of the enemy king.
            u08      king_ring_attackers_count [CLR_NO];
            // Sum of the "weight" of the pieces of the given color which attack a square in the king_ring of the enemy king.
            // The weights of the individual piece types are given by the KingAttackWeights[piece-type]
            i32      king_ring_attackers_weight[CLR_NO];
            // Number of attacks by the given color to squares directly adjacent to the enemy king.
            // Pieces which attack more than one square are counted multiple times.
            u08      king_zone_attacks_count   [CLR_NO];

            EvalInfo () = delete;
            EvalInfo (const EvalInfo&) = delete;
            EvalInfo& operator= (const EvalInfo&) = delete;

            EvalInfo (const Position &pos, Pawns::Entry *const &p, Material::Entry *const &m)
                : pe (p)
                , me (m)
            {
                initialize<WHITE> (pos);
                initialize<BLACK> (pos);
            }

        };

    #define S(mg, eg) mk_score (mg, eg)

        // PieceMobility[piece-type][attacks] contains bonuses for mobility,
        // indexed by piece type and number of attacked squares in the mobility area.
        const Score PieceMobility[][28] =
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
            }
        };

        // Outpost[supported by pawn] contains bonuses for knights and bishops outposts.
        const Score KnightOutpost[2] = { S(43,11), S(65,20) };
        const Score BishopOutpost[2] = { S(20, 3), S(29, 8) };

        // ReachableOutpost[supported by pawn] contains bonuses for knights and bishops
        // which can reach a outpost square in one move.
        const Score KnightReachableOutpost[2] = { S(21, 5), S(35, 8) };
        const Score BishopReachableOutpost[2] = { S( 8, 0), S(14, 4) };

        // RookOnFile[semiopen/open] contains bonuses for rooks
        // when there is no friend pawn on the rook file.
        const Score RookOnFile[2] = { S(20, 7), S(45,20) };
        // Bonus for minor behind a pawn
        const Score MinorBehindPawn = S(16, 0);
        // Penalty for bishop with pawns on same color
        const Score BishopPawns     = S( 8,12);
        // Penalty for bishop trapped with pawns (Chess960)
        const Score BishopTrapped   = S(50,50);

        // Bonus for rook on pawns
        const Score RookOnPawns     = S( 8,24);
        // Penalty for rook trapped
        const Score RookTrapped     = S(92, 0);
        // Penalty for queen weaken
        const Score QueenWeaken     = S(50,10);

        const Score SafeChecked     = S(20,20);
        const Score ProbChecked     = S(10,10);
        // King tropism
        const Score EnemyInFlank    = S( 7, 0);
        const Score PawnlessFlank   = S(20,80);

        // Bonus for each hanged piece
        const Score PieceHanged     = S(48,27);
        // Bonus for each loosed piece
        const Score PieceLoosed     = S( 0,25);

        const Score PawnPushThreat  = S(38,22);

        const Score HangPawnThreat  = S(71,61);
        // SafePawnThreat[piece-type] contains bonuses
        // according to which piece type is attacked by pawn which is protected or is not attacked.
        const Score SafePawnThreat[NONE] = { S( 0, 0), S(176,139), S(131,127), S(217,218), S(203,215), S( 0, 0) };

        // PieceThreat[attacker category][attacked type] contains bonuses
        // according to which piece type attacks which one.
        // Attacks on lesser pieces which are pawn-defended are not considered.
        const Score PieceThreat[2][NONE] =
        {
            { S( 0,33), S(45,43), S(46,47), S(72,107), S(48,118), S( 0, 0) }, // Minor attackers
            { S( 0,25), S(40,62), S(40,59), S( 0, 34), S(35, 48), S( 0, 0) }  // Major attackers
        };
        const Score PieceThreatRank = S(16, 3);

        // KingThreat[one/more] contains bonuses for king attacks on pawns or pieces which are not pawn-defended.
        const Score KingThreat[2] = { S( 3, 62), S( 9,138) };

        const Score PawnPassHinder  = S( 7, 0);

        // PawnPassFile[file] contains a bonus for passed pawns according to distance from edge.
        const Score PawnPassFile[F_NO/2] = { S( 9, 10), S( 2, 10), S( 1, -8), S(-20,-12) };

    #undef S

    #define V(v) Value(v)
        // PawnPassRank[rank] contains bonuses for passed pawns according to the rank of the pawn.
        const Value PawnPassRank[R_NO] = { V(0), V(5), V(5), V(35), V(75), V(165), V(255), V(0) };
    #undef V

        // King attack weights by piece type
        const i32 KingAttackWeights [NONE] = { 0,  78,  56,  45,  11, 0 };
        // Penalties for enemy's piece safe checks by piece type
        const i32 PieceSafeChecks   [NONE] = { 0, 874, 538, 638, 695, 0 };
        // Penalty for enemy's queen contact checks
        const i32 QueenContactCheck = 997;

        template<Color Own>
        void init_king_ring (const Position &pos, EvalInfo &ei)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;

            if (pos.si->non_pawn_matl[Own] >= VALUE_MG_QUEN)
            {
                auto ek_sq = pos.square (Opp, KING);
                Bitboard king_zone = PieceAttacks[KING][ek_sq];
                ei.king_ring[Opp] = king_zone
                                  | (  dist_rings_bb (ek_sq, 1)
                                     & (rel_rank (Opp, ek_sq) < R_5 ? pawn_pass_span (Opp, ek_sq) :
                                        rel_rank (Opp, ek_sq) < R_7 ? pawn_pass_span (Opp, ek_sq)|pawn_pass_span (Own, ek_sq) :
                                                                      pawn_pass_span (Own, ek_sq)));
                ei.king_ring_attackers_count[Own] = u08(pop_count (  (  king_zone
                                                                      | (dist_rings_bb (ek_sq, 1) & pawn_pass_span (Opp, ek_sq)))
                                                                   & ei.pin_attacked_by[Own][PAWN]));
            }
        }

        // Evaluates bonuses and penalties of the pieces of the given color and type
        template<Color Own, PieceType PT, bool Trace>
        Score evaluate_pieces (const Position &pos, EvalInfo &ei, const Bitboard mobility_area, Score &mobility)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N : DEL_S;
            static const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
            static const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;

            assert(NIHT <= PT && PT <= QUEN);

            auto score = SCORE_ZERO;

            ei.pin_attacked_by[Own][PT] = 0;
            for (auto s : pos.squares[Own][PT])
            {
                // Find attacked squares, including x-ray attacks for bishops and rooks
                Bitboard attacks = 0;
                switch (PT)
                {
                case NIHT: attacks = PieceAttacks[NIHT][s];                                                                       break;
                case BSHP: attacks = attacks_bb<BSHP> (s, (pos.pieces () ^ pos.pieces (Own, PT, QUEN)) | pos.abs_blockers (Own)); break;
                case ROOK: attacks = attacks_bb<ROOK> (s, (pos.pieces () ^ pos.pieces (Own, PT, QUEN)) | pos.abs_blockers (Own)); break;
                case QUEN: attacks = attacks_bb<QUEN> (s, (pos.pieces () ^ pos.pieces (Own, PT      )) | pos.abs_blockers (Own)); break;
                }

                ei.ful_attacked_by[Own] |= attacks;

                if (contains (pos.abs_blockers (Own), s))
                {
                    attacks &= strline_bb (pos.square (Own, KING), s);
                }
                ei.dbl_attacked[Own] |= ei.pin_attacked_by[Own][NONE] & attacks;
                ei.pin_attacked_by[Own][NONE] |=
                ei.pin_attacked_by[Own][PT]   |= attacks;

                if ((ei.king_ring[Opp] & attacks) != 0)
                {
                    ei.king_ring_attackers_count [Own]++;
                    ei.king_ring_attackers_weight[Own] += KingAttackWeights[PT];
                    ei.king_zone_attacks_count[Own] += u08(pop_count (ei.pin_attacked_by[Opp][KING] & attacks));
                }

                if (PT == QUEN)
                {
                    attacks &=
                        ~(  ei.pin_attacked_by[Opp][NIHT]
                          | ei.pin_attacked_by[Opp][BSHP]
                          | ei.pin_attacked_by[Opp][ROOK]);
                }

                auto mob = pop_count (mobility_area & attacks);
                mobility += PieceMobility[PT][mob];

                // Special extra evaluation for pieces
                if (   PT == NIHT
                    || PT == BSHP)
                {
                    // Bonus for minors when behind a pawn
                    if (   rel_rank (Own, s) < R_5
                        && contains (pos.pieces (PAWN), (s+Push)))
                    {
                        score += MinorBehindPawn;
                    }

                    Bitboard b = OutpostRank[Own] & ~ei.pe->attack_span[Opp];
                    // Bonus for minors outpost squares
                    if (contains (b, s))
                    {
                        score +=
                            PT == NIHT ?
                                KnightOutpost[contains (ei.pin_attacked_by[Own][PAWN], s) ? 1 : 0] :
                                BishopOutpost[contains (ei.pin_attacked_by[Own][PAWN], s) ? 1 : 0];
                    }
                    else
                    {
                        b &= attacks & ~pos.pieces (Own);
                        if (b != 0)
                        {
                            score +=
                                PT == NIHT ?
                                    KnightReachableOutpost[(ei.pin_attacked_by[Own][PAWN] & b) != 0 ? 1 : 0] :
                                    BishopReachableOutpost[(ei.pin_attacked_by[Own][PAWN] & b) != 0 ? 1 : 0];
                        }
                    }
                    
                    if (PT == BSHP)
                    {
                        // Penalty for pawns on the same color square as the bishop
                        score -= BishopPawns * i32(ei.pe->color_count[Own][color (s)]);
                        
                        if (   mob <= 2
                            && contains (FA_bb|FH_bb,  s)
                            && rel_rank (Own, s) >= R_4)
                        {
                            auto del = (F_A == _file (s) ? DEL_E : DEL_W)-Push;
                            if (pos[s+del] == (Own|PAWN))
                            {
                                score -= BishopTrapped * (pos.empty (s+del+Push) ? 1 : 2);
                            }
                        }

                        if (Position::Chess960)
                        {
                            // An important Chess960 pattern: A cornered bishop blocked by a friend pawn diagonally in front of it.
                            // It is a very serious problem, especially when that pawn is also blocked.
                            // Bishop on a1/h1 or a8/h8 (white or black) which is trapped by own pawn on b2/g2 or b7/g7 (white or black).
                            if (   contains (FA_bb|FH_bb, s)
                                && rel_rank (Own, s) == R_1)
                            {
                                auto del = (F_A == _file (s) ? DEL_E : DEL_W)+Push;
                                if (pos[s+del] == (Own|PAWN))
                                {
                                    score -= BishopTrapped * (pos.empty (s+del+Push) ?
                                                                  pos[s+del+del] != (Own|PAWN) ?
                                                                      1 : 2 : 4);
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
                    // Bonus for rook when on an open or semi-open (undefended/defended) file
                    if (ei.pe->file_semiopen (Own, _file (s)))
                    {
                        score += RookOnFile[ei.pe->file_semiopen (Opp, _file (s)) ? 1 : 0];
                    }
                    else
                    {
                        auto kf = _file (pos.square (Own, KING));
                        // Penalty for rook when trapped by the king, even more if the king can't castle
                        if (   mob <= 3
                            && ((kf < F_E) == (_file (s) < kf))
                            && (front_sqrs_bb (Own, s) & pos.pieces (Own, PAWN)) != 0
                            && !ei.pe->side_semiopen (Own, kf, kf < F_E))
                        {
                            score -= (RookTrapped - mk_score (22 * mob, 0)) * (pos.can_castle (Own) ? 1 : 2);
                        }
                    }
                }
                else
                if (PT == QUEN)
                {
                    // Penalty for pin or discover attack on the queen
                    Bitboard pinners = 0, discovers = 0;
                    if ((pos.slider_blockers<Own> (s, pos.pieces (Opp, QUEN), pinners, discovers) & ~(  (pos.pieces (Opp, PAWN) & file_bb (s) & ~(  shift<LCap> (pos.pieces (Own))
                                                                                                                                                  | shift<RCap> (pos.pieces (Own))))
                                                                                                      | pos.abs_blockers (Opp))) != 0)
                    {
                        score -= QueenWeaken;
                    }
                }
            }

            if (Trace)
            {
                write (PT, Own, score);
            }

            return score;
        }

        // Evaluates bonuses and penalties of the king of the given color.
        template<Color Own, bool Trace>
        Score evaluate_king (const Position &pos, const EvalInfo &ei)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N : DEL_S;
            static const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
            static const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;

            auto fk_sq = pos.square (Own, KING);

            // King Safety: friend pawns shelter and enemy pawns storm
            auto index = ei.pe->do_king_safety<Own> (pos, fk_sq);
            auto value = ei.pe->king_safety[Own][index];
            if (   rel_rank (Own, fk_sq) == R_1
                && pos.can_castle (Own) != CR_NONE)
            {
                if (   index != 0
                    && pos.can_castle (Castling<Own, CS_KING>::Right) != CR_NONE
                    && (pos.king_path[Castling<Own, CS_KING>::Right] & ei.ful_attacked_by[Opp]) == 0
                    && (pos.castle_path[Castling<Own, CS_KING>::Right] & pos.pieces ()) == 0)
                {
                    if (value < ei.pe->king_safety[Own][0])
                    {
                        value = ei.pe->king_safety[Own][0];
                    }
                }
                if (   index != 1
                    && pos.can_castle (Castling<Own, CS_QUEN>::Right) != CR_NONE
                    && (pos.king_path[Castling<Own, CS_QUEN>::Right] & ei.ful_attacked_by[Opp]) == 0
                    && (pos.castle_path[Castling<Own, CS_QUEN>::Right] & pos.pieces ()) == 0)
                {
                    if (value < ei.pe->king_safety[Own][1])
                    {
                        value = ei.pe->king_safety[Own][1];
                    }
                }
            }

            auto score = mk_score (value, -16 * ei.pe->king_pawn_dist[Own][index]);

            Bitboard b;
            // Main king safety evaluation
            if (ei.king_ring_attackers_count[Opp] != 0)
            {
                Bitboard non_opp = ~pos.pieces (Opp);
                // Find the attacked squares which are defended only by the king in the king zone...
                Bitboard king_zone_undef =
                       ei.pin_attacked_by[Own][KING]
                    &  ei.pin_attacked_by[Opp][NONE]
                    & ~ei.dbl_attacked[Own];
                // ... and those which are not defended at all in the king ring.
                Bitboard king_ring_undef =
                       ei.king_ring[Own]
                    &  non_opp
                    &  ei.pin_attacked_by[Opp][NONE]
                    & ~ei.pin_attacked_by[Own][NONE];
                // Initialize the king danger, which will be transformed later into a king danger score.
                // The initial value is based on the
                // - the number and types of the enemy's attacking pieces,
                // - the number of attacked and undefended squares around our king,
                // - the quality of the pawn shelter ('mg score' value).
                i32 king_danger =
                      std::min (ei.king_ring_attackers_count[Opp]*ei.king_ring_attackers_weight[Opp], 807)
                    + 101 * ei.king_zone_attacks_count[Opp]
                    + 235 * pop_count (king_zone_undef)
                    + 134 * (  pop_count (king_ring_undef)
                             + (pos.abs_blockers (Own) != 0 ? 1 : 0))
                    + 134 * ((pos.dsc_blockers (Opp) & ~(  (pos.pieces (Opp, PAWN) & file_bb (fk_sq) & ~(  shift<LCap> (pos.pieces (Own))
                                                                                                         | shift<RCap> (pos.pieces (Own))))
                                                         | pos.abs_blockers (Opp))) != 0 ? 1 : 0)
                    - 717 * (pos.count<QUEN>(Opp) == 0)
                    -   7 * i32(value) / 5
                    -   5;

                // Analyze enemy's queen safe contact checks.
                // Undefended squares around the king not occupied by enemy's and
                // attacked by enemy queen and keep squares supported by another enemy piece.
                b =    king_zone_undef
                    &  non_opp
                    &  ei.pin_attacked_by[Opp][QUEN]
                    &  ei.dbl_attacked[Opp];
                king_danger += QueenContactCheck * pop_count (b);

                Bitboard rook_attack = attacks_bb<ROOK> (fk_sq, pos.pieces ());
                Bitboard bshp_attack = attacks_bb<BSHP> (fk_sq, pos.pieces ());

                // Analyse the safe enemy's checks which are possible on safe area ...
                Bitboard safe_area =
                       non_opp
                    & ~ei.pin_attacked_by[Own][NONE];
                // ... and probable potential checks, only requiring the square to be 
                //  safe from pawn-attacks and not being occupied by a blocked pawns.
                Bitboard prob_area =
                      ~(  ei.pin_attacked_by[Own][PAWN]
                        | (pos.pieces (Opp, PAWN) & shift<Push> (pos.pieces (PAWN))));

                // Enemy queens safe checks
                b =    (rook_attack | bshp_attack)
                    &  ei.pin_attacked_by[Opp][QUEN];
                if ((b & safe_area) != 0)
                {
                    score -= SafeChecked;
                    king_danger += PieceSafeChecks[QUEN];
                }

                // Attacked twice and only defended by a queen.
                safe_area |=
                       non_opp
                    & ~ei.dbl_attacked[Own]
                    &  ei.dbl_attacked[Opp]
                    &  ei.pin_attacked_by[Own][QUEN];

                // Enemy rooks safe and other checks
                b =    rook_attack
                    &  ei.pin_attacked_by[Opp][ROOK];
                if ((b & safe_area) != 0)
                {
                    score -= SafeChecked;
                    king_danger += PieceSafeChecks[ROOK];
                }
                else
                if ((b & prob_area) != 0)
                {
                    score -= ProbChecked;
                }
                // Enemy bishops safe and other checks
                b =    bshp_attack
                    &  ei.pin_attacked_by[Opp][BSHP];
                if ((b & safe_area) != 0)
                {
                    score -= SafeChecked;
                    king_danger += PieceSafeChecks[BSHP];
                }
                else
                if ((b & prob_area) != 0)
                {
                    score -= ProbChecked;
                }
                // Enemy knights safe and other checks
                b =    PieceAttacks[NIHT][fk_sq]
                    &  ei.pin_attacked_by[Opp][NIHT];
                if ((b & safe_area) != 0)
                {
                    score -= SafeChecked;
                    king_danger += PieceSafeChecks[NIHT];
                }
                else
                if ((b & prob_area) != 0)
                {
                    score -= ProbChecked;
                }

                // Compute the king danger score and subtract it from the evaluation
                if (king_danger > 0)
                {
                    score -= mk_score (std::min (king_danger*king_danger / 0x1000, 2*i32(VALUE_MG_BSHP)), 0);
                }
            }

            // King tropism: Find squares that enemy attacks in the friend king flank
            auto kf = _file (fk_sq);
            b =    KingFlank[Own][kf]
                &  ei.pin_attacked_by[Opp][NONE];
            assert(((Own == WHITE ? b << 4 : b >> 4) & b) == 0);
            assert(pop_count (Own == WHITE ? b << 4 : b >> 4) == pop_count (b));
            // Add the squares which are attacked twice in that flank and are not protected by a friend pawn.
            b =   (Own == WHITE ? b << 4 : b >> 4)
                | (   b
                   &  ei.dbl_attacked[Opp]
                   & ~ei.pin_attacked_by[Own][PAWN]);
            score -= EnemyInFlank * pop_count (b);

            // Penalty when our king is on a pawnless flank
            if (((KingFlank[WHITE][kf]|KingFlank[BLACK][kf]) & pos.pieces (PAWN)) == 0)
            {
                score -= PawnlessFlank;
            }

            if (Trace)
            {
                write (KING, Own, score);
            }

            return score;
        }

        // Evaluates the threats of the given color.
        template<Color Own, bool Trace>
        Score evaluate_threats (const Position &pos, const EvalInfo &ei)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N  : DEL_S;
            static const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
            static const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;

            static const Bitboard Rank2BB = Own == WHITE ? R2_bb : R7_bb;
            static const Bitboard Rank7BB = Own == WHITE ? R7_bb : R2_bb;

            enum { MINOR, MAJOR };

            auto score = SCORE_ZERO;

            Bitboard b;
            // Enemy non-pawns
            Bitboard nonpawns =
                  pos.pieces (Opp)
                ^ pos.pieces (Opp, PAWN);

            // Enemy not defended by a pawn and attacked by any friend piece
            Bitboard weak_pieces =
                   pos.pieces (Opp)
                & ~ei.pin_attacked_by[Opp][PAWN]
                &  ei.pin_attacked_by[Own][NONE];

            // Add a bonus according to the kind of attacking pieces

            // Enemies attacked by minor pieces
            b =   (  weak_pieces
                    // Rooks or Queens
                   | pos.pieces (Opp, ROOK, QUEN)
                    // Enemy non-pawn defended by a pawn
                   | (  nonpawns
                      & ei.pin_attacked_by[Opp][PAWN]))
                & (  ei.pin_attacked_by[Own][NIHT]
                   | ei.pin_attacked_by[Own][BSHP]);
            while (b != 0)
            {
                auto s = pop_lsq (b);
                auto pt = ptype (pos[s]);
                score += PieceThreat[MINOR][pt];
                if (pt != PAWN)
                {
                    score += PieceThreatRank * rel_rank (Opp, s);
                }
            }
            // Enemies attacked by rooks
            b =   (  weak_pieces
                    // Queens
                   | pos.pieces (Opp, QUEN))
                & (ei.pin_attacked_by[Own][ROOK]);
            while (b != 0)
            {
                auto s = pop_lsq (b);
                auto pt = ptype (pos[s]);
                score += PieceThreat[MAJOR][pt];
                if (pt != PAWN)
                {
                    score += PieceThreatRank * rel_rank (Opp, s);
                }
            }
            // Enemies attacked by king
            b =    weak_pieces
                &  ei.pin_attacked_by[Own][KING];
            if (b != 0)
            {
                score += KingThreat[more_than_one (b) ? 1 : 0];
            }
            // Enemies attacked by friend are hanging
            b =    weak_pieces
                & ~ei.pin_attacked_by[Opp][NONE];
            score += PieceHanged * pop_count (b);

            // Loose enemies (except Queen and King)
            b =    (  pos.pieces (Opp)
                    ^ pos.pieces (Opp, QUEN, KING))
                & ~(  ei.pin_attacked_by[Own][NONE]
                    | ei.pin_attacked_by[Opp][NONE]);
            if (b != 0)
            {
                score += PieceLoosed;
            }

            Bitboard safe =
                  ~ei.pin_attacked_by[Opp][NONE]
                |  ei.pin_attacked_by[Own][NONE];

            // Enemy non-pawns attacked by any friend pawn
            Bitboard weak_nonpawns =
                   nonpawns
                &  ei.pin_attacked_by[Own][PAWN];
            if (weak_nonpawns != 0)
            {
                // Safe friend pawns
                b =   safe
                    & pos.pieces (Own, PAWN);
                // Enemy non-pawns attacked by safe friend pawns
                b =   (  shift<LCap> (b)
                       | shift<RCap> (b))
                    & weak_nonpawns;
                // Enemy non-pawns attacked by unsafe friend pawns
                if ((weak_nonpawns ^ b) != 0)
                {
                    score += HangPawnThreat;
                }
                while (b != 0)
                {
                    score += SafePawnThreat[ptype (pos[pop_lsq (b)])];
                }
            }

            // Bonus if some friend pawns safely push can attack an enemy piece
            b =    pos.pieces (Own, PAWN)
                & ~(  Rank7BB
                    | pos.abs_blockers (Own));
            // Friend pawns push
            b =   shift<Push> (b | (  shift<Push> (b & Rank2BB)
                                    & ~pos.pieces ()))
                & ~pos.pieces ();
            // Friend pawns safe push
            b &=   safe
                & ~ei.pin_attacked_by[Opp][PAWN];
            // Friend pawns safe push attacks an enemy piece not already attacked by pawn
            b =    (  shift<LCap> (b)
                    | shift<RCap> (b))
                &  pos.pieces (Opp)
                & ~ei.pin_attacked_by[Own][PAWN];
            score += PawnPushThreat * pop_count (b);

            if (Trace)
            {
                write (THREAT, Own, score);
            }

            return score;
        }

        // Evaluates the passed pawns of the given color.
        template<Color Own, bool Trace>
        Score evaluate_passed_pawns (const Position &pos, const EvalInfo &ei)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N : DEL_S;

            auto score = SCORE_ZERO;

            Bitboard passers = ei.pe->passers[Own];
            while (passers != 0)
            {
                auto s = pop_lsq (passers);
                assert(pos.pawn_passed_at (Own, s));
                assert((front_sqrs_bb (Own, s) & pos.pieces (Own, PAWN)) == 0);

                auto rank = rel_rank (Own, s);
                // Base bonus depending on rank.
                auto mg_value = PawnPassRank[rank];
                auto eg_value = PawnPassRank[rank];

                score -= PawnPassHinder * pop_count (front_sqrs_bb (Own, s) & (ei.pin_attacked_by[Opp][NONE] | pos.pieces (Opp)));

                auto r  = dist (rank, R_2);
                auto rr = r*(r-1);

                if (rr != 0)
                {
                    auto push_sq = s+Push;

                    // Adjust bonus based on kings proximity.
                    eg_value +=
                          5*rr*dist (pos.square (Opp, KING), push_sq)
                        - 2*rr*dist (pos.square (Own, KING), push_sq);
                    // If block square is not the queening square then consider also a second push.
                    if (rel_rank (Own, push_sq) != R_8)
                    {
                        eg_value +=
                            - 1*rr*dist (pos.square (Own, KING), push_sq+Push);
                    }

                    // If the pawn is free to advance.
                    if (pos.empty (push_sq))
                    {
                        // Squares to queen
                        Bitboard front_squares = front_sqrs_bb (Own, s);
                        Bitboard safe_front_squares = front_squares
                            ,  unsafe_front_squares = front_squares;
                        // If there is a rook or queen attacking/defending the pawn from behind, consider front squares.
                        // Otherwise consider only the squares in the pawn's path attacked or occupied by the enemy.
                        Bitboard behind_majors = pos.pieces (ROOK, QUEN) & front_sqrs_bb (Opp, s);
                        if (behind_majors != 0)
                        {
                            behind_majors &= attacks_bb<ROOK> (s, pos.pieces ());
                        }
                        // If there is an enemy rook or queen attacking the pawn from behind, add all X-ray attacks by the rook or queen.
                        // Otherwise consider only the squares in the pawn's path attacked or occupied by the enemy.
                        if ((behind_majors & pos.pieces (Opp)) == 0)
                        {
                            unsafe_front_squares &= ei.pin_attacked_by[Opp][NONE] | pos.pieces (Opp);
                        }
                        if ((behind_majors & pos.pieces (Own)) == 0)
                        {
                            safe_front_squares   &= ei.pin_attacked_by[Own][NONE];
                        }

                        // Give a big bonus if the path to the queen is not attacked,
                        // a smaller bonus if the block square is not attacked.
                        i32 k = unsafe_front_squares != 0 ?
                                contains (unsafe_front_squares, push_sq) ?
                                    0 : 8 : 18;
                        // Give a big bonus if the path to the queen is fully defended,
                        // a smaller bonus if the block square is defended.
                        k += safe_front_squares != front_squares ?
                                !contains (safe_front_squares, push_sq) ?
                                    0 : 4 : 6;

                        mg_value += k*rr;
                        eg_value += k*rr;
                    }
                    else
                    // If the pawn is blocked by own pieces.
                    if (contains (pos.pieces (Own), push_sq))
                    {
                        mg_value += 1*rr + 2*r;
                        eg_value += 1*rr + 2*r;
                    }
                }

                // Assign a small bonus when no pieces left (unstoppable)
                if (   pos.si->non_pawn_matl[WHITE] == VALUE_ZERO
                    && pos.si->non_pawn_matl[BLACK] == VALUE_ZERO)
                {
                    eg_value += 20;
                }

                score += mk_score (mg_value, eg_value) + PawnPassFile[std::min (_file (s), F_H - _file (s))];
            }

            if (Trace)
            {
                write (PASSED_PAWN, Own, score);
            }

            return score;
        }

        // Computes the space evaluation of the given color.
        // The space evaluation is a simple bonus based on the number of safe squares
        // available for minor pieces on the central four files on ranks 2--4.
        // Safe squares one, two or three squares behind a friend pawn are counted twice.
        // The aim is to improve play on game opening.
        template<Color Own, bool Trace>
        Score evaluate_space_activity (const Position &pos, const EvalInfo &ei)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Pull = Own == WHITE ? DEL_S : DEL_N;
            static const auto Dull = Own == WHITE ? DEL_SS : DEL_NN;

            // Find the safe squares for our pieces inside the area defined by SpaceArea.
            // A square is safe:
            // - it is not occupied by friend pawns
            // - it is not attacked by an enemy pawns
            // - it is defended or not attacked by an enemy pieces.
            Bitboard safe_space =
                   SpaceArea[Own]
                & ~pos.pieces (Own, PAWN)
                & ~ei.pin_attacked_by[Opp][PAWN]
                & (   ei.pin_attacked_by[Own][NONE]
                   | ~ei.pin_attacked_by[Opp][NONE]);

            // Since SpaceArea[Own] is fully on our half of the board
            assert((Own == WHITE ? safe_space & U64(0xFFFFFFFF00000000)
                                 : safe_space & U64(0x00000000FFFFFFFF)) == 0);

            // Find all squares which are at most three squares behind some friend pawn
            Bitboard behind = pos.pieces (Own, PAWN);
            behind |= shift<Pull> (behind);
            behind |= shift<Dull> (behind);
            i32 count = std::min (pop_count (  (behind & safe_space)
                                             | (Own == WHITE ? safe_space << 32 : safe_space >> 32)), 16);
            i32 weight = pos.count<NONE> (Own) - 2 * ei.pe->open_count;
            auto score = mk_score (count * weight * weight / 18, 0);

            if (Trace)
            {
                write (SPACE_ACTIVITY, Own, score);
            }

            return score;
        }

        // Computes the initiative correction value for the position
        // i.e. second order bonus/malus based on the known attacking/defending status of the players.
        Score evaluate_initiative (const Position &pos, u08 asymmetry, Value eg)
        {
            i32 king_dist = dist<File> (pos.square (WHITE, KING), pos.square (BLACK, KING))
                          - dist<Rank> (pos.square (WHITE, KING), pos.square (BLACK, KING));
            // Compute the initiative bonus for the attacking side
            i32 initiative = 8 * (king_dist + asymmetry) + 12 * pos.count<PAWN> () - 120;
            // Now apply the bonus: note that we find the attacking side by extracting
            // the sign of the endgame value, and that we carefully cap the bonus so
            // that the endgame score will never be divided by more than two.
            return mk_score (0, sign (eg) * std::max (initiative, -abs (eg / 2)));
        }

        // Computes the scale for the position
        Scale evaluate_scale (const Position &pos, const EvalInfo &ei, Value eg)
        {
            assert(PHASE_ENDGAME <= ei.me->phase && ei.me->phase <= PHASE_MIDGAME);

            auto strong_color = eg >= VALUE_ZERO ? WHITE : BLACK;
            Scale scale;
            if (   ei.me->scale_func[strong_color] == nullptr
                || (scale = (*ei.me->scale_func[strong_color])(pos)) == SCALE_NONE)
            {
                scale = ei.me->scale[strong_color];
            }
            assert(scale != SCALE_NONE);

            // If don't already have an unusual scale, check for certain types of endgames.
            if (   ei.me->phase < PHASE_MIDGAME
                && (   scale == SCALE_NORMAL
                    || scale == SCALE_ONEPAWN))
            {
                if (pos.opposite_bishops ())
                {
                    return
                        // Endgame with opposite-colored bishops and no other pieces (ignoring pawns)
                           pos.si->non_pawn_matl[WHITE] == VALUE_MG_BSHP
                        && pos.si->non_pawn_matl[BLACK] == VALUE_MG_BSHP ?
                               pos.count<PAWN> () <= 1 ?
                                Scale( 9) :
                                Scale(31) :
                        // Endgame with opposite-colored bishops but also other pieces
                        // is still a bit drawish, but not as drawish as with only the two bishops. 
                            Scale(46);
                }
                else
                // Endings where weaker side can place his king in front of the strong side pawns are drawish.
                if (   abs (eg) <= VALUE_EG_BSHP
                    && pos.count<PAWN> (strong_color) <= 2
                    && !pos.pawn_passed_at (~strong_color, pos.square (~strong_color, KING)))
                {
                    return Scale(37 + 7 * pos.count<PAWN> (strong_color));
                }
            }
            return scale;
        }
    }

    // Returns a static evaluation of the position from the point of view of the side to move.
    template<bool Trace>
    Value evaluate (const Position &pos)
    {
        assert(pos.si->checkers == 0);

        // Probe the material hash table
        auto *me = Material::probe (pos);
        // If have a specialized evaluation function for the material configuration
        if (me->value_func != nullptr)
        {
            return (*me->value_func) (pos);
        }
        // Probe the pawn hash table
        auto *pe = Pawns::probe (pos);

        EvalInfo ei (pos, pe, me);

        init_king_ring<WHITE> (pos, ei);
        init_king_ring<BLACK> (pos, ei);

        // Evaluate pieces and mobility
        Score mobility[CLR_NO] =
        {
            SCORE_ZERO,
            SCORE_ZERO
        };
        // Do not include in mobility area
        // - squares protected by enemy pawns or
        // - squares occupied by block pawns (Pawns blocked or on ranks 2-3) or
        // - squares occupied by friend blocked pawns or king
        const Bitboard mobility_area[CLR_NO] =
        {
            ~((ei.pin_attacked_by[BLACK][PAWN] | (pos.pieces (WHITE, PAWN) & (shift<DEL_S> (pos.pieces ()) | PawnFlank[WHITE]))) | pos.square (WHITE, KING)),
            ~((ei.pin_attacked_by[WHITE][PAWN] | (pos.pieces (BLACK, PAWN) & (shift<DEL_N> (pos.pieces ()) | PawnFlank[BLACK]))) | pos.square (BLACK, KING))
        };

        // Score is computed internally from the white point of view, initialize by
        // - the incrementally updated scores (material + piece square tables).
        // - the material imbalance.
        // - the pawn score
        auto score =
              pos.si->psq_score
            + me->imbalance
            + pe->score;

        // Evaluate all pieces except pawns and king
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
        // Evaluate kings, needed full attack information including king
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
        if (   pos.si->non_pawn_matl[WHITE]
             + pos.si->non_pawn_matl[BLACK] >= VALUE_SPACE)
        {
            // Evaluate space activity
            score +=
                + evaluate_space_activity<WHITE, Trace> (pos, ei)
                - evaluate_space_activity<BLACK, Trace> (pos, ei);
        }

        // Evaluate potential for the position
        score += evaluate_initiative (pos, ei.pe->asymmetry, eg_value (score));

        assert(-VALUE_INFINITE < mg_value (score) && mg_value (score) < +VALUE_INFINITE);
        assert(-VALUE_INFINITE < eg_value (score) && eg_value (score) < +VALUE_INFINITE);

        // Interpolates between a midgame and a endgame score, scaled based on game phase.
        auto value = Value((  mg_value (score) * i32(ei.me->phase)
                            + eg_value (score) * i32(PHASE_MIDGAME - ei.me->phase)
                                                // Evaluate scale for the position
                                               * i32(evaluate_scale (pos, ei, eg_value (score)))/SCALE_NORMAL)
                            / PHASE_MIDGAME);

        if (Trace)
        {
            // Write remaining evaluation terms
            write (PAWN     , pe->score);
            write (MATERIAL , pos.si->psq_score);
            write (IMBALANCE, me->imbalance);
            write (MOBILITY , mobility[WHITE], mobility[BLACK]);
            write (TOTAL    , score);
        }

        return (pos.active == WHITE ? +value : -value) + Tempo;
    }
    // Explicit template instantiations
    template Value evaluate<false> (const Position&);
    template Value evaluate<true > (const Position&);

    // Returns a string (suitable to be print on stdout) that contains the detailed descriptions.
    string trace (const Position &pos)
    {
        std::memset (cp, 0x00, sizeof (cp));
        auto value = evaluate<true> (pos)*(pos.active == WHITE ? +1 : -1); // White's point of view

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
}
