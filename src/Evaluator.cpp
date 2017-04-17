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
                PASSER,
                SPACE,
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

        // Struct EvalInfo contains various information computed by the evaluation functions
        struct EvalInfo
        {
        private:
            template<Color Own>
            void initialize (const Position &pos)
            {
                static const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
                static const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;

                auto fk_sq = pos.square (Own, KING);

                Bitboard pinned_pawns = pos.abs_blockers (Own) & pos.pieces (Own, PAWN);
                if (0 != pinned_pawns)
                {
                    Bitboard loosed_pawns = pos.pieces (Own, PAWN) & ~pinned_pawns;
                    pin_attacked_by[Own][PAWN] = (  shift<LCap> (loosed_pawns)
                                                  | shift<RCap> (loosed_pawns))
                                               | (  (  shift<LCap> (pinned_pawns)
                                                     | shift<RCap> (pinned_pawns))
                                                  & PieceAttacks[BSHP][fk_sq]);
                }
                else
                {
                    pin_attacked_by[Own][PAWN] = pe->any_attacks[Own];
                }

                pin_attacked_by[Own][KING] = PieceAttacks[KING][fk_sq];

                ful_attacked_by[Own]       = pin_attacked_by[Own][KING] | pe->any_attacks[Own];
                pin_attacked_by[Own][NONE] = pin_attacked_by[Own][KING] | pin_attacked_by[Own][PAWN];
                dbl_attacked[Own]          = pe->dbl_attacks[Own]
                                           | (pin_attacked_by[Own][KING] & pin_attacked_by[Own][PAWN]);

                king_ring[Own] = 0;
                king_ring_attackers_count[Own] = 0;
                king_ring_attackers_weight[Own] = 0;
                king_zone_attacks_count[Own] = 0;
            }

        public:
            // Pointers to pawn and material hash table entries
            Pawns   ::Entry *const &pe = nullptr;
            Material::Entry *const &me = nullptr;

            Bitboard mobility_area[CLR_NO];

            // Contains all squares attacked by the color and piece type.
            Bitboard ful_attacked_by[CLR_NO];
            // Contains all squares attacked by the color and piece type with pinned removed.
            Bitboard pin_attacked_by[CLR_NO][MAX_PTYPE];
            // Squares attacked by more than one pieces of a color, possibly via x-ray or by one pawn and one piece.
            // Diagonal x-ray through pawn or squares attacked by 2 pawns are not explicitly added.
            Bitboard dbl_attacked[CLR_NO];

            // Zone around the king which is considered by the king safety evaluation.
            // This consists of the squares directly adjacent to the king, and the three (or two, for a king on an edge file) squares two ranks in front of the king.
            // For instance, if black's king is on g8, king_ring[BLACK] is a bitboard containing the squares f8, h8, f7, g7, h7, f6, g6 and h6.
            Bitboard king_ring[CLR_NO];
            // Number of pieces of the color, which attack a square in the king_ring of the enemy king.
            u08      king_ring_attackers_count[CLR_NO];
            // Sum of the "weight" of the pieces of the color which attack a square in the king_ring of the enemy king.
            // The weights of the individual piece types are given by the PieceKingAttacks[piece-type]
            i32      king_ring_attackers_weight[CLR_NO];
            // Number of attacks by the color to squares directly adjacent to the enemy king.
            // Pieces which attack more than one square are counted multiple times.
            u08      king_zone_attacks_count[CLR_NO];

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
        // indexed by piece type and number of attacked squares in the mobility area
        const Score PieceMobility[][28] =
        {
            {},
            { // Knight
                S(-75,-76), S(-57,-54), S( -9,-28), S( -2,-10), S(  6,  5), S( 14, 12),
                S( 22, 26), S( 29, 29), S( 36, 29)
            },
            { // Bishop
                S(-48,-59), S(-20,-23), S( 16, -3), S( 26, 13), S( 38, 24), S( 51, 42),
                S( 55, 54), S( 63, 57), S( 63, 65), S( 68, 73), S( 81, 78), S( 81, 86),
                S( 91, 88), S( 98, 97)
            },
            { // Rook
                S(-60,-77), S(-26,-20), S(-11, 27), S( -6, 57), S( -3, 69), S( -1, 82),
                S( 10,109), S( 16,121), S( 24,131), S( 25,143), S( 32,155), S( 32,163),
                S( 43,167), S( 48,171), S( 56,173)
            },
            { // Queen
                S(-39,-36), S(-21,-15), S(  3,  8), S(  3, 18), S( 14, 34), S( 22, 54),
                S( 28, 61), S( 41, 73), S( 43, 79), S( 48, 92), S( 56, 94), S( 60,104),
                S( 60,113), S( 66,120), S( 67,123), S( 70,126), S( 71,133), S( 73,136),
                S( 79,140), S( 88,143), S( 88,148), S( 99,166), S(102,170), S(102,175),
                S(106,184), S(109,191), S(113,206), S(116,212)
            }
        };

        // PieceCloseness[piece-type][distance] contains a bonus for piece closeness,
        // indexed by piece type and distance from the sqaure
        const Score PieceCloseness[][8] =
        {
            {},
            { S( 0, 0), S( 7, 9), S( 7, 1), S( 1, 5), S(-10,-4), S( -1,-4), S( -7,-3), S(-16,-10) }, // Knight
            { S( 0, 0), S(11, 8), S(-7,-1), S(-1,-2), S( -1,-7), S(-11,-3), S( -9,-1), S(-16, -1) }, // Bishop
            { S( 0, 0), S(10, 0), S(-2, 2), S(-5, 4), S( -6, 2), S(-14,-3), S( -2,-9), S(-12, -7) }, // Rook
            { S( 0, 0), S( 3,-5), S( 2,-5), S(-4, 0), S( -9,-6), S( -4, 7), S(-13,-7), S(-10, -7) }  // Queen
        };

        // PieceOutpost[supported by pawn] contains bonuses for piece outposts
        // If they can reach an outpost square, bigger if that square is supported by a pawn
        // If the minor piece occupies an outpost square then score is doubled
        const Score PieceOutpost[][2] =
        {
            {},
            { S(22, 6), S(33, 9) },
            { S( 9, 2), S(14, 4) }
        };

        // RookOnFile[semiopen/open] contains bonuses for rooks
        // when there is no friend pawn on the rook file
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

        const Score ProbChecked     = S(10,10);
        // King tropism
        const Score EnemyInFlank    = S( 7, 0);
        const Score PawnlessFlank   = S(20,80);

        // Bonus for each hanged piece
        const Score PieceHanged     = S(48,27);

        const Score PawnPushThreat  = S(38,22);

        const Score HangPawnThreat  = S( 71,  61);
        const Score SafePawnThreat  = S(182, 175);

        // PieceThreat[piece-type][piece-type] contains bonuses according to piece type
        const Score PieceThreat[][NONE] =
        {
            {},
            { S( 0,33), S(45,43), S(46,47), S(47,107), S(48,118), S( 0, 0) },
            { S( 0,33), S(45,43), S(46,47), S(47,107), S(48,118), S( 0, 0) },
            { S( 0,25), S(40,62), S(40,59), S( 0, 34), S(35, 48), S( 0, 0) }
        };

        const Score PieceRankThreat = S(16, 3);

        // KingThreat[one/more] contains bonuses for king attacks on pawns or pieces which are not pawn-defended
        const Score KingThreat[2] = { S( 3, 62), S( 9,138) };

        const Score PawnPassHinder  = S( 7, 0);

        // PawnPassFile[file] contains a bonus for passed pawns according to distance from edge
        const Score PawnPassFile[F_NO/2] = { S( 9, 10), S( 2, 10), S( 1, -8), S(-20,-12) };

    #undef S

    #define V(v) Value(v)

        // PawnPassRank[rank] contains bonuses for passed pawns according to the rank of the pawn
        const Value PawnPassRank[R_NO] = { V(  0), V(  5), V(  5), V( 35), V( 75), V(165), V(255), V(  0) };

        // Threshold for lazy evaluation
        const Value LazyThreshold = V(1500);

    #undef V

        // Bonuses for king attack by piece type
        const i32 PieceKingAttacks[NONE] = {  0, 78, 56, 45, 11,  0 };

        template<Color Own>
        void init_eval (const Position &pos, EvalInfo &ei)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N : DEL_S;
            static const auto Pull = Own == WHITE ? DEL_S : DEL_N;
            static const Bitboard LowRanks = Own == WHITE ?
                                                 R2_bb|R3_bb :
                                                 R7_bb|R6_bb;
            // Do not include in mobility area
            // - squares protected by enemy pawns
            // - squares occupied by block pawns (pawns blocked or on ranks 2-3)
            // - square occupied by friend king
            Bitboard b = ei.pin_attacked_by[Opp][PAWN]
                       | (  pos.pieces (Own, PAWN)
                          & (shift<Pull> (pos.pieces ()) | LowRanks));
            ei.mobility_area[Own] = ~(b | pos.square (Own, KING));
            
            if (pos.si->non_pawn_material (Opp) >= VALUE_MG_QUEN)
            {
                Bitboard king_zone = PieceAttacks[KING][pos.square (Own, KING)];
                ei.king_ring[Own] = king_zone | shift<Push> (king_zone);
                ei.king_ring_attackers_count[Opp] = u08(pop_count (king_zone & ei.pin_attacked_by[Opp][PAWN]));
            }
        }

        // Evaluates bonuses and penalties of the pieces of the color and type
        template<Color Own, PieceType PT, bool Trace>
        Score evaluate_pieces (const Position &pos, EvalInfo &ei, Score &mobility)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N : DEL_S;
            static const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
            static const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;
            // Mask of allowed outpost squares
            static const Bitboard OutpostRank = Own == WHITE ?
                                                    R4_bb|R5_bb|R6_bb :
                                                    R5_bb|R4_bb|R3_bb;

            assert(NIHT <= PT && PT <= QUEN);

            Score score = SCORE_ZERO;
            ei.pin_attacked_by[Own][PT] = 0;

            for (auto s : pos.squares[Own][PT])
            {
                assert(pos[s] == (Own|PT));
                // Find attacked squares, including x-ray attacks for bishops and rooks
                Bitboard attacks;
                switch (PT)
                {
                case NIHT: attacks = PieceAttacks[NIHT][s];                                                                         break;
                case BSHP: attacks = attacks_bb<BSHP> (s, pos.pieces () ^ (pos.pieces (Own, BSHP, QUEN) & ~pos.abs_blockers (Own))); break;
                case ROOK: attacks = attacks_bb<ROOK> (s, pos.pieces () ^ (pos.pieces (Own, ROOK, QUEN) & ~pos.abs_blockers (Own))); break;
                case QUEN: attacks = attacks_bb<QUEN> (s, pos.pieces () ^ (pos.pieces (Own,       QUEN) & ~pos.abs_blockers (Own))); break;
                default:   assert(false);
                           attacks = 0;
                }

                ei.ful_attacked_by[Own] |= attacks;

                if (contains (pos.abs_blockers (Own), s))
                {
                    attacks &= strline_bb (pos.square (Own, KING), s);
                }

                if (QUEN == PT)
                {
                    Bitboard att = attacks & ~pos.abs_blockers (Own);
                    Bitboard qb = pos.pieces (Own, BSHP) & PieceAttacks[BSHP][s] & att;
                    Bitboard qr = pos.pieces (Own, ROOK) & PieceAttacks[ROOK][s] & att;
                    ei.dbl_attacked[Own] |= ei.pin_attacked_by[Own][NONE]
                                          & (  attacks
                                             | (0 != qb ? attacks_bb<BSHP> (s, pos.pieces () ^ qb) : 0)
                                             | (0 != qr ? attacks_bb<ROOK> (s, pos.pieces () ^ qr) : 0));
                }
                else
                {
                    ei.dbl_attacked[Own] |= ei.pin_attacked_by[Own][NONE] & attacks;
                }
                
                ei.pin_attacked_by[Own][NONE] |=
                ei.pin_attacked_by[Own][PT]   |= attacks;

                if (0 != (ei.king_ring[Opp] & attacks))
                {
                    ei.king_ring_attackers_count[Own]++;
                    ei.king_ring_attackers_weight[Own] += PieceKingAttacks[PT];
                    ei.king_zone_attacks_count[Own] += u08(pop_count (ei.pin_attacked_by[Opp][KING] & attacks));
                }

                auto mob = pop_count (ei.mobility_area[Own] & attacks);
                assert(0 <= mob && mob <= 27);
                // Bonus for piece mobility
                mobility += PieceMobility[PT][mob];

                // Bonus for piece closeness to King
                score += PieceCloseness[PT][dist (s, pos.square (Own, KING))];
                //score += PieceCloseness[PT][dist (s, pos.square (Opp, KING))];

                Bitboard b;
                // Special extra evaluation for pieces
                switch (PT)
                {
                case NIHT:
                case BSHP:
                {
                    // Bonus for minors when behind a pawn
                    if (   rel_rank (Own, s) < R_5
                        && contains (pos.pieces (PAWN), s+Push))
                    {
                        score += MinorBehindPawn;
                    }

                    b = OutpostRank
                      & ~ei.pe->attack_span[Opp];
                    // Bonus for minors outpost squares
                    if (contains (b, s))
                    {
                        score += PieceOutpost[PT][contains (ei.pin_attacked_by[Own][PAWN], s) ? 1 : 0] * 2;
                    }
                    else
                    {
                        b &= attacks
                           & ~pos.pieces (Own);
                        if (0 != b)
                        {
                            score += PieceOutpost[PT][0 != (ei.pin_attacked_by[Own][PAWN] & b)] * 1;
                        }
                    }

                    if (BSHP == PT)
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
                            // Bishop (white or black) on a1/h1 or a8/h8 which is trapped by own pawn on b2/g2 or b7/g7.
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
                    break;

                case ROOK:
                {
                    // Bonus for rook aligning with enemy pawns on the same rank/file
                    if (rel_rank (Own, s) > R_4)
                    {
                        score += RookOnPawns * pop_count (pos.pieces (Opp, PAWN) & PieceAttacks[ROOK][s]);
                    }

                    // Bonus for rook when on an open or semi-open file
                    if (ei.pe->file_semiopen (Own, _file (s)))
                    {
                        score += RookOnFile[ei.pe->file_semiopen (Opp, _file (s)) ? 1 : 0];
                    }
                    else
                    if (   mob <= 3
                        && rel_rank (Own, s) < R_5
                        && 0 != (front_sqrs_bb (Own, s) & pos.pieces (Own, PAWN))
                        && 0 == (front_sqrs_bb (Opp, s) & pos.pieces (Own, PAWN)))
                    {
                        // Penalty for rook when trapped by the king, even more if the king can't castle
                        auto kf = _file (pos.square (Own, KING));
                        if (   ((kf < F_E) == (_file (s) < kf))
                            && !ei.pe->side_semiopen (Own, kf, kf < F_E))
                        {
                            score -= (RookTrapped - mk_score (22 * mob, 0)) * (pos.can_castle (Own) ? 1 : 2);
                        }
                    }
                }
                    break;

                case QUEN:
                {
                    // Penalty for pin or discover attack on the queen
                    if (0 != (pos.slider_blockers<Own> (s, pos.pieces (Opp, QUEN), b, b) & ~(  (pos.pieces (Opp, PAWN) & file_bb (s) & ~(  shift<LCap> (pos.pieces (Own))
                                                                                                                                         | shift<RCap> (pos.pieces (Own))))
                                                                                             | pos.abs_blockers (Opp))))
                    {
                        score -= QueenWeaken;
                    }
                }
                    break;
                }
            }

            if (Trace)
            {
                write (PT, Own, score);
            }

            return score;
        }

        // Evaluates bonuses and penalties of the king of the color
        template<Color Own, bool Trace>
        Score evaluate_king (const Position &pos, const EvalInfo &ei)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N : DEL_S;
            //static const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
            //static const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;
            static const Bitboard Camp = Own == WHITE ?
                                             R1_bb|R2_bb|R3_bb|R4_bb|R5_bb :
                                             R8_bb|R7_bb|R6_bb|R5_bb|R4_bb;

            auto fk_sq = pos.square (Own, KING);

            // King Safety: friend pawns shelter and enemy pawns storm
            auto index = ei.pe->do_king_safety<Own> (pos, fk_sq);
            auto value = ei.pe->king_safety[Own][index];
            if (   rel_rank (Own, fk_sq) == R_1
                && pos.can_castle (Own))
            {
                if (   value < ei.pe->king_safety[Own][0]
                    && pos.can_castle (Own, CS_KING)
                    && pos.expeded_castle (Own, CS_KING)
                    && 0 == (pos.king_path[Own][CS_KING] & ei.ful_attacked_by[Opp]))
                {
                    value = ei.pe->king_safety[Own][0];
                }
                if (   value < ei.pe->king_safety[Own][1]
                    && pos.can_castle (Own, CS_QUEN)
                    && pos.expeded_castle (Own, CS_QUEN)
                    && 0 == (pos.king_path[Own][CS_QUEN] & ei.ful_attacked_by[Opp]))
                {
                    value = ei.pe->king_safety[Own][1];
                }
            }

            Score score = mk_score (value, -16 * ei.pe->king_pawn_dist[Own][index]);

            Bitboard b;
            // Main king safety evaluation
            if (0 != ei.king_ring_attackers_count[Opp])
            {
                // Find the attacked squares which are defended only by the king in the king zone...
                Bitboard king_zone_undef =
                       ei.pin_attacked_by[Own][KING]
                    &  ei.pin_attacked_by[Opp][NONE]
                    & ~ei.dbl_attacked[Own];
                // ... and those which are not defended at all in the king ring.
                Bitboard king_ring_undef =
                       ei.king_ring[Own]
                    & ~pos.pieces (Opp)
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
                    + 134 * pop_count (king_ring_undef | pos.abs_blockers (Own))
                    //+ 134 * (0 != (pos.dsc_blockers (Opp) & ~(  (pos.pieces (Opp, PAWN) & file_bb (fk_sq) & ~(  shift<LCap> (pos.pieces (Own))
                    //                                                                                          | shift<RCap> (pos.pieces (Own))))
                    //                                          | pos.abs_blockers (Opp))) ? 1 : 0)
                    - 717 * (0 == pos.count<QUEN>(Opp))
                    -   7 * i32(value) / 5
                    -   5;

                Bitboard rook_attack = attacks_bb<ROOK> (fk_sq, pos.pieces ());
                Bitboard bshp_attack = attacks_bb<BSHP> (fk_sq, pos.pieces ());
                assert(0 == (rook_attack & pos.pieces (Opp, ROOK, QUEN)));
                assert(0 == (bshp_attack & pos.pieces (Opp, BSHP, QUEN)));

                // For safe enemy's checks on the safe square which are possible on next move ...
                Bitboard safe_area =
                      ~pos.pieces (Opp)
                    &  (  ~ei.pin_attacked_by[Own][NONE]
                        |  (  king_zone_undef
                            & ei.dbl_attacked[Opp]));
                // ... and for some other probable potential checks, 
                // the square to be safe from pawn-attacks and not being occupied by a blocked pawns.
                Bitboard prob_area =
                      ~(  ei.pin_attacked_by[Own][PAWN]
                        | (  pos.pieces (Opp, PAWN)
                           & shift<Push> (pos.pieces (PAWN))));

                // Enemy queens safe checks
                b =    (rook_attack | bshp_attack)
                    &  ei.pin_attacked_by[Opp][QUEN];
                if (0 != (b & safe_area))
                {
                    king_danger += 745;
                }

                // For other pieces, the safe square also if attacked twice and only defended by a queen.
                safe_area |=
                       ei.dbl_attacked[Opp]
                    & ~(  ei.dbl_attacked[Own]
                        | pos.pieces (Opp))
                    &  ei.pin_attacked_by[Own][QUEN];

                // Enemy rooks safe and other checks
                b =    rook_attack
                    &  ei.pin_attacked_by[Opp][ROOK];
                if (0 != (b & safe_area))
                {
                    king_danger += 688;
                }
                else
                if (0 != (b & prob_area))
                {
                    score -= ProbChecked;
                }
                // Enemy bishops safe and other checks
                b =    bshp_attack
                    &  ei.pin_attacked_by[Opp][BSHP];
                if (0 != (b & safe_area))
                {
                    king_danger += 588;
                }
                else
                if (0 != (b & prob_area))
                {
                    score -= ProbChecked;
                }
                // Enemy knights safe and other checks
                b =    PieceAttacks[NIHT][fk_sq]
                    &  ei.pin_attacked_by[Opp][NIHT];
                if (0 != (b & safe_area))
                {
                    king_danger += 924;
                }
                else
                if (0 != (b & prob_area))
                {
                    score -= ProbChecked;
                }

                // Transform the king units into a score, and substract it from the evaluation
                if (king_danger > 0)
                {
                    score -= mk_score (king_danger*king_danger / 0x1000, 0);
                }
            }

            // King tropism: Find squares that enemy attacks in the friend king flank
            auto kf = _file (fk_sq);
            b =    Camp
                &  KingFlank[kf]
                &  ei.pin_attacked_by[Opp][NONE];
            assert(0 == ((Own == WHITE ? b << 4 : b >> 4) & b));
            assert(pop_count (Own == WHITE ? b << 4 : b >> 4) == pop_count (b));
            // Add the squares which are attacked twice in that flank and are not protected by a friend pawn.
            b =   (Own == WHITE ? b << 4 : b >> 4)
                | (   b
                   &  ei.dbl_attacked[Opp]
                   & ~ei.pin_attacked_by[Own][PAWN]);
            score -= EnemyInFlank * pop_count (b);

            // Penalty when our king is on a pawnless flank
            if (0 == (KingFlank[kf] & pos.pieces (PAWN)))
            {
                score -= PawnlessFlank;
            }

            if (Trace)
            {
                write (KING, Own, score);
            }

            return score;
        }

        // Evaluates the threats of the color
        template<Color Own, bool Trace>
        Score evaluate_threats (const Position &pos, const EvalInfo &ei)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N  : DEL_S;
            static const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
            static const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;
            static const Bitboard Rank2BB = Own == WHITE ? R2_bb : R7_bb;
            static const Bitboard Rank7BB = Own == WHITE ? R7_bb : R2_bb;

            Score score = SCORE_ZERO;

            Bitboard b;
            
            // Enemy non-pawns
            Bitboard nonpawns =
                  pos.pieces (Opp)
                ^ pos.pieces (Opp, PAWN);

            // Squares defended by the opponent,
            // - attack the square with a pawn
            // - attack the square twice and not defended twice.
            Bitboard defended =
                   ei.pin_attacked_by[Opp][PAWN]
                | (   ei.dbl_attacked[Opp]
                   & ~ei.dbl_attacked[Own]);

            // Enemy not defended and attacked by any friend piece
            Bitboard weak_pieces =
                   pos.pieces (Opp)
                & ~defended
                &  ei.pin_attacked_by[Own][NONE];

            // Add a bonus according to the type of attacking pieces

            // Enemies attacked by knights
            b =   (  weak_pieces
                    // Rooks or Queens
                   | pos.pieces (Opp, ROOK, QUEN)
                    // Enemy defended non-pawns
                   | (  nonpawns
                      & defended))
                & ei.pin_attacked_by[Own][NIHT];
            while (0 != b)
            {
                auto s = pop_lsq (b);
                auto pt = ptype (pos[s]);
                score += PieceThreat[NIHT][pt];
                if (pt != PAWN)
                {
                    score += PieceRankThreat * rel_rank (Opp, s);
                }
            }
            // Enemies attacked by bishops
            b =   (  weak_pieces
                    // Rooks or Queens
                   | pos.pieces (Opp, ROOK, QUEN)
                    // Enemy defended non-pawns
                   | (  nonpawns
                      & defended))
                & ei.pin_attacked_by[Own][BSHP];
            while (0 != b)
            {
                auto s = pop_lsq (b);
                auto pt = ptype (pos[s]);
                score += PieceThreat[BSHP][pt];
                if (pt != PAWN)
                {
                    score += PieceRankThreat * rel_rank (Opp, s);
                }
            }
            // Enemies attacked by rooks
            b =   (  weak_pieces
                    // Queens
                   | pos.pieces (Opp, QUEN))
                & ei.pin_attacked_by[Own][ROOK];
            while (0 != b)
            {
                auto s = pop_lsq (b);
                auto pt = ptype (pos[s]);
                score += PieceThreat[ROOK][pt];
                if (pt != PAWN)
                {
                    score += PieceRankThreat * rel_rank (Opp, s);
                }
            }
            // Enemies attacked by king
            b =    weak_pieces
                &  ei.pin_attacked_by[Own][KING];
            if (0 != b)
            {
                score += KingThreat[more_than_one (b) ? 1 : 0];
            }
            // Enemies attacked by friend are hanging
            b =    weak_pieces
                & ~ei.pin_attacked_by[Opp][NONE];
            score += PieceHanged * pop_count (b);

            Bitboard safe =
                  ~ei.pin_attacked_by[Opp][NONE]
                |  ei.pin_attacked_by[Own][NONE];

            // Enemy non-pawns attacked by any friend pawn
            Bitboard weak_nonpawns =
                   nonpawns
                &  ei.pin_attacked_by[Own][PAWN];
            if (0 != weak_nonpawns)
            {
                // Safe friend pawns
                b =   safe
                    & pos.pieces (Own, PAWN);
                // Enemy non-pawns attacked by safe friend pawns
                b =   (  shift<LCap> (b)
                       | shift<RCap> (b))
                    & weak_nonpawns;
                // Enemy non-pawns attacked by unsafe friend pawns
                if (0 != (weak_nonpawns ^ b))
                {
                    score += HangPawnThreat;
                }

                score += SafePawnThreat * pop_count (b);
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

        // Evaluates the passed pawns of the color
        template<Color Own, bool Trace>
        Score evaluate_passers (const Position &pos, const EvalInfo &ei)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N : DEL_S;

            Score score = SCORE_ZERO;

            Bitboard passers = ei.pe->passers[Own];
            while (0 != passers)
            {
                auto s = pop_lsq (passers);
                assert(0 == (front_sqrs_bb (Own, s) & pos.pieces (Own, PAWN)));

                auto rank = rel_rank (Own, s);
                // Base bonus depending on rank.
                auto mg_value = PawnPassRank[rank];
                auto eg_value = PawnPassRank[rank];

                auto r  = dist (rank, R_2);
                auto rr = r*(r-1);

                if (0 != rr)
                {
                    auto push_sq = s+Push;

                    // Adjust bonus based on kings proximity.
                    if (!contains (pawn_pass_span (Own, s), pos.square (Opp, KING)))
                    {
                        eg_value += 5*rr*dist (pos.square (Opp, KING), push_sq);
                    }
                    eg_value -= 2*rr*dist (pos.square (Own, KING), push_sq);
                    // If block square is not the queening square then consider also a second push.
                    if (rel_rank (Own, push_sq) != R_8)
                    {
                        eg_value -= 1*rr*dist (pos.square (Own, KING), push_sq+Push);
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
                        if (0 != behind_majors)
                        {
                            behind_majors &= attacks_bb<ROOK> (s, pos.pieces ());
                        }
                        // If there is an enemy rook or queen attacking the pawn from behind, add all X-ray attacks by the rook or queen.
                        // Otherwise consider only the squares in the pawn's path attacked or occupied by the enemy.
                        if (0 == (behind_majors & pos.pieces (Opp)))
                        {
                            unsafe_front_squares &= ei.pin_attacked_by[Opp][NONE] | pos.pieces (Opp);
                        }
                        if (0 == (behind_majors & pos.pieces (Own)))
                        {
                            safe_front_squares   &= ei.pin_attacked_by[Own][NONE];
                        }

                        // Give a big bonus if the path to the queen is not attacked,
                        // a smaller bonus if the block square is not attacked.
                        i32 k = 0 != unsafe_front_squares ?
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

                // Scale down bonus for candidate passers which need more than one pawn push to become passed.
                if (!pos.pawn_passed_at (Own, s+Push))
                {
                    mg_value /= 2;
                    eg_value /= 2;
                }

                Bitboard path_attackers = 0;
                Bitboard b = front_sqrs_bb (Own, s);
                while (0 != b)
                {
                    path_attackers |= pos.xattackers_to (pop_lsq (b), Opp, pos.pieces () ^ s) & ~pos.abs_blockers (Opp);
                }

                score += mk_score (mg_value, eg_value)
                       + PawnPassFile[std::min (_file (s), F_H - _file (s))]
                       - PawnPassHinder * pop_count (path_attackers | (front_sqrs_bb (Own, s) & pos.pieces (Opp)));
            }

            if (Trace)
            {
                write (PASSER, Own, score);
            }

            return score;
        }

        // Evaluates the space of the color
        // The space evaluation is a simple bonus based on the number of safe squares
        // available for minor pieces on the central four files on ranks 2-4
        // Safe squares one, two or three squares behind a friend pawn are counted twice
        // The aim is to improve play on opening
        template<Color Own, bool Trace>
        Score evaluate_space (const Position &pos, const EvalInfo &ei)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Pull = Own == WHITE ? DEL_S : DEL_N;
            static const auto Dull = Own == WHITE ? DEL_SS : DEL_NN;
            // SpaceArea contains the area of the board which is considered by the space evaluation.
            // Bonus based on how many squares inside this area are safe.
            static const Bitboard SpaceArea = Side_bb[CS_NO] & (Own == WHITE ?
                                                                    R2_bb|R3_bb|R4_bb :
                                                                    R7_bb|R6_bb|R5_bb);
            // Find the safe squares for our pieces inside the area defined by SpaceArea.
            // A square is safe:
            // - if not occupied by friend pawns
            // - if not attacked by an enemy pawns
            // - if defended or not attacked by an enemy pieces.
            Bitboard safe_space =
                   SpaceArea
                & ~pos.pieces (Own, PAWN)
                & ~ei.pin_attacked_by[Opp][PAWN]
                & (   ei.pin_attacked_by[Own][NONE]
                   | ~ei.pin_attacked_by[Opp][NONE]);

            // Since SpaceArea[Own] is fully on our half of the board
            assert((Own == WHITE ?
                        safe_space & U64(0xFFFFFFFF00000000) :
                        safe_space & U64(0x00000000FFFFFFFF)) == 0);

            // Find all squares which are at most three squares behind some friend pawn
            Bitboard behind = pos.pieces (Own, PAWN);
            behind |= shift<Pull> (behind);
            behind |= shift<Dull> (behind);
            i32 count = std::min (pop_count (  (behind & safe_space)
                                             | (Own == WHITE ?
                                                    safe_space << 32 :
                                                    safe_space >> 32)), 16);
            i32 weight = pos.count<NONE> (Own) - 2 * ei.pe->open_count;
            Score score = mk_score (count * weight * weight / 18, 0);

            if (Trace)
            {
                write (SPACE, Own, score);
            }

            return score;
        }

        // Evaluates the initiative correction value for the position
        // i.e. second order bonus/malus based on the known attacking/defending status of the players
        Score evaluate_initiative (const Position &pos, u08 asymmetry, Value eg)
        {
            i32 king_dist = dist<File> (pos.square (WHITE, KING), pos.square (BLACK, KING))
                          - dist<Rank> (pos.square (WHITE, KING), pos.square (BLACK, KING));

            // Compute the initiative bonus for the attacking side
            i32 initiative =  8 * (king_dist + asymmetry - 17)
                           + 12 * pos.count<PAWN> ()
                             // Pawn on both flanks
                           + 16 * (   0 != (pos.pieces (PAWN) & Side_bb[CS_KING])
                                   && 0 != (pos.pieces (PAWN) & Side_bb[CS_QUEN]) ? 1 : 0);
            // Now apply the bonus: note that we find the attacking side by extracting
            // the sign of the endgame value, and that we carefully cap the bonus so
            // that the endgame score will never change sign after the bonus.
            return mk_score (0, sign (eg) * std::max (initiative, -abs (eg)));
        }

        // Evaluates the scale for the position
        Scale evaluate_scale (const Position &pos, const EvalInfo &ei, Value eg)
        {
            assert(PHASE_ENDGAME <= pos.phase () && pos.phase () <= PHASE_MIDGAME);

            auto strong_color = eg >= VALUE_ZERO ? WHITE : BLACK;
            Scale scale;
            if (   ei.me->scale_func[strong_color] == nullptr
                || (scale = (*ei.me->scale_func[strong_color])(pos)) == SCALE_NONE)
            {
                scale = ei.me->scale[strong_color];
            }
            assert(scale != SCALE_NONE);

            // If don't already have an unusual scale, check for certain types of endgames.
            if (   SCALE_NORMAL == scale
                || SCALE_ONEPAWN == scale)
            {
                if (pos.opposite_bishops ())
                {
                    return
                        // Endgame with opposite-colored bishops and no other pieces (ignoring pawns)
                           VALUE_MG_BSHP == pos.si->non_pawn_material (WHITE) && 1 == pos.count<BSHP> (WHITE)
                        && VALUE_MG_BSHP == pos.si->non_pawn_material (BLACK) && 1 == pos.count<BSHP> (BLACK) ?
                                1 >= pos.count<PAWN> () ?
                                    Scale( 9) :
                                    Scale(31) :
                        // Endgame with opposite-colored bishops but also other pieces
                        // is still a bit drawish, but not as drawish as with only the two bishops. 
                                Scale(46);
                }
                // Endings where weaker side can place his king in front of the strong side pawns are drawish.
                if (   VALUE_EG_BSHP >= abs (eg)
                    && 2 >= pos.count<PAWN> (strong_color)
                    && !pos.pawn_passed_at (~strong_color, pos.square (~strong_color, KING)))
                {
                    return Scale(37 + 7 * pos.count<PAWN> (strong_color));
                }
            }
            return scale;
        }
    }

    // Returns a static evaluation of the position from the point of view of the side to move
    template<bool Trace>
    Value evaluate (const Position &pos)
    {
        assert(0 == pos.si->checkers);

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

        // Score is computed internally from the white point of view, initialize by
        // - the incrementally updated scores (material + piece square tables).
        // - the material imbalance.
        // - the pawn score
        Score score =
              pos.si->psq_score
            + me->imbalance
            + pe->score;

        // Early exit if score is high
        auto v = (mg_value (score) + eg_value (score)) / 2;
        if (abs (v) > LazyThreshold)
        {
            return WHITE == pos.active ? +v : -v;
        }

        init_eval<WHITE> (pos, ei);
        init_eval<BLACK> (pos, ei);

        // Evaluate pieces and mobility
        Score mobility[CLR_NO] =
        {
            SCORE_ZERO,
            SCORE_ZERO
        };

        // Evaluate all pieces except pawns and king
        score +=
            + evaluate_pieces<WHITE, NIHT, Trace> (pos, ei, mobility[WHITE])
            - evaluate_pieces<BLACK, NIHT, Trace> (pos, ei, mobility[BLACK]);
        score +=
            + evaluate_pieces<WHITE, BSHP, Trace> (pos, ei, mobility[WHITE])
            - evaluate_pieces<BLACK, BSHP, Trace> (pos, ei, mobility[BLACK]);
        score +=
            + evaluate_pieces<WHITE, ROOK, Trace> (pos, ei, mobility[WHITE])
            - evaluate_pieces<BLACK, ROOK, Trace> (pos, ei, mobility[BLACK]);
        score +=
            + evaluate_pieces<WHITE, QUEN, Trace> (pos, ei, mobility[WHITE])
            - evaluate_pieces<BLACK, QUEN, Trace> (pos, ei, mobility[BLACK]);
        // Evaluate piece mobility
        score +=
            + mobility[WHITE]
            - mobility[BLACK];
        // Evaluate kings, full attack information needed including king
        score +=
            + evaluate_king<WHITE, Trace> (pos, ei)
            - evaluate_king<BLACK, Trace> (pos, ei);
        // Evaluate threats, full attack information needed including king
        score +=
            + evaluate_threats<WHITE, Trace> (pos, ei)
            - evaluate_threats<BLACK, Trace> (pos, ei);
        // Evaluate passers, full attack information needed including king
        score +=
            + evaluate_passers<WHITE, Trace> (pos, ei)
            - evaluate_passers<BLACK, Trace> (pos, ei);

        // Evaluate space, if in the opening phase
        if (pos.si->non_pawn_material () >= 12222)
        {
            score +=
                + evaluate_space<WHITE, Trace> (pos, ei)
                - evaluate_space<BLACK, Trace> (pos, ei);
        }

        // Evaluate potential for the position
        score += evaluate_initiative (pos, ei.pe->asymmetry, eg_value (score));

        assert(-VALUE_INFINITE < mg_value (score) && mg_value (score) < +VALUE_INFINITE);
        assert(-VALUE_INFINITE < eg_value (score) && eg_value (score) < +VALUE_INFINITE);

        auto phase = pos.phase ();
        // Interpolates between a midgame and a endgame score, scaled based on game phase.
        auto value = Value((  mg_value (score) * i32(phase)
                            + eg_value (score) * i32(PHASE_MIDGAME - phase)
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

        return (WHITE == pos.active ? +value : -value) + Tempo;
    }
    // Explicit template instantiations
    template Value evaluate<false> (const Position&);
    template Value evaluate<true > (const Position&);

    // Returns a string that contains the detailed descriptions
    string trace (const Position &pos)
    {
        std::memset (cp, 0x00, sizeof (cp));
        auto value = evaluate<true> (pos)*(WHITE == pos.active ? +1 : -1); // White's point of view

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
            << "           King" << Term(KING)
            << "         Threat" << Term(THREAT)
            << "    Pawn Passer" << Term(PASSER)
            << "          Space" << Term(SPACE)
            << "----------------+-------------+-------------+--------------\n"
            << "          Total" << Term(TOTAL)
            << "\nEvaluation: " << value_to_cp (value) << " (white side)\n"
            << std::noshowpoint << std::noshowpos;
        return oss.str ();
    }
}
