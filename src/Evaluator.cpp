#include "Evaluator.h"

#include <ostream>
#include "BitBoard.h"
#include "Material.h"
#include "MoveGenerator.h"
#include "Pawns.h"

using namespace std;
using namespace BitBoard;

atomic<Score> Contempt;

namespace {

    class Tracer
    {
    public:

        enum Term : u08
        {
            // The first 6 entries are for PieceType
            MATERIAL = NONE,
            IMBALANCE,
            MOBILITY,
            THREAT,
            PASSER,
            SPACE,
            INITIATIVE,
            TOTAL,
        };

        static double cp[TOTAL + 1][CLR_NO][2];

        static void initialize ()
        {
            std::memset (cp, 0x00, sizeof (cp));
        }

        static void write (Tracer::Term term, Color c, Score score)
        {
            cp[term][c][MG] = value_to_cp (mg_value (score)) / 100.0;
            cp[term][c][EG] = value_to_cp (eg_value (score)) / 100.0;
        }
        static void write (Tracer::Term term, Score wscore, Score bscore = SCORE_ZERO)
        {
            write (term, WHITE, wscore);
            write (term, BLACK, bscore);
        }

    };

    double Tracer::cp[TOTAL + 1][CLR_NO][2];

    ostream& operator<< (ostream &os, Tracer::Term term)
    {
        switch (u08(term))
        {
        case PieceType::PAWN:
        case Tracer::Term::MATERIAL:
        case Tracer::Term::IMBALANCE:
        case Tracer::Term::INITIATIVE:
        case Tracer::Term::TOTAL:
            os << " | ----- ----- | ----- ----- | ";
            break;
        default:
            os << " | "
               << std::setw (5) << Tracer::cp[term][WHITE][MG] << " "
               << std::setw (5) << Tracer::cp[term][WHITE][EG] << " | "
               << std::setw (5) << Tracer::cp[term][BLACK][MG] << " "
               << std::setw (5) << Tracer::cp[term][BLACK][EG] << " | ";
            break;
        }
        os << std::setw (5) << Tracer::cp[term][WHITE][MG] - Tracer::cp[term][BLACK][MG] << " "
           << std::setw (5) << Tracer::cp[term][WHITE][EG] - Tracer::cp[term][BLACK][EG]
           << std::endl;
        return os;
    }

    // Evaluator class contains various evaluation functions.
    template<bool Trace>
    class Evaluator
    {
    private:

    #define V(v) Value(v)
    #define S(mg, eg) mk_score (mg, eg)

        // Bonus for knight behind a pawn
        const Score KnightBehindPawn =  S(16, 0);
        // Bonus for bishop behind a pawn
        const Score BishopBehindPawn =  S(16, 0);
        // Bonus for bishop long range
        const Score BishopOnDiagonal =  S(22, 0);
        // Penalty for bishop with pawns on same color
        const Score BishopPawns =       S( 8,12);
        // Penalty for bishop trapped with pawns (Chess960)
        const Score BishopTrapped =     S(50,50);
        // Bonus for rook on pawns
        const Score RookOnPawns =       S( 8,24);
        // Penalty for rook trapped
        const Score RookTrapped =       S(92, 0);
        // Penalty for queen weaken
        const Score QueenWeaken =       S(50,10);

        // King tropism
        const Score EnemyInFlank =      S( 7, 0);
        const Score PawnlessFlank =     S(20,80);

        const Score PawnWeakUnopposed = S( 5,25);

        // Bonus for each hanged piece
        const Score PieceHanged =       S(52,30);

        const Score SafePawnThreat =    S(175,168);

        const Score PawnPushThreat =    S(47,26);

        const Score PieceRankThreat =   S(16, 3);

        const Score QueenThreat =       S(42,21);

        const Score PawnPassHinder =    S( 8, 1);

        const Value LazyThreshold =     V(1500);
        const Value SpaceThreshold =    V(12222);

    #undef S
    #undef V

        // PieceMobility[piece-type][attacks] contains bonuses for mobility,
        // indexed by piece type and number of attacked squares in the mobility area
        static const Score PieceMobility[5][28];

        // PieceCloseness[piece-type] * "distance to own king" determines a bonus for each piece.
        static const Score PieceCloseness[NONE];

        // Outpost[supported by pawn] contains bonuses for outposts
        // indexed by piece type supported by friend pawns
        static const Score KnightOutpost[2];
        static const Score BishopOutpost[2];

        // RookOnFile[semiopen/open] contains bonuses for rooks
        // when there is no friend pawn on the rook file
        static const Score RookOnFile[2];

        // MinorPieceThreat[piece-type] contains bonus for minor attacks according to piece type
        static const Score MinorPieceThreat[NONE];
        // MajorPieceThreat[piece-type] contains bonus for major attacks according to piece type
        static const Score MajorPieceThreat[NONE];

        // KingThreat[one/more] contains bonus for king attacks on pawns or pieces which are not pawn-defended
        static const Score KingThreat[2];

        // PawnPassFile[file] contains bonus for passed pawns according to distance from edge
        static const Score PawnPassFile[F_NO/2];
        // PawnPassRank[rank] contains bonus for passed pawns according to the rank of the pawn
        static const Value PawnPassRank[2][R_NO];

        static const i32 RankFactor[R_NO];

        // Bonus for king attack by piece type
        static const i32 PieceAttackWeights[NONE];

        const Position &pos;

        Pawns::Entry *pe = nullptr;
        Material::Entry *me = nullptr;

        Bitboard mob_area[CLR_NO];
        Score    mobility[CLR_NO];

        // Contains all squares attacked by the color and piece type.
        Bitboard ful_attacked_by[CLR_NO];
        // Contains all squares attacked by the color and piece type with pinned removed.
        Bitboard pin_attacked_by[CLR_NO][MAX_PTYPE];
        // Contains all squares attacked by more than one pieces of a color, possibly via x-ray or by one pawn and one piece.
        Bitboard dbl_attacked[CLR_NO];

        Bitboard pin_attacked_queen[CLR_NO][2];

        // Zone around the king which is considered by the king safety evaluation.
        // This consists of the squares directly adjacent to the king, and the three (or two, for a king on an edge file) squares two ranks in front of the king.
        // For instance, if black's king is on g8, king_ring[BLACK] is a bitboard containing the squares f8, h8, f7, g7, h7, f6, g6 and h6.
        Bitboard king_ring[CLR_NO];
        // Number of pieces of the color, which attack a square in the king_ring of the enemy king.
        u08 king_ring_attackers_count[CLR_NO];
        // Sum of the "weight" of the pieces of the color which attack a square in the king_ring of the enemy king.
        // The weights of the individual piece types are given by the PieceAttackWeights[piece-type]
        i32 king_ring_attackers_weight[CLR_NO];
        // Number of attacks by the color to squares directly adjacent to the enemy king.
        // Pieces which attack more than one square are counted multiple times.
        u08 king_zone_attacks_count[CLR_NO];

        template<Color>
        void initialize ();
        template<Color, PieceType>
        Score evaluate_pieces ();
        template<Color>
        Score evaluate_king ();
        template<Color>
        Score evaluate_threats ();
        template<Color>
        i32 limit_king_dist (Square);
        template<Color>
        Score evaluate_passers ();
        template<Color>
        Score evaluate_space ();

        Score evaluate_initiative (Value);
        Scale evaluate_scale (Value);

    public:
        Evaluator () = delete;
        Evaluator (const Evaluator&) = delete;
        Evaluator& operator= (const Evaluator&) = delete;

        Evaluator (const Position &p)
            : pos (p)
        {}

        Value value ();
    };

#define V(v) Value(v)
#define S(mg, eg) mk_score (mg, eg)

    template<bool Trace>
    const Score Evaluator<Trace>::PieceMobility[5][28] =
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
    };

    template<bool Trace>
    const Score Evaluator<Trace>::PieceCloseness[NONE] = { S( 0, 0), S(-3,-5), S(-4,-3), S(-3, 0), S(-1,+1) };

    template<bool Trace>
    const Score Evaluator<Trace>::KnightOutpost[2] = { S(22, 6), S(36,12) };
    template<bool Trace>
    const Score Evaluator<Trace>::BishopOutpost[2] = { S( 9, 2), S(15, 5) };

    template<bool Trace>
    const Score Evaluator<Trace>::RookOnFile[2] = { S(20, 7), S(45,20) };

    template<bool Trace>
    const Score Evaluator<Trace>::MinorPieceThreat[NONE] = { S( 0,31), S(39,42), S(57,44), S(68,112), S(47,120), S( 0, 0) };

    template<bool Trace>
    const Score Evaluator<Trace>::MajorPieceThreat[NONE] = { S( 0,24), S(38,71), S(38,61), S( 0, 38), S(36, 38), S( 0, 0) };

    template<bool Trace>
    const Score Evaluator<Trace>::KingThreat[2] = { S( 3, 65), S( 9,145) };

    template<bool Trace>
    const Score Evaluator<Trace>::PawnPassFile[F_NO/2] = { S( 9, 10), S( 2, 10), S( 1, -8), S(-20,-12) };

    template<bool Trace>
    const Value Evaluator<Trace>::PawnPassRank[2][R_NO] =
    {
        { V(0), V(5), V( 5), V(32), V(70), V(172), V(217), V(0) },
        { V(0), V(7), V(13), V(42), V(70), V(170), V(269), V(0) }
    };

    template<bool Trace>
    const i32 Evaluator<Trace>::RankFactor[R_NO] = { 0, 0, 0, 2, 7, 12, 19 };

#undef S
#undef V

    template<bool Trace>
    const i32 Evaluator<Trace>::PieceAttackWeights[NONE] = { 0, 78, 56, 45, 11, 0 };

    /// initialize() computes king and pawn attacks, and the king ring bitboard of the color.
    template<bool Trace>
    template<Color Own>
    void Evaluator<Trace>::initialize ()
    {
        const auto Opp = WHITE == Own ? BLACK : WHITE;

        Bitboard pinned_pawns = pos.abs_blockers (Own) & pos.pieces (Own, PAWN);
        if (0 != pinned_pawns)
        {
            Bitboard loosed_pawns = pos.pieces (Own, PAWN) & ~pinned_pawns;
            pin_attacked_by[Own][PAWN] = (  shift<WHITE == Own ? DEL_NW : DEL_SE> (loosed_pawns)
                                          | shift<WHITE == Own ? DEL_NE : DEL_SW> (loosed_pawns))
                                       | (  (  shift<WHITE == Own ? DEL_NW : DEL_SE> (pinned_pawns)
                                             | shift<WHITE == Own ? DEL_NE : DEL_SW> (pinned_pawns))
                                          & PieceAttacks[BSHP][pos.square<KING> (Own)]);
        }
        else
        {
            pin_attacked_by[Own][PAWN] = pe->any_attacks[Own];
        }

        pin_attacked_by[Own][KING] = PieceAttacks[KING][pos.square<KING> (Own)];

        ful_attacked_by[Own]       = pin_attacked_by[Own][KING]
                                   | pe->any_attacks[Own];
        pin_attacked_by[Own][NONE] = pin_attacked_by[Own][KING]
                                   | pin_attacked_by[Own][PAWN];
        dbl_attacked[Own]          = (  pin_attacked_by[Own][KING]
                                      | pe->dbl_attacks[Own])
                                   & pin_attacked_by[Own][PAWN];

        for (auto pt : { NIHT, BSHP, ROOK, QUEN })
        {
            pin_attacked_by[Own][pt] = 0;
        }
        for (auto x : { 0, 1 })
        {
            pin_attacked_queen[Own][x] = 0;
        }

        // Do not include in mobility area
        // - squares protected by enemy pawns
        // - squares occupied by block pawns (pawns blocked or on ranks 2-3)
        // - square occupied by friend king
        Bitboard b = pin_attacked_by[Own][PAWN]
                   | (  pos.pieces (Opp, PAWN)
                      & (  LowRanks_bb[Opp]
                         | shift<WHITE == Own ? DEL_N : DEL_S> (pos.pieces ())));
        mob_area[Opp] = ~(b | pos.square<KING> (Opp));
        mobility[Opp] = SCORE_ZERO;

        king_ring_attackers_weight[Own] = 0;
        king_zone_attacks_count[Own] = 0;
        if (pos.si->non_pawn_material (Own) >= VALUE_MG_ROOK + VALUE_MG_NIHT)
        {
            b = king_ring[Opp] = PieceAttacks[KING][pos.square<KING> (Opp)];
            if (R_1 == rel_rank (Opp, pos.square<KING> (Opp)))
            {
                king_ring[Opp] |= shift<WHITE == Own ? DEL_S : DEL_N> (b);
            }
            king_ring_attackers_count[Own] = u08(pop_count (b & pin_attacked_by[Own][PAWN]));
        }
        else
        {
            king_ring[Opp] = 0;
            king_ring_attackers_count[Own] = 0;
        }
    }

    /// evaluate_pieces() evaluates the pieces of the color and type.
    template<bool Trace>
    template<Color Own, PieceType PT>
    Score Evaluator<Trace>::evaluate_pieces ()
    {
        static_assert (NIHT == PT
                    || BSHP == PT
                    || ROOK == PT
                    || QUEN == PT, "PT incorrect");

        const auto Opp = WHITE == Own ? BLACK : WHITE;

        auto score = SCORE_ZERO;

        for (auto s : pos.squares[Own][PT])
        {
            assert(pos[s] == (Own|PT));
            Bitboard attacks;
            // Find attacked squares, including x-ray attacks for bishops and rooks
            attacks = NIHT == PT ? PieceAttacks[NIHT][s] :
                      BSHP == PT ? attacks_bb<BSHP> (s, pos.pieces () ^ ((pos.pieces (QUEN) | pos.pieces (Own, BSHP)) & ~pos.abs_blockers (Own))) :
                      ROOK == PT ? attacks_bb<ROOK> (s, pos.pieces () ^ ((pos.pieces (QUEN) | pos.pieces (Own, ROOK)) & ~pos.abs_blockers (Own))) :
                      QUEN == PT ? attacks_bb<QUEN> (s, pos.pieces () ^ ((                    pos.pieces (Own, QUEN)) & ~pos.abs_blockers (Own))) : (assert(false), 0);

            ful_attacked_by[Own] |= attacks;

            if (contains (pos.abs_blockers (Own), s))
            {
                attacks &= strline_bb (pos.square<KING> (Own), s);
            }

            if (BSHP == PT)
            {
                Bitboard att = attacks & ~pos.abs_blockers (Own);
                Bitboard bp = pos.pieces (Own, PAWN) & att & front_rank_bb (Own, s);
                dbl_attacked[Own] |= pin_attacked_by[Own][NONE]
                                   & (  attacks
                                      | (  (  shift<WHITE == Own ? DEL_NW : DEL_SE> (bp)
                                            | shift<WHITE == Own ? DEL_NE : DEL_SW> (bp))
                                         & PieceAttacks[BSHP][s]));
            }
            else
            if (QUEN == PT)
            {
                Bitboard att = attacks & ~pos.abs_blockers (Own);
                Bitboard qp = pos.pieces (Own, PAWN) & att & front_rank_bb (Own, s);
                Bitboard qb = pos.pieces (Own, BSHP) & att & PieceAttacks[BSHP][s];
                Bitboard qr = pos.pieces (Own, ROOK) & att & PieceAttacks[ROOK][s];
                dbl_attacked[Own] |= pin_attacked_by[Own][NONE]
                                    & (  attacks
                                       | (  (  shift<WHITE == Own ? DEL_NW : DEL_SE> (qp)
                                             | shift<WHITE == Own ? DEL_NE : DEL_SW> (qp))
                                          & PieceAttacks[BSHP][s])
                                        | (0 != qb ? attacks_bb<BSHP> (s, pos.pieces () ^ qb) : 0)
                                        | (0 != qr ? attacks_bb<ROOK> (s, pos.pieces () ^ qr) : 0));

                pin_attacked_queen[Own][0] |= attacks & PieceAttacks[BSHP][s];
                pin_attacked_queen[Own][1] |= attacks & PieceAttacks[ROOK][s];
            }
            else
            {
                dbl_attacked[Own] |= pin_attacked_by[Own][NONE]
                                   & attacks;
            }

            pin_attacked_by[Own][NONE] |=
            pin_attacked_by[Own][PT]   |= attacks;

            if (0 != (king_ring[Opp] & attacks))
            {
                king_ring_attackers_count[Own]++;
                king_ring_attackers_weight[Own] += PieceAttackWeights[PT];
                king_zone_attacks_count[Own] += u08(pop_count (pin_attacked_by[Opp][KING] & attacks));
            }

            auto mob = pop_count (mob_area[Own] & attacks);
            assert(0 <= mob && mob <= 27);
            // Bonus for piece mobility
            mobility[Own] += PieceMobility[PT][mob];

            // Bonus for piece closeness to king
            score += PieceCloseness[PT] * dist (s, pos.square<KING> (Own));

            Bitboard b;
            // Special extra evaluation for pieces
            if (NIHT == PT)
            {
                // Bonus for knight behind a pawn
                if (   R_5 > rel_rank (Own, s)
                    && contains (pos.pieces (PAWN), s+pawn_push (Own)))
                {
                    score += KnightBehindPawn;
                }

                b = Outposts_bb[Own]
                  & ~pe->attack_span[Opp];
                // Bonus for knight outpost squares
                if (contains (b, s))
                {
                    score += KnightOutpost[contains (pin_attacked_by[Own][PAWN], s) ? 1 : 0] * 2;
                }
                else
                {
                    b &= attacks
                      & ~pos.pieces (Own);
                    if (0 != b)
                    {
                        score += KnightOutpost[0 != (pin_attacked_by[Own][PAWN] & b) ? 1 : 0] * 1;
                    }
                }
            }
            else
            if (BSHP == PT)
            {
                // Bonus for bishop when behind a pawn
                if (   R_5 > rel_rank (Own, s)
                    && contains (pos.pieces (PAWN), s+pawn_push (Own)))
                {
                    score += BishopBehindPawn;
                }

                b = Outposts_bb[Own]
                  & ~pe->attack_span[Opp];
                // Bonus for bishop outpost squares
                if (contains (b, s))
                {
                    score += BishopOutpost[contains (pin_attacked_by[Own][PAWN], s) ? 1 : 0] * 2;
                }
                else
                {
                    b &= attacks
                      & ~pos.pieces (Own);
                    if (0 != b)
                    {
                        score += BishopOutpost[0 != (pin_attacked_by[Own][PAWN] & b) ? 1 : 0] * 1;
                    }
                }

                // Bonus for bishop on a long diagonal which can "see" both center squares
                if (2 == pop_count (Center_bb & (attacks_bb<BSHP> (s, pos.pieces (PAWN)) | s)))
                {
                    score += BishopOnDiagonal;
                }

                // Penalty for pawns on the same color square as the bishop
                score -= BishopPawns * i32(pe->color_count[Own][color (s)]);

                if (Position::Chess960)
                {
                    // An important Chess960 pattern: A cornered bishop blocked by a friend pawn diagonally in front of it.
                    // It is a very serious problem, especially when that pawn is also blocked.
                    // Bishop (white or black) on a1/h1 or a8/h8 which is trapped by own pawn on b2/g2 or b7/g7.
                    if (   1 >= mob
                        && contains (FA_bb|FH_bb, s)
                        && R_1 == rel_rank (Own, s))
                    {
                        auto del = Delta((F_E - _file (s))/3) + pawn_push (Own);
                        if (contains (pos.pieces (Own, PAWN), s+del))
                        {
                            score -= BishopTrapped * (!contains (pos.pieces (), s+del+pawn_push (Own)) ?
                                                          !contains (pos.pieces (Own, PAWN), s+del+del) ?
                                                              1 : 2 : 4);
                        }
                    }
                }
            }
            else
            if (ROOK == PT)
            {
                // Bonus for rook aligning with enemy pawns on the same rank/file
                if (   R_4 < rel_rank (Own, s)
                    && 0 != (b = pos.pieces (Opp, PAWN) & PieceAttacks[ROOK][s]))
                {
                    score += RookOnPawns * pop_count (b);
                }

                // Bonus for rook when on an open or semi-open file
                if (pe->file_semiopen (Own, _file (s)))
                {
                    score += RookOnFile[pe->file_semiopen (Opp, _file (s)) ? 1 : 0];
                }
                else
                // Penalty for rook when trapped by the king, even more if the king can't castle
                if (   3 >= mob
                    && !contains (pos.abs_blockers (Own), s)
                    && R_5 > rel_rank (Own, s))
                {
                    auto kf = _file (pos.square<KING> (Own));
                    if ((kf < F_E) == (_file (s) < kf))
                    {
                        score -= (RookTrapped - mk_score (22 * mob, 0)) * (pos.si->can_castle (Own) ? 1 : 2);
                    }
                }
            }
            else
            if (QUEN == PT)
            {
                // Penalty for pin or discover attack on the queen
                b = 0;
                if (0 != (  pos.slider_blockers (Own, s, pos.pieces (Opp, QUEN), b, b)
                          & ~(  (  pos.pieces (Opp, PAWN)
                                 & file_bb (s)
                                 & ~(  shift<WHITE == Own ? DEL_NW : DEL_SE> (pos.pieces (Own))
                                     | shift<WHITE == Own ? DEL_NE : DEL_SW> (pos.pieces (Own))))
                              | pos.abs_blockers (Opp))))
                {
                    score -= QueenWeaken;
                }
            }
        }

        if (Trace)
        {
            Tracer::write (Tracer::Term(PT), Own, score);
        }

        return score;
    }

    /// evaluate_king() evaluates the king of the color.
    template<bool Trace>
    template<Color Own>
    Score Evaluator<Trace>::evaluate_king ()
    {
        const auto Opp = WHITE == Own ? BLACK : WHITE;

        auto fk_sq = pos.square<KING> (Own);

        // King Safety: friend pawns shelter and enemy pawns storm
        auto index = pe->king_safety_on<Own> (pos, fk_sq);
        auto value = pe->king_safety[Own][index];
        if (   R_1 == rel_rank (Own, fk_sq)
            && pos.si->can_castle (Own))
        {
            if (   value < pe->king_safety[Own][0]
                && pos.si->can_castle (Own, CS_KING)
                && pos.expeded_castle (Own, CS_KING)
                && 0 == (pos.king_path[Own][CS_KING] & ful_attacked_by[Opp]))
            {
                value = pe->king_safety[Own][0];
            }
            if (   value < pe->king_safety[Own][1]
                && pos.si->can_castle (Own, CS_QUEN)
                && pos.expeded_castle (Own, CS_QUEN)
                && 0 == (pos.king_path[Own][CS_QUEN] & ful_attacked_by[Opp]))
            {
                value = pe->king_safety[Own][1];
            }
        }

        auto score = mk_score (value, -16 * pe->king_pawn_dist[Own][index]);

        Bitboard b;
        // Main king safety evaluation
        if (king_ring_attackers_count[Opp] + pos.count (Opp, QUEN) > 1)
        {
            i32 king_danger = 0;
            Bitboard unsafe_check = 0;

            // Attacked squares defended at most once by our queen or king
            Bitboard weak_area =  pin_attacked_by[Opp][NONE]
                               & ~dbl_attacked[Own]
                               & (   pin_attacked_by[Own][KING]
                                  |  pin_attacked_by[Own][QUEN]
                                  | ~pin_attacked_by[Own][NONE]);

            // Safe squares where enemy's safe checks are possible on next move
            Bitboard safe_area = ~pos.pieces (Opp)
                               & (  ~pin_attacked_by[Own][NONE]
                                  | (  weak_area
                                     & dbl_attacked[Opp]));

            Bitboard rook_attack = attacks_bb<ROOK> (fk_sq, pos.pieces () ^ pos.pieces (Own, QUEN));
            Bitboard bshp_attack = attacks_bb<BSHP> (fk_sq, pos.pieces () ^ pos.pieces (Own, QUEN));

            // Enemy queens safe checks
            b = (  rook_attack
                 | bshp_attack)
              &  pin_attacked_by[Opp][QUEN]
              & ~pin_attacked_by[Own][QUEN];
            if (0 != (b & safe_area))
            {
                king_danger += 780;
            }

            b = rook_attack
              & pin_attacked_by[Opp][ROOK];
            if (0 != (b & safe_area))
            {
                king_danger += 880;
            }
            else
            {
                unsafe_check |= b;
            }

            b = bshp_attack
              & pin_attacked_by[Opp][BSHP];
            if (0 != (b & safe_area))
            {
                king_danger += 435;
            }
            else
            {
                unsafe_check |= b;
            }

            b = PieceAttacks[NIHT][fk_sq]
              & pin_attacked_by[Opp][NIHT];
            if (0 != (b & safe_area))
            {
                king_danger += 790;
            }
            else
            {
                unsafe_check |= b;
            }

            // Unsafe check must be in mobility area.
            unsafe_check &= mob_area[Opp];

            // Initialize the king danger, which will be transformed later into a score.
            // The initial value is based on the
            // - number and types of the enemy's attacking pieces,
            // - number of attacked and undefended squares around our king,
            // - quality of the pawn shelter ('mg score' value).
            king_danger +=  1 * king_ring_attackers_count[Opp]*king_ring_attackers_weight[Opp]
                        + 102 * king_zone_attacks_count[Opp]
                        + 191 * pop_count (king_ring[Own] & weak_area)
                        + 143 * pop_count (pos.abs_blockers (Own) | unsafe_check)
                        - 848 * (0 == pos.count (Opp, QUEN) ? 1 : 0)
                        -   9 * value / 8
                        +  40;

            if (king_danger > 0)
            {
                // Transform the king danger into a score, and subtract it from the score
                king_danger = std::max (king_danger + mg_value (mobility[Opp] - mobility[Own]), 0);
                score -= mk_score (king_danger*king_danger / 0x1000, king_danger / 0x10);
            }
        }

        // King tropism: Find squares that enemy attacks in the friend king flank
        auto kf = _file (fk_sq);
        b = Camp_bb[Own]
          & KingFlank_bb[kf]
          & pin_attacked_by[Opp][NONE];
        assert(0 == ((WHITE == Own ? b << 4 : b >> 4) & b));
        assert(pop_count (WHITE == Own ? b << 4 : b >> 4) == pop_count (b));
        // Add the squares which are attacked twice in that flank and are not protected by a friend pawn.
        b = (   b
             &  dbl_attacked[Opp]
             & ~pin_attacked_by[Own][PAWN])
          | (WHITE == Own ? b << 4 : b >> 4);
        score -= EnemyInFlank * pop_count (b);

        // Penalty for king on a pawnless flank
        if (0 == (KingFlank_bb[kf] & pos.pieces (PAWN)))
        {
            score -= PawnlessFlank;
        }

        if (Trace)
        {
            Tracer::write (Tracer::Term(KING), Own, score);
        }

        return score;
    }

    /// evaluate_threats() evaluates the threats of the color.
    template<bool Trace>
    template<Color Own>
    Score Evaluator<Trace>::evaluate_threats ()
    {
        const auto Opp = WHITE == Own ? BLACK : WHITE;

        auto score = SCORE_ZERO;

        Bitboard b;

        // Bonus for opponent unopposed weak pawns
        if (0 != pos.pieces (Own, ROOK, QUEN))
        {
            score += PawnWeakUnopposed * pop_count (pe->weak_unopposed[Opp]);
        }

        // Enemy non-pawns
        Bitboard nonpawns = pos.pieces (Opp) ^ pos.pieces (Opp, PAWN);
        // Squares defended by the opponent,
        // - attack the square with a pawn
        // - attack the square twice and not defended twice.
        Bitboard defended_area = pin_attacked_by[Opp][PAWN]
                               | (   dbl_attacked[Opp]
                                  & ~dbl_attacked[Own]);
        // Enemy not defended and attacked by any friend piece
        Bitboard weak_pieces =  pos.pieces (Opp)
                             & ~defended_area
                             &  pin_attacked_by[Own][NONE];

        // Add a bonus according to the type of attacking pieces

        // Enemies attacked by minors
        b =  (  weak_pieces
                // Rooks or Queens
              | pos.pieces (Opp, ROOK, QUEN)
                // Enemy defended non-pawns
              | (  nonpawns
                 & defended_area))
          &  (  pin_attacked_by[Own][NIHT]
              | pin_attacked_by[Own][BSHP]);
        while (0 != b)
        {
            auto s = pop_lsq (b);
            auto pt = ptype (pos[s]);
            score += MinorPieceThreat[pt];
            if (PAWN != pt)
            {
                score += PieceRankThreat * rel_rank (Opp, s);
            }
        }
        // Enemies attacked by majors
        b =  (  weak_pieces
                // Queens
              | pos.pieces (Opp, QUEN))
          &  pin_attacked_by[Own][ROOK];
        while (0 != b)
        {
            auto s = pop_lsq (b);
            auto pt = ptype (pos[s]);
            score += MajorPieceThreat[pt];
            if (PAWN != pt)
            {
                score += PieceRankThreat * rel_rank (Opp, s);
            }
        }
        // Enemies attacked by king
        b =  weak_pieces
          &  pin_attacked_by[Own][KING];
        if (0 != b)
        {
            score += KingThreat[more_than_one (b) ? 1 : 0];
        }
        // Enemies attacked by friend are hanging
        b =  weak_pieces
          & ~pin_attacked_by[Opp][NONE];
        score += PieceHanged * pop_count (b);

        Bitboard safe_area =  pin_attacked_by[Own][NONE]
                           | ~pin_attacked_by[Opp][NONE];

        // Enemy non-pawns attacked by any friend pawn
        Bitboard weak_nonpawns = nonpawns
                               & pin_attacked_by[Own][PAWN];
        if (0 != weak_nonpawns)
        {
            // Safe friend pawns
            b = safe_area
              & pos.pieces (Own, PAWN);
            b = (  shift<WHITE == Own ? DEL_NW : DEL_SE> (b)
                 | shift<WHITE == Own ? DEL_NE : DEL_SW> (b))
              & weak_nonpawns;

            score += SafePawnThreat * pop_count (b);
        }

        // Friend pawns can push on the next move
        b =  pos.pieces (Own, PAWN)
          & ~pos.abs_blockers (Own);
        // Friend pawns push
        b =  shift<WHITE == Own ? DEL_N : DEL_S> (b)
          & ~pos.pieces ();
        b |= shift<WHITE == Own ? DEL_N : DEL_S> (b & rank_bb (WHITE == Own ? R_3 : R_6))
          & ~pos.pieces ();
        // Friend pawns push safe
        b &= safe_area
          & ~pin_attacked_by[Opp][PAWN];
        // Friend pawns push safe attacks an enemy piece not already attacked by pawn
        b =  (  shift<WHITE == Own ? DEL_NW : DEL_SE> (b)
              | shift<WHITE == Own ? DEL_NE : DEL_SW> (b))
          &  pos.pieces (Opp)
          & ~pin_attacked_by[Own][PAWN];
        // Bonus for friend pawns push safely can attack an enemy piece not already attacked by pawn
        score += PawnPushThreat * pop_count (b);

        b = (pin_attacked_by[Own][BSHP] & pin_attacked_queen[Opp][0])
          | (pin_attacked_by[Own][ROOK] & pin_attacked_queen[Opp][1]);
        // Bonus for safe slider attack threats on enemy queen
        score += QueenThreat * pop_count (   b
                                          & ~pos.pieces (Own)
                                          &  dbl_attacked[Own]
                                          & ~dbl_attacked[Opp]);

        if (Trace)
        {
            Tracer::write (Tracer::Term::THREAT, Own, score);
        }

        return score;
    }

    // limit_king_dist() returns a limit of the distance that the king of the color has to run to reach square s.
    template<bool Trace>
    template<Color Own>
    i32 Evaluator<Trace>::limit_king_dist (Square s)
    {
        return std::min (dist (pos.square<KING> (Own), s), 5);
    }

    /// evaluate_passers() evaluates the passed pawns of the color.
    template<bool Trace>
    template<Color Own>
    Score Evaluator<Trace>::evaluate_passers ()
    {
        const auto Opp = WHITE == Own ? BLACK : WHITE;

        auto score = SCORE_ZERO;

        Bitboard passers = pe->passers[Own];
        while (0 != passers)
        {
            auto s = pop_lsq (passers);
            assert(0 == (pos.pieces (Own, PAWN) & front_line_bb (Own, s))
                && 0 == (pos.pieces (Opp, PAWN) & front_line_bb (Own, s+pawn_push (Own))));

            i32 r  = rel_rank (Own, s);
            i32 rr = RankFactor[r];

            // Base bonus depending on rank.
            auto mg_value = PawnPassRank[MG][r],
                 eg_value = PawnPassRank[EG][r];

            if (0 != rr)
            {
                auto push_sq = s+pawn_push (Own);

                // Adjust bonus based on the king's proximity
                if (!contains (pawn_pass_span (Own, s), pos.square<KING> (Opp)))
                {
                    eg_value += 5*rr*limit_king_dist<Opp> (push_sq);
                }
                eg_value -= 2*rr*limit_king_dist<Own> (push_sq);

                // If block square is not the queening square then consider also a second push.
                if (R_7 != r)
                {
                    eg_value -= 1*rr*limit_king_dist<Own> (push_sq + pawn_push (Own));
                }

                // If the pawn is free to advance.
                if (pos.empty (push_sq))
                {
                    // Squares to queen
                    Bitboard front_line = front_line_bb (Own, s);
                    Bitboard safe_front_line = front_line
                        ,  unsafe_front_line = front_line;
                    // If there is a rook or queen attacking/defending the pawn from behind, consider front squares.
                    // Otherwise consider only the squares in the pawn's path attacked or occupied by the enemy.
                    Bitboard behind_major = front_line_bb (Opp, s) & pos.pieces (ROOK, QUEN);
                    if (0 != behind_major)
                    {
                        behind_major &= attacks_bb<ROOK> (s, pos.pieces ());
                        assert(1 >= pop_count (behind_major));
                    }
                    Bitboard b;
                    // If there is no enemy rook or queen attacking the pawn from behind,
                    // consider only the squares in the pawn's path attacked or occupied by the enemy,
                    // Otherwise add all X-ray attacks by the enemy rook or queen.
                    if (   0 == (b = (behind_major & pos.pieces (Opp)))
                        || 0 != (b & pos.abs_blockers (Opp)))
                    {
                        unsafe_front_line &= pin_attacked_by[Opp][NONE] | pos.pieces (Opp);
                    }
                    // If there is no friend rook or queen attacking the pawn from behind,
                    // consider only the squares in the pawn's path attacked by the friend.
                    // Otherwise add all X-ray attacks by the friend rook or queen.
                    if (   0 == (b = (behind_major & pos.pieces (Own)))
                        || 0 != (b & pos.abs_blockers (Own)))
                    {
                        safe_front_line &= pin_attacked_by[Own][NONE];
                    }

                    // Give a big bonus if the path to the queen is not attacked,
                    // a smaller bonus if the block square is not attacked.
                    i32 k = 0 != unsafe_front_line ?
                                contains (unsafe_front_line, push_sq) ?
                                    0 : 9 : 20;
                    // Give a big bonus if the path to the queen is fully defended,
                    // a smaller bonus if the block square is defended.
                    k += safe_front_line != front_line ?
                                !contains (safe_front_line, push_sq) ?
                                    0 : 4 : 6;

                    mg_value += k*rr,
                    eg_value += k*rr;
                }
                else
                // If the pawn is blocked by own pieces.
                if (contains (pos.pieces (Own), push_sq))
                {
                    mg_value += 1*rr + 2*r,
                    eg_value += 1*rr + 2*r;
                }
            }

            // Scale down bonus for candidate passers which need more than one 
            // pawn push to become passed or have a pawn in front of them.
            if (   !pos.pawn_passed_at (Own, s+pawn_push (Own))
                || 0 != (pos.pieces (PAWN) & front_line_bb (Own, s)))
            {
                mg_value /= 2,
                eg_value /= 2;
            }

            score += mk_score (mg_value, eg_value)
                   + PawnPassFile[std::min (_file (s), F_H - _file (s))]
                   - PawnPassHinder * pop_count (front_line_bb (Own, s) & (pin_attacked_by[Opp][NONE] | pos.pieces (Opp)));
        }

        if (Trace)
        {
            Tracer::write (Tracer::Term::PASSER, Own, score);
        }

        return score;
    }

    /// evaluate_space() evaluates the space of the color.
    /// The space evaluation is a simple bonus based on the number of safe squares
    /// available for minor pieces on the central four files on ranks 2-4
    /// Safe squares one, two or three squares behind a friend pawn are counted twice
    /// The aim is to improve play on opening
    template<bool Trace>
    template<Color Own>
    Score Evaluator<Trace>::evaluate_space ()
    {
        const auto Opp = WHITE == Own ? BLACK : WHITE;

        // Find the safe squares for our pieces inside the area defined by SpaceMask.
        // A square is safe:
        // - if not occupied by friend pawns
        // - if not attacked by an enemy pawns
        // - if defended by friend pieces or not attacked by enemy pieces.
        Bitboard safe_space = Space_bb[Own]
                            & Side_bb[CS_NO]
                            & ~pos.pieces (Own, PAWN)
                            & ~pin_attacked_by[Opp][PAWN]
                            & (   pin_attacked_by[Own][NONE]
                               | ~pin_attacked_by[Opp][NONE]);

        // Since SpaceMask is fully on our half of the board
        assert(0 == u32(safe_space >> (WHITE == Own ? 0x20 : 0x00)));
        // Find all squares which are at most three squares behind some friend pawn
        Bitboard behind = pos.pieces (Own, PAWN);
        behind |= shift<WHITE == Own ? DEL_S  : DEL_N > (behind);
        behind |= shift<WHITE == Own ? DEL_SS : DEL_NN> (behind);
        i32 count = pop_count (  (behind & safe_space)
                               | (WHITE == Own ? safe_space << 0x20 : safe_space >> 0x20));
        i32 weight = pos.count (Own) - 2 * pe->open_count;
        auto score = mk_score (count * weight * weight / 16, 0);

        if (Trace)
        {
            Tracer::write (Tracer::Term::SPACE, Own, score);
        }

        return score;
    }

    /// evaluate_initiative() evaluates the initiative correction value for the position
    /// i.e. second order bonus/malus based on the known attacking/defending status of the players
    template<bool Trace>
    Score Evaluator<Trace>::evaluate_initiative (Value eg)
    {
        i32 king_dist = dist<File> (pos.square<KING> (WHITE), pos.square<KING> (BLACK))
                      - dist<Rank> (pos.square<KING> (WHITE), pos.square<KING> (BLACK));

        // Compute the initiative bonus for the attacking side
        i32 initiative =  8 * (king_dist + pe->asymmetry - 17)
                       + 12 * pos.count (PAWN)
                            // Pawn on both flanks
                      + 16 * (   0 != (pos.pieces (PAWN) & Side_bb[CS_KING])
                              && 0 != (pos.pieces (PAWN) & Side_bb[CS_QUEN]) ? 1 : 0);
        // Now apply the bonus: note that we find the attacking side by extracting
        // the sign of the endgame value, and that we carefully cap the bonus so
        // that the endgame score will never change sign after the bonus.
        auto score = mk_score (0, sign (eg) * initiative);

        if (Trace)
        {
            Tracer::write (Tracer::Term::INITIATIVE, score);
        }

        return score;
    }

    /// evaluate_scale() evaluates the scale for the position
    template<bool Trace>
    Scale Evaluator<Trace>::evaluate_scale (Value eg)
    {
        auto strong_color = eg >= VALUE_ZERO ? WHITE : BLACK;

        Scale scale;
        if (   nullptr == me->scale_func[strong_color]
            || SCALE_NONE == (scale = (*me->scale_func[strong_color])(pos)))
        {
            scale = me->scale[strong_color];
        }
        assert(SCALE_NONE != scale);

        // If don't already have an unusual scale, check for certain types of endgames.
        if (   SCALE_NORMAL == scale
            || SCALE_ONEPAWN == scale)
        {
            // Endings with opposite-colored bishops
            if (pos.opposite_bishops ())
            {
                // With no other pieces (ignoring pawns)
                // or
                // With some other pieces is still a bit drawish, but not as drawish as with only the two bishops.
                return VALUE_MG_BSHP == pos.si->non_pawn_material (WHITE)
                    && VALUE_MG_BSHP == pos.si->non_pawn_material (BLACK) ?
                        1 >= pos.count (PAWN) ?
                            Scale( 9) :
                            Scale(31) :
                        Scale(46);
            }
            // Endings where weaker side can place his king in front of the strong side pawns are drawish.
            if (   VALUE_EG_BSHP >= abs (eg)
                && 2 >= pos.count (strong_color, PAWN)
                && !pos.pawn_passed_at (~strong_color, pos.square<KING> (~strong_color)))
            {
                return Scale(37 + 7 * pos.count (strong_color, PAWN));
            }
        }
        return scale;
    }

    /// value() computes the various parts of the evaluation and
    /// returns the value of the position from the point of view of the side to move.
    template<bool Trace>
    Value Evaluator<Trace>::value ()
    {
        assert(0 == pos.si->checkers);

        // Probe the material hash table
        me = Material::probe (pos);
        // If have a specialized evaluation function for the material configuration
        if (nullptr != me->value_func)
        {
            return (*me->value_func) (pos);
        }

        // Probe the pawn hash table
        pe = Pawns::probe (pos);

        Score score;
        // Score is computed internally from the white point of view, initialize by
        // - the incrementally updated scores (material + piece square tables).
        // - the material imbalance.
        // - the pawn score
        score = pos.si->psq_score
              + me->imbalance
              + pe->score
              + Contempt;

        // Early exit if score is high
        Value v = (mg_value (score) + eg_value (score)) / 2;
        if (abs (v) > LazyThreshold)
        {
            return WHITE == pos.active ? +v : -v;
        }

        if (Trace)
        {
            Tracer::initialize ();
        }

        initialize<WHITE> ();
        initialize<BLACK> ();

        // Evaluate all pieces except pawns and king
        score += evaluate_pieces<WHITE, NIHT> ()
              -  evaluate_pieces<BLACK, NIHT> ();
        score += evaluate_pieces<WHITE, BSHP> ()
              -  evaluate_pieces<BLACK, BSHP> ();
        score += evaluate_pieces<WHITE, ROOK> ()
              -  evaluate_pieces<BLACK, ROOK> ();
        score += evaluate_pieces<WHITE, QUEN> ()
              -  evaluate_pieces<BLACK, QUEN> ();
        // Evaluate piece mobility
        score += mobility[WHITE]
              -  mobility[BLACK];

        // Full attack information needed including king
        // Evaluate kings
        score += evaluate_king<WHITE> ()
              -  evaluate_king<BLACK> ();
        // Evaluate threats
        score += evaluate_threats<WHITE> ()
              -  evaluate_threats<BLACK> ();
        // Evaluate passers
        score += evaluate_passers<WHITE> ()
              -  evaluate_passers<BLACK> ();

        // In the opening phase
        if (pos.si->non_pawn_material () >= SpaceThreshold)
        {
            // Evaluate space
            score += evaluate_space<WHITE> ()
                  -  evaluate_space<BLACK> ();
        }

        // Evaluate potential
        score += evaluate_initiative (eg_value (score));

        assert(-VALUE_INFINITE < mg_value (score) && mg_value (score) < +VALUE_INFINITE);
        assert(-VALUE_INFINITE < eg_value (score) && eg_value (score) < +VALUE_INFINITE);
        assert(0 <= me->phase && me->phase <= Material::PhaseResolution);

        // Interpolates between midgame and scaled endgame values.
        v = Value(  (  mg_value (score) * (me->phase)
                     + eg_value (score) * (Material::PhaseResolution - me->phase) * evaluate_scale (eg_value (score)) / SCALE_NORMAL)
                  / Material::PhaseResolution);

        if (Trace)
        {
            // Write remaining evaluation terms
            Tracer::write (Tracer::Term(PAWN), pe->score);
            Tracer::write (Tracer::Term::MATERIAL, pos.si->psq_score);
            Tracer::write (Tracer::Term::IMBALANCE, me->imbalance);
            Tracer::write (Tracer::Term::MOBILITY, mobility[WHITE], mobility[BLACK]);
            Tracer::write (Tracer::Term::TOTAL, score);
        }

        return WHITE == pos.active ? +v : -v; // Side to move point of view
    }
}

/// evaluate() returns a static evaluation of the position from the point of view of the side to move.
Value evaluate (const Position &pos)
{
    return Evaluator<false> (pos).value () + Tempo;
}

/// trace() returns a string (suitable for outputting to stdout) that contains
/// the detailed descriptions and values of each evaluation term.
string trace (const Position &pos)
{
    Contempt = SCORE_ZERO;
    auto value = Evaluator<true> (pos).value () + Tempo;
    value = WHITE == pos.active ? +value : -value; // White's point of view

    ostringstream oss;
    oss << std::showpos << std::showpoint << std::setprecision (2) << std::fixed
        << "      Eval Term |    White    |    Black    |    Total     \n"
        << "                |   MG    EG  |   MG    EG  |   MG    EG   \n"
        << "----------------+-------------+-------------+--------------\n"
        << "       Material" << Tracer::Term::MATERIAL
        << "      Imbalance" << Tracer::Term::IMBALANCE
        << "           Pawn" << Tracer::Term(PAWN)
        << "         Knight" << Tracer::Term(NIHT)
        << "         Bishop" << Tracer::Term(BSHP)
        << "           Rook" << Tracer::Term(ROOK)
        << "          Queen" << Tracer::Term(QUEN)
        << "       Mobility" << Tracer::Term::MOBILITY
        << "           King" << Tracer::Term(KING)
        << "         Threat" << Tracer::Term::THREAT
        << "    Pawn Passer" << Tracer::Term::PASSER
        << "          Space" << Tracer::Term::SPACE
        << "     Initiative" << Tracer::Term::INITIATIVE
        << "----------------+-------------+-------------+--------------\n"
        << "          Total" << Tracer::Term::TOTAL
        << "\nEvaluation: " << value_to_cp (value) / 100.0 << " (white side)\n"
        << std::noshowpoint << std::noshowpos;
    return oss.str ();
}
