#include "Searcher.h"

#include <cmath>
#include <map>
#include <stdlib.h>
#include <time.h>

#include "Evaluator.h"
#include "Logger.h"
#include "Notation.h"
#include "Option.h"
#include "Polyglot.h"
#include "TBsyzygy.h"
#include "Thread.h"
#include "Transposition.h"

using namespace std;
using namespace BitBoard;
using namespace TBSyzygy;

/// RootMove::operator string()
RootMove::operator string () const
{
    ostringstream oss;
    for (auto move : *this)
    {
        assert(MOVE_NONE != move);
        oss << " " << move;
    }
    return oss.str ();
}

/// RootMoves::initialize() initializes with search moves
void RootMoves::initialize (const Position &pos, const vector<Move> &search_moves)
{
    assert(empty ());
    for (const auto &vm : MoveList<GenType::LEGAL> (pos))
    {
        if (   search_moves.empty ()
            || std::find (search_moves.begin (), search_moves.end (), vm) != search_moves.end ())
        {
            *this += vm;
            assert(back ().tb_rank == 0
                && back ().tb_value == VALUE_ZERO);
        }
    }
}
/// RootMoves::operator string()
RootMoves::operator string () const
{
    ostringstream oss;
    for (const auto &rm : *this)
    {
        oss << rm << "\n";
    }
    return oss.str ();
}

/// MovePicker class constructors. As arguments pass information to help
/// it to return the (presumably) good moves first, to decide which moves to return
/// (in the quiescence search, for instance, only want to search captures, promotions, and some checks)
/// and about how important good move ordering is at the current node.

/// MovePicker constructor for the main search
MovePicker::MovePicker (const Position &p, Move ttm, i16 d, const PieceDestinyHistory **pdhs, const Move *km, Move cm)
    : pos (p)
    , tt_move (ttm)
    , depth (d)
    , threshold (Value(-4000*d))
    , recap_sq (SQ_NO)
    , pd_histories (pdhs)
    , refutation_moves {km[0], km[1], cm}
    , pick_quiets (true)
{
    assert(MOVE_NONE == tt_move
        || (pos.pseudo_legal (tt_move)
         && pos.legal (tt_move)));
    assert(depth > 0);
    assert(threshold < VALUE_ZERO);

    if (0 != pos.si->checkers)
    {
        stage = Stage::EV_TT;
    }
    else
    {
        stage = Stage::NT_TT;
    }

    if (MOVE_NONE == tt_move)
    {
        ++stage;
    }
}
/// MovePicker constructor for quiescence search
/// Because the depth <= DepthZero here, only captures, queen promotions
/// and checks (only if depth >= DepthQSCheck) will be generated.
MovePicker::MovePicker (const Position &p, Move ttm, i16 d, const PieceDestinyHistory **pdhs, Square rs)
    : pos (p)
    , tt_move (ttm)
    , depth (d)
    , threshold (VALUE_ZERO)
    , recap_sq (rs)
    , pd_histories (pdhs)
    , pick_quiets (true)
{
    assert(MOVE_NONE == tt_move
        || (pos.pseudo_legal (tt_move)
         && pos.legal (tt_move)));
    assert(depth <= 0);

    if (0 != pos.si->checkers)
    {
        stage = Stage::EV_TT;
    }
    else
    {
        stage = Stage::QS_TT;

        if (   MOVE_NONE != tt_move
            && !(   DepthQSRecapture < depth
                 || dst_sq (tt_move) == recap_sq))
        {
            tt_move = MOVE_NONE;
        }
    }

    if (MOVE_NONE == tt_move)
    {
        ++stage;
    }
}
/// MovePicker constructor for ProbCut search.
/// Generate captures with SEE greater than or equal to the given threshold.
MovePicker::MovePicker (const Position &p, Move ttm, Value thr)
    : pos (p)
    , tt_move (ttm)
    , depth (0)
    , threshold (thr)
    , recap_sq (SQ_NO)
    , pd_histories (nullptr)
    , pick_quiets (true)
{
    assert(0 == pos.si->checkers);
    assert(MOVE_NONE == tt_move
        || (pos.pseudo_legal (tt_move)
         && pos.legal (tt_move)));

    stage = Stage::PC_TT;

    if (   MOVE_NONE != tt_move
        && !(   pos.capture (tt_move)
             && pos.see_ge (tt_move, threshold)))
    {
        tt_move = MOVE_NONE;
    }

    if (MOVE_NONE == tt_move)
    {
        ++stage;
    }
}

/// MovePicker::value() assigns a numerical value to each move in a list, used for sorting.
/// Captures are ordered by Most Valuable Victim (MVV) with using the histories.
/// Quiets are ordered using the histories.
template<GenType GT>
void MovePicker::value ()
{
    static_assert (GenType::CAPTURE == GT
                || GenType::QUIET == GT
                || GenType::EVASION == GT, "GT incorrect");

    auto *thread = pos.thread;

    for (auto &m : moves)
    {
        if (GenType::CAPTURE == GT)
        {
            assert(pos.capture_or_promotion (m));
            m.value = i32(PieceValues[MG][pos.cap_type (m)])
                    + thread->capture_history[pos[org_sq (m)]][move_pp (m)][pos.cap_type (m)] / 8;
        }
        else
        if (GenType::QUIET == GT)
        {
            m.value = thread->butterfly_history[pos.active][move_pp (m)]
                    + (*pd_histories[0])[pos[org_sq (m)]][dst_sq (m)]
                    + (*pd_histories[1])[pos[org_sq (m)]][dst_sq (m)]
                    + (*pd_histories[3])[pos[org_sq (m)]][dst_sq (m)];
        }
        else // GenType::EVASION == GT
        {
            m.value = pos.capture (m) ?
                          i32(PieceValues[MG][pos.cap_type (m)])
                        - ptype (pos[org_sq (m)]) :
                          thread->butterfly_history[pos.active][move_pp (m)]
                        + (*pd_histories[0])[pos[org_sq (m)]][dst_sq (m)]
                        - (1 << 28);
        }
    }
}

/// MovePicker::pick() returns the next move satisfying a predicate function
template<MovePicker::PickType PT, class Pred>
bool MovePicker::pick (Pred filter)
{
    while (i < moves.size ())
    {
        auto beg = moves.begin () + i++;
        if (BEST == PT)
        {
            std::swap (*beg, *std::max_element (beg, moves.end ()));
        }
        vm = *beg;
        if (   tt_move != vm
            && (   !(   pos.enpassant (vm)
                     || contains (pos.si->king_blockers[pos.active] | pos.pieces (pos.active, KING), org_sq (vm)))
                || pos.legal (vm))
            && filter ())
        {
            return true;
        }
    }
    return false;
}

/// MovePicker::next_move() is the most important method of the MovePicker class.
/// It returns a new legal move every time it is called, until there are no more moves left.
/// It picks the move with the biggest value from a list of generated moves
/// taking care not to return the tt_move if it has already been searched.
Move MovePicker::next_move ()
{
    restage:
    switch (stage)
    {

    case Stage::NT_TT:
    case Stage::EV_TT:
    case Stage::PC_TT:
    case Stage::QS_TT:
        ++stage;
        return tt_move;

    case Stage::NT_INIT:
    case Stage::PC_INIT:
    case Stage::QS_INIT:
        generate<GenType::CAPTURE> (moves, pos);
        value<GenType::CAPTURE> ();
        ++stage;
        i = 0;
        // Re-branch at the top of the switch
        goto restage;

    case Stage::NT_GOOD_CAPTURES:
        if (pick<BEST> ([&]() { return pos.see_ge (vm, Value(-vm.value * 55 / 1024)) ?
                                        true :
                                        // Put losing capture to bad_capture_moves to be tried later
                                        (bad_capture_moves.push_back (vm), false); }))
        {
            return vm;
        }
        // If the countermove is the same as a killers, skip it
        if (   MOVE_NONE != refutation_moves[2]
            && (   refutation_moves[0] == refutation_moves[2]
                || refutation_moves[1] == refutation_moves[2]))
        {
            refutation_moves.erase (refutation_moves.begin () + 2);
        }
        refutation_moves.erase (std::remove_if (refutation_moves.begin (),
                                                refutation_moves.end (),
                                                [&](const Move m) { return MOVE_NONE == m
                                                                        || tt_move == m
                                                                        ||  pos.capture (m)
                                                                        || !pos.pseudo_legal (m)
                                                                        || !pos.legal (m); }),
                                refutation_moves.end ());
        ++stage;
        i = 0;
        /* fall through */
    case NT_REFUTATIONS:
        // Refutation moves: Killers, Counter moves
        if (i < refutation_moves.size ())
        {
            return refutation_moves[i++];
        }

        generate<GenType::QUIET> (moves, pos);
        value<GenType::QUIET> ();
        ++stage;
        i = 0;
        /* fall through */
    case Stage::NT_QUIETS:
        if (   pick_quiets
            && pick<BEST> ([&]() { return std::find (refutation_moves.begin (), refutation_moves.end (), vm.move) == refutation_moves.end (); }))
        {
            return vm;
        }
        ++stage;
        i = 0;
        /* fall through */
    case Stage::NT_BAD_CAPTURES:
        return i < bad_capture_moves.size () ?
                bad_capture_moves[i++] :
                MOVE_NONE;
        /* end */
    case Stage::EV_INIT:
        assert(0 != pos.si->checkers);
        generate<GenType::EVASION> (moves, pos);
        value<GenType::EVASION> ();
        ++stage;
        i = 0;
        /* fall through */
    case Stage::EV_MOVES:
        return pick<BEST> ([]() { return true; }) ?
                vm :
                MOVE_NONE;
        /* end */
    case Stage::PC_CAPTURES:
        return pick<BEST> ([&]() { return pos.see_ge (vm, threshold); }) ?
                vm :
                MOVE_NONE;
        /* end */
    case Stage::QS_CAPTURES:
        if (pick<BEST> ([&]() { return DepthQSRecapture < depth
                                    || dst_sq (vm) == recap_sq; }))
        {
            return vm;
        }
        // If did not find any move then do not try checks, finished.
        if (DepthQSCheck > depth)
        {
            return MOVE_NONE;
        }

        generate<GenType::QUIET_CHECK> (moves, pos);
        ++stage;
        i = 0;
        /* fall through */
    case Stage::QS_CHECKS:
        return pick<NEXT> ([]() { return true; }) ?
                vm :
                MOVE_NONE;
        /* end */
    default:
        assert(false);
        break;
    }
    return MOVE_NONE;
}

namespace Searcher {

    Limit Limits;

    i16 TBProbeDepth = 1;
    i32 TBLimitPiece = 6;
    bool TBUseRule50 = true;
    bool TBHasRoot = false;

    namespace {

        constexpr u08 SkipIndex = 20;
        constexpr u08 SkipSize[SkipIndex] =
        {
            1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4
        };
        constexpr u08 SkipPhase[SkipIndex] =
        {
            0, 1, 0, 1, 2, 3, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 6, 7
        };

        // Razoring and futility margin
        constexpr Value RazorMargin = Value(600);

        // FutilityMoveCounts[improving][depth]
        u08 FutilityMoveCounts[2][16];

        // ReductionDepths[pv][improving][depth][move-count]
        i16 ReductionDepths[2][2][64][64];
        i16 reduction_depth (bool pv, bool imp, i16 d, u08 mc)
        {
            return ReductionDepths[pv ? 1 : 0][imp ? 1 : 0][std::min (d, i16(63))][std::min (mc, u08(63))];
        }

        TimePoint DebugTime;

        i32 BasicContempt = 0;

        ofstream OutputStream;

        /// stat_bonus() is the bonus, based on depth
        i32 stat_bonus (i16 depth)
        {
            return depth < 18 ? (29*depth + 138)*depth - 134 : 0;
        }

        // Add a small random component to draw evaluations to keep search dynamic and to avoid 3-fold-blindness.
        Value value_draw (i16 depth /*, Thread* thread*/)
        {
            //return VALUE_DRAW + (depth < 4 ? 0 : 2 * i32(thread->nodes.load (std::memory_order_relaxed) % 2) - 1);
            return VALUE_DRAW + (depth < 4 ? 0 : rand () % 3 - 1);
        }

        /// update_continuation_histories() updates tables of the move pairs with current move.
        void update_continuation_histories (Stack *const &ss, Piece pc, Square dst, i32 bonus)
        {
            for (const auto *const &s : { ss-1, ss-2, ss-4 })
            {
                if (_ok (s->played_move))
                {
                    (*s->pd_history)[pc][dst] << bonus;
                }
            }
        }
        /// update_killers() updates move sorting heuristics
        void update_killers (Stack *const &ss, const Position &pos, Move move)
        {
            if (ss->killer_moves[0] != move)
            {
                ss->killer_moves[1] = ss->killer_moves[0];
                ss->killer_moves[0] = move;
            }
            assert(1 == std::count (ss->killer_moves, ss->killer_moves + 2, move));

            if (_ok ((ss-1)->played_move))
            {
                pos.thread->move_history[pos[fix_dst_sq ((ss-1)->played_move)]][move_pp ((ss-1)->played_move)] = move;
            }
        }

        /// update_pv() appends the move and child pv
        void update_pv (vector<Move> &pv, Move move, const vector<Move> &child_pv)
        {
            pv.assign (child_pv.begin (), child_pv.end ());
            pv.insert (pv.begin (), move);
        }

        /// It adjusts a mate score from "plies to mate from the root" to
        /// "plies to mate from the current position". Non-mate scores are unchanged.
        /// The function is called before storing a value to the transposition table.
        Value value_to_tt (Value v, i32 ply)
        {
            assert(VALUE_NONE != v);
            return v >= +VALUE_MATE_MAX_PLY ? v + ply :
                   v <= -VALUE_MATE_MAX_PLY ? v - ply :
                                              v;
        }
        /// It adjusts a mate score from "plies to mate from the current position" to "plies to mate from the root".
        /// Non-mate scores are unchanged.
        /// The function is called after retrieving a value of the transposition table.
        Value value_of_tt (Value v, i32 ply)
        {
            return v ==  VALUE_NONE         ? VALUE_NONE :
                   v >= +VALUE_MATE_MAX_PLY ? v - ply :
                   v <= -VALUE_MATE_MAX_PLY ? v + ply :
                                              v;
        }

        /// quien_search() is quiescence search function, which is called by the main depth limited search function when the remaining depth <= 0.
        template<bool PVNode>
        Value quien_search (Position &pos, Stack *const &ss, Value alfa, Value beta, i16 depth = DepthZero)
        {
            assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            assert(PVNode || (alfa == beta-1));
            assert(DepthZero >= depth);

            Value prev_alfa;

            if (PVNode)
            {
                prev_alfa = alfa; // To flag BOUND_EXACT when eval above alpha and no available moves
                ss->pv.clear ();
            }

            auto *thread = pos.thread;
            ss->played_move = MOVE_NONE;
            ss->pd_history = &thread->continuation_history[NO_PIECE][0];

            auto active = pos.active;
            bool in_check = 0 != pos.si->checkers;

            // Check for maximum ply reached or immediate draw.
            if (   ss->ply >= MaxDepth
                || pos.draw (ss->ply))
            {
                return ss->ply >= MaxDepth
                    && !in_check ?
                            evaluate (pos) :
                            VALUE_DRAW;
            }

            assert(ss->ply >= 1
                && ss->ply == (ss-1)->ply + 1
                && ss->ply < MaxDepth);

            Move move;
            // Transposition table lookup.
            Key key = pos.si->posi_key;
            bool tt_hit;
            auto *tte = TT.probe (key, tt_hit);
            auto tt_move = tt_hit
                        && MOVE_NONE != (move = tte->move ())
                        && pos.pseudo_legal (move)
                        && pos.legal (move) ?
                            move :
                            MOVE_NONE;
            assert(MOVE_NONE == tt_move
                || (pos.pseudo_legal (tt_move)
                 && pos.legal (tt_move)));
            auto tt_value = tt_hit ?
                            value_of_tt (tte->value (), ss->ply) :
                            VALUE_NONE;
            auto tt_pv = tt_hit
                      && tte->is_pv ();

            // Decide whether or not to include checks.
            // Fixes also the type of TT entry depth that are going to use.
            // Note that in quien_search use only 2 types of depth: DepthQSCheck or DepthQSNoCheck.
            i16 qs_depth = in_check
                        || DepthQSCheck <= depth ?
                            DepthQSCheck :
                            DepthQSNoCheck;

            if (   !PVNode
                && VALUE_NONE != tt_value // Handle tt_hit
                && qs_depth <= tte->depth ()
                && BOUND_NONE != (tte->bound () & (tt_value >= beta ? BOUND_LOWER : BOUND_UPPER)))
            {
                return tt_value;
            }

            Value best_value
                , futility_base;

            auto best_move = MOVE_NONE;

            StateInfo si;

            // Evaluate the position statically.
            if (in_check)
            {
                ss->static_eval = VALUE_NONE;
                // Starting from the worst case which is checkmate
                best_value = futility_base = mated_in (ss->ply);
            }
            else
            {
                if (tt_hit)
                {
                    ss->static_eval = best_value = tte->eval ();
                    // Never assume anything on values stored in TT.
                    if (VALUE_NONE == best_value)
                    {
                        ss->static_eval = best_value = evaluate (pos);
                    }

                    // Can tt_value be used as a better position evaluation?
                    if (   VALUE_NONE != tt_value
                        && BOUND_NONE != (tte->bound () & (tt_value > best_value ? BOUND_LOWER : BOUND_UPPER)))
                    {
                        best_value = tt_value;
                    }
                }
                else
                {
                    if (MOVE_NULL != (ss-1)->played_move)
                    {
                        ss->static_eval = best_value = evaluate (pos);
                    }
                    else
                    {
                        ss->static_eval = best_value = -(ss-1)->static_eval + Tempo * 2;
                    }
                }

                if (alfa < best_value)
                {
                    // Stand pat. Return immediately if static value is at least beta.
                    if (best_value >= beta)
                    {
                        if (!tt_hit)
                        {
                            tte->save (key,
                                       MOVE_NONE,
                                       value_to_tt (best_value, ss->ply),
                                       ss->static_eval,
                                       DepthNone,
                                       BOUND_LOWER,
                                       tt_pv ? 4 : 0);
                        }

                        assert(-VALUE_INFINITE < best_value && best_value < +VALUE_INFINITE);
                        return best_value;
                    }

                    assert(best_value < beta);
                    // Update alfa! Always alfa < beta
                    if (PVNode)
                    {
                        alfa = best_value;
                    }
                }

                futility_base = best_value + 128;
            }

            u08 move_count = 0;

            const PieceDestinyHistory *pd_histories[4] =
            {
                (ss-1)->pd_history,
                (ss-2)->pd_history,
                (ss-3)->pd_history,
                (ss-4)->pd_history
            };
            auto recap_sq = _ok ((ss-1)->played_move) ?
                                dst_sq ((ss-1)->played_move) :
                                SQ_NO;
            // Initialize movepicker (2) for the current position
            MovePicker move_picker (pos, tt_move, depth, pd_histories, recap_sq);
            // Loop through the moves until no moves remain or a beta cutoff occurs
            while (MOVE_NONE != (move = move_picker.next_move ()))
            {
                assert(pos.pseudo_legal (move)
                    && pos.legal (move));

                ++move_count;

                auto org = org_sq (move);
                auto dst = dst_sq (move);

                auto mpc = pos[org];
                assert(NO_PIECE != mpc);

                bool gives_check = pos.gives_check (move);

                // Futility pruning
                if (   !in_check
                    && !gives_check
                    && futility_base > -VALUE_KNOWN_WIN
                    && !Limits.mate_search ()
                        // Advance pawn push
                    && !(   PAWN == ptype (mpc)
                         && R_4 < rel_rank (active, org)))
                {
                    // Futility pruning parent node
                    if (CASTLE != mtype (move))
                    {
                        auto futility_value = futility_base + PieceValues[EG][ptype (pos[dst])];
                        if (futility_value <= alfa)
                        {
                            best_value = std::max (futility_value, best_value);
                            continue;
                        }
                    }
                    // Prune moves with negative or zero SEE
                    if (   futility_base <= alfa
                        && !pos.see_ge (move, Value(1)))
                    {
                        best_value = std::max (futility_base, best_value);
                        continue;
                    }
                }

                // Don't search moves with negative SEE values
                if (   (   !in_check
                        // Evasion Prunable: Detect non-capture evasions that are candidate to be pruned
                        || (   (   DepthZero != depth
                                || 2 < move_count)
                            && best_value > -VALUE_MATE_MAX_PLY
                            && !pos.capture (move)))
                    && !Limits.mate_search ()
                    && !pos.see_ge (move))
                {
                    continue;
                }

                // Speculative prefetch as early as possible
                prefetch (TT.cluster (pos.posi_move_key (move))->entries);

                // Update the current move.
                ss->played_move = move;
                ss->pd_history = &thread->continuation_history[mpc][dst];

                // Make the move.
                pos.do_move (move, si, gives_check);

                auto value = -quien_search<PVNode> (pos, ss+1, -beta, -alfa, depth - 1);

                // Undo the move.
                pos.undo_move (move);

                assert(-VALUE_INFINITE < value && value < +VALUE_INFINITE);

                // Check for new best move.
                if (best_value < value)
                {
                    best_value = value;

                    if (alfa < value)
                    {
                        best_move = move;

                        // Update pv even in fail-high case
                        if (PVNode)
                        {
                            update_pv (ss->pv, move, (ss+1)->pv);
                        }

                        // Update alfa! Always alfa < beta
                        if (   PVNode
                            && value < beta)
                        {
                            alfa = value;
                        }
                        else
                        {
                            assert(value >= beta); // Fail high
                            break;
                        }
                    }
                }
            }

            tte->save (key,
                       best_move,
                       value_to_tt (best_value, ss->ply),
                       ss->static_eval,
                       qs_depth,
                       best_value >= beta ?
                            BOUND_LOWER :
                               PVNode
                            && best_value > prev_alfa ?
                                BOUND_EXACT :
                                BOUND_UPPER,
                       tt_pv ? 4 : 0);

            assert(-VALUE_INFINITE < best_value && best_value < +VALUE_INFINITE);
            return best_value;
        }
        /// depth_search() is main depth limited search function, which is called when the remaining depth > 0.
        template<bool PVNode>
        Value depth_search (Position &pos, Stack *const &ss, Value alfa, Value beta, i16 depth, bool cut_node)
        {
            bool root_node = PVNode
                          && 0 == ss->ply;

            // Check if there exists a move which draws by repetition,
            // or an alternative earlier move to this position.
            if (   !root_node
                && alfa < VALUE_DRAW
                && pos.si->clock_ply >= 3
                && pos.cycled (ss->ply))
            {
                alfa = value_draw (depth/*, pos.thread*/);
                if (alfa >= beta)
                {
                    return alfa;
                }
            }

            // Dive into quiescence search when the depth reaches zero
            if (DepthZero >= depth)
            {
                return quien_search<PVNode> (pos, ss, alfa, beta);
            }

            assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            assert(PVNode || (alfa == beta-1));
            assert(!(PVNode && cut_node));
            assert(DepthZero < depth && depth < MaxDepth);

            // Step 1. Initialize node.
            auto *thread = pos.thread;
            auto active = pos.active;
            bool in_check = 0 != pos.si->checkers;
            ss->move_count = 0;

            // Check for the available remaining limit.
            if (Threadpool.main_thread () == thread)
            {
                Threadpool.main_thread ()->tick ();
            }

            if (PVNode)
            {
                // Used to send sel_depth info to GUI (sel_depth from 1, ply from 0)
                if (thread->sel_depth < i16(ss->ply + 1))
                {
                    thread->sel_depth = i16(ss->ply + 1);
                }
            }

            Value value;
            auto best_value = -VALUE_INFINITE;
            auto max_value = +VALUE_INFINITE;

            auto best_move = MOVE_NONE;

            if (!root_node)
            {
                // Step 2. Check for aborted search, maximum ply reached or immediate draw.
                if (   Threadpool.stop.load (std::memory_order::memory_order_relaxed)
                    || ss->ply >= MaxDepth
                    || pos.draw (ss->ply))
                {
                    return ss->ply >= MaxDepth
                        && !in_check ?
                                evaluate (pos) :
                                value_draw (depth/*, pos.thread*/);
                }

                // Step 3. Mate distance pruning.
                // Even if mate at the next move our score would be at best mates_in(ss->ply+1),
                // but if alfa is already bigger because a shorter mate was found upward in the tree
                // then there is no need to search further, will never beat current alfa.
                // Same logic but with reversed signs applies also in the opposite condition of
                // being mated instead of giving mate, in this case return a fail-high score.
                alfa = std::max (mated_in (ss->ply+0), alfa);
                beta = std::min (mates_in (ss->ply+1), beta);
                if (alfa >= beta)
                {
                    return alfa;
                }
            }

            assert(ss->ply >= 0
                && ss->ply == (ss-1)->ply + 1
                && ss->ply < MaxDepth);

            ss->played_move = MOVE_NONE;
            ss->pd_history = &thread->continuation_history[NO_PIECE][0];

            assert(MOVE_NONE == (ss+1)->excluded_move);
            std::fill_n ((ss+2)->killer_moves, 2, MOVE_NONE);

            // Initialize stats to zero for the grandchildren of the current position.
            // So stats is shared between all grandchildren and only the first grandchild starts with stats = 0.
            // Later grandchildren start with the last calculated stats of the previous grandchild.
            // This influences the reduction rules in LMR which are based on the stats of parent position.
            (ss+2)->stats = 0;

            Move move;
            // Step 4. Transposition table lookup.
            // Don't want the score of a partial search to overwrite a previous full search
            // TT value, so use a different position key in case of an excluded move.
            Key key = pos.si->posi_key ^ (Key(ss->excluded_move) << 0x10);
            bool tt_hit;
            auto *tte = TT.probe (key, tt_hit);
            auto tt_move = root_node ?
                            thread->root_moves[thread->pv_cur][0] :
                               tt_hit
                            && MOVE_NONE != (move = tte->move ())
                            && pos.pseudo_legal (move)
                            && pos.legal (move) ?
                                move :
                                MOVE_NONE;
            assert(MOVE_NONE == tt_move
                || (pos.pseudo_legal (tt_move)
                 && pos.legal (tt_move)));
            auto tt_value = tt_hit ?
                            value_of_tt (tte->value (), ss->ply) :
                            VALUE_NONE;
            auto tt_pv = (  tt_hit
                          && tte->is_pv ())
                      || (   PVNode
                          && 4 < depth);

            bool improving;

            // At non-PV nodes we check for an early TT cutoff.
            if (   !PVNode
                && MOVE_NONE == ss->excluded_move
                && VALUE_NONE != tt_value // Handle tt_hit
                && depth <= tte->depth ()
                && BOUND_NONE != (tte->bound () & (tt_value >= beta ? BOUND_LOWER : BOUND_UPPER)))
            {
                // Update move sorting heuristics on tt_move.
                if (MOVE_NONE != tt_move)
                {
                    if (tt_value >= beta)
                    {
                        // Bonus for a quiet tt_move that fails high.
                        if (!pos.capture_or_promotion (tt_move))
                        {
                            update_killers (ss, pos, tt_move);
                            auto bonus = stat_bonus (depth);
                            thread->butterfly_history[active][move_pp (tt_move)] << bonus;
                            update_continuation_histories (ss, pos[org_sq (tt_move)], dst_sq (tt_move), bonus);
                        }

                        // Extra penalty for a quiet TT move or main killer move in previous ply when it gets refuted.
                        if (   NONE == pos.si->capture
                            && NONE == pos.si->promote
                            && (   1 == (ss-1)->move_count
                                || (ss-1)->played_move == (ss-1)->killer_moves[0]))
                        {
                            update_continuation_histories (ss-1, pos[fix_dst_sq ((ss-1)->played_move)], dst_sq ((ss-1)->played_move), -stat_bonus (depth + 1));
                        }
                    }
                    else
                    // Penalty for a quiet tt_move that fails low.
                    if (!pos.capture_or_promotion (tt_move))
                    {
                        auto bonus = stat_bonus (depth);
                        thread->butterfly_history[active][move_pp (tt_move)] << -bonus;
                        update_continuation_histories (ss, pos[org_sq (tt_move)], dst_sq (tt_move), -bonus);
                    }
                }
                return tt_value;
            }

            // Step 5. Tablebases probe.
            if (   !root_node
                && 0 != TBLimitPiece)
            {
                auto piece_count = pos.count ();

                if (   (   piece_count < TBLimitPiece
                        || (   piece_count == TBLimitPiece
                            && depth >= TBProbeDepth))
                    && 0 == pos.si->clock_ply
                    && !pos.si->can_castle (CR_ANY))
                {
                    ProbeState state;
                    auto wdl = probe_wdl (pos, state);

                    // Force check of time on the next occasion
                    if (Threadpool.main_thread () == thread)
                    {
                        Threadpool.main_thread ()->check_count = 1;
                    }

                    if (ProbeState::FAILURE != state)
                    {
                        thread->tb_hits.fetch_add (1, std::memory_order::memory_order_relaxed);

                        auto draw = TBUseRule50 ? 1 : 0;

                        value = wdl < -draw ? -VALUE_MATE + (MaxDepth + ss->ply + 1) :
                                wdl > +draw ? +VALUE_MATE - (MaxDepth + ss->ply + 1) :
                                               VALUE_ZERO + 2 * wdl * draw;

                        auto bound = wdl < -draw ? BOUND_UPPER :
                                     wdl > +draw ? BOUND_LOWER :
                                                   BOUND_EXACT;

                        if (   BOUND_EXACT == bound
                            || (BOUND_LOWER == bound ? beta <= value : value <= alfa))
                        {
                            tte->save (key,
                                       MOVE_NONE,
                                       value_to_tt (value, ss->ply),
                                       VALUE_NONE,
                                       i16(std::min (depth + 6, MaxDepth - 1)),
                                       bound,
                                       tt_pv ? 4 : 0);

                            return value;
                        }

                        if (PVNode)
                        {
                            if (BOUND_LOWER == bound)
                            {
                                best_value = value;
                                alfa = std::max (best_value, alfa);
                            }
                            else
                            {
                                max_value = value;
                            }
                        }

                    }
                }
            }

            StateInfo si;
            Value tt_eval;

            // Step 6. Static evaluation of the position
            if (in_check)
            {
                ss->static_eval = tt_eval = VALUE_NONE;
                improving = false;
            }
            else
            {
                Value eval;
                if (tt_hit)
                {
                    ss->static_eval = eval = tt_eval = tte->eval ();
                    // Never assume anything on values stored in TT.
                    if (VALUE_NONE == tt_eval)
                    {
                        ss->static_eval = eval = tt_eval = evaluate (pos);
                    }

                    // Can tt_value be used as a better position evaluation?
                    if (   VALUE_NONE != tt_value
                        && BOUND_NONE != (tte->bound () & (tt_value > eval ? BOUND_LOWER : BOUND_UPPER)))
                    {
                        eval = tt_value;
                    }
                }
                else
                {
                    if (MOVE_NULL != (ss-1)->played_move)
                    {
                        i32 bonus = -(ss-1)->stats / 512;
                        tt_eval = evaluate (pos);
                        ss->static_eval = eval = tt_eval + bonus;
                    }
                    else
                    {
                        ss->static_eval = eval = tt_eval = -(ss-1)->static_eval + Tempo * 2;
                    }

                    tte->save (key,
                               MOVE_NONE,
                               VALUE_NONE,
                               tt_eval,
                               DepthNone,
                               BOUND_NONE,
                               tt_pv ? 4 : 0);
                }

                // Step 7. Razoring. (~2 ELO)
                if (   !root_node // The required RootNode PV handling is not available in qsearch
                    && 2 > depth
                    && eval <= std::max (alfa - RazorMargin, -VALUE_INFINITE))
                {
                    return quien_search<PVNode> (pos, ss, alfa, beta);
                }

                improving = (ss-0)->static_eval >= (ss-2)->static_eval
                         || VALUE_NONE == (ss-2)->static_eval;

                // Step 8. Futility pruning: child node. (~30 ELO)
                // Betting that the opponent doesn't have a move that will reduce
                // the score by more than futility margins [depth] if do a null move.
                if (   !root_node
                    && 7 > depth
                    && !Limits.mate_search ()
                    && eval - (improving ? 125 : 175) * depth >= beta
                    && eval < +VALUE_KNOWN_WIN) // Don't return unproven wins.
                {
                    return eval;
                }

                // Step 9. Null move search with verification search. (~40 ELO)
                if (   !PVNode
                    && !Limits.mate_search ()
                    && MOVE_NULL != (ss-1)->played_move
                    && MOVE_NONE == ss->excluded_move
                    && VALUE_ZERO != pos.si->non_pawn_material (active)
                    && 23200 > (ss-1)->stats
                    && eval >= beta
                    && std::min (tt_eval + 36*depth - 225, +VALUE_INFINITE) >= beta
                    && (   thread->nmp_ply <= ss->ply
                        || thread->nmp_color != active))
                {
                    // Null move dynamic reduction based on depth and static evaluation.
                    auto R = i16((67*depth + 823) / 256 + std::min (i32(eval - beta)/200, 3));

                    ss->played_move = MOVE_NULL;
                    ss->pd_history = &thread->continuation_history[NO_PIECE][0];

                    pos.do_null_move (si);

                    auto null_value = -depth_search<false> (pos, ss+1, -beta, -beta+1, depth-R, !cut_node);

                    pos.undo_null_move ();

                    if (null_value >= beta)
                    {
                        bool unproven = null_value >= +VALUE_MATE_MAX_PLY;

                        // Skip verification search
                        if (   0 != thread->nmp_ply
                            || (   12 > depth
                                && abs (beta) < +VALUE_KNOWN_WIN))
                        {
                            // Don't return unproven wins
                            return unproven ?
                                    beta :
                                    null_value;
                        }

                        assert(0 == thread->nmp_ply); // Recursive verification is not allowed

                        // Do verification search at high depths
                        thread->nmp_ply = ss->ply + 3 * (depth-R) / 4;
                        thread->nmp_color = active;

                        value = depth_search<false> (pos, ss, beta-1, beta, depth-R, false);

                        thread->nmp_ply = 0;
                        thread->nmp_color = CLR_NO;

                        if (value >= beta)
                        {
                            // Don't return unproven wins
                            return unproven ?
                                    beta :
                                    null_value;
                        }
                    }
                }

                // Step 10. ProbCut. (~10 ELO)
                // If good enough capture and a reduced search returns a value much above beta,
                // then can (almost) safely prune the previous move.
                if (   !PVNode
                    && 4 < depth
                    && !Limits.mate_search ()
                    && abs (beta) < +VALUE_MATE_MAX_PLY)
                {
                    u08 pc_movecount = 0;
                    auto raised_beta = std::min (beta + (improving ? 168 : 216), +VALUE_INFINITE);
                    // Initialize movepicker (3) for the current position
                    MovePicker move_picker (pos, tt_move, raised_beta - ss->static_eval);
                    // Loop through all legal moves until no moves remain or a beta cutoff occurs
                    while (   MOVE_NONE != (move = move_picker.next_move ())
                           && 3 > pc_movecount)
                    {
                        assert(pos.pseudo_legal (move)
                            && pos.legal (move)
                            && pos.capture_or_promotion (move));

                        if (move == ss->excluded_move)
                        {
                            continue;
                        }

                        ++pc_movecount;

                        // Speculative prefetch as early as possible
                        prefetch (TT.cluster (pos.posi_move_key (move))->entries);

                        ss->played_move = move;
                        ss->pd_history = &thread->continuation_history[pos[org_sq (move)]][dst_sq (move)];

                        pos.do_move (move, si);

                        // Perform a preliminary quien_search to verify that the move holds
                        value = -quien_search<false> (pos, ss+1, -raised_beta, -raised_beta+1);

                        // If the quien_search held perform the regular search
                        if (value >= raised_beta)
                        {
                            value = -depth_search<false> (pos, ss+1, -raised_beta, -raised_beta+1, depth - 4, !cut_node);
                        }

                        pos.undo_move (move);

                        if (value >= raised_beta)
                        {
                            return value;
                        }
                    }
                }
            }

            // Step 11. Internal iterative deepening (IID). (~2 ELO)
            if (   7 < depth
                && MOVE_NONE == tt_move)
            {
                depth_search<PVNode> (pos, ss, alfa, beta, depth - 7, cut_node);

                tte = TT.probe (key, tt_hit);
                tt_move = tt_hit
                       && MOVE_NONE != (move = tte->move ())
                       && pos.pseudo_legal (move)
                       && pos.legal (move) ?
                            move :
                            MOVE_NONE;
                tt_value = tt_hit ?
                            value_of_tt (tte->value (), ss->ply) :
                            VALUE_NONE;
            }

            value = best_value;

            bool ttm_capture = MOVE_NONE != tt_move
                            && pos.capture_or_promotion (tt_move);

            u08 move_count = 0;

            vector<Move> quiet_moves
                ,        capture_moves;

            const PieceDestinyHistory *pd_histories[4] =
            {
                (ss-1)->pd_history,
                (ss-2)->pd_history,
                (ss-3)->pd_history,
                (ss-4)->pd_history
            };
            auto counter_move = _ok ((ss-1)->played_move) ?
                                    thread->move_history[pos[fix_dst_sq ((ss-1)->played_move)]][move_pp ((ss-1)->played_move)] :
                                    MOVE_NONE;
            // Initialize movepicker (1) for the current position
            MovePicker move_picker (pos, tt_move, depth, pd_histories, ss->killer_moves, counter_move);
            // Step 12. Loop through all legal moves until no moves remain or a beta cutoff occurs.
            while (MOVE_NONE != (move = move_picker.next_move ()))
            {
                assert(pos.pseudo_legal (move)
                    && pos.legal (move));

                if (   // Skip exclusion move
                       (move == ss->excluded_move)
                       // Skip at root node:
                    || (   root_node
                           // In "searchmoves" mode, skip moves not listed in RootMoves, as a consequence any illegal move is also skipped.
                           // In MultiPV mode we not only skip PV moves which have already been searched and those of lower "TB rank" if we are in a TB root position.
                        && std::find (thread->root_moves.begin () + thread->pv_cur,
                                      thread->root_moves.begin () + thread->pv_end, move) == thread->root_moves.end ()))
                {
                    continue;
                }

                ss->move_count = ++move_count;

                bool move_count_pruning = 16 > depth
                                       && FutilityMoveCounts[improving ? 1 : 0][depth] <= move_count;

                auto org = org_sq (move);
                auto dst = dst_sq (move);

                auto mpc = pos[org];
                assert(NO_PIECE != mpc);

                bool gives_check = pos.gives_check (move);
                bool capture_or_promotion = pos.capture_or_promotion (move);

                if (   root_node
                    && Threadpool.main_thread () == thread)
                {
                    auto elapsed_time = Threadpool.main_thread ()->time_mgr.elapsed_time ();
                    if (elapsed_time > 3000)
                    {
                        sync_cout << "info"
                                  << " currmove " << move
                                  << " currmovenumber " << thread->pv_cur + move_count
                                  << " maxmoves " << thread->root_moves.size ()
                                  << " depth " << depth
                                  << " seldepth " << (*std::find (thread->root_moves.begin (), thread->root_moves.end (), move)).sel_depth
                                  << " time " << elapsed_time << sync_endl;
                    }
                }

                if (PVNode)
                {
                    (ss+1)->pv.clear ();
                }

                // Step 13. Extensions. (~70 ELO)

                i16 extension = DepthZero;

                // Singular extension (SE) (~60 ELO)
                // Extend the TT move if its value is much better than its siblings.
                // If all moves but one fail low on a search of (alfa-s, beta-s),
                // and just one fails high on (alfa, beta), then that move is singular and should be extended.
                // To verify this do a reduced search on all the other moves but the tt_move,
                // if result is lower than tt_value minus a margin then extend tt_move.
                if (   !root_node
                    && MOVE_NONE == ss->excluded_move // Avoid recursive singular search.
                    && move == tt_move
                    && VALUE_NONE != tt_value // Handle tt_hit
                    && 7 < depth && depth < tte->depth () + 4
                    && BOUND_NONE != (tte->bound () & BOUND_LOWER))
                {
                    auto singular_beta = std::max (tt_value - 2*depth, -VALUE_MATE);

                    ss->excluded_move = move;
                    value = depth_search<false> (pos, ss, singular_beta -1, singular_beta, depth/2, cut_node);
                    ss->excluded_move = MOVE_NONE;

                    if (value < singular_beta)
                    {
                        extension = 1;
                    }
                    // Multi-cut pruning
                    // Our tt_move is assumed to fail high, and now we failed high also on a reduced
                    // search without the tt_move. So we assume this expected Cut-node is not singular,
                    // that is multiple moves fail high, and we can prune the whole subtree by returning
                    // the hard beta bound.
                    else
                    if (   cut_node
                        && singular_beta > beta)
                    {
                        return beta;
                    }
                }
                else
                if (// Castle extension
                       CASTLE == mtype (move)
                    // Check extension (~2 ELO)
                    || (   gives_check
                        && pos.see_ge (move)))
                {
                    extension = 1;
                }

                // Calculate new depth for this move
                i16 new_depth = depth - 1 + extension;

                // Step 14. Pruning at shallow depth. (~170 ELO)
                if (   !root_node
                    && !Limits.mate_search ()
                    && best_value > -VALUE_MATE_MAX_PLY
                    && VALUE_ZERO < pos.si->non_pawn_material (active))
                {
                    if (   !capture_or_promotion
                        && !gives_check
                            // Advance pawn push.
                        && !(   PAWN == ptype (mpc)
                             && R_4 < rel_rank (active, org)))
                    {
                        // Move count based pruning. (~30 ELO)
                        if (move_count_pruning)
                        {
                            move_picker.pick_quiets = false;
                            continue;
                        }

                        // Reduced depth of the next LMR search.
                        i16 lmr_depth = i16(std::max (new_depth - reduction_depth (PVNode, improving, depth, move_count), 0));
                        if (    // Countermoves based pruning. (~20 ELO)
                               (   ((ss-1)->stats > 0 || (ss-1)->move_count == 1 ? 4 : 3) > lmr_depth
                                && (*pd_histories[0])[mpc][dst] < CounterMovePruneThreshold
                                && (*pd_histories[1])[mpc][dst] < CounterMovePruneThreshold)
                                // Futility pruning: parent node. (~2 ELO)
                            || (   7 > lmr_depth
                                && !in_check
                                && ss->static_eval + 200*lmr_depth + 256 <= alfa)
                                // SEE based pruning: -ve SEE (~10 ELO)
                            || (!pos.see_ge (move, Value(-29*lmr_depth*lmr_depth))))
                        {
                            continue;
                        }
                    }
                    else
                    // SEE based pruning. (~20 ELO)
                    if (   DepthZero == extension
                        && !pos.see_ge (move, -VALUE_EG_PAWN*i32(depth)))
                    {
                        continue;
                    }
                }

                // Speculative prefetch as early as possible
                prefetch (TT.cluster (pos.posi_move_key (move))->entries);

                // Update the current move.
                ss->played_move = move;
                ss->pd_history = &thread->continuation_history[mpc][dst];

                // Step 15. Make the move.
                pos.do_move (move, si, gives_check);

                bool search_fd;
                // Step 16. Reduced depth search (LMR).
                // If the move fails high will be re-searched at full depth.
                if (   2 < depth
                    && 1 < move_count
                    && (   move_count_pruning
                        || !capture_or_promotion))
                {
                    i16 reduct_depth = reduction_depth (PVNode, improving, depth, move_count);

                    // Decrease reduction if position is or has been on the PV
                    if (tt_pv)
                    {
                        reduct_depth -= 1;
                    }

                    // Decrease reduction if opponent's move count is high (~10 Elo)
                    if ((ss-1)->move_count >= 16)
                    {
                        reduct_depth -= 1;
                    }

                    if (!capture_or_promotion)
                    {
                        // Increase reduction if TT move is a capture (~0 Elo)
                        if (ttm_capture)
                        {
                            reduct_depth += 1;
                        }

                        // Increase reduction for cut nodes (~5 Elo)
                        if (cut_node)
                        {
                            reduct_depth += 2;
                        }
                        else
                        // Decrease reduction for moves that escape a capture in no-cut nodes (~5 Elo)
                        if (   NORMAL == mtype (move)
                            && !pos.see_ge (mk_move<NORMAL> (dst, org)))
                        {
                            reduct_depth -= 2;
                        }

                        ss->stats = thread->butterfly_history[active][move_pp (move)]
                                  + (*pd_histories[0])[mpc][dst]
                                  + (*pd_histories[1])[mpc][dst]
                                  + (*pd_histories[3])[mpc][dst]
                                  - 4000;

                        // Decrease/Increase reduction by comparing stats (~10 Elo)
                        if (   ss->stats < 0
                            && (ss-1)->stats >= 0)
                        {
                            reduct_depth += 1;
                        }
                        else
                        if (   ss->stats >= 0
                            && (ss-1)->stats < 0)
                        {
                            reduct_depth -= 1;
                        }

                        // Decrease/Increase reduction for moves with +/-ve stats (~30 Elo)
                        reduct_depth -= i16(ss->stats / 20000);
                    }

                    reduct_depth = std::min (std::max (reduct_depth, i16(0)), i16(new_depth - 1));

                    value = -depth_search<false> (pos, ss+1, -alfa-1, -alfa, new_depth - reduct_depth, true);

                    search_fd = alfa < value
                             && 0 != reduct_depth;
                }
                else
                {
                    search_fd = !PVNode
                             || 1 < move_count;
                }

                // Step 17. Full depth search when LMR is skipped or fails high.
                if (search_fd)
                {
                    value = -depth_search<false> (pos, ss+1, -alfa-1, -alfa, new_depth, !cut_node);
                }

                // Full PV search.
                if (   PVNode
                    && (   1 == move_count
                        || (   alfa < value
                            && (   root_node
                                || value < beta))))
                {
                    (ss+1)->pv.clear ();

                    value = -depth_search<true> (pos, ss+1, -beta, -alfa, new_depth, false);
                }

                // Step 18. Undo move.
                pos.undo_move (move);

                assert(-VALUE_INFINITE < value && value < +VALUE_INFINITE);

                // Step 19. Check for the new best move.
                // Finished searching the move. If a stop or a cutoff occurred,
                // the return value of the search cannot be trusted,
                // and return immediately without updating best move, PV and TT.
                if (Threadpool.stop.load (std::memory_order::memory_order_relaxed))
                {
                    return VALUE_ZERO;
                }

                if (root_node)
                {
                    assert(std::find (thread->root_moves.begin (), thread->root_moves.end (), move) != thread->root_moves.end ());
                    auto &rm = *std::find (thread->root_moves.begin (), thread->root_moves.end (), move);
                    // First PV move or new best move?
                    if (   1 == move_count
                        || alfa < value)
                    {
                        rm.resize (1);
                        rm.insert (rm.end (), (ss+1)->pv.begin (), (ss+1)->pv.end ());
                        rm.new_value = value;
                        rm.sel_depth = thread->sel_depth;

                        // Record how often the best move has been changed in each iteration.
                        // This information is used for time management:
                        // When the best move changes frequently, allocate some more time.
                        if (   1 < move_count
                            && Limits.time_mgr_used ()
                            && Threadpool.main_thread () == thread)
                        {
                            ++Threadpool.main_thread ()->best_move_change;
                        }
                    }
                    else
                    {
                        // All other moves but the PV are set to the lowest value, this
                        // is not a problem when sorting because sort is stable and move
                        // position in the list is preserved, just the PV is pushed up.
                        rm.new_value = -VALUE_INFINITE;
                    }
                }

                // Step 20. Check best value.
                if (best_value < value)
                {
                    best_value = value;

                    if (alfa < value)
                    {
                        best_move = move;

                        // Update pv even in fail-high case.
                        if (   PVNode
                            && !root_node)
                        {
                            update_pv (ss->pv, move, (ss+1)->pv);
                        }

                        // Update alfa! Always alfa < beta
                        if (   PVNode
                            && value < beta)
                        {
                            alfa = value;
                        }
                        else
                        {
                            assert(value >= beta); // Fail high
                            ss->stats = 0;
                            break;
                        }

                    }
                }

                if (move != best_move)
                {
                    if (!capture_or_promotion)
                    {
                        quiet_moves.push_back (move);
                    }
                    else
                    {
                        capture_moves.push_back (move);
                    }
                }
            }

            assert(!in_check
                || MOVE_NONE != ss->excluded_move
                || 0 != move_count
                || 0 == MoveList<GenType::LEGAL> (pos).size ());

            // Step 21. Check for checkmate and stalemate.
            // If all possible moves have been searched and if there are no legal moves,
            // If in a singular extension search then return a fail low score (alfa).
            // Otherwise it must be a checkmate or a stalemate, so return value accordingly.
            if (0 == move_count)
            {
                best_value = MOVE_NONE != ss->excluded_move ?
                                alfa :
                                in_check ?
                                    mated_in (ss->ply) :
                                    VALUE_DRAW;
            }
            else
            // Quiet best move: update move sorting heuristics.
            if (MOVE_NONE != best_move)
            {
                auto cbonus = stat_bonus (depth + 1);
                if (!pos.capture_or_promotion (best_move))
                {
                    update_killers (ss, pos, best_move);
                    auto bonus = stat_bonus (depth + (best_value > beta + VALUE_MG_PAWN ? 1 : 0));
                    thread->butterfly_history[active][move_pp (best_move)] << bonus;
                    update_continuation_histories (ss, pos[org_sq (best_move)], dst_sq (best_move), bonus);
                    // Decrease all the other played quiet moves.
                    for (auto qm : quiet_moves)
                    {
                        thread->butterfly_history[active][move_pp (qm)] << -bonus;
                        update_continuation_histories (ss, pos[org_sq (qm)], dst_sq (qm), -bonus);
                    }
                }
                else
                {
                    thread->capture_history[pos[org_sq (best_move)]][move_pp (best_move)][pos.cap_type (best_move)] << cbonus;
                }
                // Decrease all the other played capture moves.
                for (auto cm : capture_moves)
                {
                    thread->capture_history[pos[org_sq (cm)]][move_pp (cm)][pos.cap_type (cm)] << -cbonus;
                }

                // Extra penalty for a quiet TT move or main killer move in previous ply when it gets refuted
                if (   NONE == pos.si->capture
                    && NONE == pos.si->promote
                    && (   1 == (ss-1)->move_count
                        || (ss-1)->played_move == (ss-1)->killer_moves[0]))
                {
                    update_continuation_histories (ss-1, pos[fix_dst_sq ((ss-1)->played_move)], dst_sq ((ss-1)->played_move), -stat_bonus (depth + 1));
                }
            }
            else
            // Bonus for prior countermove that caused the fail low.
            if (   (   2 < depth
                    || PVNode)
                && NONE == pos.si->capture
                && NONE == pos.si->promote)
            {
                update_continuation_histories (ss-1, pos[fix_dst_sq ((ss-1)->played_move)], dst_sq ((ss-1)->played_move), stat_bonus (depth));
            }

            if (PVNode)
            {
                if (best_value > max_value)
                {
                    best_value = max_value;
                }
            }

            if (MOVE_NONE == ss->excluded_move)
            {
                tte->save (key,
                           best_move,
                           value_to_tt (best_value, ss->ply),
                           tt_eval,
                           depth,
                           best_value >= beta ?
                               BOUND_LOWER :
                                  PVNode
                               && MOVE_NONE != best_move ?
                                   BOUND_EXACT :
                                   BOUND_UPPER,
                           tt_pv ? 4 : 0);
            }

            assert(-VALUE_INFINITE < best_value && best_value < +VALUE_INFINITE);
            return best_value;
        }

    }

    /// initialize() initializes some lookup tables.
    void initialize ()
    {
        srand ((unsigned int)(time (NULL)));

        for (i08 d = 0; d < 16; ++d)
        {
            FutilityMoveCounts[0][d] = u08(0.74 * std::pow (d, 1.78) + 2.4);
            FutilityMoveCounts[1][d] = u08(1.00 * std::pow (d, 2.00) + 5.0);
        }
        for (i08 imp = 0; imp < 2; ++imp)
        {
            ReductionDepths[0][imp][0][0] = 0;
            ReductionDepths[1][imp][0][0] = 0;
            for (i08 d = 1; d < 64; ++d)
            {
                for (i08 mc = 1; mc < 64; ++mc)
                {
                    double r = std::log (d) * std::log (mc) / 1.95;
                    ReductionDepths[0][imp][d][mc] = i16(std::round (r));
                    ReductionDepths[1][imp][d][mc] = i16(std::max (ReductionDepths[0][imp][d][mc] - 1, 0));
                    if (   0 == imp
                        && 1.0 < r)
                    {
                        ReductionDepths[0][imp][d][mc]++;
                    }
                }
            }
        }
    }
    /// clear() resets search state to its initial value.
    void clear ()
    {
        Threadpool.stop = true;
        Threadpool.main_thread ()->wait_while_busy ();
        
        TT.clear ();
        Threadpool.clear ();
        TBSyzygy::initialize (string(Options["SyzygyPath"])); // Free up mapped files
    }

}

using namespace Searcher;

/// Thread::search() is thread iterative deepening loop function.
/// It calls depth_search() repeatedly with increasing depth until
/// - Force stop requested.
/// - Allocated thinking time has been consumed.
/// - Maximum search depth is reached.
void Thread::search ()
{
    // To allow access to (ss-5) up to (ss+2), the stack must be oversized.
    // The former is needed to allow update_continuation_histories(ss-1, ...),
    // which accesses its argument at ss-4, also near the root.
    // The latter is needed for stats and killer initialization.
    Stack stacks[MaxDepth + 8];
    for (auto ss = stacks; ss < stacks + MaxDepth + 8; ++ss)
    {
        ss->ply = i16(ss - (stacks+5));
        ss->played_move = MOVE_NONE;
        ss->excluded_move = MOVE_NONE;
        std::fill_n (ss->killer_moves, 2, MOVE_NONE);
        ss->move_count = 0;
        ss->static_eval = VALUE_ZERO;
        ss->stats = 0;
        ss->pd_history = &continuation_history[NO_PIECE][0];
    }

    auto *main_thread = Threadpool.main_thread () == this ?
                            Threadpool.main_thread () :
                            nullptr;

    contempt = WHITE == root_pos.active ?
                +mk_score (BasicContempt, BasicContempt / 2) :
                -mk_score (BasicContempt, BasicContempt / 2);

    auto best_value = VALUE_ZERO
        , window = VALUE_ZERO
        , alfa = -VALUE_INFINITE
        , beta = +VALUE_INFINITE;

    // Iterative deepening loop until requested to stop or the target depth is reached.
    while (   ++running_depth < MaxDepth
           && !Threadpool.stop
           && (   nullptr == main_thread
               || DepthZero == Limits.depth
               || running_depth <= Limits.depth))
    {
        if (nullptr != main_thread)
        {
            if (Limits.time_mgr_used ())
            {
                main_thread->failed_low = false;
                // Age out PV variability metric
                main_thread->best_move_change *= 0.517;
            }
        }
        else
        {
            // Thread Redistribution Scheme: distribute search depths across the threads.
            assert(0 != index);
            i32 i = (index - 1) % SkipIndex;
            if (0 != ((running_depth + SkipPhase[i]) / SkipSize[i]) % 2)
            {
                continue;
            }
        }

        // Save the last iteration's values before first PV line is searched and
        // all the move scores except the (new) PV are set to -VALUE_INFINITE.
        for (auto &rm : root_moves)
        {
            rm.old_value = rm.new_value;
        }

        size_t pv_beg = 0;
        pv_end = 0;

        // MultiPV loop. Perform a full root search for each PV line.
        for (pv_cur = 0; !Threadpool.stop && pv_cur < Threadpool.pv_limit; ++pv_cur)
        {
            if (pv_cur == pv_end)
            {
                for (pv_beg = pv_end++; pv_end < root_moves.size (); ++pv_end)
                {
                    if (root_moves[pv_beg].tb_rank != root_moves[pv_end].tb_rank)
                    {
                        break;
                    }
                }
            }

            // Reset UCI info sel_depth for each depth and each PV line
            sel_depth = DepthZero;

            // Reset aspiration window starting size.
            if (4 < running_depth)
            {
                auto old_value = root_moves[pv_cur].old_value;
                window = Value(20);
                alfa = std::max (old_value - window, -VALUE_INFINITE);
                beta = std::min (old_value + window, +VALUE_INFINITE);

                // Dynamic contempt
                auto contempt_value = i32(Options["Contempt Value"]);
                if (0 != contempt_value)
                {
                    i32 dynamic_contempt = BasicContempt + i32(((880 / contempt_value) * old_value) / (abs (old_value) + 200));
                    contempt = WHITE == root_pos.active ?
                                +mk_score (dynamic_contempt, dynamic_contempt / 2) :
                                -mk_score (dynamic_contempt, dynamic_contempt / 2);
                }
            }

            // Start with a small aspiration window and, in case of fail high/low,
            // research with bigger window until not failing high/low anymore.
            i16 failed_high_count = 0;
            do
            {
                i16 adjusted_depth = i16(std::max (running_depth - failed_high_count, 1));
                best_value = depth_search<true> (root_pos, stacks+5, alfa, beta, adjusted_depth, false);

                // Bring the best move to the front. It is critical that sorting is
                // done with a stable algorithm because all the values but the first
                // and eventually the new best one are set to -VALUE_INFINITE and
                // want to keep the same order for all the moves but the new PV
                // that goes to the front. Note that in case of MultiPV search
                // the already searched PV lines are preserved.
                std::stable_sort (root_moves.begin () + pv_cur, root_moves.begin () + pv_end);

                // If search has been stopped, break immediately.
                // Sorting is safe because RootMoves is still valid, although it refers to the previous iteration.
                if (Threadpool.stop)
                {
                    break;
                }

                // Give some update before to re-search.
                if (   nullptr != main_thread
                    && 1 == Threadpool.pv_limit
                    && (best_value <= alfa || beta <= best_value)
                    && main_thread->time_mgr.elapsed_time () > 3000)
                {
                    sync_cout << multipv_info (main_thread, running_depth, alfa, beta) << sync_endl;
                }

                // If fail low set new bounds.
                if (best_value <= alfa)
                {
                    beta = (alfa + beta) / 2;
                    alfa = std::max (best_value - window, -VALUE_INFINITE);

                    if (nullptr != main_thread)
                    {
                        failed_high_count = 0;
                        if (Limits.time_mgr_used ())
                        {
                            main_thread->failed_low = true;
                        }
                        main_thread->stop_on_ponderhit = false;
                    }
                }
                else
                // If fail high set new bounds.
                if (beta <= best_value)
                {
                    // NOTE:: Don't change alfa = (alfa + beta) / 2
                    beta = std::min (best_value + window, +VALUE_INFINITE);

                    if (nullptr != main_thread)
                    {
                        ++failed_high_count;
                    }
                }
                // Otherwise exit the loop.
                else
                {
                    break;
                }

                window += window / 4 + 5;

                assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            } while (true);

            // Sort the PV lines searched so far and update the GUI.
            std::stable_sort (root_moves.begin () + pv_beg, root_moves.begin () + pv_cur + 1);

            if (   nullptr != main_thread
                && (   Threadpool.stop
                    || Threadpool.pv_limit - 1 == pv_cur
                    || main_thread->time_mgr.elapsed_time () > 3000))
            {
                sync_cout << multipv_info (main_thread, running_depth, alfa, beta) << sync_endl;
            }
        }

        if (!Threadpool.stop)
        {
            finished_depth = running_depth;
        }

        // Has any of the threads found a "mate in <x>"?
        if (   0 != Limits.mate
            && !Limits.time_mgr_used ()
            && !Threadpool.stop
            && best_value >= +VALUE_MATE - 2 * Limits.mate)
        {
            Threadpool.stop = true;
        }

        if (nullptr != main_thread)
        {
            // If skill level is enabled and can pick move, pick a sub-optimal best move.
            if (   skill_mgr_enabled ()
                && running_depth == i16(i32(Options["Skill Level"])) + 1)
            {
                main_thread->skill_mgr.best_move = MOVE_NONE;
                main_thread->skill_mgr.pick_best_move (i16(i32(Options["Skill Level"])));
            }

            if (   Limits.time_mgr_used ()
                && !Threadpool.stop
                && !main_thread->stop_on_ponderhit)
            {
                if (main_thread->best_move != root_moves[0][0])
                {
                    main_thread->best_move = root_moves[0][0];
                    main_thread->best_move_depth = running_depth;
                }

                // Reduce time if the best_move is stable over 10 iterations
                double time_reduction = main_thread->best_move_depth + 10 < finished_depth ?
                                            1.953125 :
                                            1.00;
                // Stop the search
                // -If there is only one legal move available
                // -If all of the available time has been used
                if (   1 == root_moves.size ()
                    || (main_thread->time_mgr.elapsed_time () >
                        main_thread->time_mgr.optimum_time
                        // Best Move Instability factor
                     * (main_thread->best_move_change + 1)
                        // Time reduction factor - Use part of the gained time from a previous stable move for the current move
                     * std::pow (main_thread->last_time_reduction, 0.528) / time_reduction
                        // Falling Eval factor
                     * std::min (1.5,
                        std::max (0.5,
                                  (306
                                 + 119 * (main_thread->failed_low ? 1 : 0)
                                 +   6 * (VALUE_NONE != main_thread->last_value ? main_thread->last_value - best_value: 0)) / 581.0))))
                {
                    // If allowed to ponder do not stop the search now but
                    // keep pondering until GUI sends "stop"/"ponderhit".
                    if (main_thread->ponder)
                    {
                        main_thread->stop_on_ponderhit = true;
                    }
                    else
                    {
                        Threadpool.stop = true;
                    }
                }

                main_thread->last_time_reduction = time_reduction;
            }

            if (OutputStream.is_open ())
            {
                OutputStream << pretty_pv_info (main_thread) << std::endl;
            }
        }
    }
}

/// MainThread::search() is main thread search function.
/// It searches from root position and outputs the "bestmove"/"ponder".
void MainThread::search ()
{
    assert(Threadpool.main_thread () == this
        && 0 == index);

    time_mgr.start_time = now ();
    DebugTime = 0;

    auto output_fn = string(Options["Output File"]);
    if (!white_spaces (output_fn))
    {
        OutputStream.open (output_fn, ios_base::out|ios_base::app);
        if (OutputStream.is_open ())
        {
            OutputStream << std::boolalpha
                         << "RootPos  : " << root_pos.fen () << "\n"
                         << "MaxMoves : " << root_moves.size () << "\n"
                         << "ClockTime: " << Limits.clock[root_pos.active].time << " ms\n"
                         << "ClockInc : " << Limits.clock[root_pos.active].inc << " ms\n"
                         << "MovesToGo: " << Limits.movestogo+0 << "\n"
                         << "MoveTime : " << Limits.movetime << " ms\n"
                         << "Depth    : " << Limits.depth << "\n"
                         << "Infinite : " << Limits.infinite << "\n"
                         << "Ponder   : " << ponder << "\n"
                         << " Depth Score    Time       Nodes PV\n"
                         << "-----------------------------------------------------------"
                         << std::noboolalpha << std::endl;
        }
    }

    if (Limits.time_mgr_used ())
    {
        // Set the time manager before searching.
        time_mgr.set (root_pos.active,
                      root_pos.ply,
                      u16(i32(Options["Nodes Time"])),
                      TimePoint(i32(Options["Minimum Move Time"])),
                      TimePoint(i32(Options["Overhead Move Time"])),
                      i32(Options["Move Slowness"]) / 100.0,
                      bool(Options["Ponder"]));
    }

    TEntry::Generation = u08((root_pos.ply + 1) << 3);
    assert(0 == (TEntry::Generation & 0x07));

    bool think = true;

    if (root_moves.empty ())
    {
        think = false;

        root_moves += MOVE_NONE;

        sync_cout << "info"
                  << " depth " << 0
                  << " score " << to_string (0 != root_pos.si->checkers ? -VALUE_MATE : VALUE_DRAW)
                  << " time "  << 0 << sync_endl;
    }
    else
    {
        if (   !Limits.infinite
            && !Limits.mate_search ()
            && bool(Options["Use Book"]))
        {
            auto book_bm = Book.probe (root_pos, i16(i32(Options["Book Move Num"])), bool(Options["Book Pick Best"]));
            if (MOVE_NONE != book_bm)
            {
                auto itr = std::find (root_moves.begin (), root_moves.end (), book_bm);
                if (itr != root_moves.end ())
                {
                    think = false;
                    std::swap (root_moves[0], *itr);
                    root_moves[0].new_value = VALUE_NONE;
                    StateInfo si;
                    root_pos.do_move (book_bm, si);
                    auto book_pm = Book.probe (root_pos, i16(i32(Options["Book Move Num"])), bool(Options["Book Pick Best"]));
                    if (MOVE_NONE != book_pm)
                    {
                        root_moves[0] += book_pm;
                    }
                    root_pos.undo_move (book_bm);
                }
            }
        }

        if (think)
        {
            i16 timed_contempt = 0;
            i64 diff_time;
            auto contempt_time = i32(Options["Contempt Time"]);
            if (   Limits.time_mgr_used ()
                && 0 != contempt_time
                && 0 != (diff_time = (i64(Limits.clock[ root_pos.active].time)
                                    - i64(Limits.clock[~root_pos.active].time)) / 1000))
            {
                timed_contempt = i16(diff_time/contempt_time);
            }

            BasicContempt = i32(cp_to_value (i16(i32(Options["Fixed Contempt"])) + timed_contempt));
            // In analysis mode, adjust contempt in accordance with user preference
            if (   Limits.infinite
                || bool(Options["UCI_AnalyseMode"]))
            {
                BasicContempt = Options["Analysis Contempt"] == "Off"                               ? 0 :
                                Options["Analysis Contempt"] == "White" && BLACK == root_pos.active ? -BasicContempt :
                                Options["Analysis Contempt"] == "Black" && WHITE == root_pos.active ? -BasicContempt :
                                /*Options["Analysis Contempt"] == "Both"                            ? +BasicContempt :*/ +BasicContempt;
            }

            if (Limits.time_mgr_used ())
            {
                failed_low = false;
                best_move_change = 0.0;
                best_move = MOVE_NONE;
                best_move_depth = DepthZero;
            }

            if (skill_mgr_enabled ())
            {
                skill_mgr.best_move = MOVE_NONE;
            }

            // Have to play with skill handicap?
            // In this case enable MultiPV search by skill pv size
            // that will use behind the scenes to get a set of possible moves.
            Threadpool.pv_limit = std::min (size_t(std::max (i32(Options["MultiPV"]), skill_mgr_enabled () ? 4 : 1)), root_moves.size ());
            assert(0 < Threadpool.pv_limit);

            set_check_count ();

            for (auto *th : Threadpool)
            {
                if (th != this)
                {
                    th->start ();
                }
            }

            Thread::search (); // Let's start searching !

            // Swap best PV line with the sub-optimal one if skill level is enabled
            if (skill_mgr_enabled ())
            {
                skill_mgr.pick_best_move (i16(i32(Options["Skill Level"])));
                std::swap (root_moves[0], *std::find (root_moves.begin (), root_moves.end (), skill_mgr.best_move));
            }
        }
    }

    // When reach the maximum depth, can arrive here without a raise of Threads.stop.
    // However, if in an infinite search or pondering, shouldn't print the best move
    // before receiving a "stop"/"ponderhit" command. Therefore simply wait here until
    // receives one of those commands (which also raises Threads.stop).
    // Busy wait for a "stop"/"ponderhit" command.
    while (   (   ponder
               || Limits.infinite)
           && !Threadpool.stop)
    {} // Busy wait for a stop or a ponder reset

    Thread *best_thread = this;
    if (think)
    {
        // Stop the threads if not already stopped (Also raise the stop if "ponderhit" just reset Threads.ponder).
        Threadpool.stop = true;
        // Wait until all threads have finished.
        for (auto *th : Threadpool)
        {
            if (th != this)
            {
                th->wait_while_busy ();
            }
        }
        // Check if there is better thread than main thread.
        if (   1 == Threadpool.pv_limit
            && DepthZero == Limits.depth // Depth limit search don't use deeper thread
            && MOVE_NONE != root_moves[0][0]
            && !skill_mgr_enabled ())
        {
            std::map<Move, i64> votes;

            auto min_value = root_moves[0].new_value;
            // Find out minimum value and reset votes for moves which can be voted
            for (auto *th : Threadpool)
            {
                min_value = std::min (th->root_moves[0].new_value, min_value);
            }
            // Vote according to value and depth
            for (auto *th : Threadpool)
            {
                i64 s = th->root_moves[0].new_value - min_value + 1;
                votes[th->root_moves[0][0]] += 200 + s * s * th->finished_depth;
            }
            // Select best thread
            auto best_vote = votes[root_moves[0][0]];
            for (auto *th : Threadpool)
            {
                if (best_vote < votes[th->root_moves[0][0]])
                {
                    best_vote = votes[th->root_moves[0][0]];
                    best_thread = th;
                }
            }
            // If new best thread then send PV info again.
            if (best_thread != this)
            {
                sync_cout << multipv_info (best_thread, best_thread->finished_depth, -VALUE_INFINITE, +VALUE_INFINITE) << sync_endl;
            }
        }
    }

    assert(!best_thread->root_moves.empty ()
        && !best_thread->root_moves[0].empty ());

    auto &rm = best_thread->root_moves[0];

    if (Limits.time_mgr_used ())
    {
        // Update the time manager after searching.
        time_mgr.update (root_pos.active);
        last_value = rm.new_value;
    }

    auto bm = rm[0];
    auto pm = MOVE_NONE != bm ?
                1 < rm.size () ?
                    rm[1] :
                    TT.extract_pm (root_pos, bm) :
                MOVE_NONE;
    assert(MOVE_NONE != bm
        || (MOVE_NONE == bm
         && MOVE_NONE == pm));

    if (OutputStream.is_open ())
    {
        auto total_nodes = Threadpool.nodes ();
        auto elapsed_time = std::max (time_mgr.elapsed_time (), TimePoint(1));
        OutputStream << "Nodes      : " << total_nodes << " N\n"
                     << "Time       : " << elapsed_time << " ms\n"
                     << "Speed      : " << total_nodes * 1000 / elapsed_time << " N/s\n"
                     << "Hash-full  : " << TT.hash_full () << "\n"
                     << "Best Move  : " << move_to_san (bm, root_pos) << "\n"
                     << "Ponder Move: ";
        if (MOVE_NONE != bm)
        {
            StateInfo si;
            root_pos.do_move (bm, si);
            OutputStream << move_to_san (pm, root_pos);
            root_pos.undo_move (bm);
        }
        else
        {
            OutputStream << "(none)";
        }
        OutputStream << std::endl;
        OutputStream.close ();
    }

    // Best move could be MOVE_NONE when searching on a stalemate position.
    sync_cout << "bestmove " << bm << " ponder " << pm << sync_endl;
}
/// MainThread::set_check_count()
void MainThread::set_check_count ()
{
    //assert(0 == check_count);
    // At low node count increase the checking rate otherwise use a default value.
    check_count = 0 != Limits.nodes ? 
                    std::min ((i32)std::max (Limits.nodes / 1024, u64(1)), 1024) :
                    1024;
    assert(0 != check_count);
}
/// MainThread::tick() is used as timer function.
/// Used to detect when out of available limit and thus stop the search, also print debug info.
void MainThread::tick ()
{
    if (0 < --check_count)
    {
        return;
    }
    set_check_count ();

    auto elapsed_time = time_mgr.elapsed_time ();

    if (DebugTime + 1000 <= elapsed_time)
    {
        DebugTime = elapsed_time;

        debug_print ();
    }

    // Do not stop until told so by the GUI.
    if (ponder)
    {
        return;
    }

    if (   (   Limits.time_mgr_used ()
            && (   stop_on_ponderhit
                || time_mgr.maximum_time < elapsed_time + 10))
        || (   0 != Limits.movetime
            && Limits.movetime <= elapsed_time)
        || (   0 != Limits.nodes
            && Limits.nodes <= Threadpool.nodes ()))
    {
        Threadpool.stop = true;
    }
}
