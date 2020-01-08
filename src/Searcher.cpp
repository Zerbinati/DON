#include "Searcher.h"

#include <cmath>
#include <cstdlib>
#include <ctime>
#include <iterator>

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
RootMove::operator string() const
{
    ostringstream oss;
    for (auto move : *this)
    {
        assert(MOVE_NONE != move);
        oss << " " << move;
    }
    return oss.str();
}
/// RootMoves::operator string()
RootMoves::operator string() const
{
    ostringstream oss;
    std::copy(begin(), end(), ostream_iterator<RootMove>(oss, "\n"));
    return oss.str();
}

namespace Searcher {

    Limit Limits;

    Depth TBProbeDepth = 1;
    i32   TBLimitPiece = 6;
    bool  TBUseRule50 = true;
    bool  TBHasRoot = false;

    namespace {

        /// Stack keeps the information of the nodes in the tree during the search.
        struct Stack
        {
        public:
            i16   ply;
            Move  played_move;
            Move  excluded_move;
            array<Move, 2> killer_moves;

            u08   move_count;
            Value static_eval;
            i32   stats;
            PieceDestinyHistory *pd_history;

            list<Move> pv;
        };

        /// MovePicker class is used to pick one legal moves from the current position.
        class MovePicker
        {
        private:
            enum Stage : u08
            {
                NT_TT, NT_INIT, NT_GOOD_CAPTURES, NT_REFUTATIONS, NT_QUIETS, NT_BAD_CAPTURES,
                EV_TT, EV_INIT, EV_MOVES,
                PC_TT, PC_INIT, PC_CAPTURES,
                QS_TT, QS_INIT, QS_CAPTURES, QS_CHECKS,
            };

            Position const &pos;

            Move    tt_move;
            Depth   depth;

            array<PieceDestinyHistory const*, 6> pd_histories;

            Value   threshold;
            Square  recap_sq;

            ValMoves vmoves;
            ValMoves::iterator vmItr;

            std::vector<Move> refutation_moves
                ,             bad_capture_moves;
            std::vector<Move>::iterator mItr;

            u08 stage;

            /// value() assigns a numerical value to each move in a list, used for sorting.
            /// Captures are ordered by Most Valuable Victim (MVV) with using the histories.
            /// Quiets are ordered using the histories.
            template<GenType GT>
            void value()
            {
                static_assert (GenType::CAPTURE == GT
                            || GenType::QUIET == GT
                            || GenType::EVASION == GT, "GT incorrect");

                auto *thread = pos.thread;

                for (auto &vm : vmoves)
                {
                    if (GenType::CAPTURE == GT)
                    {
                        assert(pos.capture_or_promotion(vm.move));
                        vm.value = i32(PieceValues[MG][pos.cap_type(vm.move)]) * 6
                                 + thread->capture_history[pos[org_sq(vm.move)]][dst_sq(vm.move)][pos.cap_type(vm.move)];
                    }
                    else
                    if (GenType::QUIET == GT)
                    {
                        vm.value = thread->butterfly_history[pos.active][move_index(vm.move)]
                                 + (*pd_histories[0])[pos[org_sq(vm.move)]][dst_sq(vm.move)] * 2
                                 + (*pd_histories[1])[pos[org_sq(vm.move)]][dst_sq(vm.move)] * 2
                                 + (*pd_histories[3])[pos[org_sq(vm.move)]][dst_sq(vm.move)] * 2
                                 + (*pd_histories[5])[pos[org_sq(vm.move)]][dst_sq(vm.move)];
                        if (vm.value < threshold)
                            vm.value = threshold - 1;
                    }
                    else // GenType::EVASION == GT
                    {
                        vm.value = pos.capture(vm.move) ?
                                       i32(PieceValues[MG][pos.cap_type(vm.move)])
                                     - ptype(pos[org_sq(vm.move)]) :
                                       thread->butterfly_history[pos.active][move_index(vm.move)]
                                     + (*pd_histories[0])[pos[org_sq(vm.move)]][dst_sq(vm.move)]
                                     - (0x10000000);
                    }
                }
            }

            /// pick() returns the next move satisfying a predicate function
            template<typename Pred>
            bool pick(Pred filter)
            {
                while (vmItr != vmoves.end())
                {
                    std::swap(*vmItr, *std::max_element(vmItr, vmoves.end()));
                    if (   tt_move != vmItr->move
                        && (   (   ENPASSANT != mtype(vmItr->move)
                                && !contains(pos.si->king_blockers[pos.active] | pos.square(pos.active|KING), org_sq(vmItr->move)))
                            || pos.legal(vmItr->move))
                        && filter ())
                    {
                        return ++vmItr, true;
                    }
                    ++vmItr;
                }
                return false;
            }

        public:

            bool skip_quiets;

            MovePicker() = delete;
            MovePicker(MovePicker const&) = delete;
            MovePicker& operator=(MovePicker const&) = delete;

            /// MovePicker constructor for the main search
            MovePicker(Position const &p, Move ttm, Depth d, array<PieceDestinyHistory const*, 6> const &pdhs, array<Move, 2> const &km, Move cm)
                : pos(p)
                , tt_move(ttm)
                , depth(d)
                , pd_histories(pdhs)
                , threshold(Value(-3000 * d))
                , refutation_moves{ km[0], km[1], cm }
                , skip_quiets(false)
            {
                assert(MOVE_NONE == tt_move
                   || (pos.pseudo_legal(tt_move)
                    && pos.legal(tt_move)));
                assert(DEP_ZERO < depth);

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
            /// Because the depth <= DEP_ZERO here, only captures, queen promotions
            /// and quiet checks (only if depth >= DEP_QS_CHECK) will be generated.
            MovePicker(Position const &p, Move ttm, Depth d, array<PieceDestinyHistory const*, 6> const &pdhs, Square rs)
                : pos(p)
                , tt_move(ttm)
                , depth(d)
                , pd_histories(pdhs)
                , recap_sq(rs)
            {
                assert(MOVE_NONE == tt_move
                    || (pos.pseudo_legal(tt_move)
                     && pos.legal(tt_move)));
                assert(DEP_ZERO >= depth);

                if (0 != pos.si->checkers)
                {
                    stage = Stage::EV_TT;
                }
                else
                {
                    stage = Stage::QS_TT;

                    if (   MOVE_NONE != tt_move
                        && !(   DEP_QS_RECAP < depth
                             || dst_sq(tt_move) == recap_sq))
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
            MovePicker(Position const &p, Move ttm, Value thr)
                : pos(p)
                , tt_move(ttm)
                , threshold(thr)
            {
                assert(0 == pos.si->checkers);
                assert(MOVE_NONE == tt_move
                    || (pos.pseudo_legal(tt_move)
                     && pos.legal(tt_move)));

                stage = Stage::PC_TT;

                if (   MOVE_NONE != tt_move
                    && !(   pos.capture(tt_move)
                         && pos.see_ge(tt_move, threshold)))
                {
                    tt_move = MOVE_NONE;
                }

                if (MOVE_NONE == tt_move)
                {
                    ++stage;
                }
            }

            /// next_move() is the most important method of the MovePicker class.
            /// It returns a new legal move every time it is called, until there are no more moves left.
            /// It picks the move with the biggest value from a list of generated moves
            /// taking care not to return the tt_move if it has already been searched.
            Move next_move()
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
                    generate<GenType::CAPTURE>(vmoves, pos);
                    value<GenType::CAPTURE>();
                    vmItr = vmoves.begin();
                    ++stage;
                    // Re-branch at the top of the switch
                    goto restage;

                case Stage::NT_GOOD_CAPTURES:
                    if (pick([&]() { if (pos.see_ge(vmItr->move, Value(-(vmItr->value) * 55 / 1024)))
                                     {
                                         return true;
                                     }
                                     else
                                     {
                                         // Put losing capture to bad_capture_moves to be tried later
                                         bad_capture_moves.push_back(vmItr->move);
                                         return false;
                                     } }))
                    {
                        return (vmItr-1)->move;
                    }
                    // If the countermove is the same as a killers, skip it
                    if (   MOVE_NONE != refutation_moves[2]
                        && (   refutation_moves[0] == refutation_moves[2]
                            || refutation_moves[1] == refutation_moves[2]))
                    {
                        refutation_moves.erase(std::next(refutation_moves.begin(), 2));
                    }
                    refutation_moves.erase(std::remove_if(refutation_moves.begin(), refutation_moves.end(),
                                                            [&](Move m) { return MOVE_NONE == m
                                                                              || tt_move == m
                                                                              || pos.capture(m)
                                                                              || !pos.pseudo_legal(m)
                                                                              || !pos.legal(m); }),
                                            refutation_moves.end());
                    mItr = refutation_moves.begin();
                    ++stage;
                    /* fall through */
                case NT_REFUTATIONS:
                    // Refutation moves: Killers, Counter moves
                    if (mItr != refutation_moves.end())
                    {
                        return *mItr++;
                    }
                    if (!skip_quiets)
                    {
                        generate<GenType::QUIET>(vmoves, pos);
                        value<GenType::QUIET>();
                        vmItr = vmoves.begin();
                    }
                    ++stage;
                    /* fall through */
                case Stage::NT_QUIETS:
                    if (   !skip_quiets
                        && pick([&]() { return std::find(refutation_moves.begin(),
                                                         refutation_moves.end(), vmItr->move)
                                                      == refutation_moves.end(); }))
                    {
                        return (vmItr-1)->move;
                    }
                    mItr = bad_capture_moves.begin();
                    ++stage;
                    /* fall through */
                case Stage::NT_BAD_CAPTURES:
                    return mItr != bad_capture_moves.end() ?
                            *mItr++ :
                            MOVE_NONE;
                    /* end */

                case Stage::EV_INIT:
                    generate<GenType::EVASION>(vmoves, pos);
                    value<GenType::EVASION>();
                    vmItr = vmoves.begin();
                    ++stage;
                    /* fall through */
                case Stage::EV_MOVES:
                    return pick([]() { return true; }) ?
                            (vmItr-1)->move :
                            MOVE_NONE;
                    /* end */

                case Stage::PC_CAPTURES:
                    return pick([&]() { return pos.see_ge(vmItr->move, threshold); }) ?
                            (vmItr-1)->move :
                            MOVE_NONE;
                    /* end */

                case Stage::QS_CAPTURES:
                    if (pick([&]() { return DEP_QS_RECAP < depth
                                         || dst_sq(vmItr->move) == recap_sq; }))
                    {
                        return (vmItr-1)->move;
                    }
                    // If did not find any move then do not try checks, finished.
                    if (DEP_QS_CHECK > depth)
                    {
                        return MOVE_NONE;
                    }

                    generate<GenType::QUIET_CHECK>(vmoves, pos);
                    vmoves.erase(std::remove_if(vmoves.begin(), vmoves.end(),
                                                [&](ValMove &vm) { return tt_move == vm
                                                                       //|| !pos.pseudo_legal(vm)
                                                                       || !pos.legal(vm); }),
                                 vmoves.end());
                    vmItr = vmoves.begin();
                    ++stage;
                    /* fall through */
                case Stage::QS_CHECKS:
                    return vmItr != vmoves.end() ?
                            *vmItr++ :
                            MOVE_NONE;
                    /* end */

                default:
                    assert(false);
                    break;
                }
                return MOVE_NONE;
            }
        };

        /// Breadcrumbs are used to pair thread and position key
        struct Breadcrumb
        {
            std::atomic<Thread const*> thread;
            std::atomic<Key>           posi_key;

            void store(Thread const *th, Key key)
            {
                thread.store(th, std::memory_order::memory_order_relaxed);
                posi_key.store(key, std::memory_order::memory_order_relaxed);
            }
        };

        array<Breadcrumb, 1024> Breadcrumbs;

        /// ThreadMarker structure keeps track of which thread left breadcrumbs at the given
        /// node for potential reductions. A free node will be marked upon entering the moves
        /// loop by the constructor, and unmarked upon leaving that loop by the destructor.
        class ThreadMarker
        {
        private:
            Breadcrumb *breadcrumb;

        public:

            bool marked;

            explicit ThreadMarker(Thread const *thread, Key posi_key, i16 ply)
                : breadcrumb(nullptr)
                , marked(false)
            {
                auto *bc = ply < 8 ?
                            &Breadcrumbs[posi_key & (Breadcrumbs.size() - 1)] :
                            nullptr;
                if (nullptr != bc)
                {
                    // Check if another already marked it, if not, mark it.
                    auto *th = bc->thread.load(std::memory_order::memory_order_relaxed);
                    if (nullptr == th)
                    {
                        bc->store(thread, posi_key);
                        breadcrumb = bc;
                    }
                    else
                    {
                        if (   th != thread
                            && bc->posi_key.load(std::memory_order::memory_order_relaxed) == posi_key)
                        {
                            marked = true;
                        }
                    }
                }
            }

            virtual ~ThreadMarker()
            {
                if (nullptr != breadcrumb) // Free the marked one.
                {
                    breadcrumb->store(nullptr, 0);
                }
            }
        };

        u64 constexpr ttHitAverageWindow = 4096;
        u64 constexpr ttHitAverageResolution = 1024;

        // Razor margin
        Value constexpr RazorMargin = Value(531);
        // Futility margin
        Value constexpr futility_margin(bool imp, Depth d)
        {
            return Value(217 * (d - (imp)));
        }
        // Futility move count threshold
        i16 constexpr futility_move_count(bool imp, Depth d)
        {
            return (5 + d * d) * (1 + (imp)) / 2 - 1;
        }

        Depth reduction(bool imp, Depth d, u08 mc)
        {
            if (0 == d
             || 0 == mc)
            {
                return DEP_ZERO;
            }

            auto r = Threadpool.factor * std::log(d) * std::log(mc);
            return Depth((r + 511) / 1024 + (!imp && r > 1007));
        }

        i32 BasicContempt = 0;

        TimePoint DebugTime;
        bool      Output;
        ofstream  OutputStream;

        /// stat_bonus() is the bonus, based on depth
        i32 stat_bonus(Depth depth)
        {
            return 15 >= depth ? (19 * depth + 155) * depth - 132 : -8;
        }

        // Add a small random component to draw evaluations to keep search dynamic and to avoid 3-fold-blindness.
        Value draw_value()
        {
            return VALUE_DRAW + rand() % 3 - 1;
        }

        /// update_continuation_histories() updates tables of the move pairs with current move.
        void update_continuation_histories(Stack *const &ss, Piece pc, Square dst, i32 bonus)
        {
            for (auto const *const &s : { ss-1, ss-2, ss-4, ss-6 })
            {
                if (_ok(s->played_move))
                {
                    (*s->pd_history)[pc][dst] << bonus;
                }
            }
        }
        /// update_quiet_stats() updates move sorting heuristics when a new quiet best move is found
        void update_quiet_stats(Stack *const &ss, Position const &pos, Move move, i32 bonus)
        {
            if (ss->killer_moves[0] != move)
            {
                ss->killer_moves[1] = ss->killer_moves[0];
                ss->killer_moves[0] = move;
            }
            assert(1 == std::count(ss->killer_moves.begin(), ss->killer_moves.end(), move));

            if (_ok((ss-1)->played_move))
            {
                pos.thread->move_history[pos[dst_sq((ss-1)->played_move)]][move_index((ss-1)->played_move)] = move;
            }

            pos.thread->butterfly_history[pos.active][move_index(move)] << bonus;
            if (PAWN != ptype(pos[org_sq(move)]))
            {
                pos.thread->butterfly_history[pos.active][move_index(reverse_move(move))] << -bonus;
            }
            update_continuation_histories(ss, pos[org_sq(move)], dst_sq(move), bonus);
        }

        /// update_pv() appends the move and child pv
        void update_pv(list<Move> &pv, Move move, list<Move> const &child_pv)
        {
            pv.assign(child_pv.begin(), child_pv.end());
            pv.push_front(move);
            assert(pv.front() == move
                && ((pv.size() == 1 && child_pv.empty())
                 || (pv.back() == child_pv.back() && !child_pv.empty())));
        }

        /// quien_search() is quiescence search function, which is called by the main depth limited search function when the remaining depth <= 0.
        template<bool PVNode>
        Value quien_search(Position &pos, Stack *const &ss, Value alfa, Value beta, Depth depth = DEP_ZERO)
        {
            assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            assert(PVNode || (alfa == beta-1));
            assert(DEP_ZERO >= depth);

            Value prev_alfa;

            if (PVNode)
            {
                prev_alfa = alfa; // To flag BOUND_EXACT when eval above alpha and no available moves
                ss->pv.clear();
            }

            bool in_check = 0 != pos.si->checkers;

            // Check for maximum ply reached or immediate draw.
            if (   ss->ply >= DEP_MAX
                || pos.draw(ss->ply))
            {
                return ss->ply >= DEP_MAX
                    && !in_check ?
                           evaluate(pos) :
                           VALUE_DRAW;
            }

            assert(ss->ply >= 1
                && ss->ply == (ss-1)->ply + 1
                && ss->ply < DEP_MAX);

            // Transposition table lookup.
            Key key = pos.si->posi_key;
            bool tt_hit;
            auto *tte = TT.probe(key, tt_hit);
            auto tt_move = tt_hit ?
                            tte->move() :
                            MOVE_NONE;
            auto tt_value = tt_hit ?
                            value_of_tt(tte->value(), ss->ply, pos.si->clock_ply) :
                            VALUE_NONE;
            auto tt_pv = tt_hit
                      && tte->is_pv();

            // Decide whether or not to include checks.
            // Fixes also the type of TT entry depth that are going to use.
            // Note that in quien_search use only 2 types of depth: DEP_QS_CHECK or DEP_QS_NO_CHECK.
            Depth qs_depth = in_check
                        || DEP_QS_CHECK <= depth ?
                            DEP_QS_CHECK :
                            DEP_QS_NO_CHECK;

            if (   !PVNode
                && VALUE_NONE != tt_value // Handle tt_hit
                && qs_depth <= tte->depth()
                && BOUND_NONE != (tte->bound() & (tt_value >= beta ? BOUND_LOWER : BOUND_UPPER)))
            {
                return tt_value;
            }

            Value best_value
                , futility_base;

            auto *thread = pos.thread;
            auto best_move = MOVE_NONE;
            StateInfo si;

            // Evaluate the position statically.
            if (in_check)
            {
                ss->static_eval = VALUE_NONE;
                // Starting from the worst case which is checkmate
                best_value = futility_base = -VALUE_INFINITE;
            }
            else
            {
                if (tt_hit)
                {
                    ss->static_eval = best_value = tte->eval();
                    // Never assume anything on values stored in TT.
                    if (VALUE_NONE == best_value)
                    {
                        ss->static_eval = best_value = evaluate(pos);
                    }

                    // Can tt_value be used as a better position evaluation?
                    if (   VALUE_NONE != tt_value
                        && BOUND_NONE != (tte->bound() & (tt_value > best_value ? BOUND_LOWER : BOUND_UPPER)))
                    {
                        best_value = tt_value;
                    }
                }
                else
                {
                    if (MOVE_NULL != (ss-1)->played_move)
                    {
                        ss->static_eval = best_value = evaluate(pos);
                    }
                    else
                    {
                        ss->static_eval = best_value = -(ss-1)->static_eval + 2*Tempo;
                    }
                }

                if (alfa < best_value)
                {
                    // Stand pat. Return immediately if static value is at least beta.
                    if (best_value >= beta)
                    {
                        if (!tt_hit)
                        {
                            tte->save(key,
                                      MOVE_NONE,
                                      value_to_tt(best_value, ss->ply),
                                      ss->static_eval,
                                      DEP_NONE,
                                      BOUND_LOWER,
                                      tt_pv);
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

                futility_base = best_value + 154;
            }

            if (   MOVE_NONE != tt_move
                && !(   pos.pseudo_legal(tt_move)
                     && pos.legal(tt_move)))
            {
                tt_move = MOVE_NONE;
            }

            Move move;
            u08 move_count = 0;

            array<PieceDestinyHistory const*, 6> const pd_histories
            {
                (ss-1)->pd_history,
                (ss-2)->pd_history,
                nullptr,//(ss-3)->pd_history,
                (ss-4)->pd_history,
                nullptr,//(ss-5)->pd_history,
                (ss-6)->pd_history,
            };
            auto recap_sq = _ok((ss-1)->played_move) ?
                                dst_sq((ss-1)->played_move) :
                                SQ_NO;
            // Initialize move picker (2) for the current position
            MovePicker move_picker(pos, tt_move, depth, pd_histories, recap_sq);
            // Loop through the moves until no moves remain or a beta cutoff occurs
            while (MOVE_NONE != (move = move_picker.next_move()))
            {
                assert(pos.pseudo_legal(move)
                    && pos.legal(move));

                ++move_count;

                auto org = org_sq(move);
                auto dst = dst_sq(move);

                auto mpc = pos[org];
                bool gives_check = pos.gives_check(move);
                bool capture_or_promotion = pos.capture_or_promotion(move);

                // Futility pruning
                if (   !in_check
                    && !gives_check
                    && !Limits.mate_on()
                    && -VALUE_KNOWN_WIN < futility_base
                    && !pos.pawn_advance_at(pos.active, org))
                {
                    assert(ENPASSANT != mtype(move)); // Due to !pos.pawn_advance_at

                    // Futility pruning parent node
                    auto futility_value = futility_base + PieceValues[EG][CASTLE != mtype(move) ? ptype(pos[dst]) : NONE];
                    if (futility_value <= alfa)
                    {
                        best_value = std::max(futility_value, best_value);
                        continue;
                    }

                    // Prune moves with negative or zero SEE
                    if (   futility_base <= alfa
                        && !pos.see_ge(move, Value(1)))
                    {
                        best_value = std::max(futility_base, best_value);
                        continue;
                    }
                }

                // Pruning: Don't search moves with negative SEE
                if (   (   !in_check
                        // Evasion pruning: Detect non-capture evasions for pruning
                        || (   (DEP_ZERO != depth || 2 < move_count)
                            && -VALUE_MATE_MAX_PLY < best_value
                            && !pos.capture(move)))
                    && !Limits.mate_on()
                    && !pos.see_ge(move))
                {
                    continue;
                }

                // Speculative prefetch as early as possible
                prefetch(TT.cluster(pos.posi_move_key(move))->entries);

                // Update the current move.
                ss->played_move = move;
                ss->pd_history = &thread->continuation_history[in_check][capture_or_promotion][mpc][dst];

                // Make the move.
                pos.do_move(move, si, gives_check);

                auto value = -quien_search<PVNode>(pos, ss+1, -beta, -alfa, depth - 1);

                // Undo the move.
                pos.undo_move(move);

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
                            update_pv(ss->pv, move, (ss+1)->pv);
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

            // All legal moves have been searched. A special case: If we're in check
            // and no legal moves were found, it is checkmate.
            if (   in_check
                && -VALUE_INFINITE == best_value)
            {
                return mated_in(ss->ply); // Plies to mate from the root
            }

            tte->save(key,
                      best_move,
                      value_to_tt(best_value, ss->ply),
                      ss->static_eval,
                      qs_depth,
                      best_value >= beta ?
                          BOUND_LOWER :
                             PVNode
                          && best_value > prev_alfa ?
                              BOUND_EXACT :
                              BOUND_UPPER,
                      tt_pv);

            assert(-VALUE_INFINITE < best_value && best_value < +VALUE_INFINITE);
            return best_value;
        }
        /// depth_search() is main depth limited search function, which is called when the remaining depth > 0.
        template<bool PVNode>
        Value depth_search(Position &pos, Stack *const &ss, Value alfa, Value beta, Depth depth, bool cut_node)
        {
            bool root_node = PVNode
                          && 0 == ss->ply;

            // Check if there exists a move which draws by repetition,
            // or an alternative earlier move to this position.
            if (   !root_node
                && alfa < VALUE_DRAW
                && pos.si->clock_ply >= 3
                && pos.cycled(ss->ply))
            {
                alfa = draw_value();
                if (alfa >= beta)
                {
                    return alfa;
                }
            }

            // Dive into quiescence search when the depth reaches zero
            if (DEP_ZERO >= depth)
            {
                return quien_search<PVNode>(pos, ss, alfa, beta);
            }

            assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            assert(PVNode || (alfa == beta-1));
            assert(!(PVNode && cut_node));
            assert(DEP_ZERO < depth && depth < DEP_MAX);

            // Step 1. Initialize node.
            auto *thread = pos.thread;
            bool in_check = 0 != pos.si->checkers;
            ss->move_count = 0;

            // Check for the available remaining limit.
            if (Threadpool.main_thread() == thread)
            {
                Threadpool.main_thread()->tick();
            }

            if (PVNode)
            {
                // Used to send sel_depth info to GUI (sel_depth from 1, ply from 0)
                thread->sel_depth = std::max(Depth(ss->ply + 1), thread->sel_depth);
            }

            Value value;
            auto best_value = -VALUE_INFINITE;
            auto max_value = +VALUE_INFINITE;

            auto best_move = MOVE_NONE;

            if (!root_node)
            {
                // Step 2. Check for aborted search, maximum ply reached or immediate draw.
                if (   Threadpool.stop.load(std::memory_order::memory_order_relaxed)
                    || ss->ply >= DEP_MAX
                    || pos.draw(ss->ply))
                {
                    return ss->ply >= DEP_MAX
                        && !in_check ?
                               evaluate(pos) :
                               draw_value();
                }

                // Step 3. Mate distance pruning.
                // Even if mate at the next move our score would be at best mates_in(ss->ply+1),
                // but if alfa is already bigger because a shorter mate was found upward in the tree
                // then there is no need to search further, will never beat current alfa.
                // Same logic but with reversed signs applies also in the opposite condition of
                // being mated instead of giving mate, in this case return a fail-high score.
                alfa = std::max(mated_in(ss->ply+0), alfa);
                beta = std::min(mates_in(ss->ply+1), beta);
                if (alfa >= beta)
                {
                    return alfa;
                }
            }

            assert(ss->ply >= 0
                && ss->ply == (ss-1)->ply + 1
                && ss->ply < DEP_MAX);

            assert(MOVE_NONE == (ss+1)->excluded_move);
            (ss+2)->killer_moves.fill(MOVE_NONE);

            // Initialize stats to zero for the grandchildren of the current position.
            // So stats is shared between all grandchildren and only the first grandchild starts with stats = 0.
            // Later grandchildren start with the last calculated stats of the previous grandchild.
            // This influences the reduction rules in LMR which are based on the stats of parent position.
            (ss+2+2*(root_node))->stats = 0;

            // Step 4. Transposition table lookup.
            // Don't want the score of a partial search to overwrite a previous full search
            // TT value, so use a different position key in case of an excluded move.
            Key key = pos.si->posi_key ^ (Key(ss->excluded_move) << 0x10);
            bool tt_hit;
            auto *tte = TT.probe(key, tt_hit);
            auto tt_move = root_node ?
                            thread->root_moves[thread->pv_cur].front() :
                               tt_hit ?
                                tte->move() :
                                MOVE_NONE;
            auto tt_value = tt_hit ?
                            value_of_tt(tte->value(), ss->ply, pos.si->clock_ply) :
                            VALUE_NONE;
            auto tt_pv = PVNode
                      || (   tt_hit
                          && tte->is_pv());

            if (   MOVE_NONE != tt_move
                && !(   pos.pseudo_legal(tt_move)
                     && pos.legal(tt_move)))
            {
                tt_move = MOVE_NONE;
            }

            // thread->tt_hit_avg can be used to approximate the running average of ttHit
            thread->tt_hit_avg = thread->tt_hit_avg * (ttHitAverageWindow - 1) / ttHitAverageWindow
                               + (tt_hit ? ttHitAverageResolution : 0);

            bool prior_capture = NONE != pos.si->capture;

            // At non-PV nodes we check for an early TT cutoff.
            if (   !PVNode
                && VALUE_NONE != tt_value // Handle tt_hit
                && depth <= tte->depth()
                && BOUND_NONE != (tte->bound() & (tt_value >= beta ? BOUND_LOWER : BOUND_UPPER)))
            {
                // Update move sorting heuristics on tt_move.
                if (MOVE_NONE != tt_move)
                {
                    if (tt_value >= beta)
                    {
                        // Bonus for a quiet tt_move that fails high.
                        if (!pos.capture_or_promotion(tt_move))
                        {
                            auto bonus = stat_bonus(depth);
                            update_quiet_stats(ss, pos, tt_move, bonus);
                        }

                        // Extra penalty for early quiet moves in previous ply when it gets refuted.
                        if (   !prior_capture
                            && NONE == pos.si->promote
                            && 2 >= (ss-1)->move_count)
                        {
                            update_continuation_histories(ss-1, pos[dst_sq((ss-1)->played_move)], dst_sq((ss-1)->played_move), -stat_bonus(depth + 1));
                        }
                    }
                    else
                    // Penalty for a quiet tt_move that fails low.
                    if (!pos.capture_or_promotion(tt_move))
                    {
                        auto bonus = stat_bonus(depth);
                        thread->butterfly_history[pos.active][move_index(tt_move)] << -bonus;
                        update_continuation_histories(ss, pos[org_sq(tt_move)], dst_sq(tt_move), -bonus);
                    }
                }
                return tt_value;
            }

            // Step 5. Tablebases probe.
            if (   !root_node
                && 0 != TBLimitPiece)
            {
                auto piece_count = pos.count();

                if (   (   piece_count < TBLimitPiece
                        || (   piece_count == TBLimitPiece
                            && depth >= TBProbeDepth))
                    && 0 == pos.si->clock_ply
                    && !pos.si->can_castle(CR_ANY))
                {
                    ProbeState state;
                    auto wdl = probe_wdl(pos, state);

                    // Force check of time on the next occasion
                    if (Threadpool.main_thread() == thread)
                    {
                        Threadpool.main_thread()->check_count = 1;
                    }

                    if (ProbeState::FAILURE != state)
                    {
                        thread->tb_hits.fetch_add(1, std::memory_order::memory_order_relaxed);

                        i16 draw = TBUseRule50;

                        value = wdl < -draw ? -VALUE_MATE + (DEP_MAX + ss->ply + 1) :
                                wdl > +draw ? +VALUE_MATE - (DEP_MAX + ss->ply + 1) :
                                               VALUE_ZERO + 2 * wdl * draw;

                        auto bound = wdl < -draw ? BOUND_UPPER :
                                     wdl > +draw ? BOUND_LOWER :
                                                   BOUND_EXACT;

                        if (   BOUND_EXACT == bound
                            || (BOUND_LOWER == bound ? beta <= value : value <= alfa))
                        {
                            tte->save(key,
                                      MOVE_NONE,
                                      value_to_tt(value, ss->ply),
                                      VALUE_NONE,
                                      Depth(std::min(depth + 6, DEP_MAX - 1)),
                                      bound,
                                      tt_pv);

                            return value;
                        }

                        if (PVNode)
                        {
                            if (BOUND_LOWER == bound)
                            {
                                best_value = value;
                                alfa = std::max(best_value, alfa);
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
            Move move;
            bool improving;
            Value eval;

            // Step 6. Static evaluation of the position
            if (in_check)
            {
                ss->static_eval = eval = VALUE_NONE;
                improving = false;
            }
            else
            {
                if (tt_hit)
                {
                    ss->static_eval = eval = tte->eval();
                    // Never assume anything on values stored in TT.
                    if (VALUE_NONE == eval)
                    {
                        ss->static_eval = eval = evaluate(pos);
                    }

                    if (VALUE_DRAW == eval)
                    {
                        eval = draw_value();
                    }
                    // Can tt_value be used as a better position evaluation?
                    if (   VALUE_NONE != tt_value
                        && BOUND_NONE != (tte->bound() & (tt_value > eval ? BOUND_LOWER : BOUND_UPPER)))
                    {
                        eval = tt_value;
                    }
                }
                else
                {
                    if (MOVE_NULL != (ss-1)->played_move)
                    {
                        ss->static_eval = eval = evaluate(pos) - (ss-1)->stats / 512;
                    }
                    else
                    {
                        ss->static_eval = eval = -(ss-1)->static_eval + 2*Tempo;
                    }

                    tte->save(key,
                              MOVE_NONE,
                              VALUE_NONE,
                              eval,
                              DEP_NONE,
                              BOUND_NONE,
                              tt_pv);
                }

                // Step 7. Razoring. (~2 ELO)
                if (   !root_node // The required RootNode PV handling is not available in qsearch
                    && 2 > depth
                    && eval + RazorMargin <= alfa)
                {
                    return quien_search<PVNode>(pos, ss, alfa, beta);
                }

                improving = VALUE_NONE != (ss-2)->static_eval ?
                                ss->static_eval >= (ss-2)->static_eval :
                                VALUE_NONE != (ss-4)->static_eval ?
                                    ss->static_eval >= (ss-4)->static_eval :
                                    VALUE_NONE != (ss-6)->static_eval ?
                                        ss->static_eval >= (ss-6)->static_eval :
                                        true;

                // Step 8. Futility pruning: child node. (~30 ELO)
                // Betting that the opponent doesn't have a move that will reduce
                // the score by more than futility margins [depth] if do a null move.
                if (   !root_node
                    && 6 > depth
                    && !Limits.mate_on()
                    && eval < +VALUE_KNOWN_WIN // Don't return unproven wins.
                    && eval - futility_margin(improving, depth) >= beta)
                {
                    return eval;
                }

                // Step 9. Null move search with verification search. (~40 ELO)
                if (   !PVNode
                    && MOVE_NULL != (ss-1)->played_move
                    && MOVE_NONE == ss->excluded_move
                    && !Limits.mate_on()
                    && VALUE_ZERO != pos.non_pawn_material(pos.active)
                    && 23397 > (ss-1)->stats
                    && eval >= beta
                    && eval >= ss->static_eval
                    && ss->static_eval >= beta - 32 * depth + 292 - 30 * (improving)
                    && (   thread->nmp_ply <= ss->ply
                        || thread->nmp_color != pos.active))
                {
                    // Null move dynamic reduction based on depth and static evaluation.
                    auto R = Depth((68 * depth + 854) / 258 + std::min(i32(eval - beta) / 192, 3));

                    ss->played_move = MOVE_NULL;
                    ss->pd_history = &thread->continuation_history[0][0][NO_PIECE][0];

                    pos.do_null_move(si);

                    auto null_value = -depth_search<false>(pos, ss+1, -beta, -beta+1, depth-R, !cut_node);

                    pos.undo_null_move();

                    if (null_value >= beta)
                    {
                        // Skip verification search
                        if (   0 != thread->nmp_ply // Recursive verification is not allowed
                            || (   13 > depth
                                && abs(beta) < +VALUE_KNOWN_WIN))
                        {
                            // Don't return unproven wins
                            return null_value >= +VALUE_MATE_MAX_PLY ? beta : null_value;
                        }

                        // Do verification search at high depths,
                        // with null move pruning disabled for nmp_color until ply exceeds nmp_ply
                        thread->nmp_color = pos.active;
                        thread->nmp_ply = ss->ply + 3 * (depth-R) / 4;
                        value = depth_search<false>(pos, ss, beta-1, beta, depth-R, false);
                        thread->nmp_ply = 0;

                        if (value >= beta)
                        {
                            // Don't return unproven wins
                            return null_value >= +VALUE_MATE_MAX_PLY ? beta : null_value;
                        }
                    }
                }

                // Step 10. ProbCut. (~10 ELO)
                // If good enough capture and a reduced search returns a value much above beta,
                // then can (almost) safely prune the previous move.
                if (   !PVNode
                    && 4 < depth
                    && !Limits.mate_on()
                    && abs(beta) < +VALUE_MATE_MAX_PLY)
                {
                    auto raised_beta = std::min(beta + 189 - 45 * (improving), +VALUE_INFINITE);
                    u08 pc_movecount = 0;
                    // Initialize move picker (3) for the current position
                    MovePicker move_picker(pos, tt_move, raised_beta - ss->static_eval);
                    // Loop through all legal moves until no moves remain or a beta cutoff occurs
                    while (   pc_movecount < 2 + 2 * (cut_node)
                           && MOVE_NONE != (move = move_picker.next_move()))
                    {
                        assert(pos.pseudo_legal(move)
                            && pos.legal(move)
                            && pos.capture_or_promotion(move));

                        if (move == ss->excluded_move)
                        {
                            continue;
                        }

                        ++pc_movecount;

                        // Speculative prefetch as early as possible
                        prefetch(TT.cluster(pos.posi_move_key(move))->entries);

                        ss->played_move = move;
                        ss->pd_history = &thread->continuation_history[0][1][pos[org_sq(move)]][dst_sq(move)];

                        pos.do_move(move, si);

                        // Perform a preliminary quien_search to verify that the move holds
                        value = -quien_search<false>(pos, ss+1, -raised_beta, -raised_beta+1);

                        // If the quien_search held perform the regular search
                        if (value >= raised_beta)
                        {
                            value = -depth_search<false>(pos, ss+1, -raised_beta, -raised_beta+1, depth - 4, !cut_node);
                        }

                        pos.undo_move(move);

                        if (value >= raised_beta)
                        {
                            return value;
                        }
                    }
                }
            }

            // Step 11. Internal iterative deepening (IID). (~2 ELO)
            if (   6 < depth
                && MOVE_NONE == tt_move)
            {
                depth_search<PVNode>(pos, ss, alfa, beta, depth - 7, cut_node);

                tte = TT.probe(key, tt_hit);
                tt_move = tt_hit ?
                            tte->move() :
                            MOVE_NONE;
                tt_value = tt_hit ?
                            value_of_tt(tte->value(), ss->ply, pos.si->clock_ply) :
                            VALUE_NONE;

                if (   MOVE_NONE != tt_move
                    && !(   pos.pseudo_legal(tt_move)
                         && pos.legal(tt_move)))
                {
                    tt_move = MOVE_NONE;
                }
            }

            value = best_value;

            u08 move_count = 0;

            // Mark this node as being searched.
            ThreadMarker thread_marker(thread, pos.si->posi_key, ss->ply);

            vector<Move> quiet_moves
                ,        capture_moves;
            quiet_moves.reserve(32);
            capture_moves.reserve(16);
            bool singular_lmr = false;
            bool ttm_capture = MOVE_NONE != tt_move
                            && pos.capture_or_promotion(tt_move);

            array<PieceDestinyHistory const*, 6> const pd_histories
            {
                (ss-1)->pd_history,
                (ss-2)->pd_history,
                nullptr,//(ss-3)->pd_history,
                (ss-4)->pd_history,
                nullptr,//(ss-5)->pd_history,
                (ss-6)->pd_history,
            };
            auto counter_move = _ok((ss-1)->played_move) ?
                                    thread->move_history[pos[dst_sq((ss-1)->played_move)]][move_index((ss-1)->played_move)] :
                                    MOVE_NONE;
            // Initialize move picker (1) for the current position
            MovePicker move_picker(pos, tt_move, depth, pd_histories, ss->killer_moves, counter_move);
            // Step 12. Loop through all legal moves until no moves remain or a beta cutoff occurs.
            while (MOVE_NONE != (move = move_picker.next_move()))
            {
                assert(pos.pseudo_legal(move)
                    && pos.legal(move));

                if (   // Skip exclusion move
                       (move == ss->excluded_move)
                       // Skip at root node:
                    || (   root_node
                           // In "searchmoves" mode, skip moves not listed in RootMoves, as a consequence any illegal move is also skipped.
                           // In MultiPV mode we not only skip PV moves which have already been searched and those of lower "TB rank" if we are in a TB root position.
                        && std::find(thread->root_moves.begin() + thread->pv_cur,
                                     thread->root_moves.begin() + thread->pv_end, move)
                                  == thread->root_moves.begin() + thread->pv_end))
                {
                    continue;
                }

                ss->move_count = ++move_count;

                if (   root_node
                    && Threadpool.main_thread() == thread)
                {
                    auto elapsed_time = Threadpool.main_thread()->time_mgr.elapsed_time();
                    if (elapsed_time > 3000)
                    {
                        sync_cout << "info"
                                  << " currmove " << move
                                  << " currmovenumber " << thread->pv_cur + move_count
                                  << " maxmoves " << thread->root_moves.size()
                                  << " depth " << depth
                                  << " seldepth " << (*std::find(thread->root_moves.begin() + thread->pv_cur,
                                                                 thread->root_moves.begin() + thread->pv_end, move)).sel_depth
                                  << " time " << elapsed_time << sync_endl;
                    }
                }

                /*
                // In MultiPV mode also skip moves which will be searched later as PV moves
                if (   root_node
                    //&& thread->pv_cur < Threadpool.pv_limit
                    && std::find(thread->root_moves.begin() + thread->pv_cur + 1,
                                 thread->root_moves.begin() + Threadpool.pv_limit, move)
                              != thread->root_moves.begin() + Threadpool.pv_limit)
                {
                    continue;
                }
                */

                if (PVNode)
                {
                    (ss+1)->pv.clear();
                }

                auto org = org_sq(move);
                auto dst = dst_sq(move);

                auto mpc = pos[org];
                bool gives_check = pos.gives_check(move);
                bool capture_or_promotion = pos.capture_or_promotion(move);

                // Calculate new depth for this move
                Depth new_depth = depth - 1;

                // Step 13. Pruning at shallow depth. (~170 ELO)
                if (   !root_node
                    && -VALUE_MATE_MAX_PLY < best_value
                    && !Limits.mate_on()
                    && VALUE_ZERO < pos.non_pawn_material(pos.active))
                {
                    // Skip quiet moves if move count exceeds our FutilityMoveCount threshold
                    move_picker.skip_quiets = futility_move_count(improving, depth) <= move_count;

                    if (   !capture_or_promotion
                        && !gives_check)
                    {
                        // Reduced depth of the next LMR search.
                        auto lmr_depth = Depth(std::max(new_depth - reduction(improving, depth, move_count), 0));
                        // Counter moves based pruning: (~20 ELO)
                        if (   (4 + (   0 < (ss-1)->stats
                                     || 1 == (ss-1)->move_count)) > lmr_depth
                            && (*pd_histories[0])[mpc][dst] < CounterMovePruneThreshold
                            && (*pd_histories[1])[mpc][dst] < CounterMovePruneThreshold)
                        {
                            continue;
                        }
                        // Futility pruning: parent node. (~2 ELO)
                        if (   !in_check
                            && 6 > lmr_depth
                            && ss->static_eval + 182 * lmr_depth + 255 <= alfa
                            && (  thread->butterfly_history[pos.active][move_index(move)]
                                + (*pd_histories[0])[mpc][dst]
                                + (*pd_histories[1])[mpc][dst]
                                + (*pd_histories[3])[mpc][dst]) < 30000)
                        {
                            continue;
                        }
                        // SEE based pruning: negative SEE (~10 ELO)
                        if (!pos.see_ge(move, Value(-(32 - std::min(i32(lmr_depth), 18)) * i32(pow(i32(lmr_depth), 2)))))
                        {
                            continue;
                        }
                    }
                    else
                    // SEE based pruning: negative SEE (~20 ELO)
                    if (!pos.see_ge(move, Value(-194 * depth)))
                    {
                        continue;
                    }
                }

                // Step 14. Extensions. (~70 ELO)
                Depth extension = DEP_ZERO;

                // Singular extension (SE) (~60 ELO)
                // Extend the TT move if its value is much better than its siblings.
                // If all moves but one fail low on a search of (alfa-s, beta-s),
                // and just one fails high on (alfa, beta), then that move is singular and should be extended.
                // To verify this do a reduced search on all the other moves but the tt_move,
                // if result is lower than tt_value minus a margin then extend tt_move.
                if (   !root_node
                    && 5 < depth
                    && move == tt_move
                    && MOVE_NONE == ss->excluded_move // Avoid recursive singular search.
                    && +VALUE_KNOWN_WIN > abs(tt_value) // Handle tt_hit
                    && depth < tte->depth() + 4
                    && BOUND_NONE != (tte->bound() & BOUND_LOWER))
                {
                    auto singular_beta = tt_value - 2 * depth;

                    ss->excluded_move = move;
                    value = depth_search<false>(pos, ss, singular_beta -1, singular_beta, depth/2, cut_node);
                    ss->excluded_move = MOVE_NONE;

                    if (value < singular_beta)
                    {
                        extension = 1;
                        singular_lmr = true;
                    }
                    else
                    // Multi-cut pruning
                    // Our tt_move is assumed to fail high, and now failed high also on a reduced
                    // search without the tt_move. So assume this expected Cut-node is not singular,
                    // multiple moves fail high, and can prune the whole subtree by returning the soft bound.
                    if (singular_beta >= beta)
                    {
                        return singular_beta;
                    }
                }
                else
                if (// Castle extension
                       CASTLE == mtype (move)
                    // Last captures extension
                    || (   PieceValues[EG][pos.si->capture] > VALUE_EG_PAWN
                        && pos.non_pawn_material() <= 2 * VALUE_MG_ROOK)
                    // Check extension (~2 ELO)
                    || (   gives_check
                        && (   pos.discovery_check_blocker_at(org)
                            || pos.see_ge(move)))
                    // Passed pawn extension
                    || (   move == ss->killer_moves[0]
                        && pos.pawn_advance_at(pos.active, org)
                        && pos.pawn_passed_at(pos.active, dst)))
                {
                    extension = 1;
                }

                // Add extension to new depth
                new_depth += extension;

                // Speculative prefetch as early as possible
                prefetch(TT.cluster(pos.posi_move_key(move))->entries);

                // Update the current move.
                ss->played_move = move;
                ss->pd_history = &thread->continuation_history[in_check][capture_or_promotion][mpc][dst];

                // Step 15. Make the move.
                pos.do_move(move, si, gives_check);

                bool do_lmr =
                       2 < depth
                    && (1 + 2 * (root_node)) < move_count
                    && (   !root_node
                        || thread->move_best_count(move) == 0)
                    && (   cut_node
                        || !capture_or_promotion
                        || move_picker.skip_quiets
                        || ss->static_eval + PieceValues[EG][std::min(pos.si->capture, pos.si->promote)] <= alfa
                        || thread->tt_hit_avg < 375 * ttHitAverageResolution * ttHitAverageWindow / 1024);

                bool full_search;
                // Step 16. Reduced depth search (LMR).
                // If the move fails high will be re-searched at full depth.
                if (do_lmr)
                {
                    auto reduct_depth = reduction(improving, depth, move_count);

                    // Decrease reduction if the ttHit running average is large
                    if (thread->tt_hit_avg > 500 * ttHitAverageResolution * ttHitAverageWindow / 1024)
                    {
                        reduct_depth -= 1;
                    }

                    // Reduction if other threads are searching this position.
                    if (thread_marker.marked)
                    {
                        reduct_depth += 1;
                    }

                    // Decrease reduction if position is or has been on the PV
                    if (tt_pv)
                    {
                        reduct_depth -= 2;
                    }

                    // Decrease reduction if opponent's move count is high (~10 ELO)
                    if ((ss-1)->move_count >= 15)
                    {
                        reduct_depth -= 1;
                    }

                    // Decrease reduction if move has been singularly extended
                    if (singular_lmr)
                    {
                        reduct_depth -= 2;
                    }
                    if (!capture_or_promotion)
                    {
                        // Increase reduction if TT move is a capture(~0 ELO)
                        if (ttm_capture)
                        {
                            reduct_depth += 1;
                        }

                        // Increase reduction for cut nodes (~5 ELO)
                        if (cut_node)
                        {
                            reduct_depth += 2;
                        }
                        else
                        // Decrease reduction for moves that escape a capture in no-cut nodes (~5 ELO)
                        if (   NORMAL == mtype(move)
                            && !pos.see_ge(reverse_move(move)))
                        {
                            reduct_depth -= 2;
                        }

                        auto stats = thread->butterfly_history[~pos.active][move_index(move)]
                                   + (*pd_histories[0])[mpc][dst]
                                   + (*pd_histories[1])[mpc][dst]
                                   + (*pd_histories[3])[mpc][dst]
                                   - 4926;
                        // Reset stats to zero if negative and most stats shows >= 0
                        if (   0 >  stats
                            && 0 <= (*pd_histories[0])[mpc][dst]
                            && 0 <= (*pd_histories[1])[mpc][dst]
                            && 0 <= thread->butterfly_history[~pos.active][move_index(move)])
                        {
                            stats = 0;
                        }

                        ss->stats = stats;

                        // Decrease/Increase reduction by comparing stats (~10 ELO)
                        if (   (ss-1)->stats >= -116
                            && ss->stats < -154)
                        {
                            reduct_depth += 1;
                        }
                        else
                        if (   ss->stats >= -102
                            && (ss-1)->stats < -114)
                        {
                            reduct_depth -= 1;
                        }

                        // Decrease/Increase reduction for moves with +/-ve stats (~30 ELO)
                        reduct_depth -= Depth(ss->stats / 0x4000);
                    }

                    auto d = Depth(std::max(new_depth - std::max(reduct_depth, DEP_ZERO), 1));

                    value = -depth_search<false>(pos, ss+1, -alfa-1, -alfa, d, true);

                    full_search = alfa < value
                               && 0 != reduct_depth;
                }
                else
                {
                    full_search = !PVNode
                               || 1 < move_count;
                }

                // Step 17. Full depth search when LMR is skipped or fails high.
                if (full_search)
                {
                    value = -depth_search<false>(pos, ss+1, -alfa-1, -alfa, new_depth, !cut_node);

                    if (   do_lmr
                        && !capture_or_promotion)
                    {
                        int bonus = alfa < value ?
                                        +stat_bonus(new_depth) :
                                        -stat_bonus(new_depth);
                        if (move == ss->killer_moves[0])
                        {
                            bonus += bonus / 4;
                        }
                        update_continuation_histories(ss, mpc, dst, bonus);
                    }
                }

                // Full PV search.
                if (   PVNode
                    && (   1 == move_count
                        || (   alfa < value
                            && (   root_node
                                || value < beta))))
                {
                    (ss+1)->pv.clear();

                    value = -depth_search<true>(pos, ss+1, -beta, -alfa, new_depth, false);
                }

                // Step 18. Undo move.
                pos.undo_move(move);

                assert(-VALUE_INFINITE < value && value < +VALUE_INFINITE);

                // Step 19. Check for the new best move.
                // Finished searching the move. If a stop or a cutoff occurred,
                // the return value of the search cannot be trusted,
                // and return immediately without updating best move, PV and TT.
                if (Threadpool.stop.load(std::memory_order::memory_order_relaxed))
                {
                    return VALUE_ZERO;
                }

                if (root_node)
                {
                    assert(std::find(thread->root_moves.begin(),
                                     thread->root_moves.end(), move)
                                  != thread->root_moves.end());
                    auto &rm = *std::find(thread->root_moves.begin(), thread->root_moves.end(), move);
                    // First PV move or new best move?
                    if (   1 == move_count
                        || alfa < value)
                    {
                        rm.new_value = value;
                        rm.sel_depth = thread->sel_depth;
                        rm.resize(1);
                        rm.insert (rm.end(), (ss+1)->pv.begin(), (ss+1)->pv.end());

                        // Record how often the best move has been changed in each iteration.
                        // This information is used for time management:
                        // When the best move changes frequently, allocate some more time.
                        if (   1 < move_count
                            && Limits.time_mgr_used())
                        {
                            ++thread->pv_change;
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
                            update_pv(ss->pv, move, (ss+1)->pv);
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
                        quiet_moves.push_back(move);
                    }
                    else
                    {
                        capture_moves.push_back(move);
                    }
                }
            }

            assert(0 != move_count
                || !in_check
                || MOVE_NONE != ss->excluded_move
                || 0 == MoveList<GenType::LEGAL>(pos).size());

            // Step 21. Check for checkmate and stalemate.
            // If all possible moves have been searched and if there are no legal moves,
            // If in a singular extension search then return a fail low score (alfa).
            // Otherwise it must be a checkmate or a stalemate, so return value accordingly.
            if (0 == move_count)
            {
                best_value = MOVE_NONE != ss->excluded_move ?
                                alfa :
                                in_check ?
                                    mated_in(ss->ply) :
                                    VALUE_DRAW;
            }
            else
            // Quiet best move: update move sorting heuristics.
            if (MOVE_NONE != best_move)
            {
                auto cbonus = stat_bonus(depth + 1);

                if (!pos.capture_or_promotion(best_move))
                {
                    auto qbonus = stat_bonus(depth + (best_value > beta + VALUE_MG_PAWN));

                    update_quiet_stats(ss, pos, best_move, qbonus);
                    // Decrease all the other played quiet moves.
                    for (auto &qm : quiet_moves)
                    {
                        thread->butterfly_history[pos.active][move_index(qm)] << -qbonus;
                        update_continuation_histories(ss, pos[org_sq(qm)], dst_sq(qm), -qbonus);
                    }
                }
                else
                {
                    thread->capture_history[pos[org_sq(best_move)]][dst_sq(best_move)][pos.cap_type(best_move)] << cbonus;
                }

                // Decrease all the other played capture moves.
                for (auto &cm : capture_moves)
                {
                    thread->capture_history[pos[org_sq(cm)]][dst_sq(cm)][pos.cap_type(cm)] << -cbonus;
                }

                // Extra penalty for a quiet TT move or main killer move in previous ply when it gets refuted
                if (   (   1 == (ss-1)->move_count
                        || (ss-1)->played_move == (ss-1)->killer_moves[0])
                    && !prior_capture
                    && NONE == pos.si->promote)
                {
                    update_continuation_histories(ss-1, pos[dst_sq((ss-1)->played_move)], dst_sq((ss-1)->played_move), -stat_bonus(depth + 1));
                }
            }
            else
            // Bonus for prior countermove that caused the fail low.
            if (   (   PVNode
                    || 2 < depth)
                && !prior_capture
                && NONE == pos.si->promote)
            {
                update_continuation_histories(ss-1, pos[dst_sq((ss-1)->played_move)], dst_sq((ss-1)->played_move), stat_bonus(depth));
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
                tte->save(key,
                          best_move,
                          value_to_tt(best_value, ss->ply),
                          ss->static_eval,
                          depth,
                          best_value >= beta ?
                              BOUND_LOWER :
                                 PVNode
                              && MOVE_NONE != best_move ?
                                  BOUND_EXACT :
                                  BOUND_UPPER,
                          tt_pv);
            }

            assert(-VALUE_INFINITE < best_value && best_value < +VALUE_INFINITE);
            return best_value;
        }

    }

    /// initialize() initializes some lookup tables.
    void initialize()
    {
        srand((unsigned int)(time(NULL)));
    }
    /// clear() resets search state to its initial value.
    void clear()
    {
        Threadpool.stop = true;
        Threadpool.main_thread()->wait_while_busy();

        TT.clear();
        Threadpool.clear();
        TBSyzygy::initialize(string(Options["SyzygyPath"])); // Free up mapped files
    }

}

using namespace Searcher;

/// Thread::search() is thread iterative deepening loop function.
/// It calls depth_search() repeatedly with increasing depth until
/// - Force stop requested.
/// - Allocated thinking time has been consumed.
/// - Maximum search depth is reached.
void Thread::search()
{
    // To allow access to (ss-7) up to (ss+2), the stack must be over-sized.
    // The former is needed to allow update_continuation_histories(ss-1, ...),
    // which accesses its argument at ss-4, also near the root.
    // The latter is needed for stats and killer initialization.
    Stack stacks[DEP_MAX + 10];
    for (auto ss = stacks; ss < stacks + DEP_MAX + 10; ++ss)
    {
        ss->ply = i16(ss - (stacks+7));
        ss->played_move = MOVE_NONE;
        ss->excluded_move = MOVE_NONE;
        ss->killer_moves.fill(MOVE_NONE);
        ss->move_count = 0;
        ss->static_eval = VALUE_ZERO;
        ss->stats = 0;
        ss->pd_history = &continuation_history[0][0][NO_PIECE][0];
    }

    auto *main_thread = Threadpool.main_thread() == this ?
                            Threadpool.main_thread() :
                            nullptr;
    i16 iter_idx = 0;
    pv_change = 0;
    double total_pv_changes = 0.0;

    tt_hit_avg = ttHitAverageWindow * ttHitAverageResolution / 2;

    contempt = WHITE == root_pos.active ?
                +make_score(BasicContempt, BasicContempt / 2) :
                -make_score(BasicContempt, BasicContempt / 2);

    if (nullptr != main_thread)
    {
        main_thread->iter_value.fill(main_thread->best_value);
    }

    auto best_value = -VALUE_INFINITE;
    auto window = +VALUE_ZERO;
    auto  alfa = -VALUE_INFINITE
        , beta = +VALUE_INFINITE;

    // Iterative deepening loop until requested to stop or the target depth is reached.
    while (   ++root_depth < DEP_MAX
           && !Threadpool.stop
           && (   nullptr == main_thread
               || DEP_ZERO == Limits.depth
               || root_depth <= Limits.depth))
    {
        if (   nullptr != main_thread
            && Limits.time_mgr_used())
        {
            // Age out PV variability metric
            total_pv_changes *= 0.5;
        }

        // Save the last iteration's values before first PV line is searched and
        // all the move scores except the (new) PV are set to -VALUE_INFINITE.
        for (auto &rm : root_moves)
        {
            rm.old_value = rm.new_value;
        }

        pv_beg = 0;
        pv_end = 0;

        // MultiPV loop. Perform a full root search for each PV line.
        for (pv_cur = 0; pv_cur < Threadpool.pv_limit && !Threadpool.stop; ++pv_cur)
        {
            if (pv_cur == pv_end)
            {
                for (pv_beg = pv_end++; pv_end < root_moves.size(); ++pv_end)
                {
                    if (root_moves[pv_beg].tb_rank != root_moves[pv_end].tb_rank)
                    {
                        break;
                    }
                }
            }

            // Reset UCI info sel_depth for each depth and each PV line
            sel_depth = DEP_ZERO;

            // Reset aspiration window starting size.
            if (4 <= root_depth)
            {
                auto old_value = root_moves[pv_cur].old_value;
                window = Value(21 + std::abs(old_value) / 256);
                alfa = std::max(old_value - window, -VALUE_INFINITE);
                beta = std::min(old_value + window, +VALUE_INFINITE);

                i32 dynamic_contempt = BasicContempt;
                // Dynamic contempt
                auto contempt_value = i32(Options["Contempt Value"]);
                if (0 != contempt_value)
                {
                    dynamic_contempt += (102 - dynamic_contempt / 2) * old_value / (abs(old_value) + 157) * (100 / contempt_value);
                }
                contempt = WHITE == root_pos.active ?
                            +make_score(dynamic_contempt, dynamic_contempt / 2) :
                            -make_score(dynamic_contempt, dynamic_contempt / 2);
            }

            i16 fail_high_count = 0;

            // Start with a small aspiration window and, in case of fail high/low,
            // research with bigger window until not failing high/low anymore.
            do
            {
                auto adjusted_depth = Depth(std::max(root_depth - fail_high_count, 1));
                best_value = depth_search<true>(root_pos, stacks+7, alfa, beta, adjusted_depth, false);

                // Bring the best move to the front. It is critical that sorting is
                // done with a stable algorithm because all the values but the first
                // and eventually the new best one are set to -VALUE_INFINITE and
                // want to keep the same order for all the moves but the new PV
                // that goes to the front. Note that in case of MultiPV search
                // the already searched PV lines are preserved.
                std::stable_sort(root_moves.begin() + pv_cur, root_moves.begin() + pv_end);

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
                    && main_thread->time_mgr.elapsed_time() > 3000)
                {
                    sync_cout << multipv_info(main_thread, root_depth, alfa, beta) << sync_endl;
                }

                // If fail low set new bounds.
                if (best_value <= alfa)
                {
                    beta = (alfa + beta) / 2;
                    alfa = std::max(best_value - window, -VALUE_INFINITE);

                    fail_high_count = 0;
                    if (nullptr != main_thread)
                    {
                        main_thread->stop_on_ponderhit = false;
                    }
                }
                else
                // If fail high set new bounds.
                if (beta <= best_value)
                {
                    // NOTE:: Don't change alfa = (alfa + beta) / 2
                    beta = std::min(best_value + window, +VALUE_INFINITE);

                    ++fail_high_count;
                }
                // Otherwise exit the loop.
                else
                {
                    //// Research if fail count is not zero
                    //if (0 != fail_high_count)
                    //{
                    //    fail_high_count = 0;
                    //    continue;
                    //}

                    ++root_moves[pv_cur].best_count;
                    break;
                }

                window += window / 4 + 5;

                assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            } while (true);

            // Sort the PV lines searched so far and update the GUI.
            std::stable_sort(root_moves.begin() + pv_beg, root_moves.begin() + pv_cur + 1);

            if (   nullptr != main_thread
                && (   Threadpool.stop
                    || Threadpool.pv_limit - 1 == pv_cur
                    || main_thread->time_mgr.elapsed_time() > 3000))
            {
                sync_cout << multipv_info(main_thread, root_depth, alfa, beta) << sync_endl;
            }
        }

        if (!Threadpool.stop)
        {
            finished_depth = root_depth;
        }

        // Has any of the threads found a "mate in <x>"?
        if (    Limits.mate_on()
            && !Limits.time_mgr_used()
            && best_value >= +VALUE_MATE - 2 * Limits.mate)
        {
            Threadpool.stop = true;
        }

        if (nullptr != main_thread)
        {
            // If skill level is enabled and can pick move, pick a sub-optimal best move.
            if (   root_depth == main_thread->skill_mgr.level + 1
                && main_thread->skill_mgr.enabled())
            {
                main_thread->skill_mgr.best_move = MOVE_NONE;
                main_thread->skill_mgr.pick_best_move();
            }

            if (   Limits.time_mgr_used()
                && !Threadpool.stop
                && !main_thread->stop_on_ponderhit)
            {
                if (main_thread->best_move != root_moves.front().front())
                {
                    main_thread->best_move = root_moves.front().front();
                    main_thread->best_move_depth = root_depth;
                }

                // Use part of the gained time from a previous stable move for the current move
                for (auto *th : Threadpool)
                {
                    total_pv_changes += th->pv_change;
                    th->pv_change = 0;
                }
                // Reduce time if the best_move is stable over 10 iterations
                double time_reduction = 0.91 + 1.03 * (9 < finished_depth - main_thread->best_move_depth);

                // Stop the search
                // -If there is only one legal move available
                // -If all of the available time has been used
                if (   1 == root_moves.size()
                    || (main_thread->time_mgr.elapsed_time() >
                        main_thread->time_mgr.optimum_time
                        // Best Move Instability factor
                      * (1.00 + total_pv_changes / Threadpool.size())
                        // Time Reduction factor - Use part of the gained time from a previous stable move for the current move
                      * (1.41 + main_thread->time_reduction) / (2.27 * time_reduction)
                        // Falling Eval factor
                      * ::clamp((332
                                 + 6 * (main_thread->best_value * i32(+VALUE_INFINITE != main_thread->best_value) - best_value)
                                 + 6 * (main_thread->iter_value[iter_idx] * i32(+VALUE_INFINITE != main_thread->iter_value[iter_idx]) - best_value))
                                / 704.0, 0.5, 1.5)))
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

                main_thread->iter_value[iter_idx] = best_value;
                iter_idx = (iter_idx + 1) % 4;

                main_thread->time_reduction = time_reduction;
            }

            if (Output)
            {
                OutputStream << pretty_pv_info(main_thread) << endl;
            }
        }
    }
}

/// MainThread::search() is main thread search function.
/// It searches from root position and outputs the "bestmove"/"ponder".
void MainThread::search()
{
    assert(Threadpool.main_thread() == this
        && 0 == index);

    time_mgr.start_time = now();
    DebugTime = 0;
    Output = false;
    auto output_fn = string(Options["Output File"]);
    if (!white_spaces(output_fn))
    {
        OutputStream.open(output_fn, ios_base::out|ios_base::app);
        Output = OutputStream.is_open();
        if (Output)
        {
            OutputStream << boolalpha
                         << "RootPos  : " << root_pos.fen() << "\n"
                         << "MaxMoves : " << root_moves.size() << "\n"
                         << "ClockTime: " << Limits.clock[root_pos.active].time << " ms\n"
                         << "ClockInc : " << Limits.clock[root_pos.active].inc << " ms\n"
                         << "MovesToGo: " << Limits.movestogo+0 << "\n"
                         << "MoveTime : " << Limits.movetime << " ms\n"
                         << "Depth    : " << Limits.depth << "\n"
                         << "Infinite : " << Limits.infinite << "\n"
                         << "Ponder   : " << ponder << "\n"
                         << " Depth Score    Time       Nodes PV\n"
                         << "-----------------------------------------------------------"
                         << noboolalpha << endl;
        }
    }

    if (Limits.time_mgr_used())
    {
        // Set the time manager before searching.
        time_mgr.set(root_pos.active, root_pos.ply);
    }

    TEntry::Generation = u08((root_pos.ply + 1) << 3);
    assert(0 == (TEntry::Generation & 0x07));

    bool think = true;

    if (root_moves.empty())
    {
        think = false;

        root_moves += MOVE_NONE;

        sync_cout << "info"
                  << " depth " << 0
                  << " score " << to_string(0 != root_pos.si->checkers ? -VALUE_MATE : VALUE_DRAW)
                  << " time "  << 0 << sync_endl;
    }
    else
    {
        if (   !Limits.infinite
            && !Limits.mate_on()
            && bool(Options["Use Book"]))
        {
            auto book_bm = Book.probe(root_pos, i16(i32(Options["Book Move Num"])), bool(Options["Book Pick Best"]));
            if (MOVE_NONE != book_bm)
            {
                auto rmItr = std::find(root_moves.begin(), root_moves.end(), book_bm);
                if (rmItr != root_moves.end())
                {
                    think = false;
                    std::swap(root_moves.front(), *rmItr);
                    root_moves.front().new_value = VALUE_NONE;
                    StateInfo si;
                    root_pos.do_move(book_bm, si);
                    auto book_pm = Book.probe(root_pos, i16(i32(Options["Book Move Num"])), bool(Options["Book Pick Best"]));
                    if (MOVE_NONE != book_pm)
                    {
                        root_moves.front() += book_pm;
                    }
                    root_pos.undo_move(book_bm);
                }
            }
        }

        if (think)
        {
            i16 timed_contempt = 0;
            i64 diff_time;
            auto contempt_time = i32(Options["Contempt Time"]);
            if (   0 != contempt_time
                && Limits.time_mgr_used()
                && 0 != (diff_time = (i64(Limits.clock[ root_pos.active].time)
                                    - i64(Limits.clock[~root_pos.active].time)) / 1000))
            {
                timed_contempt = i16(diff_time/contempt_time);
            }

            BasicContempt = i32(cp_to_value(i16(i32(Options["Fixed Contempt"])) + timed_contempt));
            // In analysis mode, adjust contempt in accordance with user preference
            if (   Limits.infinite
                || bool(Options["UCI_AnalyseMode"]))
            {
                BasicContempt = Options["Analysis Contempt"] == "Off"                               ? 0 :
                                Options["Analysis Contempt"] == "White" && BLACK == root_pos.active ? -BasicContempt :
                                Options["Analysis Contempt"] == "Black" && WHITE == root_pos.active ? -BasicContempt :
                                /*Options["Analysis Contempt"] == "Both"                            ? +BasicContempt :*/ +BasicContempt;
            }

            if (Limits.time_mgr_used())
            {
                best_move = MOVE_NONE;
                best_move_depth = DEP_ZERO;
            }

            skill_mgr.set_level(bool(Options["UCI_LimitStrength"]) ?
                                    ::clamp(i16(std::pow((i32(Options["UCI_Elo"]) - 1346.6) / 143.4, 1.240)), i16(0), MaxLevel) :
                                    i16(i32(Options["Skill Level"])));

            // Have to play with skill handicap?
            // In this case enable MultiPV search by skill pv size
            // that will use behind the scenes to get a set of possible moves.
            Threadpool.pv_limit = ::clamp(u32(i32(Options["MultiPV"])), u32(1 + 3 * skill_mgr.enabled()), u32(root_moves.size()));

            set_check_count();

            for (auto *th : Threadpool)
            {
                if (th != this)
                {
                    th->start();
                }
            }

            Thread::search(); // Let's start searching !

            // Swap best PV line with the sub-optimal one if skill level is enabled
            if (skill_mgr.enabled())
            {
                skill_mgr.pick_best_move();
                std::swap(root_moves.front(), *std::find(root_moves.begin(), root_moves.end(), skill_mgr.best_move));
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

    Thread const *best_thread = this;
    if (think)
    {
        // Stop the threads if not already stopped (Also raise the stop if "ponderhit" just reset Threads.ponder).
        Threadpool.stop = true;
        // Wait until all threads have finished.
        for (auto *th : Threadpool)
        {
            if (th != this)
            {
                th->wait_while_busy();
            }
        }
        // Check if there is better thread than main thread.
        if (   1 == Threadpool.pv_limit
            && DEP_ZERO == Limits.depth // Depth limit search don't use deeper thread
            && !skill_mgr.enabled()
            && !bool(Options["UCI_LimitStrength"]))
        {
            assert(MOVE_NONE != root_moves.front().front());

            best_thread = Threadpool.best_thread();

            // If new best thread then send PV info again.
            if (best_thread != this)
            {
                sync_cout << multipv_info(best_thread, best_thread->finished_depth, -VALUE_INFINITE, +VALUE_INFINITE) << sync_endl;
            }
        }
    }

    assert(!best_thread->root_moves.empty()
        && !best_thread->root_moves.front().empty());

    auto &rm = best_thread->root_moves.front();

    if (Limits.time_mgr_used())
    {
        // Update the time manager after searching.
        time_mgr.update(root_pos.active);
        best_value = rm.new_value;
    }

    auto bm = rm.front();
    auto pm = MOVE_NONE;
    if (MOVE_NONE != bm)
    {
        auto mItr = std::next(rm.begin());
        pm = mItr != rm.end() ?
            *mItr :
            TT.extract_opp_move(root_pos, bm);
    }

    if (Output)
    {
        auto total_nodes = Threadpool.nodes();
        auto elapsed_time = std::max(time_mgr.elapsed_time(), TimePoint(1));
        OutputStream << "Nodes      : " << total_nodes << " N\n"
                     << "Time       : " << elapsed_time << " ms\n"
                     << "Speed      : " << total_nodes * 1000 / elapsed_time << " N/s\n"
                     << "Hash-full  : " << TT.hash_full() << "\n"
                     << "Best Move  : " << move_to_san(bm, root_pos) << "\n"
                     << "Ponder Move: ";
        if (MOVE_NONE != bm)
        {
            StateInfo si;
            root_pos.do_move(bm, si);
            OutputStream << move_to_san(pm, root_pos);
            root_pos.undo_move(bm);
        }
        else
        {
            OutputStream << "(none)";
        }
        OutputStream << "\n" << endl;
        OutputStream.close();
    }

    // Best move could be MOVE_NONE when searching on a stalemate position.
    sync_cout << "bestmove " << bm;
    if (MOVE_NONE != pm)
    {
        std::cout << " ponder " << pm;
    }
    std::cout << sync_endl;
}
/// MainThread::set_check_count()
void MainThread::set_check_count()
{
    // At low node count increase the checking rate otherwise use a default value.
    check_count = 0 != Limits.nodes ?
                    ::clamp(Limits.nodes / 1024, u64(1), u64(1024)) :
                    u64(1024);
    assert(0 != check_count);
}
/// MainThread::tick() is used as timer function.
/// Used to detect when out of available limit and thus stop the search, also print debug info.
void MainThread::tick()
{
    if (0 < --check_count)
    {
        return;
    }
    set_check_count();

    auto elapsed_time = time_mgr.elapsed_time();

    if (DebugTime + 1000 <= elapsed_time)
    {
        DebugTime = elapsed_time;

        debug_print();
    }

    // Do not stop until told so by the GUI.
    if (ponder)
    {
        return;
    }

    if (   (   Limits.time_mgr_used()
            && (   stop_on_ponderhit
                || time_mgr.maximum_time < elapsed_time + 10))
        || (   0 != Limits.movetime
            && Limits.movetime <= elapsed_time)
        || (   0 != Limits.nodes
            && Limits.nodes <= Threadpool.nodes()))
    {
        Threadpool.stop = true;
    }
}
