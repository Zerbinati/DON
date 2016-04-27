#include "Searcher.h"

#include <sstream>
#include <iomanip>
#include <iterator>

#include "UCI.h"
#include "PRNG.h"
#include "MovePicker.h"
#include "Transposition.h"
#include "Evaluator.h"
#include "Thread.h"
#include "TBsyzygy.h"
#include "Polyglot.h"
#include "Notation.h"
#include "Debugger.h"

using namespace std;

const Value MaxStatsValue = Value(1 << 28);

namespace Searcher {

    using namespace BitBoard;
    using namespace MoveGen;
    using namespace MovePick;
    using namespace Transposition;
    using namespace Evaluator;
    using namespace Threading;
    using namespace TBSyzygy;
    using namespace Polyglot;
    using namespace Notation;
    using namespace Debugger;

    typedef vector<bool> BoolVector;

    //const size_t Stack::Size = sizeof (Stack);

    bool Chess960 = false;
    Limit Limits;

    atomic_bool ForceStop       { false }  // Stop search on request
        ,       PonderhitStop   { false }; // Stop search on ponder-hit

    u16    MultiPV         = 1;
    //i32    MultiPV_cp      = 0;

    i16    FixedContempt   = 0
        ,  ContemptTime    = 30
        ,  ContemptValue   = 50;

    string HashFile        = "Hash.dat";

    bool   OwnBook         = false;
    string BookFile        = "Book.bin";
    bool   BookMoveBest    = true;
    i16    BookUptoMove    = 20;

    Depth  TBDepthLimit    = 1*DEPTH_ONE;
    i32    TBPieceLimit    = 6;
    bool   TBUseRule50     = true;
    u16    TBHits          = 0;
    bool   TBHasRoot       = false;

    string LogFile         = "<empty>";

    // ------------------------------------

    namespace {

// prefetch() preloads the given address in L1/L2 cache.
// This is a non-blocking function that doesn't stall
// the CPU waiting for data to be loaded from memory,
// which can be quite slow.
#if defined(PREFETCH)

#   if defined(_MSC_VER) || defined(__INTEL_COMPILER)

#   include <xmmintrin.h> // Intel and Microsoft header for _mm_prefetch()

    void prefetch (const void *addr)
    {
#       if defined(__INTEL_COMPILER)
        {
            // This hack prevents prefetches from being optimized away by
            // Intel compiler. Both MSVC and gcc seem not be affected by this.
            __asm__ ("");
        }
#       endif
        _mm_prefetch (reinterpret_cast<const char*> (addr), _MM_HINT_T0);
    }

#   else

    void prefetch (const void *addr)
    {
        __builtin_prefetch (addr);
    }

#   endif

#else

    void prefetch (const void *) {}

#endif

        const i32 RazorDepth    = 4;

    #define V(v) Value(v)

        // Razoring margin lookup table (initialized at startup)
        // [depth]
        const Value RazorMargins[RazorDepth] = { V(483), V(570), V(603), V(554) };

    #undef V

        const i32 FutilityMarginDepth   = 7;
        // Futility margin lookup table (initialized at startup)
        // [depth]
        Value FutilityMargins[FutilityMarginDepth];

        const i32 FutilityMoveCountDepth = 16;
        // Futility move count lookup table (initialized at startup)
        // [improving][depth]
        u08   FutilityMoveCounts[2][FutilityMoveCountDepth];

        const i32 ReductionDepth = 64;
        const u08 ReductionMoveCount = 64;
        // ReductionDepths lookup table (initialized at startup)
        // [pv][improving][depth][move_count]
        Depth ReductionDepths[2][2][ReductionDepth][ReductionMoveCount];
        Depth reduction_depths (bool PVNode, bool imp, Depth d, u08 mc)
        {
            return ReductionDepths[PVNode][imp][min (d/DEPTH_ONE, ReductionDepth-1)][min (mc/1, ReductionMoveCount-1)];
        }

        const i32 ProbCutDepth = 4;
        const i32 HistoryPruningDepth = 4;
        const i32 NullVerificationDepth = 12;

        const u08 FullDepthMoveCount = 1;

        const u08 TimerResolution = 5; // Seconds between two check_limits() calls

        bool  MateSearch = false;

        u16   PVLimit = 1;

        Value DrawValue[CLR_NO]
            , BaseContempt[CLR_NO];

        // Counter move history value statistics
        CM2DValueStats CounterMoveHistoryValues;

        ofstream LogStream;

        // check_limits() is used to print debug info and, more importantly,
        // to detect when out of available limits and thus stop the search.
        void check_limits ()
        {
            static auto last_info_time = now ();
            static const auto *main_thread = Threadpool.main ();

            auto elapsed_time = main_thread->time_mgr.elapsed_time ();

            auto now_time = Limits.start_time + elapsed_time;
            if (now_time - last_info_time >= MilliSec)
            {
                last_info_time = now_time;
                dbg_print ();
            }

            // An engine may not stop pondering until told so by the GUI
            if (Limits.ponder)
            {
                return;
            }

            if (   (main_thread->time_mgr_used && elapsed_time > main_thread->time_mgr.maximum_time () - 2 * TimerResolution)
                || (Limits.movetime != 0 && elapsed_time >= Limits.movetime)
                || (Limits.nodes != 0 && Threadpool.game_nodes () >= Limits.nodes)
               )
            {
                ForceStop = true;
            }
        }

        // update_stats() updates killers, history, countermoves, countermoves and followupmoves history stats
        // when a new quiet best move is found.
        void update_stats (const Position &pos, Stack *ss, Move move, Depth depth, const MoveVector &quiet_moves)
        {
            if (ss->killer_moves[0] != move)
            {
                ss->killer_moves[1] = ss->killer_moves[0];
                //std::copy_backward (ss->killer_moves, ss->killer_moves + Killers - 1, ss->killer_moves + Killers);
                ss->killer_moves[0] = move;
            }

            auto bonus = Value((depth/DEPTH_ONE)*((depth/DEPTH_ONE) + 1) - 1);

            auto *thread = pos.thread ();
            auto opp_move_dst = dst_sq ((ss-1)->current_move);
            
            auto *const &cmv  = (ss-1)->counter_move_values;
            auto *const &fmv1 = (ss-2)->counter_move_values;
            auto *const &fmv2 = (ss-4)->counter_move_values;

            thread->history_values.update (pos[org_sq (move)], dst_sq (move), bonus);
            if (cmv != nullptr)
            {
                assert(_ok ((ss-1)->current_move));
                thread->counter_moves.update (pos[opp_move_dst], opp_move_dst, move);
                cmv->update (pos[org_sq (move)], dst_sq (move), bonus);
            }
            if (fmv1 != nullptr)
            {
                fmv1->update (pos[org_sq (move)], dst_sq (move), bonus);
            }
            if (fmv2 != nullptr)
            {
                fmv2->update (pos[org_sq (move)], dst_sq (move), bonus);
            }
            
            // Decrease all the other played quiet moves
            assert(std::find (quiet_moves.begin (), quiet_moves.end (), move) == quiet_moves.end ());
            for (const auto m : quiet_moves)
            {
                assert(m != move);
                thread->history_values.update (pos[org_sq (m)], dst_sq (m), -bonus);
                if (cmv != nullptr)
                {
                    cmv->update (pos[org_sq (m)], dst_sq (m), -bonus);
                }
                if (fmv1 != nullptr)
                {
                    fmv1->update (pos[org_sq (m)], dst_sq (m), -bonus);
                }
                if (fmv2 != nullptr)
                {
                    fmv2->update (pos[org_sq (m)], dst_sq (m), -bonus);
                }
            }

            // Extra penalty for PV move in previous ply when it gets refuted
            if (   (ss-1)->move_count == 1
                && pos.capture_type () == NONE
                && cmv != nullptr
                //&& mtype ((ss-1)->current_move) != PROMOTE
               )
            {
                bonus = -bonus - 2*((depth/DEPTH_ONE) + 1);

                auto *const &ocmv  = (ss-2)->counter_move_values;
                auto *const &ofmv1 = (ss-3)->counter_move_values;
                auto *const &ofmv2 = (ss-5)->counter_move_values;

                if (ocmv != nullptr)
                {
                    ocmv->update (pos[opp_move_dst], opp_move_dst, bonus);
                }
                if (ofmv1 != nullptr)
                {
                    ofmv1->update (pos[opp_move_dst], opp_move_dst, bonus);
                }
                if (ofmv2 != nullptr)
                {
                    ofmv2->update (pos[opp_move_dst], opp_move_dst, bonus);
                }
            }
        }
        void update_stats (const Position &pos, Stack *ss, Move move, Depth depth)
        {
            static const MoveVector quiet_moves (0);
            update_stats (pos, ss, move, depth, quiet_moves);
        }
        // update_pv() add current move and appends child pv[]
        void update_pv (MoveVector &pv, Move move, const MoveVector &child_pv)
        {
            assert(_ok (move));
            pv.clear ();
            pv.push_back (move);
            if (!child_pv.empty ())
            {
                pv.reserve (child_pv.size () + 1);
                std::copy (child_pv.begin (), child_pv.end (), std::back_inserter (pv));
                //pv.shrink_to_fit ();
            }
        }

        // value_to_tt() adjusts a mate score from "plies to mate from the root" to
        // "plies to mate from the current position". Non-mate scores are unchanged.
        // The function is called before storing a value to the transposition table.
        Value value_to_tt (Value v, i32 ply)
        {
            assert(v != VALUE_NONE);
            return v >= +VALUE_MATE_IN_MAX_PLY ? v + ply :
                   v <= -VALUE_MATE_IN_MAX_PLY ? v - ply :
                   v;
        }
        // value_of_tt() is the inverse of value_to_tt ():
        // It adjusts a mate score from the transposition table
        // (where refers to the plies to mate/be mated from current position)
        // to "plies to mate/be mated from the root".
        Value value_of_tt (Value v, i32 ply)
        {
            return v == VALUE_NONE             ? VALUE_NONE :
                   v >= +VALUE_MATE_IN_MAX_PLY ? v - ply :
                   v <= -VALUE_MATE_IN_MAX_PLY ? v + ply :
                   v;
        }

        // multipv_info() formats PV information according to UCI protocol.
        // UCI requires to send all the PV lines also if are still to be searched
        // and so refer to the previous search score.
        string multipv_info (Value alfa, Value beta)
        {
            static const auto *main_thread = Threadpool.main ();

            auto elapsed_time = std::max (main_thread->time_mgr.elapsed_time (), TimePoint(1));
            assert(elapsed_time > 0);
            auto game_nodes = Threadpool.game_nodes ();

            stringstream ss;
            for (u16 i = 0; i < PVLimit; ++i)
            {
                Depth d;
                Value v;

                if (i <= main_thread->pv_index) // New updated pv?
                {
                    d = main_thread->root_depth;
                    v = main_thread->root_moves[i].new_value;
                }
                else                            // Old expired pv?
                {
                    if (DEPTH_ONE == main_thread->root_depth)
                    {
                        continue;
                    }

                    d = main_thread->root_depth - DEPTH_ONE;
                    v = main_thread->root_moves[i].old_value;
                }

                bool tb = TBHasRoot && abs (v) < +VALUE_MATE - i32(MaxPlies);

                ss  << (ss.rdbuf ()->in_avail () ? "\n" : "") // Not at first line
                    << "info"
                    << " multipv "  << i + 1
                    << " depth "    << d/DEPTH_ONE
                    << " seldepth " << main_thread->max_ply
                    << " score "    << to_string (tb ? ProbeValue : v)
                    << (!tb && i == main_thread->pv_index ? (beta <= v ? " lowerbound" : v <= alfa ? " upperbound" : "") : "")
                    << " nodes "    << game_nodes
                    << " time "     << elapsed_time
                    << " nps "      << game_nodes * MilliSec / elapsed_time
                    << " hashfull " << (elapsed_time > MilliSec ? TT.hash_full () : 0)
                    << " tbhits "   << TBHits
                    << " pv"        << main_thread->root_moves[i];
            }
            return ss.str ();
        }

        template<bool PVNode, bool InCheck>
        // quien_search<>() is the quiescence search function,
        // which is called by the main depth limited search function
        // when the remaining depth is less than DEPTH_ONE.
        Value quien_search (Position &pos, Stack *ss, Value alfa, Value beta, Depth depth)
        {
            assert(InCheck == (pos.checkers () != 0));
            assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            assert(PVNode || alfa == beta-1);
            assert(depth <= DEPTH_ZERO);
            assert(1 <= ss->ply && ss->ply == (ss-1)->ply + 1 && ss->ply < MaxPlies);

            auto pv_alfa = -VALUE_INFINITE;

            if (PVNode)
            {
                pv_alfa = alfa; // To flag BOUND_EXACT when eval above alfa and no available moves

                ss->pv.clear ();
            }

            ss->current_move = MOVE_NONE;

            // Check for an immediate draw or maximum ply reached
            if (   pos.draw ()
                || ss->ply >= MaxPlies
               )
            {
                return ss->ply >= MaxPlies && !InCheck ? evaluate (pos) : DrawValue[pos.active ()];
            }

            CheckInfo ci (pos);
            // Transposition table lookup
            auto posi_key = pos.posi_key ();
            bool tt_hit;
            auto *tte = TT.probe (posi_key, tt_hit);
            auto tt_move  = tt_hit ? tte->move () : MOVE_NONE;
            if (   tt_hit
                && tt_move != MOVE_NONE
                && !(pos.pseudo_legal (tt_move) && pos.legal (tt_move, ci.pinneds))
               )
            {
                tt_hit  = false;
                tt_move = MOVE_NONE;
                //tte->clear ();
            }
            assert(tt_move == MOVE_NONE || (pos.pseudo_legal (tt_move) && pos.legal (tt_move, ci.pinneds)));
            auto tt_value = tt_hit ? value_of_tt (tte->value (), ss->ply) : VALUE_NONE;
            auto tt_eval  = tt_hit ? tte->eval () : VALUE_NONE;
            auto tt_depth = tt_hit ? tte->depth () : DEPTH_NONE;
            auto tt_bound = tt_hit ? tte->bound () : BOUND_NONE;

            // Decide whether or not to include checks, this fixes also the type of
            // TT entry depth that are going to use. Note that in quien_search use
            // only two types of depth in TT: DEPTH_QS_CHECKS or DEPTH_QS_NO_CHECKS.
            auto qs_depth = InCheck || depth >= DEPTH_QS_CHECKS ? DEPTH_QS_CHECKS : DEPTH_QS_NO_CHECKS;

            if (   !PVNode
                && tt_hit
                && tt_depth >= qs_depth
                && tt_value != VALUE_NONE // Only in case of TT access race
                && (tt_bound & (tt_value >= beta ? BOUND_LOWER : BOUND_UPPER)) != BOUND_NONE
               )
            {
                ss->current_move = tt_move; // Can be MOVE_NONE
                return tt_value;
            }

            Value best_value
                , futility_base;

            // Evaluate the position statically
            if (InCheck)
            {
                ss->static_eval = VALUE_NONE;
                best_value = futility_base = -VALUE_INFINITE;
            }
            else
            {
                if (tt_hit)
                {
                    // Never assume anything on values stored in TT
                    ss->static_eval = tt_eval = tt_eval != VALUE_NONE ? tt_eval : evaluate (pos);
                    // Can tt_value be used as a better position evaluation?
                    if (   tt_value != VALUE_NONE
                        && (tt_bound & (tt_value > tt_eval ? BOUND_LOWER : BOUND_UPPER)) != BOUND_NONE
                       )
                    {
                        tt_eval = tt_value;
                    }
                }
                else
                {
                    ss->static_eval = tt_eval = (ss-1)->current_move != MOVE_NULL ?
                                                        evaluate (pos) : -(ss-1)->static_eval + 2*Tempo;
                }

                if (alfa < tt_eval)
                {
                    // Stand pat. Return immediately if static value is at least beta
                    if (tt_eval >= beta)
                    {
                        if (!tt_hit)
                        {
                            tte->save (posi_key, MOVE_NONE, value_to_tt (tt_eval, ss->ply), ss->static_eval, DEPTH_NONE, BOUND_LOWER, TT.generation ());
                        }

                        assert(-VALUE_INFINITE < tt_eval && tt_eval < +VALUE_INFINITE);
                        return tt_eval;
                    }

                    assert(tt_eval < beta);
                    // Update alfa! Always alfa < beta
                    if (PVNode)
                    {
                        alfa = tt_eval;
                    }
                }

                best_value = tt_eval;
                futility_base = best_value + i32(VALUE_EG_PAWN)/2; // QS Futility Margin
            }

            auto *thread = pos.thread ();
            //auto *main_thread = Threadpool.main () == thread ? Threadpool.main () : nullptr;
            auto best_move = MOVE_NONE;

            // Initialize a MovePicker object for the current position, and prepare
            // to search the moves. Because the depth is <= 0 here, only captures,
            // queen promotions and checks (only if depth >= DEPTH_QS_CHECKS) will
            // be generated.
            MovePicker mp (pos, tt_move, depth, _ok ((ss-1)->current_move) ? dst_sq ((ss-1)->current_move) : SQ_NO);
            StateInfo si;
            Move move;
            // Loop through the moves until no moves remain or a beta cutoff occurs.
            while ((move = mp.next_move ()) != MOVE_NONE)
            {
                assert(_ok (move) && pos.pseudo_legal (move));

                // Check for legality before making the move
                if (!pos.legal (move, ci.pinneds))
                {
                    continue;
                }

                bool gives_check = mtype (move) == NORMAL && ci.discoverers == 0 ?
                                    (ci.checking_bb[ptype (pos[org_sq (move)])] & dst_sq (move)) != 0 :
                                    pos.gives_check (move, ci);
                if (!MateSearch)
                {
                    // Futility pruning
                    if (   !InCheck
                        && !gives_check
                        && futility_base > -VALUE_KNOWN_WIN
                        && futility_base <= alfa
                        && !pos.advanced_pawn_push (move)
                       )
                    {
                        assert(mtype (move) != ENPASSANT); // Due to !pos.advanced_pawn_push()

                        auto futility_value = futility_base + PieceValues[EG][ptype (pos[dst_sq (move)])];
                        
                        if (futility_value <= alfa)
                        {
                            if (best_value < futility_value)
                            {
                                best_value = futility_value;
                            }
                            continue;
                        }
                        // Prune moves with negative or zero SEE
                        if (pos.see_sign (move) <= VALUE_ZERO)
                        {
                            if (best_value < futility_base)
                            {
                                best_value = futility_base;
                            }
                            continue;
                        }
                    }

                    // Don't search moves with negative SEE values
                    if (   (   !InCheck
                            // Detect non-capture evasions that are candidate to be pruned (evasion_prunable)
                            || (   best_value > -VALUE_MATE_IN_MAX_PLY
                                && !pos.capture (move)
                               )
                           )
                        && mtype (move) != PROMOTE
                        && pos.see_sign (move) < VALUE_ZERO //-VALUE_MG_PAWN/2
                       )
                    {
                        continue;
                    }
                }

                bool capture_or_promotion = pos.capture_or_promotion (move);

                // Speculative prefetch as early as possible
                prefetch (TT.cluster_entry (pos.move_posi_key (move)));

                ss->current_move = move;

                // Make and search the move
                pos.do_move (move, si, gives_check);

                if (   ptype (pos[dst_sq (move)]) == PAWN
                    || pos.capture_type () == PAWN
                   )
                {
                    prefetch (thread->pawn_table[pos.pawn_key ()]);
                }
                if (capture_or_promotion)
                {
                    prefetch (thread->matl_table[pos.matl_key ()]);
                }
                auto value =
                    gives_check ?
                        -quien_search<PVNode, true > (pos, ss+1, -beta, -alfa, depth-DEPTH_ONE) :
                        -quien_search<PVNode, false> (pos, ss+1, -beta, -alfa, depth-DEPTH_ONE);

                // Undo the move
                pos.undo_move ();

                assert(-VALUE_INFINITE < value && value < +VALUE_INFINITE);

                // Check for new best move
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
                        // Fail high
                        if (value >= beta)
                        {
                            if (   tt_hit
                                && tte->key16 () != u16(posi_key >> 0x30)
                               )
                            {
                                tte = TT.probe (posi_key, tt_hit);
                            }
                            tte->save (posi_key, move, value_to_tt (value, ss->ply), ss->static_eval, qs_depth, BOUND_LOWER, TT.generation ());

                            assert(-VALUE_INFINITE < value && value < +VALUE_INFINITE);
                            return value;
                        }

                        assert(value < beta);
                        // Update alfa! Always alfa < beta
                        if (PVNode) alfa = value;
                    }
                }
            }

            // All legal moves have been searched.
            // A special case: If in check and no legal moves were found, it is checkmate.
            if (   InCheck
                && best_value == -VALUE_INFINITE
               )
            {
                return mated_in (ss->ply); // Plies to mate from the root
            }

            if (   tt_hit
                && tte->key16 () != u16(posi_key >> 0x30)
               )
            {
                tte = TT.probe (posi_key, tt_hit);
            }
            tte->save (posi_key, best_move, value_to_tt (best_value, ss->ply), ss->static_eval, qs_depth,
                PVNode && pv_alfa < best_value ? BOUND_EXACT : BOUND_UPPER, TT.generation ());

            assert(-VALUE_INFINITE < best_value && best_value < +VALUE_INFINITE);
            return best_value;
        }

        template<bool PVNode, bool CutNode, bool InCheck>
        // depth_search<>() is the main depth limited search function.
        Value depth_search (Position &pos, Stack *ss, Value alfa, Value beta, Depth depth)
        {
            const bool root_node = PVNode && ss->ply == 1;
            assert(InCheck == (pos.checkers () != 0));
            assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            assert(PVNode || alfa == beta-1);
            assert(DEPTH_ZERO < depth && depth < DEPTH_MAX);
            assert(1 <= ss->ply && ss->ply == (ss-1)->ply + 1 && ss->ply < MaxPlies);

            // Step 1. Initialize node
            auto *thread = pos.thread ();
            auto *main_thread = Threadpool.main () == thread ? Threadpool.main () : nullptr;

            // Check for the available remaining limit
            if (thread->reset_check.load (std::memory_order_relaxed))
            {
                thread->reset_check = false;
                thread->chk_count = 0;
            }
            if (++thread->chk_count >= TimerResolution*MilliSec)
            {
                for (auto *th : Threadpool)
                {
                    th->reset_check = true;
                }
                check_limits ();
            }

            if (PVNode)
            {
                // Used to send 'seldepth' info to GUI
                if (thread->max_ply < ss->ply)
                {
                    thread->max_ply = ss->ply;
                }
            }

            ss->move_count = 0;
            ss->current_move = MOVE_NONE;
            ss->counter_move_values = nullptr;

            if (!root_node)
            {
                // Step 2. Check end condition
                // Check for aborted search, immediate draw or maximum ply reached
                if (   ForceStop.load (std::memory_order_relaxed)
                    || pos.draw ()
                    || ss->ply >= MaxPlies
                   )
                {
                    return ss->ply >= MaxPlies && !InCheck ? evaluate (pos) : DrawValue[pos.active ()];
                }

                // Step 3. Mate distance pruning. Even if mate at the next move our score
                // would be at best mates_in(ss->ply+1), but if alfa is already bigger because
                // a shorter mate was found upward in the tree then there is no need to search
                // further, will never beat current alfa. Same logic but with reversed signs
                // applies also in the opposite condition of being mated instead of giving mate,
                // in this case return a fail-high score.
                if (alfa < mated_in (ss->ply +0))
                {
                    alfa = mated_in (ss->ply +0);
                }
                if (beta > mates_in (ss->ply +1))
                {
                    beta = mates_in (ss->ply +1);
                }
                if (alfa >= beta)
                {
                    return alfa;
                }
            }

            (ss+1)->exclude_move = MOVE_NONE;
            (ss+1)->skip_pruning = false;
            std::fill ((ss+2)->killer_moves, (ss+2)->killer_moves + Killers, MOVE_NONE);

            CheckInfo ci (pos);
            // Step 4. Transposition table lookup
            // Don't want the score of a partial search to overwrite a previous full search
            // TT value, so use a different position key in case of an excluded move.
            auto exclude_move = ss->exclude_move;

            auto posi_key = exclude_move == MOVE_NONE ?
                        pos.posi_key () :
                        pos.posi_key () ^ Zobrist::ExclusionKey;
            bool tt_hit;
            auto *tte = TT.probe (posi_key, tt_hit);
            auto tt_move  = root_node ? thread->root_moves[thread->pv_index][0] :
                            tt_hit ? tte->move () : MOVE_NONE;
            if (   !root_node
                && tt_hit
                && tt_move != MOVE_NONE
                && !(pos.pseudo_legal (tt_move) && pos.legal (tt_move, ci.pinneds))
               )
            {
                tt_hit  = false;
                tt_move = MOVE_NONE;
                //tte->clear ();
            }
            assert(tt_move == MOVE_NONE || (pos.pseudo_legal (tt_move) && pos.legal (tt_move, ci.pinneds)));
            auto tt_value = tt_hit ? value_of_tt (tte->value (), ss->ply) : VALUE_NONE;
            auto tt_eval  = tt_hit ? tte->eval () : VALUE_NONE;
            auto tt_depth = tt_hit ? tte->depth () : DEPTH_NONE;
            auto tt_bound = tt_hit ? tte->bound () : BOUND_NONE;

            // At non-PV nodes we check for an early TT cutoff
            if (   !PVNode
                && tt_hit
                && tt_depth >= depth
                && tt_value != VALUE_NONE // Only in case of TT access race
                && (tt_bound & (tt_value >= beta ? BOUND_LOWER : BOUND_UPPER)) != BOUND_NONE
               )
            {
                ss->current_move = tt_move; // Can be MOVE_NONE
                if (tt_move != MOVE_NONE)
                {
                    ss->counter_move_values = &CounterMoveHistoryValues[pos[org_sq (tt_move)]][dst_sq (tt_move)];

                    // If tt_move is quiet, update killers, history, countermove and countermoves history on TT hit
                    if (   tt_value >= beta
                        && !pos.capture_or_promotion (tt_move)
                       )
                    {
                        update_stats (pos, ss, tt_move, depth);
                    }
                }
                
                return tt_value;
            }

            // Step 4A. Tablebase probe
            if (   !root_node
                && TBPieceLimit != 0
               )
            {
                auto piece_count = pos.count<NONE> ();

                if (   (   piece_count <  TBPieceLimit
                       || (piece_count == TBPieceLimit && depth >= TBDepthLimit)
                       )
                    && pos.clock_ply () == 0
                    && pos.can_castle (CR_ANY) == CR_NONE
                   )
                {
                    i32 found;
                    auto v = probe_wdl (pos, found);

                    if (found != 0)
                    {
                        ++TBHits;

                        auto draw_v = TBUseRule50 ? 1 : 0;

                        auto value =
                                v < -draw_v ? -VALUE_MATE + i32(MaxPlies + ss->ply) :
                                v > +draw_v ? +VALUE_MATE - i32(MaxPlies + ss->ply) :
                                VALUE_ZERO + 2 * draw_v * v;

                        if (   tt_hit
                            && tte->key16 () != u16(posi_key >> 0x30)
                           )
                        {
                            tte = TT.probe (posi_key, tt_hit);
                        }
                        tte->save (posi_key, MOVE_NONE, value_to_tt (value, ss->ply), /*InCheck ?*/ VALUE_NONE /*: evaluate (pos)*/,
                            std::min (depth + 6*DEPTH_ONE, DEPTH_MAX - DEPTH_ONE), BOUND_EXACT, TT.generation ());

                        return value;
                    }
                }
            }

            StateInfo si;
            Move move;

            // Step 5. Evaluate the position statically
            if (InCheck)
            {
                ss->static_eval = tt_eval = VALUE_NONE;
            }
            else
            {
                if (tt_hit)
                {
                    // Never assume anything on values stored in TT
                    ss->static_eval = tt_eval = tt_eval != VALUE_NONE ? tt_eval : evaluate (pos);
                    // Can tt_value be used as a better position evaluation?
                    if (   tt_value != VALUE_NONE
                        && (tt_bound & (tt_value > tt_eval ? BOUND_LOWER : BOUND_UPPER)) != BOUND_NONE
                       )
                    {
                        tt_eval = tt_value;
                    }
                }
                else
                {
                    ss->static_eval = tt_eval = (ss-1)->current_move != MOVE_NULL ?
                                                        evaluate (pos) : -(ss-1)->static_eval + 2*Tempo;

                    tte->save (posi_key, MOVE_NONE, VALUE_NONE, ss->static_eval, DEPTH_NONE, BOUND_NONE, TT.generation ());
                }

                if (!ss->skip_pruning)
                {
                    // Step 6. Razoring sort of forward pruning where rather than skipping an entire subtree,
                    // you search it to a reduced depth, typically one less than normal depth.
                    if (   !PVNode
                        && !MateSearch
                        && tt_move == MOVE_NONE
                        && depth < RazorDepth*DEPTH_ONE
                        && tt_eval + RazorMargins[depth/DEPTH_ONE] <= alfa
                       )
                    {
                        if (   depth <= 1*DEPTH_ONE
                            && tt_eval + RazorMargins[3] <= alfa
                           )
                        {
                            return quien_search<false, false> (pos, ss, alfa, beta, DEPTH_ZERO);
                        }

                        auto reduced_alpha = std::max (alfa - RazorMargins[depth/DEPTH_ONE], -VALUE_INFINITE);

                        auto value = quien_search<false, false> (pos, ss, reduced_alpha, reduced_alpha+1, DEPTH_ZERO);

                        if (value <= reduced_alpha)
                        {
                            return value;
                        }
                    }

                    // Step 7. Futility pruning: child node
                    // Betting that the opponent doesn't have a move that will reduce
                    // the score by more than FutilityMargins[depth] if do a null move.
                    if (   !root_node
                        && !MateSearch
                        && depth < FutilityMarginDepth*DEPTH_ONE
                        && tt_eval < +VALUE_KNOWN_WIN // Do not return unproven wins
                        && pos.non_pawn_material (pos.active ()) != VALUE_ZERO
                       )
                    {
                        auto stand_pat = tt_eval - FutilityMargins[depth/DEPTH_ONE];

                        if (stand_pat >= beta)
                        {
                            return stand_pat;
                        }
                    }

                    // Step 8. Null move search with verification search
                    if (   !PVNode
                        && !MateSearch
                        && depth >= 2*DEPTH_ONE
                        && tt_eval >= beta
                        && pos.non_pawn_material (pos.active ()) != VALUE_ZERO
                       )
                    {
                        assert(_ok ((ss-1)->current_move));
                        assert(exclude_move == MOVE_NONE);

                        ss->current_move = MOVE_NULL;
                        ss->counter_move_values = nullptr;

                        // Null move dynamic reduction based on depth and static evaluation
                        auto reduced_depth = depth - ((67*(depth/DEPTH_ONE) + 823) / 256 + std::min ((tt_eval - beta)/VALUE_MG_PAWN, 3))*DEPTH_ONE;

                        // Speculative prefetch as early as possible
                        prefetch (TT.cluster_entry (pos.posi_key () ^ Zob.act_side ^ (pos.en_passant_sq () != SQ_NO ? Zob.en_passant[_file (pos.en_passant_sq ())] : 0)));

                        // Do null move
                        pos.do_null_move (si);

                        (ss+1)->skip_pruning = true;
                        // Null (zero) window (alfa, beta) = (beta-1, beta):
                        auto null_value =
                            reduced_depth < DEPTH_ONE ?
                                -quien_search<false, false> (pos, ss+1, -beta, -(beta-1), DEPTH_ZERO) :
                                -depth_search<false, !CutNode, false> (pos, ss+1, -beta, -(beta-1), reduced_depth);
                        (ss+1)->skip_pruning = false;
                        // Undo null move
                        pos.undo_null_move ();

                        if (null_value >= beta)
                        {
                            // Don't do verification search at low depths
                            if (   depth < NullVerificationDepth*DEPTH_ONE
                                && abs (beta) < +VALUE_KNOWN_WIN
                               )
                            {
                                // Don't return unproven unproven mates
                                return null_value < +VALUE_MATE_IN_MAX_PLY ? null_value : beta;
                            }
                            
                            // Do verification search at high depths
                            ss->skip_pruning = true;
                            auto value =
                                reduced_depth < DEPTH_ONE ?
                                    quien_search<false, false> (pos, ss, beta-1, beta, DEPTH_ZERO) :
                                    depth_search<false, false, false> (pos, ss, beta-1, beta, reduced_depth);
                            ss->skip_pruning = false;

                            if (value >= beta)
                            {
                                // Don't return unproven unproven mates
                                return null_value < +VALUE_MATE_IN_MAX_PLY ? null_value : beta;
                            }
                        }
                    }

                    // Step 9. ProbCut
                    // If have a very good capture (i.e. SEE > see[captured_piece_type])
                    // and a reduced search returns a value much above beta,
                    // can (almost) safely prune the previous move.
                    if (   !PVNode
                        && !MateSearch
                        && depth > ProbCutDepth*DEPTH_ONE
                        && abs (beta) < +VALUE_MATE_IN_MAX_PLY
                       )
                    {
                        auto reduced_depth = depth - ProbCutDepth*DEPTH_ONE; // Shallow Depth
                        auto extended_beta = std::min (beta + VALUE_MG_PAWN, +VALUE_INFINITE); // ProbCut Threshold

                        assert(reduced_depth > DEPTH_ZERO);
                        assert(_ok ((ss-1)->current_move));
                        assert(alfa < extended_beta);

                        // Initialize a MovePicker object for the current position, and prepare to search the moves.
                        MovePicker mp (pos, tt_move, PieceValues[MG][pos.capture_type ()]);
                        // Loop through all pseudo-legal moves until no moves remain or a beta cutoff occurs.
                        while ((move = mp.next_move ()) != MOVE_NONE)
                        {
                            assert(_ok (move) && pos.pseudo_legal (move));

                            if (!pos.legal (move, ci.pinneds))
                            {
                                continue;
                            }

                            // Speculative prefetch as early as possible
                            prefetch (TT.cluster_entry (pos.move_posi_key (move)));

                            ss->current_move = move;
                            ss->counter_move_values = &CounterMoveHistoryValues[pos[org_sq (move)]][dst_sq (move)];

                            bool gives_check = mtype (move) == NORMAL && ci.discoverers == 0 ?
                                                (ci.checking_bb[ptype (pos[org_sq (move)])] & dst_sq (move)) != 0 :
                                                pos.gives_check (move, ci);
                            bool capture_or_promotion = pos.capture_or_promotion (move);

                            pos.do_move (move, si, gives_check);

                            if (   ptype (pos[dst_sq (move)]) == PAWN
                                || pos.capture_type () == PAWN
                               )
                            {
                                prefetch (thread->pawn_table[pos.pawn_key ()]);
                            }
                            if (capture_or_promotion)
                            {
                                prefetch (thread->matl_table[pos.matl_key ()]);
                            }
                            auto value =
                                gives_check ?
                                    -depth_search<false, !CutNode, true > (pos, ss+1, -extended_beta, -extended_beta+1, reduced_depth) :
                                    -depth_search<false, !CutNode, false> (pos, ss+1, -extended_beta, -extended_beta+1, reduced_depth);

                            pos.undo_move ();

                            if (value >= extended_beta)
                            {
                                return value;
                            }
                        }
                    }

                    bool int_itr_deep =
                           tt_move == MOVE_NONE
                        && depth >= (PVNode ? 5 : 8)*DEPTH_ONE                  // IID Activation Depth
                        && (PVNode || ss->static_eval + VALUE_EG_PAWN >= beta); // IID Margin

                    // Step 10. Internal iterative deepening
                    if (int_itr_deep)
                    {
                        assert(!root_node);

                        auto iid_depth = depth - 2*DEPTH_ONE - (PVNode ? DEPTH_ZERO : depth/4); // IID Reduced Depth

                        ss->skip_pruning = true;
                        depth_search<PVNode, true, false> (pos, ss, alfa, beta, iid_depth);
                        ss->skip_pruning = false;
                    }

                    if (tt_move == MOVE_NONE)
                    {
                        tte = TT.probe (posi_key, tt_hit);
                        if (tt_hit)
                        {
                            if (   !root_node
                                && tte->move () != MOVE_NONE
                                && tt_move != tte->move ()
                               )
                            {
                                tt_move = tte->move ();
                                if (   tt_move != MOVE_NONE
                                    && !(pos.pseudo_legal (tt_move) && pos.legal (tt_move, ci.pinneds))
                                   )
                                {
                                    tt_hit  = false;
                                    tt_move = MOVE_NONE;
                                    //tte->clear ();
                                }
                                assert(tt_move == MOVE_NONE || (pos.pseudo_legal (tt_move) && pos.legal (tt_move, ci.pinneds)));
                            }
                            if (tt_hit)
                            {
                                tt_value = value_of_tt (tte->value (), ss->ply);
                                tt_depth = tte->depth ();
                                tt_bound = tte->bound ();
                            }
                        }
                    }
                }
            }

            assert(!root_node || tt_move == thread->root_moves[thread->pv_index][0]);

            // When in check search starts from here
            auto value      = -VALUE_INFINITE
               , best_value = -VALUE_INFINITE;

            auto best_move  = MOVE_NONE;

            bool singular_ext_node =
                   !root_node
                && tt_hit
                && exclude_move == MOVE_NONE // Recursive singular search is not allowed
                && tt_move != MOVE_NONE
                &&    depth >= 8*DEPTH_ONE
                && tt_depth >= depth - 3*DEPTH_ONE
                && abs (tt_value) < +VALUE_KNOWN_WIN
                && (tt_bound & BOUND_LOWER) != BOUND_NONE;

            bool improving =
                   (ss-0)->static_eval >= (ss-2)->static_eval
                || (ss-0)->static_eval == VALUE_NONE
                || (ss-2)->static_eval == VALUE_NONE;

            auto &opp_cmv =
                (ss-1)->counter_move_values != nullptr ?
                    *(ss-1)->counter_move_values :
                    CounterMoveHistoryValues[NO_PIECE][dst_sq ((ss-1)->current_move)];

            u08 move_count = 0;
            ss->current_move = MOVE_NONE;
            ss->counter_move_values = nullptr;

            MoveVector quiet_moves;
            quiet_moves.reserve (16);

            // Initialize a MovePicker object for the current position, and prepare to search the moves.
            MovePicker mp (pos, tt_move, depth, ss);
            // Step 11. Loop through moves
            // Loop through all pseudo-legal moves until no moves remain or a beta cutoff occurs.
            while ((move = mp.next_move ()) != MOVE_NONE)
            {
                assert(_ok (move) && pos.pseudo_legal (move));

                if (
                    // Check for legality before making the move
                    // At root obey the "searchmoves" option and skip moves not listed in
                    // RootMove list, as a consequence any illegal move is also skipped.
                    // In MultiPV mode also skip PV moves which have been already searched.
                       (root_node ?
                            std::find (thread->root_moves.begin () + thread->pv_index, thread->root_moves.end (), move) == thread->root_moves.end () :
                            !pos.legal (move, ci.pinneds)
                       )
                    // Check for move exclusion
                    || move == exclude_move
                   )
                {
                    continue;
                }

                ss->move_count = ++move_count;

                if (   root_node
                    && main_thread != nullptr
                   )
                {
                    auto elapsed_time = main_thread->time_mgr.elapsed_time ();
                    if (elapsed_time > 3*MilliSec)
                    {
                        sync_cout
                            << "info"
                            << " depth "          << depth/DEPTH_ONE
                            << " currmovenumber " << std::setfill ('0') << std::setw (2) << main_thread->pv_index + move_count << std::setfill (' ')
                            << " currmove "       << move_to_can (move, Chess960)
                            << " time "           << elapsed_time
                            << sync_endl;
                    }
                }

                if (PVNode)
                {
                    (ss+1)->pv.clear ();
                }

                bool gives_check = mtype (move) == NORMAL && ci.discoverers == 0 ?
                                    (ci.checking_bb[ptype (pos[org_sq (move)])] & dst_sq (move)) != 0 :
                                    pos.gives_check (move, ci);

                // Step 12. Extend the move which seems dangerous like ...checks etc.
                auto extension = gives_check
                              && pos.see_sign (move) >= VALUE_ZERO ?
                                    DEPTH_ONE : DEPTH_ZERO;

                // Singular extension(SE) search.
                // We extend the TT move if its value is much better than its siblings.
                // If all moves but one fail low on a search of (alfa-s, beta-s),
                // and just one fails high on (alfa, beta), then that move is singular
                // and should be extended. To verify this do a reduced search on all the other moves
                // but the tt_move, if result is lower than tt_value minus a margin then extend tt_move.
                if (   singular_ext_node
                    && move == tt_move
                    && extension == DEPTH_ZERO
                   )
                {
                    auto r_beta = std::max (tt_value - 2*(depth/DEPTH_ONE), -VALUE_INFINITE+1);
                    assert(-VALUE_INFINITE < r_beta && r_beta <= +VALUE_INFINITE);

                    ss->exclude_move = move;
                    ss->skip_pruning = true;
                    value = depth_search<false, CutNode, InCheck> (pos, ss, r_beta-1, r_beta, depth/2);
                    ss->skip_pruning = false;
                    ss->exclude_move = MOVE_NONE;

                    if (value < r_beta)
                    {
                        extension = DEPTH_ONE;
                    }
                }

                // Update the current move (this must be done after singular extension search)
                auto new_depth = depth - DEPTH_ONE + extension;
                bool capture_or_promotion = pos.capture_or_promotion (move);

                // Step 13. Pruning at shallow depth
                if (   !InCheck
                    && !root_node
                    && !MateSearch
                    && !capture_or_promotion
                    && best_value > -VALUE_MATE_IN_MAX_PLY
                    // ! Dangerous conditions
                    && !gives_check
                    && !pos.advanced_pawn_push (move)
                   )
                {
                    // Move count based pruning
                    if (   depth < FutilityMoveCountDepth*DEPTH_ONE
                        && move_count >= FutilityMoveCounts[improving][depth/DEPTH_ONE]
                        && thread->history_values[pos[org_sq (move)]][dst_sq (move)] < Value(10962)
                        && opp_cmv[pos[org_sq (move)]][dst_sq (move)] < Value(10962)
                       )
                    {
                        continue;
                    }
                    // History based pruning
                    if (   depth <= HistoryPruningDepth*DEPTH_ONE
                        && move != ss->killer_moves[0]
                        && thread->history_values[pos[org_sq (move)]][dst_sq (move)] < VALUE_ZERO
                        && opp_cmv[pos[org_sq (move)]][dst_sq (move)] < VALUE_ZERO
                       )
                    {
                        continue;
                    }
                    // Value based pruning
                    auto predicted_depth = std::max (new_depth - reduction_depths (PVNode, improving, depth, move_count), DEPTH_ZERO);
                    // Futility pruning: parent node
                    if (predicted_depth < FutilityMarginDepth*DEPTH_ONE)
                    {
                        auto futility_value = ss->static_eval + FutilityMargins[predicted_depth/DEPTH_ONE] + VALUE_EG_PAWN;

                        if (alfa >= futility_value)
                        {
                            if (best_value < futility_value)
                            {
                                best_value = futility_value;
                            }
                            continue;
                        }
                    }
                    // Negative SEE pruning at low depths
                    if (   predicted_depth < RazorDepth*DEPTH_ONE
                        && pos.see_sign (move) < VALUE_ZERO //-VALUE_MG_PAWN/2
                       )
                    {
                        continue;
                    }
                }

                // Speculative prefetch as early as possible
                prefetch (TT.cluster_entry (pos.move_posi_key (move)));

                ss->current_move = move;
                ss->counter_move_values = &CounterMoveHistoryValues[pos[org_sq (move)]][dst_sq (move)];

                // Step 14. Make the move
                pos.do_move (move, si, gives_check);

                if (   ptype (pos[dst_sq (move)]) == PAWN
                    || pos.capture_type () == PAWN
                   )
                {
                    prefetch (thread->pawn_table[pos.pawn_key ()]);
                }
                if (capture_or_promotion)
                {
                    prefetch (thread->matl_table[pos.matl_key ()]);
                }

                bool full_depth_search;
                // Step 15. Reduced depth search (LMR).
                // If the move fails high will be re-searched at full depth.
                if (   depth >= 3*DEPTH_ONE
                    && move_count > FullDepthMoveCount
                    && !capture_or_promotion
                   )
                {
                    auto dst = dst_sq (move);
                    auto cp  = pos[dst];

                    auto h_value  = thread->history_values[cp][dst];
                    auto cm_value = opp_cmv[cp][dst];

                    const auto *const &fmv1 = (ss-2)->counter_move_values;
                    const auto *const &fmv2 = (ss-4)->counter_move_values;
                    auto fm_value1 = (fmv1 != nullptr ? (*fmv1)[cp][dst] : VALUE_ZERO);
                    auto fm_value2 = (fmv2 != nullptr ? (*fmv2)[cp][dst] : VALUE_ZERO);

                    auto reduction_depth = reduction_depths (PVNode, improving, new_depth, move_count);

                    // Increase reduction for cut node
                    if (   (!PVNode && CutNode)
                        || (h_value < VALUE_ZERO && cm_value <= VALUE_ZERO)
                       )
                    {
                        reduction_depth += DEPTH_ONE;
                    }

                    // Decrease/Increase reduction for moves with a +ve/-ve history
                    auto r_hist = i32(h_value + cm_value + fm_value1 + fm_value2)/20000;
                    reduction_depth = std::max (reduction_depth - r_hist*DEPTH_ONE, DEPTH_ZERO);

                    // Decrease reduction for moves that escape a capture.
                    // Use see() instead of see_sign(), because the destination square is empty for normal move.
                    if (   reduction_depth != DEPTH_ZERO
                        && mtype (move) == NORMAL
                        && ptype (cp) != PAWN
                        && pos.see (mk_move (dst, org_sq (move))) < VALUE_ZERO // SEE of reverse move
                       )
                    {
                        reduction_depth = std::max (reduction_depth-DEPTH_ONE, DEPTH_ZERO);
                    }

                    auto d = new_depth - reduction_depth;
                    // Search with reduced depth
                    value =
                        d < DEPTH_ONE ?
                            gives_check ?
                                -quien_search<false, true > (pos, ss+1, -(alfa+1), -alfa, DEPTH_ZERO) :
                                -quien_search<false, false> (pos, ss+1, -(alfa+1), -alfa, DEPTH_ZERO) :
                            gives_check ?
                                -depth_search<false, true, true > (pos, ss+1, -(alfa+1), -alfa, d) :
                                -depth_search<false, true, false> (pos, ss+1, -(alfa+1), -alfa, d);

                    full_depth_search = alfa < value && reduction_depth > DEPTH_ZERO;
                }
                else
                {
                    full_depth_search = !PVNode || move_count > FullDepthMoveCount;
                }

                // Step 16. Full depth search when LMR is skipped or fails high
                if (full_depth_search)
                {
                    value =
                        new_depth < DEPTH_ONE ?
                            gives_check ?
                                -quien_search<false, true > (pos, ss+1, -(alfa+1), -alfa, DEPTH_ZERO) :
                                -quien_search<false, false> (pos, ss+1, -(alfa+1), -alfa, DEPTH_ZERO) :
                            gives_check ?
                                -depth_search<false, !CutNode, true > (pos, ss+1, -(alfa+1), -alfa, new_depth) :
                                -depth_search<false, !CutNode, false> (pos, ss+1, -(alfa+1), -alfa, new_depth);
                }

                // Do a full PV search on:
                // - 'full depth move count' move
                // - 'fail high' move (search only if value < beta)
                // otherwise let the parent node fail low with
                // alfa >= value and try another move.
                if (   PVNode
                    && (   (0 < move_count && move_count <= FullDepthMoveCount)
                        || (alfa < value && (root_node || value < beta))
                       )
                   )
                {
                    (ss+1)->pv.clear ();

                    value =
                        new_depth < DEPTH_ONE ?
                            gives_check ?
                                -quien_search<true, true > (pos, ss+1, -beta, -alfa, DEPTH_ZERO) :
                                -quien_search<true, false> (pos, ss+1, -beta, -alfa, DEPTH_ZERO) :
                            gives_check ?
                                -depth_search<true, false, true > (pos, ss+1, -beta, -alfa, new_depth) :
                                -depth_search<true, false, false> (pos, ss+1, -beta, -alfa, new_depth);
                }

                // Step 17. Undo move
                pos.undo_move ();

                assert(-VALUE_INFINITE < value && value < +VALUE_INFINITE);

                // Step 18. Check for the new best move
                // Finished searching the move. If a stop or a cutoff occurred,
                // the return value of the search cannot be trusted,
                // and return immediately without updating best move, PV and TT.
                if (ForceStop.load (std::memory_order_relaxed))
                {
                    return VALUE_ZERO;
                }

                if (root_node)
                {
                    auto &root_move = *std::find (thread->root_moves.begin (), thread->root_moves.end (), move);
                    // First PV legal move or new best move ?
                    if (   move_count == 1
                        || alfa < value
                       )
                    {
                        root_move.resize (1);
                        auto &pv = (ss+1)->pv;
                        //assert(!pv.empty ());
                        if (!pv.empty ())
                        {
                            root_move.reserve (pv.size () + 1);
                            std::copy (pv.begin (), pv.end (), std::back_inserter (root_move));
                            //root_move.shrink_to_fit ();
                        }
                        root_move.new_value = value;

                        // Record how often the best move has been changed in each iteration.
                        // This information is used for time management:
                        // When the best move changes frequently, allocate some more time.
                        if (   main_thread != nullptr
                            && main_thread->time_mgr_used
                            && move_count > 1
                           )
                        {
                            main_thread->best_move_change++;
                        }
                    }
                    else
                    {
                        // All other moves but the PV are set to the lowest value, this
                        // is not a problem when sorting becuase sort is stable and move
                        // position in the list is preserved, just the PV is pushed up.
                        root_move.new_value = -VALUE_INFINITE;
                    }
                }

                if (best_value < value)
                {
                    best_value = value;

                    if (alfa < value)
                    {
                        // If there is an easy move for this position, clear it if unstable
                        if (   PVNode
                            && main_thread != nullptr
                            && main_thread->time_mgr_used
                            && main_thread->easy_move_mgr.easy_move (pos.posi_key ()) != MOVE_NONE
                            && (move != main_thread->easy_move_mgr.easy_move (pos.posi_key ()) || move_count > 1)
                           )
                        {
                            main_thread->easy_move_mgr.clear ();
                        }

                        best_move = move;

                        if (   PVNode
                            && !root_node
                           )
                        {
                            update_pv (ss->pv, move, (ss+1)->pv);
                        }
                        // Fail high
                        if (value >= beta)
                        {
                            break;
                        }

                        assert(value < beta);
                        // Update alfa! Always alfa < beta
                        if (PVNode)
                        {
                            alfa = value;
                        }
                    }
                }

                if (   move != best_move
                    && !capture_or_promotion
                   )
                {
                    quiet_moves.push_back (move);
                }
            }
            assert(((root_node || exclude_move != MOVE_NONE || value >= beta) && move_count <= MoveList<LEGAL> (pos).size ()) || move_count == MoveList<LEGAL> (pos).size ());

            // Step 19.
            // The following condition would detect a stop only after move loop has been
            // completed. But in this case bestValue is valid because we have fully
            // searched our subtree, and we can anyhow save the result in TT.
            //if (ForceStop)
            //{
            //    return VALUE_DRAW;
            //}

            //quiet_moves.shrink_to_fit ();

            // Step 20. Check for checkmate and stalemate
            // If all possible moves have been searched and if there are no legal moves,
            // If in a singular extension search then return a fail low score (alfa).
            // Otherwise it must be a checkmate or a stalemate, so return value accordingly.
            if (move_count == 0)
            {
                //assert(ss->current_move == MOVE_NONE);
                best_value = 
                    exclude_move != MOVE_NONE ?
                        alfa :
                        InCheck ?
                            mated_in (ss->ply) :
                            DrawValue[pos.active ()];
            }
            else
            // Quiet best move: update killers, history, countermoves and countermoves history
            if (   best_move != MOVE_NONE
                && !pos.capture_or_promotion (best_move)
               )
            {
                auto itr = std::find (quiet_moves.begin (), quiet_moves.end (), best_move);
                if (itr != quiet_moves.end ())
                {
                    std::swap (*itr, quiet_moves.back ());
                    quiet_moves.pop_back ();
                }
                update_stats (pos, ss, best_move, depth, quiet_moves);
            }
            else
            // Bonus for prior countermove that caused the fail low
            if (   !InCheck
                && depth >= 3*DEPTH_ONE
                && best_move == MOVE_NONE
                && (ss-1)->counter_move_values != nullptr
                && pos.capture_type () == NONE
                //&& mtype ((ss-1)->current_move) != PROMOTE
               )
            {
                auto bonus = Value((depth/DEPTH_ONE)*((depth/DEPTH_ONE) + 1) - 1);
                auto opp_move_dst = dst_sq ((ss-1)->current_move);
                
                auto *const &ocmv  = (ss-2)->counter_move_values;
                auto *const &ofmv1 = (ss-3)->counter_move_values;
                auto *const &ofmv2 = (ss-5)->counter_move_values;

                if (ocmv != nullptr)
                {
                    ocmv->update (pos[opp_move_dst], opp_move_dst, bonus);
                }
                if (ofmv1 != nullptr)
                {
                    ofmv1->update (pos[opp_move_dst], opp_move_dst, bonus);
                }
                if (ofmv2 != nullptr)
                {
                    ofmv2->update (pos[opp_move_dst], opp_move_dst, bonus);
                }
            }

            if (   tt_hit
                && tte->key16 () != u16(posi_key >> 0x30)
               )
            {
                tte = TT.probe (posi_key, tt_hit);
            }
            tte->save (posi_key, best_move, value_to_tt (best_value, ss->ply), ss->static_eval, depth,
                best_value >= beta ? BOUND_LOWER : PVNode && best_move != MOVE_NONE ? BOUND_EXACT : BOUND_UPPER, TT.generation ());

            assert(-VALUE_INFINITE < best_value && best_value < +VALUE_INFINITE);
            return best_value;
        }
    }

    // ------------------------------------

    // RootMove::insert_pv_into_tt() is called at the end of a search iteration, and
    // inserts the PV back into the TT. This makes sure the old PV moves are searched
    // first, even if the old TT entries have been overwritten.
    void RootMove::insert_pv_into_tt (Position &pos)
    {
        StateInfo states[MaxPlies], *si = states;

        //shrink_to_fit ();
        u08 ply = 0;
        for (const auto m : *this)
        {
            assert(m != MOVE_NONE && MoveList<LEGAL> (pos).contains (m));

            bool tt_hit;
            auto *tte = TT.probe (pos.posi_key (), tt_hit);
            // Don't overwrite correct entries
            if (   !tt_hit
                || tte->move () != m
               )
            {
                tte->save (pos.posi_key (), m, VALUE_NONE, /*pos.checkers () != 0 ?*/ VALUE_NONE /*: evaluate (pos)*/, DEPTH_NONE, BOUND_NONE, TT.generation ());
            }
            pos.do_move (m, *si++, pos.gives_check (m, CheckInfo (pos)));
            ++ply;
        }
        for (; ply != 0; --ply)
        {
            pos.undo_move ();
        }
    }
    // RootMove::extract_ponder_move_from_tt() is called in case have no ponder move before
    // exiting the search, for instance, in case stop the search during a fail high at root.
    // Try hard to have a ponder move which has to return to the GUI,
    // otherwise in case of 'ponder on' we have nothing to think on.
    bool RootMove::extract_ponder_move_from_tt (Position &pos)
    {
        assert(size () == 1);
        assert(_ok (at (0)));

        bool extracted = false;
        StateInfo si;
        auto m = at (0);
        pos.do_move (m, si, pos.gives_check (m, CheckInfo (pos)));
        bool tt_hit;
        auto *tte = TT.probe (pos.posi_key (), tt_hit);
        if (tt_hit)
        {
            m = tte->move (); // Local copy to be SMP safe
            if (   m != MOVE_NONE
                && MoveList<LEGAL> (pos).contains (m)
               )
            {
               *this += m;
               extracted = true;
            }
        }
        pos.undo_move ();
        //shrink_to_fit ();
        return extracted;
    }

    RootMove::operator string () const
    {
        ostringstream oss;
        for (const auto m : *this)
        {
            assert(_ok (m));
            oss << " " << move_to_can (m, Chess960);
        }
        return oss.str ();
    }

    // ------------------------------------

    // perft<>() is utility to verify move generation.
    // All the leaf nodes up to the given depth are generated, and the sum is returned.
    template<bool RootNode>
    u64 perft (Position &pos, Depth depth)
    {
        u64 leaf_nodes = 0;
        for (const auto &vm : MoveList<LEGAL> (pos))
        {
            u64 inter_nodes;
            if (   RootNode
                && depth <= 1*DEPTH_ONE
               )
            {
                inter_nodes = 1;
            }
            else
            {
                StateInfo si;
                pos.do_move (vm.move, si, pos.gives_check (vm.move, CheckInfo (pos)));
                inter_nodes = depth <= 2*DEPTH_ONE ?
                    MoveList<LEGAL> (pos).size () :
                    perft<false> (pos, depth-DEPTH_ONE);
                pos.undo_move ();
            }

            if (RootNode)
            {
                sync_cout << std::left
                    << std::setw ( 7)
                    //<< move_to_can (vm.move, Chess960)
                    << move_to_san (vm.move, pos)
                    << std::right << std::setfill ('.')
                    << std::setw (16) << inter_nodes
                    << std::setfill (' ') << std::left
                    << sync_endl;
            }

            leaf_nodes += inter_nodes;
        }

        return leaf_nodes;
    }
    // Explicit template instantiations
    // --------------------------------
    template u64 perft<false> (Position&, Depth);
    template u64 perft<true > (Position&, Depth);

    // initialize() is called during startup to initialize various lookup tables
    void initialize ()
    {
        static const i32 K0[3] =
        { 0, 200, 0 };
        for (i32 d = 0; d < FutilityMarginDepth; ++d)
        {
            FutilityMargins[d] = Value(K0[0] + (K0[1] + K0[2]*d)*d);
        }

        static const double K1[2][4] =
        {
            { 1.90, 0.773, 0.00, 1.8 },
            { 2.40, 1.045, 0.49, 1.8 }
        };
        for (u08 imp = 0; imp <= 1; ++imp)
        {
            for (i32 d = 0; d < FutilityMoveCountDepth; ++d)
            {
                FutilityMoveCounts[imp][d] = u08(std::round (K1[imp][0] + K1[imp][1] * pow (d + K1[imp][2], K1[imp][3])));
                //printf ("\n%2d", FutilityMoveCounts[imp][d]);
            }
        }

        for (u08 imp = 0; imp <= 1; ++imp)
        {
            //printf ("\n\nimproving=%d\n", imp);
            for (i32 d = 1; d < ReductionDepth; ++d)
            {
                for (u08 mc = 1; mc < ReductionMoveCount; ++mc)
                {
                    auto r = log (d) * log (mc) / 2;

                    if (r >= 0.80)
                    {
                        ReductionDepths[0][imp][d][mc] = i32(std::round (r))*DEPTH_ONE;
                        ReductionDepths[1][imp][d][mc] = std::max (ReductionDepths[0][imp][d][mc] - DEPTH_ONE, DEPTH_ZERO);
                        // If evaluation is not improving increase reduction
                        if (!imp && ReductionDepths[0][imp][d][mc] >= 2*DEPTH_ONE)
                        {
                            ReductionDepths[0][imp][d][mc] += DEPTH_ONE;
                        }
                    }
                    //if (d%4 == 1 && mc%4 == 1) printf ("%2d", ReductionDepths[0][imp][d][mc]);
                }
                //if (d%4 == 1) printf ("\n");
            }
        }
    }
    // clear() resets search state to zero, to obtain reproducible results
    void clear ()
    {
        TT.clear ();
        CounterMoveHistoryValues.clear ();

        for (auto *th : Threadpool)
        {
            th->history_values.clear ();
            th->counter_moves.clear ();
        }
        if (Threadpool.main ()->time_mgr_used)
        {
            Threadpool.main ()->previous_value = +VALUE_INFINITE;
        }
    }
}

namespace Threading {

    using namespace Searcher;

    // Thread::search() is the main iterative deepening loop. It calls depth_search()
    // repeatedly with increasing depth until
    // - the allocated thinking time has been consumed,
    // - the user stops the search,
    // - the maximum search depth is reached.
    void Thread::search ()
    {
        Stack stacks[MaxPlies+7], *ss = stacks+5; // To allow referencing (ss-5) and (ss+2)
        for (auto s = stacks; s < stacks+MaxPlies+7; ++s)
        {
            s->ply = i16(s - stacks - 4);
            if (s <= stacks + 8)
            {
                s->current_move = MOVE_NONE;
                s->exclude_move = MOVE_NONE;
                std::fill (s->killer_moves, s->killer_moves + Killers, MOVE_NONE);
                s->static_eval = VALUE_ZERO;
                s->move_count = 0;
                s->skip_pruning = false;
                s->counter_move_values = nullptr;
            }
            assert(s->pv.empty ());
        }

        auto *main_thread = Threadpool.main () == this ? Threadpool.main () : nullptr;
        auto easy_move = MOVE_NONE;

        if (main_thread != nullptr)
        {
            TT.generation (root_pos.game_ply () + 1);

            if (main_thread->time_mgr_used)
            {
                easy_move = main_thread->easy_move_mgr.easy_move (root_pos.posi_key ());
                main_thread->easy_move_mgr.clear ();
                main_thread->easy_played = false;
                main_thread->best_move_change = 0.0;
            }
            if (main_thread->skill_mgr.enabled ())
            {
                main_thread->skill_mgr.clear ();
            }
        }

        // Do have to play with skill handicap?
        // In this case enable MultiPV search by skill pv size
        // that will use behind the scenes to get a set of possible moves.
        PVLimit = std::min (std::max (MultiPV, u16(Threadpool.main ()->skill_mgr.enabled () ? SkillManager::MinMultiPV : 0)), u16(root_moves.size ()));

        auto best_value = VALUE_ZERO
           , window     = VALUE_ZERO
           , alfa       = -VALUE_INFINITE
           , beta       = +VALUE_INFINITE;

        leaf_depth = DEPTH_ZERO;

        bool in_check = root_pos.checkers () != 0;

        // Iterative deepening loop until requested to stop or the target depth is reached.
        while (   !ForceStop
               && ++root_depth < DEPTH_MAX
               && (Limits.depth == 0 || root_depth <= Limits.depth)
              )
        {
            if (main_thread != nullptr)
            {
                main_thread->failed_low = false;
                if (main_thread->time_mgr_used)
                {
                    // Age out PV variability metric
                    main_thread->best_move_change *= 0.505;
                }
            }
            else
            {
                static const size_t HalfDensityMapSize = 30;
                // Rotating symmetric patterns with increasing skipsize.
                // Set of rows with half bits set to 1 and half to 0.
                // It is used to allocate the search depths across the threads.
                static const BoolVector HalfDensityMap[HalfDensityMapSize] =
                {
                    { false, true },
                    { true, false },

                    { false, false, true, true },
                    { false, true, true, false },
                    { true, true, false, false },
                    { true, false, false, true },

                    { false, false, false, true, true, true },
                    { false, false, true, true, true, false },
                    { false, true, true, true, false, false },
                    { true, true, true, false, false, false },
                    { true, true, false, false, false, true },
                    { true, false, false, false, true, true },

                    { false, false, false, false, true, true, true, true },
                    { false, false, false, true, true, true, true, false },
                    { false, false, true, true, true, true, false, false },
                    { false, true, true, true, true, false, false, false },
                    { true, true, true, true, false, false, false, false },
                    { true, true, true, false, false, false, false, true },
                    { true, true, false, false, false, false, true, true },
                    { true, false, false, false, false, true, true, true },

                    { false, false, false, false, false, true, true, true, true, true },
                    { false, false, false, false, true, true, true, true, true, false },
                    { false, false, false, true, true, true, true, true, false, false },
                    { false, false, true, true, true, true, true, false, false, false },
                    { false, true, true, true, true, true, false, false, false, false },
                    { true, true, true, true, true, false, false, false, false, false },
                    { true, true, true, true, false, false, false, false, false, true },
                    { true, true, true, false, false, false, false, false, true, true },
                    { true, true, false, false, false, false, false, true, true, true },
                    { true, false, false, false, false, false, true, true, true, true },
                };

                const auto &hdm = HalfDensityMap[(index - 1) % HalfDensityMapSize];
                if (hdm[(u16(root_depth) + root_pos.game_ply ()) % hdm.size ()])
                {
                    continue;
                }
            }

            // Save the last iteration's scores before first PV line is searched and
            // all the move scores but the (new) PV are set to -VALUE_INFINITE.
            for (auto &rm : root_moves)
            {
                rm.old_value = rm.new_value;
            }

            const bool aspiration = root_depth > 4*DEPTH_ONE;

            // MultiPV loop. Perform a full root search for each PV line
            for (pv_index = 0; !ForceStop && pv_index < PVLimit; ++pv_index)
            {
                // Reset aspiration window starting size.
                if (aspiration)
                {
                    window = 
                        Value(18);
                        //Value(root_depth <= 32*DEPTH_ONE ? 16 + (root_depth/DEPTH_ONE-1)/4 : 24); // Increasing window

                    alfa = std::max (root_moves[pv_index].old_value - window, -VALUE_INFINITE);
                    beta = std::min (root_moves[pv_index].old_value + window, +VALUE_INFINITE);
                }

                // Start with a small aspiration window and, in case of fail high/low,
                // research with bigger window until not failing high/low anymore.
                do
                {
                    best_value =
                        in_check ?
                            depth_search<true, false, true > (root_pos, ss, alfa, beta, root_depth) :
                            depth_search<true, false, false> (root_pos, ss, alfa, beta, root_depth);

                    // Bring the best move to the front. It is critical that sorting is
                    // done with a stable algorithm because all the values but the first
                    // and eventually the new best one are set to -VALUE_INFINITE and
                    // want to keep the same order for all the moves but the new PV
                    // that goes to the front. Note that in case of MultiPV search
                    // the already searched PV lines are preserved.
                    std::stable_sort (root_moves.begin () + pv_index, root_moves.end ());

                    // Write PV back to the transposition table in case the relevant
                    // entries have been overwritten during the search.
                    for (u16 i = 0; i <= pv_index; ++i)
                    {
                        root_moves[i].insert_pv_into_tt (root_pos);
                    }

                    // If search has been stopped, break immediately.
                    // Sorting and writing PV back to TT is safe becuase
                    // root moves is still valid, although refers to the previous iteration.
                    if (ForceStop)
                    {
                        break;
                    }

                    // When failing high/low give some update
                    // (without cluttering the UI) before to re-search.
                    if (   main_thread != nullptr
                        && PVLimit == 1
                        && (alfa >= best_value || best_value >= beta)
                        && main_thread->time_mgr.elapsed_time () > 3*MilliSec
                       )
                    {
                        sync_cout << multipv_info (alfa, beta) << sync_endl;
                    }

                    // In case of failing low/high increase aspiration window and re-search,
                    // otherwise exit the loop.
                    if (best_value <= alfa)
                    {
                        beta = (alfa + beta) / 2;
                        alfa = std::max (best_value - window, -VALUE_INFINITE);

                        if (main_thread != nullptr)
                        {
                            main_thread->failed_low = true;
                            PonderhitStop = false;
                        }
                    }
                    else
                    if (best_value >= beta)
                    {
                        alfa = (alfa + beta) / 2;
                        beta = std::min (best_value + window, +VALUE_INFINITE);
                    }
                    else
                    {
                        break;
                    }

                    window += window / 4 + 5;

                    assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
                }
                while (true);

                // Sort the PV lines searched so far and update the GUI
                std::stable_sort (root_moves.begin (), root_moves.begin () + pv_index + 1);

                if (main_thread != nullptr)
                {
                    if (ForceStop)
                    {
                        sync_cout
                            << "info"
                            << " nodes " << root_pos.game_nodes ()
                            << " time "  << main_thread->time_mgr.elapsed_time ()
                            << sync_endl;
                    }
                    else
                    if (   PVLimit == (pv_index + 1)
                        || main_thread->time_mgr.elapsed_time () > 3*MilliSec
                       )
                    {
                        sync_cout << multipv_info (alfa, beta) << sync_endl;
                    }
                }
            }

            if (!ForceStop)
            {
                leaf_depth = root_depth;
            }

            if (main_thread != nullptr)
            {
                if (   ContemptValue != 0
                    && !root_moves.empty ()
                   )
                {
                    auto valued_contempt = Value(i32(root_moves[0].new_value)/ContemptValue);
                    DrawValue[ root_pos.active ()] = BaseContempt[ root_pos.active ()] - valued_contempt;
                    DrawValue[~root_pos.active ()] = BaseContempt[~root_pos.active ()] + valued_contempt;
                }

                // If skill level is enabled and can pick move, pick a sub-optimal best move
                if (   main_thread->skill_mgr.enabled ()
                    && main_thread->skill_mgr.can_pick (root_depth)
                    && !main_thread->root_moves.empty ()
                   )
                {
                    main_thread->skill_mgr.clear ();
                    main_thread->skill_mgr.pick_best_move (PVLimit);
                }

                if (LogStream.is_open ())
                {
                    LogStream << pretty_pv_info () << std::endl;
                }

                if (   !ForceStop
                    && !PonderhitStop
                   )
                {
                    // Stop the search early:
                    bool stop = false;

                    // Do have time for the next iteration? Can stop searching now?
                    if (main_thread->time_mgr_used)
                    {
                        // Take some extra time if the best move has changed
                        double instability_factor = 1.010*(1.0 + (aspiration && PVLimit == 1 ? main_thread->best_move_change : 0.0));

                        // Stop the search
                        // If there is only one legal move available or
                        // If all of the available time has been used or
                        // If matched an easy move from the previous search and just did a fast verification.
                        if (   root_moves.size () == 1
                            || (main_thread->time_mgr.elapsed_time () > TimePoint(std::round (main_thread->time_mgr.optimum_time () * instability_factor *
                                    (320.0 - 80.0 * (!main_thread->failed_low)
                                           - 63.0 * (best_value >= main_thread->previous_value)
                                           - 62.0 * (!main_thread->failed_low && best_value >= main_thread->previous_value)
                                    ) / 320.0))
                               )
                            || (main_thread->easy_played =
                                    (  !root_moves.empty ()
                                    && !root_moves[0].empty ()
                                    &&  root_moves[0] == easy_move
                                    && main_thread->best_move_change < 0.03
                                    && main_thread->time_mgr.elapsed_time () > TimePoint(std::round (main_thread->time_mgr.optimum_time () * instability_factor * 25.0 / 206.0))
                                    ), main_thread->easy_played
                               )
                           )
                        {
                            stop = true;
                        }

                        if (   !root_moves.empty ()
                            &&  root_moves[0].size () >= EasyMoveManager::PVSize
                           )
                        {
                            main_thread->easy_move_mgr.update (root_pos, root_moves[0]);
                        }
                        else
                        {
                            main_thread->easy_move_mgr.clear ();
                        }
                    }
                    else
                    // Stop if have found a "mate in <x>"
                    if (   MateSearch
                        && best_value >= +VALUE_MATE - 2*Limits.mate
                       )
                    {
                        stop = true;
                    }

                    if (stop)
                    {
                        // If allowed to ponder do not stop the search now but
                        // keep pondering until GUI sends "ponderhit" or "stop".
                        if (Limits.ponder)
                        {
                            PonderhitStop = true;
                        }
                        else
                        {
                            ForceStop = true;
                        }
                    }
                }
            }
        }

        if (main_thread != nullptr)
        {
            // Clear any candidate easy move that wasn't stable for the last search iterations;
            // the second condition prevents consecutive fast moves.
            if (   main_thread->time_mgr_used
                && (   main_thread->easy_played
                    || main_thread->easy_move_mgr.stable_count < 6
                   )
               )
            {
                main_thread->easy_move_mgr.clear ();
            }
            // If skill level is enabled, swap best PV line with the sub-optimal one
            if (   main_thread->skill_mgr.enabled ()
                && !main_thread->root_moves.empty ()
               )
            {
                std::swap (main_thread->root_moves[0], *std::find (main_thread->root_moves.begin (), main_thread->root_moves.end (), main_thread->skill_mgr.pick_best_move (PVLimit)));
            }
        }
    }
    // MainThread::search() is called by the main thread when the program receives
    // the UCI 'go' command. It searches from root position and and outputs the "bestmove" and "ponder".
    void MainThread::search ()
    {
        static Book book; // Defined static to initialize the PRNG only once

        assert(this == Threadpool.main ());

        MateSearch = Limits.mate != 0;
        time_mgr_used = Limits.time_management_used ();
        if (time_mgr_used)
        {
            // Initialize the time manager before searching.
            time_mgr.initialize (root_pos.active (), root_pos.game_ply ());
        }

        if (   !white_spaces (LogFile)
            && LogFile != "<empty>"
           )
        {
            LogStream.open (LogFile, ios_base::out|ios_base::app);

            LogStream
                << "----------->\n" << boolalpha
                << "RootPos  : " << root_pos.fen ()                 << "\n"
                << "RootSize : " << root_moves.size ()              << "\n"
                << "Infinite : " << Limits.infinite                 << "\n"
                << "Ponder   : " << Limits.ponder                   << "\n"
                << "ClockTime: " << Limits.clock[root_pos.active ()].time << "\n"
                << "Increment: " << Limits.clock[root_pos.active ()].inc  << "\n"
                << "MoveTime : " << Limits.movetime                 << "\n"
                << "MovesToGo: " << u16(Limits.movestogo)           << "\n"
                << " Depth Score    Time       Nodes  PV\n"
                << "-----------------------------------------------------------"
                << noboolalpha << std::endl;
        }

        bool filtering = false;

        if (root_moves.empty ())
        {
            root_moves += RootMove ();

            sync_cout
                << "info"
                << " depth " << 0
                << " score " << to_string (root_pos.checkers () != 0 ? -VALUE_MATE : VALUE_DRAW)
                << " time "  << 0
                << sync_endl;
        }
        else
        {
            // Check if can play with own book.
            if (   OwnBook
                && !BookFile.empty ()
                && (BookUptoMove == 0 || root_pos.move_num () <= BookUptoMove)
                && !MateSearch
                && !Limits.infinite
               )
            {
                book.open (BookFile, ios_base::in);
                bool found = false;
                auto book_best_move = book.probe_move (root_pos, BookMoveBest);
                if (   book_best_move != MOVE_NONE
                    && std::find (root_moves.begin (), root_moves.end (), book_best_move) != root_moves.end ()
                   )
                {
                    found = true;
                    std::swap (root_moves[0], *std::find (root_moves.begin (), root_moves.end (), book_best_move));
                    StateInfo si;
                    root_pos.do_move (book_best_move, si, root_pos.gives_check (book_best_move, CheckInfo (root_pos)));
                    auto book_ponder_move = book.probe_move (root_pos, BookMoveBest);
                    root_moves[0] += book_ponder_move;
                    root_pos.undo_move ();
                }
                book.close ();
                if (found)
                {
                    goto finish;
                }
            }

            i16 timed_contempt = 0;
            i64 diff_time;
            if (   time_mgr_used
                && ContemptTime != 0
                && (diff_time = i64(  Limits.clock[ root_pos.active ()].time
                                    - Limits.clock[~root_pos.active ()].time)/MilliSec) != 0
                //&& ContemptTime <= abs (diff_time)
               )
            {
                timed_contempt = i16(diff_time/ContemptTime);
            }

            auto contempt = cp_to_value ((FixedContempt + timed_contempt) / 100.0);
            DrawValue[ root_pos.active ()] = BaseContempt[ root_pos.active ()] = VALUE_DRAW - contempt;
            DrawValue[~root_pos.active ()] = BaseContempt[~root_pos.active ()] = VALUE_DRAW + contempt;

            TBDepthLimit = i32(Options["Syzygy Depth Limit"])*DEPTH_ONE;
            TBPieceLimit = i32(Options["Syzygy Piece Limit"]);
            TBUseRule50  = bool(Options["Syzygy Use Rule 50"]);
            TBHits       = 0;
            TBHasRoot    = false;

            // Skip TB probing when no TB found: !MaxPieceLimit -> !TB::PieceLimit
            if (TBPieceLimit > MaxPieceLimit)
            {
                TBPieceLimit = MaxPieceLimit;
                TBDepthLimit = DEPTH_ZERO;
            }

            if (   TBPieceLimit >= root_pos.count<NONE> ()
                && root_pos.can_castle (CR_ANY) == CR_NONE
               )
            {
                // If the current root position is in the tablebases,
                // then RootMoves contains only moves that preserve the draw or the win.
                TBHasRoot = root_probe_dtz (root_pos, root_moves);

                if (TBHasRoot)
                {
                    // Do not probe tablebases during the search
                    TBPieceLimit = 0;
                }
                // If DTZ tables are missing, use WDL tables as a fallback
                else
                {
                    // Filter out moves that do not preserve the draw or the win
                    TBHasRoot = root_probe_wdl (root_pos, root_moves);

                    // Only probe during search if winning
                    if (   TBHasRoot
                        && ProbeValue <= VALUE_DRAW
                       )
                    {
                        TBPieceLimit = 0;
                    }
                }

                if (TBHasRoot)
                {
                    TBHits = u16(root_moves.size ());

                    if (!TBUseRule50)
                    {
                        ProbeValue = ProbeValue > VALUE_DRAW ? +VALUE_MATE - i32(MaxPlies - 1) :
                                     ProbeValue < VALUE_DRAW ? -VALUE_MATE + i32(MaxPlies + 1) :
                                     VALUE_DRAW;
                    }
                }
            }

            for (auto *th : Threadpool)
            {
                if (th != this)
                {
                    th->start_searching (false);
                }
            }

            filtering = true;
            Thread::search (); // Let's start searching !

            // Update the time manager after searching.
            if (time_mgr_used)
            {
                time_mgr.update (root_pos.active ());
            }
        }

    finish:
        // When reach max depth arrive here even without Force Stop is raised,
        // but if are pondering or in infinite search, according to UCI protocol,
        // shouldn't print the best move before the GUI sends a "stop" or "ponderhit" command.
        // Simply wait here until GUI sends one of those commands (that raise Force Stop).
        if (   !ForceStop
            && (Limits.infinite || Limits.ponder)
           )
        {
            PonderhitStop = true;
            wait_until (ForceStop);
        }

        if (filtering)
        {
            // Stop the threads if not already stopped.
            ForceStop = true;

            // Wait until all threads have finished.
            for (size_t i = 1; i < Threadpool.size (); ++i)
            {
                Threadpool[i]->wait_while_searching ();
            }

            // Check if there are threads with bigger depth than main thread.
            Thread *best_thread = this;
            if (   !easy_played
                && PVLimit == 1
                && !skill_mgr.enabled ()
               )
            {
                for (size_t i = 1; i < Threadpool.size (); ++i)
                {
                    if (   best_thread->root_moves[0].new_value < Threadpool[i]->root_moves[0].new_value
                        && (   best_thread->leaf_depth < Threadpool[i]->leaf_depth
                            || (   best_thread->leaf_depth <= Threadpool[i]->leaf_depth
                                && best_thread->max_ply < Threadpool[i]->max_ply
                               )
                           )
                       )
                    {
                        best_thread = Threadpool[i];
                    }
                }
            }
            // Send new PV when needed.
            if (best_thread != this)
            {
                pv_index    = best_thread->pv_index;
                max_ply     = best_thread->max_ply;
                //root_pos    = Position (best_thread->root_pos, this); // No need!
                root_moves  = best_thread->root_moves;
                root_depth  = best_thread->root_depth;
                leaf_depth  = best_thread->leaf_depth;

                sync_cout << multipv_info (-VALUE_INFINITE, +VALUE_INFINITE) << sync_endl;
            }
        }

        assert(!root_moves.empty ()
            && !root_moves[0].empty ());

        if (time_mgr_used)
        {
            previous_value = root_moves[0].new_value;
        }

        if (LogStream.is_open ())
        {
            auto elapsed_time = std::max (time_mgr.elapsed_time (), TimePoint(1));

            LogStream
                << "Time (ms)  : " << elapsed_time                                      << "\n"
                << "Nodes (N)  : " << Threadpool.game_nodes ()                          << "\n"
                << "Speed (N/s): " << Threadpool.game_nodes ()*MilliSec / elapsed_time  << "\n"
                << "Hash-full  : " << TT.hash_full ()                                   << "\n"
                << "Best Move  : " << move_to_san (root_moves[0][0], root_pos)          << "\n";
            if (   root_moves[0][0] != MOVE_NONE
                && (root_moves[0].size () > 1 || root_moves[0].extract_ponder_move_from_tt (root_pos))
               )
            {
                StateInfo si;
                root_pos.do_move (root_moves[0][0], si, root_pos.gives_check (root_moves[0][0], CheckInfo (root_pos)));
                LogStream << "Ponder Move: " << move_to_san (root_moves[0][1], root_pos) << "\n";
                root_pos.undo_move ();
            }
            LogStream << std::endl;
            LogStream.close ();
        }
        // Best move could be MOVE_NONE when searching on a stalemate position.
        sync_cout << "bestmove " << move_to_can (root_moves[0][0], Chess960);
        if (   root_moves[0][0] != MOVE_NONE
            && (root_moves[0].size () > 1 || root_moves[0].extract_ponder_move_from_tt (root_pos))
           )
        {
            std::cout << " ponder " << move_to_can (root_moves[0][1], Chess960);
        }
        std::cout << sync_endl;
    }
}
