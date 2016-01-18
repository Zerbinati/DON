#include "Searcher.h"

#include <sstream>
#include <iomanip>
#include <cmath>
#include <iterator>

#include "PRNG.h"
#include "MovePicker.h"
#include "Transposition.h"
#include "Evaluator.h"
#include "Thread.h"
#include "TBsyzygy.h"
#include "UCI.h"
#include "Notation.h"
#include "Debugger.h"
#include "Polyglot.h"

using namespace std;

namespace Searcher {

    using namespace BitBoard;
    using namespace MoveGen;
    using namespace MovePick;
    using namespace Transposition;
    using namespace Evaluator;
    using namespace Threading;
    using namespace TBSyzygy;
    using namespace Notation;
    using namespace Debugger;

    bool            Chess960        = false;

    Limit           Limits;
    atomic_bool     ForceStop       { false }  // Stop search on request
        ,           PonderhitStop   { false }; // Stop search on ponder-hit

    StateStackPtr   SetupStates;

    u16             MultiPV         = 1;
    //i32             MultiPV_cp      = 0;

    i16             FixedContempt   = 0
        ,           ContemptTime    = 30
        ,           ContemptValue   = 50;

    string          HashFile        = "Hash.dat";
    u16             AutoSaveHashTime= 0;

    bool            OwnBook         = false;
    string          BookFile        = "Book.bin";
    bool            BookMoveBest    = true;
    i16             BookUptoMove    = 20;

    Depth           TBDepthLimit    = 1*DEPTH_ONE;
    i32             TBPieceLimit    = 6;
    bool            TBUseRule50     = true;
    u16             TBHits          = 0;
    bool            TBHasRoot       = false;

    string          LogFile         = "<empty>";

    SkillManager    SkillMgr;

    // ------------------------------------

    namespace {

// prefetch() preloads the given address in L1/L2 cache.
// This is a non-blocking function that doesn't stall
// the CPU waiting for data to be loaded from memory,
// which can be quite slow.
#ifdef PREFETCH

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
        template<bool PVNode>
        Depth reduction_depths (bool imp, Depth d, u08 mc)
        {
            return ReductionDepths[PVNode][imp][min (d/DEPTH_ONE, ReductionDepth-1)][min (mc/1, ReductionMoveCount-1)];
        }

        const i32 ProbCutDepth = 4;
        const i32 HistoryPruningDepth = 4;
        const i32 NullVerificationDepth = 12;

        const i32 LateMoveReductionDepth = 3;
        const u08 FullDepthMoveCount = 1;

        const u08 TimerResolution = 5; // Seconds between two check_limits() calls

        Color   RootColor;

        bool    MateSearch  = false;

        u16     PVLimit;

        Value   DrawValue[CLR_NO]
            ,   BaseContempt[CLR_NO];

        // Counter move history value statistics
        CM2DValueStats CounterMovesHistory;

        bool    LogWrite    = false;
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
                || (Limits.movetime != 0   && elapsed_time >= Limits.movetime)
                || (Limits.nodes != U64(0) && Threadpool.game_nodes () >= Limits.nodes)
               )
            {
                ForceStop = true;
            }
        }

        // update_stats() updates killers, history, countermoves and countermoves history
        // stats for a quiet best move.
        void update_stats (const Position &pos, Stack *ss, Move move, Depth depth, const MoveVector &quiet_moves)
        {
            if (ss->killer_moves[0] != move)
            {
                ss->killer_moves[1] = ss->killer_moves[0];
                //std::copy_backward (ss->killer_moves, ss->killer_moves + Killers - 1, ss->killer_moves + Killers);
                ss->killer_moves[0] = move;
            }

            auto bonus = Value((depth/DEPTH_ONE)*((depth/DEPTH_ONE) + 1) - 1);

            auto opp_move_dst = dst_sq ((ss-1)->current_move);
            auto opp_move_ok  = _ok ((ss-1)->current_move);
            auto &opp_cmv = CounterMovesHistory[pos[opp_move_dst]][opp_move_dst];

            auto *thread = pos.thread ();

            thread->history_values.update (pos[org_sq (move)], dst_sq (move), bonus);
            if (opp_move_ok)
            {
                thread->counter_moves.update (pos[opp_move_dst], opp_move_dst, move);
                opp_cmv.update (pos[org_sq (move)], dst_sq (move), bonus);
            }

            // Decrease all the other played quiet moves
            assert(std::find (quiet_moves.begin (), quiet_moves.end (), move) == quiet_moves.end ());
            for (const auto m : quiet_moves)
            {
                assert(m != move);
                thread->history_values.update (pos[org_sq (m)], dst_sq (m), -bonus);
                if (opp_move_ok)
                {
                    opp_cmv.update (pos[org_sq (m)], dst_sq (m), -bonus);
                }
            }

            // Extra penalty for PV move in previous ply when it gets refuted
            if (   (ss-1)->move_count == 1
                && pos.capture_type () == NONE
                && opp_move_ok // _ok ((ss-1)->current_move)
                //&& mtype ((ss-1)->current_move) != PROMOTE
                && _ok ((ss-2)->current_move)
               )
            {
                auto own_move_dst = dst_sq ((ss-2)->current_move);
                auto &own_cmv = CounterMovesHistory[pos[own_move_dst]][own_move_dst];
                own_cmv.update (pos[opp_move_dst], opp_move_dst, -bonus - 2*(depth/DEPTH_ONE) - 2);
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
            if (    pv.size () == 0
                ||  pv[0] != move
                ||  child_pv.size () + 1 != pv.size ()
                || (child_pv.size () > 0 && pv.size () > 1 && child_pv[0] != pv[1])
                || (child_pv.size () > 1 && pv.size () > 2 && child_pv[1] != pv[2])
               )
            {
                auto new_pv = MoveVector ();
                new_pv.push_back (move);
                if (child_pv.size () != 0)
                {
                    new_pv.reserve (child_pv.size () + 1);
                    std::copy (child_pv.begin (), child_pv.end (), std::back_inserter (new_pv));
                    //new_pv.shrink_to_fit ();
                }
                pv = new_pv;
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
            return v == VALUE_NONE               ? VALUE_NONE :
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

                if (i <= main_thread->pv_index)  // New updated pv?
                {
                    d = main_thread->root_depth;
                    v = main_thread->root_moves[i].new_value;
                }
                else                        // Old expired pv?
                {
                    if (DEPTH_ONE == main_thread->root_depth) continue;

                    d = main_thread->root_depth - DEPTH_ONE;
                    v = main_thread->root_moves[i].old_value;
                }
                
                bool tb = TBHasRoot && abs (v) < VALUE_MATE - i32(MaxPly);

                // Not at first line
                if (ss.rdbuf ()->in_avail ())
                {
                    ss  << "\n";
                }

                ss  << "info"
                    << " multipv "  << i + 1
                    << " depth "    << d/DEPTH_ONE
                    << " seldepth " << main_thread->max_ply
                    << " score "    << to_string (tb ? ProbeValue : v);
                if (!tb && i == main_thread->pv_index)
                    ss  << (beta <= v ? " lowerbound" : v <= alfa ? " upperbound" : "");
                ss  << " nodes "    << game_nodes
                    << " time "     << elapsed_time
                    << " nps "      << game_nodes * MilliSec / elapsed_time;
                if (elapsed_time > MilliSec)
                    ss  << " hashfull " << TT.hash_full (); // Earlier makes little sense
                ss  << " tbhits "   << TBHits
                    << " pv"        << main_thread->root_moves[i];
            }
            return ss.str ();
        }

        template<NodeType NT>
        // quien_search<>() is the quiescence search function,
        // which is called by the main depth limited search function
        // when the remaining depth is less than DEPTH_ONE.
        Value quien_search  (Position &pos, Stack *ss, Value alfa, Value beta, Depth depth, bool in_check)
        {
            const bool PVNode = NT == PV;

            assert((ss-1)->ply != 0);
            assert(in_check == (pos.checkers () != U64(0)));
            assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            assert(PVNode || alfa == beta-1);
            assert(depth <= DEPTH_ZERO);

            auto pv_alfa = -VALUE_INFINITE;

            if (PVNode)
            {
                pv_alfa = alfa; // To flag BOUND_EXACT when eval above alfa and no available moves

                ss->pv = MoveVector ();
            }

            if (ss->ply == 0) ss->ply = (ss-1)->ply + 1;
            ss->current_move = MOVE_NONE;

            // Check for an immediate draw or maximum ply reached
            if (   pos.draw ()
                || ss->ply >= MaxPly
               )
            {
                return ss->ply >= MaxPly && !in_check ?
                        evaluate (pos) :
                        DrawValue[pos.active ()];
            }

            assert(   0 <= ss->ply && ss->ply < MaxPly
                   && ss->ply == (ss-1)->ply + 1);

            // Transposition table lookup
            auto posi_key = pos.posi_key ();
            bool tt_hit;
            auto *tte = TT.probe (posi_key, tt_hit);
            auto tt_move    = tt_hit ? tte->move () : MOVE_NONE;
            auto tt_value   = tt_hit ? value_of_tt (tte->value (), ss->ply) : VALUE_NONE;
            auto tt_eval    = tt_hit ? tte->eval () : VALUE_NONE;
            auto tt_depth   = tt_hit ? tte->depth () : DEPTH_NONE;
            auto tt_bound   = tt_hit ? tte->bound () : BOUND_NONE;

            // Decide whether or not to include checks, this fixes also the type of
            // TT entry depth that are going to use. Note that in quien_search use
            // only two types of depth in TT: DEPTH_QS_CHECKS or DEPTH_QS_NO_CHECKS.
            auto qs_depth = in_check || depth >= DEPTH_QS_CHECKS ? DEPTH_QS_CHECKS : DEPTH_QS_NO_CHECKS;

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
            if (in_check)
            {
                ss->static_eval = VALUE_NONE;
                best_value = futility_base = -VALUE_INFINITE;
            }
            else
            {
                if (tt_hit)
                {
                    // Never assume anything on values stored in TT
                    if (tt_eval == VALUE_NONE)
                    {
                        tt_eval = evaluate (pos);
                    }
                    ss->static_eval = tt_eval;

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

            // Initialize a MovePicker object for the current position, and prepare
            // to search the moves. Because the depth is <= 0 here, only captures,
            // queen promotions and checks (only if depth >= DEPTH_QS_CHECKS) will
            // be generated.
            MovePicker mp (pos, thread->history_values, tt_move, depth, _ok ((ss-1)->current_move) ? dst_sq ((ss-1)->current_move) : SQ_NO);
            CheckInfo ci (pos);
            StateInfo si;
            Move move
               , best_move  = MOVE_NONE;

            // Loop through the moves until no moves remain or a beta cutoff occurs
            while ((move = mp.next_move ()) != MOVE_NONE)
            {
                assert(_ok (move));

                bool gives_check = mtype (move) == NORMAL && ci.discoverers == U64(0) ?
                                    (ci.checking_bb[ptype (pos[org_sq (move)])] & dst_sq (move)) != U64(0) :
                                    pos.gives_check (move, ci);
                if (!MateSearch)
                {
                    // Futility pruning
                    if (   !in_check
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
                        if (pos.see (move) <= VALUE_ZERO)
                        {
                            if (best_value < futility_base)
                            {
                                best_value = futility_base;
                            }
                            continue;
                        }
                    }

                    // Don't search moves with negative SEE values
                    if (   (  !in_check
                            // Detect non-capture evasions that are candidate to be pruned (evasion_prunable)
                            || (   //in_check &&
                                   best_value > -VALUE_MATE_IN_MAX_PLY
                                && !pos.capture (move)
                               )
                           )
                        && mtype (move) != PROMOTE
                        && pos.see_sign (move) < VALUE_ZERO
                       )
                    {
                        continue;
                    }
                }

                // Speculative prefetch as early as possible
                prefetch (TT.cluster_entry (pos.move_posi_key (move)));

                // Check for legality just before making the move
                if (!pos.legal (move, ci.pinneds)) continue;

                ss->current_move = move;

                // Make and search the move
                pos.do_move (move, si, gives_check);

                prefetch (thread->pawn_table[pos.pawn_key ()]);
                prefetch (thread->matl_table[pos.matl_key ()]);

                auto value = -quien_search<NT> (pos, ss+1, -beta, -alfa, depth-DEPTH_ONE, gives_check);

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
                                && tte->key16 () != (posi_key >> 0x30)
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
            if (   in_check
                && best_value == -VALUE_INFINITE
               )
            {
                return mated_in (ss->ply); // Plies to mate from the root
            }

            if (   tt_hit
                && tte->key16 () != (posi_key >> 0x30)
               )
            {
                tte = TT.probe (posi_key, tt_hit);
            }
            tte->save (posi_key, best_move, value_to_tt (best_value, ss->ply), ss->static_eval, qs_depth,
                PVNode && pv_alfa < best_value ? BOUND_EXACT : BOUND_UPPER, TT.generation ());

            assert(-VALUE_INFINITE < best_value && best_value < +VALUE_INFINITE);
            return best_value;
        }

        template<NodeType NT>
        // depth_search<>() is the main depth limited search function for Root/PV/NonPV nodes
        Value depth_search  (Position &pos, Stack *ss, Value alfa, Value beta, Depth depth, bool cut_node)
        {
            const bool PVNode   = NT == PV;
            const bool rootNode = PVNode && (ss-1)->ply == 0 /*&& (ss-2)->ply == -1*/;

            assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            assert(PVNode || alfa == beta-1);
            assert(DEPTH_ZERO < depth && depth < DEPTH_MAX);

            if (ss->ply == 0) ss->ply = (ss-1)->ply + 1;

            // Step 1. Initialize node
            auto *thread = pos.thread ();
            auto *main_thread = Threadpool.main () == thread ? Threadpool.main () : nullptr;

            // Check for the available remaining limit
            if (thread->reset_check.load (std::memory_order_relaxed))
            {
                thread->reset_check = false;
                thread->chk_count = 0;
            }
            if (++thread->chk_count > TimerResolution*MilliSec)
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

            bool in_check = pos.checkers () != U64(0);

            if (!rootNode)
            {
                // Step 2. Check end condition
                // Check for aborted search, immediate draw or maximum ply reached
                if (   ForceStop.load (std::memory_order_relaxed)
                    || pos.draw ()
                    || ss->ply >= MaxPly
                   )
                {
                    return ss->ply >= MaxPly && !in_check ?
                            evaluate (pos) :
                            DrawValue[pos.active ()];
                }

                // Step 3. Mate distance pruning. Even if mate at the next move our score
                // would be at best mates_in(ss->ply+1), but if alfa is already bigger because
                // a shorter mate was found upward in the tree then there is no need to search
                // further, will never beat current alfa. Same logic but with reversed signs
                // applies also in the opposite condition of being mated instead of giving mate,
                // in this case return a fail-high score.
                alfa = std::max (mated_in (ss->ply +0), alfa);
                beta = std::min (mates_in (ss->ply +1), beta);

                if (alfa >= beta) return alfa;
            }

            assert(   0 <= ss->ply && ss->ply < MaxPly
                   && ss->ply == (ss-1)->ply + 1);

            ss->move_count = 0;
            ss->current_move = MOVE_NONE;
            (ss+1)->exclude_move = MOVE_NONE;
            (ss+1)->skip_pruning = false;
            std::fill ((ss+2)->killer_moves, (ss+2)->killer_moves + Killers, MOVE_NONE);

            // Step 4. Transposition table lookup
            // Don't want the score of a partial search to overwrite a previous full search
            // TT value, so use a different position key in case of an excluded move.
            auto exclude_move = ss->exclude_move;

            auto posi_key = exclude_move == MOVE_NONE ?
                        pos.posi_key () :
                        pos.posi_key () ^ Zobrist::ExclusionKey;
            bool tt_hit;
            auto *tte = TT.probe (posi_key, tt_hit);
            auto tt_move = rootNode ? thread->root_moves[thread->pv_index][0] :
                                    tt_hit ? tte->move () : MOVE_NONE;
            auto tt_value   = tt_hit ? value_of_tt (tte->value (), ss->ply) : VALUE_NONE;
            auto tt_eval    = tt_hit ? tte->eval () : VALUE_NONE;
            auto tt_depth   = tt_hit ? tte->depth () : DEPTH_NONE;
            auto tt_bound   = tt_hit ? tte->bound () : BOUND_NONE;

            // At non-PV nodes we check for an early TT cutoff
            if (   !PVNode
                && tt_hit
                && tt_depth >= depth
                && tt_value != VALUE_NONE // Only in case of TT access race
                && (tt_bound & (tt_value >= beta ? BOUND_LOWER : BOUND_UPPER)) != BOUND_NONE
               )
            {
                ss->current_move = tt_move; // Can be MOVE_NONE
                // If tt_move is quiet, update killers, history, countermove and countermoves history on TT hit
                if (   tt_value >= beta
                    && tt_move != MOVE_NONE
                    && !pos.capture_or_promotion (tt_move)
                   )
                {
                    update_stats (pos, ss, tt_move, depth);
                }
                return tt_value;
            }

            // Step 4A. Tablebase probe
            if (   !rootNode
                && TBPieceLimit != 0
               )
            {
                i32 piece_count = pos.count<NONE> ();

                if (   (   piece_count <  TBPieceLimit
                       || (piece_count == TBPieceLimit && depth >= TBDepthLimit)
                       )
                    &&  pos.clock_ply () == 0
                   )
                {
                    i32 found;
                    Value v = probe_wdl (pos, found);

                    if (found != 0)
                    {
                        ++TBHits;

                        i32 draw_v = TBUseRule50 ? 1 : 0;

                        Value value =
                                v < -draw_v ? -VALUE_MATE + i32(MaxPly + ss->ply) :
                                v > +draw_v ? +VALUE_MATE - i32(MaxPly + ss->ply) :
                                VALUE_DRAW + 2 * draw_v * v;

                        if (   tt_hit
                            && tte->key16 () != (posi_key >> 0x30)
                           )
                        {
                            tte = TT.probe (posi_key, tt_hit);
                        }
                        tte->save (posi_key, MOVE_NONE, value_to_tt (value, ss->ply), in_check ? VALUE_NONE : evaluate (pos),
                            std::min (depth + 6*DEPTH_ONE, DEPTH_MAX - DEPTH_ONE), BOUND_EXACT, TT.generation ());

                        return value;
                    }
                }
            }

            CheckInfo ci (pos);
            StateInfo si;
            Move move;

            // Step 5. Evaluate the position statically
            if (in_check)
            {
                ss->static_eval = tt_eval = VALUE_NONE;
            }
            else
            {
                if (tt_hit)
                {
                    // Never assume anything on values stored in TT
                    if (tt_eval == VALUE_NONE)
                    {
                        tt_eval = evaluate (pos);
                    }
                    ss->static_eval = tt_eval;

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
                            return quien_search<NonPV> (pos, ss, alfa, beta, DEPTH_ZERO, false);
                        }

                        auto reduced_alpha = std::max (alfa - RazorMargins[depth/DEPTH_ONE], -VALUE_INFINITE);

                        auto value = quien_search<NonPV> (pos, ss, reduced_alpha, reduced_alpha+1, DEPTH_ZERO, false);

                        if (value <= reduced_alpha)
                        {
                            return value;
                        }
                    }

                    // Step 7. Futility pruning: child node
                    // Betting that the opponent doesn't have a move that will reduce
                    // the score by more than FutilityMargins[depth] if do a null move.
                    if (   !rootNode
                        && !MateSearch
                        && depth < FutilityMarginDepth*DEPTH_ONE
                        && tt_eval < +VALUE_KNOWN_WIN // Do not return unproven wins
                        && pos.non_pawn_material (pos.active ()) > VALUE_ZERO
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
                        && pos.non_pawn_material (pos.active ()) > VALUE_ZERO
                       )
                    {
                        assert(_ok ((ss-1)->current_move));
                        assert(exclude_move == MOVE_NONE);

                        ss->current_move = MOVE_NULL;

                        // Null move dynamic reduction based on depth and static evaluation
                        auto reduced_depth = depth - ((823 + 67 * depth) / 256 + std::min ((tt_eval - beta)/VALUE_EG_PAWN, 3))*DEPTH_ONE;

                        // Do null move
                        pos.do_null_move (si);

                        (ss+1)->skip_pruning = true;
                        // Null (zero) window (alfa, beta) = (beta-1, beta):
                        auto null_value =
                            reduced_depth < DEPTH_ONE ?
                                -quien_search<NonPV> (pos, ss+1, -beta, -(beta-1), DEPTH_ZERO, false) :
                                -depth_search<NonPV> (pos, ss+1, -beta, -(beta-1), reduced_depth, !cut_node);
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
                            
                            ss->skip_pruning = true;
                            // Do verification search at high depths
                            auto value =
                                reduced_depth < DEPTH_ONE ?
                                    quien_search<NonPV> (pos, ss, beta-1, beta, DEPTH_ZERO, false) :
                                    depth_search<NonPV> (pos, ss, beta-1, beta, reduced_depth, false);
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

                        assert(reduced_depth >= DEPTH_ONE);
                        assert(_ok ((ss-1)->current_move));
                        assert(alfa < extended_beta);

                        // Initialize a MovePicker object for the current position, and prepare to search the moves.
                        MovePicker mp (pos, thread->history_values, tt_move, PieceValues[MG][pos.capture_type ()]);

                        while ((move = mp.next_move ()) != MOVE_NONE)
                        {
                            // Speculative prefetch as early as possible
                            prefetch (TT.cluster_entry (pos.move_posi_key (move)));

                            if (!pos.legal (move, ci.pinneds)) continue;

                            ss->current_move = move;

                            pos.do_move (move, si, mtype (move) == NORMAL && ci.discoverers == U64(0) ?
                                                    (ci.checking_bb[ptype (pos[org_sq (move)])] & dst_sq (move)) != U64(0) :
                                                    pos.gives_check (move, ci));

                            prefetch (thread->pawn_table[pos.pawn_key ()]);
                            prefetch (thread->matl_table[pos.matl_key ()]);

                            auto value = -depth_search<NonPV> (pos, ss+1, -extended_beta, -extended_beta+1, reduced_depth, !cut_node);

                            pos.undo_move ();

                            if (value >= extended_beta)
                            {
                                return value;
                            }
                        }
                    }

                    // Step 10. Internal iterative deepening
                    if (   tt_move == MOVE_NONE
                        && depth >= (PVNode ? 5 : 8)*DEPTH_ONE        // IID Activation Depth
                        && (PVNode || ss->static_eval + VALUE_EG_PAWN >= beta) // IID Margin
                       )
                    {
                        auto iid_depth = depth - 2*DEPTH_ONE - (PVNode ? DEPTH_ZERO : depth/4); // IID Reduced Depth
                        
                        ss->skip_pruning = true;
                        depth_search<NT> (pos, ss, alfa, beta, iid_depth, true);
                        ss->skip_pruning = false;

                        tte = TT.probe (posi_key, tt_hit);
                        tt_move  = tt_hit ? tte->move () : MOVE_NONE;
                        tt_value = tt_hit ? value_of_tt (tte->value (), ss->ply) : VALUE_NONE;
                        tt_eval  = tt_hit ? tte->eval () : VALUE_NONE;
                        tt_depth = tt_hit ? tte->depth () : DEPTH_NONE;
                        tt_bound = tt_hit ? tte->bound () : BOUND_NONE;
                    }
                }
            }

            // When in check search starts from here
            auto value      = -VALUE_INFINITE
               , best_value = -VALUE_INFINITE;

            auto best_move  = MOVE_NONE;

            bool improving =
                   (ss-0)->static_eval >= (ss-2)->static_eval
                || (ss-0)->static_eval == VALUE_NONE
                || (ss-2)->static_eval == VALUE_NONE;

            bool singular_ext_node =
                   !rootNode
                && tt_hit
                && exclude_move == MOVE_NONE // Recursive singular search is not allowed
                && tt_move != MOVE_NONE
                &&    depth >= 8*DEPTH_ONE
                && tt_depth >= depth - 3*DEPTH_ONE
                && abs (tt_value) < +VALUE_KNOWN_WIN
                && (tt_bound & BOUND_LOWER) != BOUND_NONE;

            u08 move_count = 0;

            MoveVector quiet_moves;
            quiet_moves.reserve (16);

            auto opp_move_dst = dst_sq ((ss-1)->current_move);
            auto counter_move = thread->counter_moves[_ok ((ss-1)->current_move) ? pos[opp_move_dst] : NO_PIECE][opp_move_dst];
            auto &opp_cmv = CounterMovesHistory[_ok ((ss-1)->current_move) ? pos[opp_move_dst] : NO_PIECE][opp_move_dst];

            // Initialize a MovePicker object for the current position, and prepare to search the moves.
            MovePicker mp (pos, thread->history_values, opp_cmv, tt_move, depth, counter_move, ss);

            // Step 11. Loop through moves
            // Loop through all pseudo-legal moves until no moves remain or a beta cutoff occurs
            while ((move = mp.next_move ()) != MOVE_NONE)
            {
                assert(_ok (move));

                if (move == exclude_move) continue;
                
                // At root obey the "searchmoves" option and skip moves not listed in
                // RootMove list, as a consequence any illegal move is also skipped.
                // In MultiPV mode also skip PV moves which have been already searched.
                if (   rootNode
                    && std::find (thread->root_moves.begin () + thread->pv_index, thread->root_moves.end (), move) == thread->root_moves.end ()
                   )
                {
                    continue;
                }

                bool move_legal = rootNode || pos.legal (move, ci.pinneds);

                ss->move_count = ++move_count;

                if (   rootNode
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
                    (ss+1)->pv = MoveVector ();
                }

                auto extension = DEPTH_ZERO;
                bool gives_check = mtype (move) == NORMAL && ci.discoverers == U64(0) ?
                                    (ci.checking_bb[ptype (pos[org_sq (move)])] & dst_sq (move)) != U64(0) :
                                    pos.gives_check (move, ci);

                // Step 12. Extend the move which seems dangerous like ...checks etc.
                if (   gives_check
                    && pos.see_sign (move) >= VALUE_ZERO
                   )
                {
                    extension = DEPTH_ONE;
                }

                // Singular extension(SE) search.
                // We extend the TT move if its value is much better than its siblings.
                // If all moves but one fail low on a search of (alfa-s, beta-s),
                // and just one fails high on (alfa, beta), then that move is singular
                // and should be extended. To verify this do a reduced search on all the other moves
                // but the tt_move, if result is lower than tt_value minus a margin then extend tt_move.
                if (   move_legal
                    && singular_ext_node
                    && move == tt_move
                    && extension == DEPTH_ZERO
                   )
                {
                    auto r_beta = tt_value - 2*(depth/DEPTH_ONE);

                    ss->exclude_move = move;
                    ss->skip_pruning = true;
                    value = depth_search<NonPV> (pos, ss, r_beta-1, r_beta, depth/2, cut_node);
                    ss->skip_pruning = false;
                    ss->exclude_move = MOVE_NONE;

                    if (value < r_beta) extension = DEPTH_ONE;
                }

                // Update the current move (this must be done after singular extension search)
                auto new_depth = depth - DEPTH_ONE + extension;
                bool capture_or_promotion = pos.capture_or_promotion (move);

                // Step 13. Pruning at shallow depth
                if (   !rootNode
                    && !MateSearch
                    && !in_check
                    && !capture_or_promotion
                    && best_value > -VALUE_MATE_IN_MAX_PLY
                    // ! Dangerous (below)
                    && !gives_check
                    && !pos.advanced_pawn_push (move)
                   )
                {
                    // Move count based pruning
                    if (   depth < FutilityMoveCountDepth*DEPTH_ONE
                        && move_count >= FutilityMoveCounts[improving][depth/DEPTH_ONE]
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
                    auto predicted_depth = new_depth - reduction_depths<PVNode> (improving, depth, move_count);
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
                        && pos.see_sign (move) < VALUE_ZERO
                       )
                    {
                        continue;
                    }
                }

                // Speculative prefetch as early as possible
                prefetch (TT.cluster_entry (pos.move_posi_key (move)));

                // Check for legality just before making the move
                if (   !rootNode
                    && !move_legal
                   )
                {
                    ss->move_count = --move_count;
                    continue;
                }

                ss->current_move = move;

                // Step 14. Make the move
                pos.do_move (move, si, gives_check);

                prefetch (thread->pawn_table[pos.pawn_key ()]);
                prefetch (thread->matl_table[pos.matl_key ()]);

                auto full_depth_search = !PVNode || move_count > FullDepthMoveCount;

                // Step 15. Reduced depth search (LMR).
                // If the move fails high will be re-searched at full depth.
                if (   depth >= LateMoveReductionDepth*DEPTH_ONE
                    && move_count > FullDepthMoveCount
                    && !capture_or_promotion
                    //&& (!gives_check || depth >= 10*DEPTH_ONE)
                   )
                {
                    auto reduction_depth = reduction_depths<PVNode> (improving, depth, move_count);

                    // Increase reduction for cut node
                    if (!PVNode && cut_node)
                    {
                        reduction_depth += DEPTH_ONE;
                    }
                    // Decrease reduction for moves with a +ve history
                    // Increase reduction for moves with a -ve history
                    reduction_depth = std::max (reduction_depth - (i32( thread->history_values[pos[dst_sq (move)]][dst_sq (move)]
                                                                      + opp_cmv[pos[dst_sq (move)]][dst_sq (move)])/14980)*DEPTH_ONE, DEPTH_ZERO);

                    // Decrease reduction for moves that escape a capture.
                    // Filter out castling moves, because are coded as "king captures rook" hence break make_move().
                    // Also use see() instead of see_sign(), because the destination square is empty.
                    if (   reduction_depth != DEPTH_ZERO
                        && mtype (move) == NORMAL
                        && ptype (pos[dst_sq (move)]) != PAWN
                        && pos.see (mk_move (dst_sq (move), org_sq (move))) < VALUE_ZERO // Reverse move
                       )
                    {
                        reduction_depth = std::max (reduction_depth-DEPTH_ONE, DEPTH_ZERO);
                    }

                    // Search with reduced depth
                    value = -depth_search<NonPV> (pos, ss+1, -(alfa+1), -alfa, std::max (new_depth - reduction_depth, DEPTH_ONE), true);

                    full_depth_search = alfa < value && reduction_depth != DEPTH_ZERO;
                }

                // Step 16. Full depth search when LMR is skipped or fails high
                if (full_depth_search)
                {
                    value =
                        new_depth < DEPTH_ONE ?
                            -quien_search<NonPV> (pos, ss+1, -(alfa+1), -alfa, DEPTH_ZERO, gives_check) :
                            -depth_search<NonPV> (pos, ss+1, -(alfa+1), -alfa, new_depth, !cut_node);
                }

                // Do a full PV search on:
                // - 'full depth move count' move
                // - 'fail high' move (search only if value < beta)
                // otherwise let the parent node fail low with
                // alfa >= value and try another move.
                if (   PVNode
                    && (   (0 < move_count && move_count <= FullDepthMoveCount)
                        || (alfa < value && (rootNode || value < beta))
                       )
                   )
                {
                    (ss+1)->pv = MoveVector ();

                    value =
                        new_depth < DEPTH_ONE ?
                            -quien_search<PV> (pos, ss+1, -beta, -alfa, DEPTH_ZERO, gives_check) :
                            -depth_search<PV> (pos, ss+1, -beta, -alfa, new_depth, false);
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

                if (rootNode)
                {
                    auto &root_move = *std::find (thread->root_moves.begin (), thread->root_moves.end (), move);
                    // 1st legal move or new best move ?
                    if (move_count == 1 || alfa < value)
                    {
                        auto &pv = (ss+1)->pv;
                        //assert(!pv.empty ());
                        if (    pv.size () + 1 != root_move.size ()
                            || (pv.size () > 0 && root_move.size () > 1 && pv[0] != root_move[1])
                            || (pv.size () > 1 && root_move.size () > 2 && pv[1] != root_move[2])
                           )
                        {
                            auto rm = RootMove (root_move[0]);
                            rm.old_value = root_move.old_value;
                            if (pv.size () != 0)
                            {
                                rm.reserve (pv.size () + 1);
                                std::copy (pv.begin (), pv.end (), std::back_inserter (rm));
                                //rm.shrink_to_fit ();
                            }
                            root_move = rm;
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
                            main_thread->time_mgr.best_move_change++;
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
                            && !rootNode
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

            // Step 19.
            // The following condition would detect a stop only after move loop has been
            // completed. But in this case bestValue is valid because we have fully
            // searched our subtree, and we can anyhow save the result in TT.
            /*
            if (ForceStop) return VALUE_DRAW;
            */

            //quiet_moves.shrink_to_fit ();

            // Step 20. Check for checkmate and stalemate
            // If all possible moves have been searched and if there are no legal moves,
            // If in a singular extension search then return a fail low score (alfa).
            // Otherwise it must be a checkmate or a stalemate, so return value accordingly.
            if (move_count == 0)
            {
                best_value = 
                    exclude_move != MOVE_NONE ?
                        alfa :
                        in_check ?
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
            if (  !in_check
                && depth >= 3*DEPTH_ONE
                && best_move == MOVE_NONE
                && pos.capture_type () == NONE
                && _ok ((ss-1)->current_move)
                //&& mtype ((ss-1)->current_move) != PROMOTE
                && _ok ((ss-2)->current_move)
               )
            {
                opp_move_dst = dst_sq ((ss-1)->current_move);
                auto own_move_dst = dst_sq ((ss-2)->current_move);
                auto &own_cmv = CounterMovesHistory[pos[own_move_dst]][own_move_dst];
                own_cmv.update (pos[opp_move_dst], opp_move_dst, Value((depth/DEPTH_ONE)*((depth/DEPTH_ONE) + 1) - 1));
            }

            if (   tt_hit
                && tte->key16 () != (posi_key >> 0x30)
               )
            {
                tte = TT.probe (posi_key, tt_hit);
            }
            tte->save (posi_key, best_move,
                value_to_tt (best_value, ss->ply), ss->static_eval, depth,
                best_value >= beta ? BOUND_LOWER :
                    PVNode && best_move != MOVE_NONE ? BOUND_EXACT : BOUND_UPPER,
                TT.generation ());

            assert(-VALUE_INFINITE < best_value && best_value < +VALUE_INFINITE);
            return best_value;
        }
    }

    // ------------------------------------

    const size_t Stack::Size = sizeof (Stack);

    // ------------------------------------

    // RootMove::insert_pv_into_tt() is called at the end of a search iteration, and
    // inserts the PV back into the TT. This makes sure the old PV moves are searched
    // first, even if the old TT entries have been overwritten.
    void RootMove::insert_pv_into_tt (Position &pos)
    {
        StateInfo states[MaxPly], *si = states;
        
        //shrink_to_fit ();
        u08 ply = 0;
        for (const auto m : *this)
        {
            assert(MoveList<LEGAL> (pos).contains (m));

            bool tt_hit;
            auto *tte = TT.probe (pos.posi_key (), tt_hit);
            // Don't overwrite correct entries
            if (  !tt_hit
                || tte->move () != m
               )
            {
                tte->save (pos.posi_key (), m, VALUE_NONE, pos.checkers () != U64(0) ? VALUE_NONE : evaluate (pos), DEPTH_NONE, BOUND_NONE, TT.generation ());
            }
            pos.do_move (m, *si++, pos.gives_check (m, CheckInfo (pos)));
            ++ply;
        }

        while (ply != 0)
        {
            pos.undo_move ();
            --ply;
        }
    }
    // RootMove::extract_ponder_move_from_tt() is called in case have no ponder move before
    // exiting the search, for instance, in case stop the search during a fail high at root.
    // Try hard to have a ponder move which has to return to the GUI,
    // otherwise in case of 'ponder on' we have nothing to think on.
    bool RootMove::extract_ponder_move_from_tt (Position &pos)
    {
        assert(size () == 1);
        assert(at (0) != MOVE_NONE);

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
            oss << " " << move_to_can (m, Chess960);
        }
        return oss.str ();
    }

    // ------------------------------------

    // When playing with a strength handicap, choose best move among the first 'candidates'
    // RootMoves using a statistical rule dependent on 'level'. Idea by Heinz van Saanen.
    Move SkillManager::pick_best_move (const RootMoveVector &root_moves)
    {
        assert(!root_moves.empty ());

        static PRNG prng (now ()); // PRNG sequence should be non-deterministic

        _best_move = MOVE_NONE;
        // RootMoves are already sorted by value in descending order
        auto top_value  = root_moves[0].new_value;
        auto diversity  = std::min (top_value - root_moves[PVLimit - 1].new_value, VALUE_MG_PAWN);
        auto weakness   = Value(MaxPly - 4 * _level);
        auto best_value = -VALUE_INFINITE;
        // Choose best move. For each move score add two terms, both dependent on weakness.
        // One is deterministic with weakness, and one is random with diversity.
        // Then choose the move with the resulting highest value.
        for (u16 i = 0; i < PVLimit; ++i)
        {
            auto value = root_moves[i].new_value;
            // This is magic formula for push
            auto push  = (  weakness  * i32(top_value - value)
                          + diversity * i32(prng.rand<u32> () % weakness)
                         ) / (i32(VALUE_EG_PAWN) / 2);

            if (best_value < value + push)
            {
                best_value = value + push;
                _best_move = root_moves[i][0];
            }
        }
        return _best_move;
    }

    // ------------------------------------

    // perft<>() is utility to verify move generation.
    // All the leaf nodes up to the given depth are generated, and the sum is returned.
    template<bool RootNode>
    u64 perft (Position &pos, Depth depth)
    {
        u64 leaf_nodes = U64(0);
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
        i32 d; // depth

        static const i32 K0[3] = { 0, 200, 0 };
        for (d = 0; d < FutilityMarginDepth; ++d)
        {
            FutilityMargins[d] = Value(K0[0] + (K0[1] + K0[2]*d)*d);
        }

        static const double K1[2][4] =
        {
            { 2.40, 0.773, 0.00, 1.8 },
            { 2.90, 1.045, 0.49, 1.8 }
        };
        for (u08 imp = 0; imp <= 1; ++imp)
        {
            for (d = 0; d < FutilityMoveCountDepth; ++d)
            {
                FutilityMoveCounts[imp][d] = u08(K1[imp][0] + K1[imp][1] * pow (d + K1[imp][2], K1[imp][3]));
            }
        }

        static const double K2[2][2] =
        {
            { 0.799, 2.281 },
            { 0.484, 3.023 }
        };
        for (u08 pv = 0; pv <= 1; ++pv)
        {
            for (u08 imp = 0; imp <= 1; ++imp)
            {
                for (d = 1; d < ReductionDepth; ++d)
                {
                    for (u08 mc = 1; mc < ReductionMoveCount; ++mc) // move count
                    {
                        auto r = K2[pv][0] + log (d) * log (mc) / K2[pv][1];

                        if (r >= 1.5)
                        {
                            ReductionDepths[pv][imp][d][mc] = i32(r)*DEPTH_ONE;
                        }
                        // Increase reduction when eval is not improving
                        if (   !pv
                            && !imp
                            && ReductionDepths[pv][imp][d][mc] >= 2*DEPTH_ONE
                           )
                        {
                            ReductionDepths[pv][imp][d][mc] += 1*DEPTH_ONE;
                        }
                    }
                }
            }
        }
    }
    // clear() resets search state to zero, to obtain reproducible results
    void clear ()
    {
        TT.clear ();
        CounterMovesHistory.clear ();

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
        Stack stacks[MaxPly+4], *ss = stacks+2; // To allow referencing (ss-2)
        for (auto s = stacks; s <= stacks+5; ++s)
        {
            s->ply = i16(s - stacks - 1);
            s->current_move = MOVE_NONE;
            s->exclude_move = MOVE_NONE;
            std::fill (s->killer_moves, s->killer_moves + Killers, MOVE_NONE);
            s->static_eval = VALUE_ZERO;
            s->move_count = 0;
            s->skip_pruning = false;
            assert(s->pv.empty ());
        }

        auto *main_thread = Threadpool.main () == this ? Threadpool.main () : nullptr;
        auto easy_move = MOVE_NONE;

        if (main_thread != nullptr)
        {
            TT.generation (root_pos.game_ply () + 1);

            if (main_thread->time_mgr_used)
            {
                main_thread->time_mgr.best_move_change = 0.0;
                easy_move = main_thread->easy_move_mgr.easy_move (root_pos.posi_key ());
                main_thread->easy_move_mgr.clear ();
                main_thread->easy_played = false;
            }
            if (SkillMgr.enabled ())
            {
                SkillMgr.clear ();
            }
        }

        // Do have to play with skill handicap?
        // In this case enable MultiPV search by skill pv size
        // that will use behind the scenes to get a set of possible moves.
        PVLimit = std::min (std::max (MultiPV, u16(SkillMgr.enabled () ? SkillManager::MultiPV : 0)), u16(root_moves.size ()));

        Value best_value = VALUE_ZERO
            , window     = VALUE_ZERO
            , alfa       = -VALUE_INFINITE
            , beta       = +VALUE_INFINITE;

        leaf_depth = DEPTH_ZERO;

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
                    main_thread->time_mgr.best_move_change *= 0.505;
                }
            }
            else
            {
                // Set up the new depths for the helper threads skipping on average every
                // 2nd ply (using a half density map similar to a Hadamard matrix).
                u16 d = u16(root_depth) + root_pos.game_ply ();

                if (index < 7 || 24 < index)
                {
                    if (((d + index) >> (scan_msq (index + 1) - 1)) % 2 != 0)
                    {
                        continue;
                    }
                }
                else
                {
                    // Table of values of 6 bits with 3 of them set
                    static const u16 HalfDensityMap[18] = // 24 - 7 + 1
                    {
                        0x07, 0x0B, 0x0D, 0x0E,
                        0x13, 0x16, 0x19, 0x1A, 0x1C,
                        0x23, 0x25, 0x26, 0x29, 0x2C,
                        0x31, 0x32, 0x34, 0x38,
                    };
                    if (((HalfDensityMap[index - 7] >> (d % 6)) & 1) != 0)
                    {
                        continue;
                    }
                }
            }

            // Save the last iteration's scores before first PV line is searched and
            // all the move scores but the (new) PV are set to -VALUE_INFINITE.
            for (auto &rm : root_moves)
            {
                rm.backup ();
            }

            const bool aspiration = root_depth > 4*DEPTH_ONE;

            // MultiPV loop. Perform a full root search for each PV line
            for (pv_index = 0; !ForceStop && pv_index < PVLimit; ++pv_index)
            {
                // Reset aspiration window starting size.
                if (   aspiration
                    //&& leaf_depth >= DEPTH_ONE
                   )
                {
                    window = Value(18);
                        //Value(depth <= 32*DEPTH_ONE ? 14 + (u16(depth)-1)/4 : 22); // Increasing window

                    alfa = std::max (root_moves[pv_index].old_value - window, -VALUE_INFINITE);
                    beta = std::min (root_moves[pv_index].old_value + window, +VALUE_INFINITE);
                }

                // Start with a small aspiration window and, in case of fail high/low,
                // research with bigger window until not failing high/low anymore.
                do
                {
                    best_value = depth_search<PV> (root_pos, ss, alfa, beta, root_depth, false);

                    // Bring the best move to the front. It is critical that sorting is
                    // done with a stable algorithm because all the values but the first
                    // and eventually the new best one are set to -VALUE_INFINITE and
                    // want to keep the same order for all the moves but the new PV
                    // that goes to the front. Note that in case of MultiPV search
                    // the already searched PV lines are preserved.
                    std::stable_sort (root_moves.begin () + pv_index, root_moves.end ());

                    // Write PV back to the transposition table in case the relevant
                    // entries have been overwritten during the search.
                    for (i16 i = pv_index; i >= 0; --i)
                    {
                        root_moves[i].insert_pv_into_tt (root_pos);
                    }

                    // If search has been stopped, break immediately.
                    // Sorting and writing PV back to TT is safe becuase
                    // root moves is still valid, although refers to the previous iteration.
                    if (ForceStop) break;

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
                        break;

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
                    Value valued_contempt = Value(i32(root_moves[0].new_value)/ContemptValue);
                    DrawValue[ RootColor] = BaseContempt[ RootColor] - valued_contempt;
                    DrawValue[~RootColor] = BaseContempt[~RootColor] + valued_contempt;
                }

                // If skill level is enabled and can pick move, pick a sub-optimal best move
                if (   SkillMgr.enabled ()
                    && SkillMgr.can_pick (root_depth)
                    && !root_moves.empty ()
                   )
                {
                    SkillMgr.pick_best_move (root_moves);
                }

                if (LogWrite)
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
                        if (   aspiration
                            && PVLimit == 1
                           )
                        {
                            main_thread->time_mgr.instability ();
                        }

                        // Stop the search
                        // If there is only one legal move available or 
                        // If all of the available time has been used or
                        // If matched an easy move from the previous search and just did a fast verification.
                        if (   root_moves.size () == 1
                            || main_thread->time_mgr.elapsed_time () > main_thread->time_mgr.available_time () *
                                    (1.00 - 0.25000 * (!main_thread->failed_low)
                                          - 0.19687 * (best_value >= main_thread->previous_value)
                                          - 0.19375 * (best_value >= main_thread->previous_value && !main_thread->failed_low))
                            || (main_thread->easy_played =
                                    ( !root_moves.empty ()
                                    && root_moves[0] == easy_move
                                    && main_thread->time_mgr.best_move_change < 0.03
                                    && main_thread->time_mgr.elapsed_time () > main_thread->time_mgr.available_time () * 0.12135
                                    ), main_thread->easy_played
                               )
                           )
                        {
                            stop = true;
                        }

                        if (  !root_moves.empty ()
                            && root_moves[0].size () >= EasyMoveManager::PVSize
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
            if (   SkillMgr.enabled ()
                && !root_moves.empty ()
               )
            {
                std::swap (root_moves[0], *std::find (root_moves.begin (), root_moves.end (), SkillMgr.best_move (root_moves)));
            }
        }
    }
    // MainThread::search() is called by the main thread when the program receives
    // the UCI 'go' command. It searches from root position and and outputs the "bestmove" and "ponder".
    void MainThread::search ()
    {
        static Polyglot::Book book; // Defined static to initialize the PRNG only once

        assert(this == Threadpool.main ());

        RootColor = root_pos.active ();
        time_mgr_used = Limits.time_management_used ();
        if (time_mgr_used)
        {
            // Initialize the time manager before searching.
            time_mgr.initialize (RootColor, root_pos.game_ply ());
        }

        MateSearch = Limits.mate != 0;

        LogWrite = !white_spaces (LogFile) && LogFile != "<empty>";
        if (LogWrite)
        {
            LogStream.open (LogFile, ios_base::out|ios_base::app);

            LogStream
                << "----------->\n" << boolalpha
                << "RootPos  : " << root_pos.fen ()                 << "\n"
                << "RootSize : " << root_moves.size ()              << "\n"
                << "Infinite : " << Limits.infinite                 << "\n"
                << "Ponder   : " << Limits.ponder                   << "\n"
                << "ClockTime: " << Limits.clock[RootColor].time    << "\n"
                << "Increment: " << Limits.clock[RootColor].inc     << "\n"
                << "MoveTime : " << Limits.movetime                 << "\n"
                << "MovesToGo: " << u16(Limits.movestogo)           << "\n"
                << " Depth Score    Time       Nodes  PV\n"
                << "-----------------------------------------------------------"
                << noboolalpha << std::endl;
        }

        if (root_moves.empty ())
        {
            root_moves += RootMove ();

            sync_cout
                << "info"
                << " depth " << 0
                << " score " << to_string (root_pos.checkers () != U64(0) ? -VALUE_MATE : VALUE_DRAW)
                << " time "  << 0
                << sync_endl;
        }
        else
        {
            // Check if can play with own book.
            if (   OwnBook
                && !BookFile.empty ()
                && (BookUptoMove == 0 || root_pos.game_move () <= BookUptoMove)
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
                if (found) goto finish;
            }

            i16 timed_contempt = 0;
            TimePoint diff_time = 0;
            if (   ContemptTime != 0
                && time_mgr_used
                && (diff_time = (Limits.clock[ RootColor].time - Limits.clock[~RootColor].time)/MilliSec) != 0
                //&& ContemptTime <= abs (diff_time)
               )
            {
                timed_contempt = i16(diff_time/ContemptTime);
            }

            Value contempt = cp_to_value ((FixedContempt + timed_contempt) / 100.0);
            DrawValue[ RootColor] = BaseContempt[ RootColor] = VALUE_DRAW - contempt;
            DrawValue[~RootColor] = BaseContempt[~RootColor] = VALUE_DRAW + contempt;

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

            if (TBPieceLimit >= root_pos.count<NONE> ())
            {
                // If the current root position is in the tablebases,
                // then RootMoves contains only moves that preserve the draw or the win.
                TBHasRoot = root_probe_dtz (root_pos, root_moves);

                if (TBHasRoot)
                {
                    TBPieceLimit = 0; // Do not probe tablebases during the search
                }
                else // If DTZ tables are missing, use WDL tables as a fallback
                {
                    // Filter out moves that do not preserve the draw or the win
                    TBHasRoot = root_probe_wdl (root_pos, root_moves);

                    // Only probe during search if winning
                    if (ProbeValue <= VALUE_DRAW)
                    {
                        TBPieceLimit = 0;
                    }
                }

                if (TBHasRoot)
                {
                    TBHits = u16(root_moves.size ());

                    if (!TBUseRule50)
                    {
                        ProbeValue = ProbeValue > VALUE_DRAW ? +VALUE_MATE - i32(MaxPly - 1) :
                                     ProbeValue < VALUE_DRAW ? -VALUE_MATE + i32(MaxPly + 1) :
                                     VALUE_DRAW;
                    }
                }
            }

            for (auto *th : Threadpool)
            {
                th->max_ply     = 0;
                th->root_depth  = DEPTH_ZERO;
                if (th != this)
                {
                    th->root_pos    = Position (root_pos, th);
                    th->root_moves  = root_moves;
                    th->start_searching (false);
                }
            }

            Thread::search (); // Let's start searching !

            // Update the time manager after searching.
            if (time_mgr_used)
            {
                time_mgr.update (RootColor);
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

        // Stop the threads if not already stopped.
        ForceStop = true;

        // Wait until all threads have finished.
        for (size_t i = 1; i < Threadpool.size (); ++i)
        {
            Threadpool[i]->wait_while_searching ();
        }

        // Check if there are threads with bigger depth and better score than main thread.
        Thread *best_thread = this;
        if (   !easy_played
            && PVLimit == 1
            && !SkillMgr.enabled ()
           )
        {
            for (size_t i = 1; i < Threadpool.size (); ++i)
            {
                if (   best_thread->leaf_depth < Threadpool[i]->leaf_depth
                    && best_thread->root_moves[0].new_value <= Threadpool[i]->root_moves[0].new_value
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

        assert(!root_moves.empty ()
            && !root_moves[0].empty ());

        if (time_mgr_used)
        {
            previous_value = root_moves[0].new_value;
        }

        if (LogWrite)
        {
            auto elapsed_time = std::max (time_mgr.elapsed_time (), TimePoint(1));

            LogStream
                << "Time (ms)  : " << elapsed_time                                      << "\n"
                << "Nodes (N)  : " << Threadpool.game_nodes ()                          << "\n"
                << "Speed (N/s): " << Threadpool.game_nodes ()*MilliSec / elapsed_time << "\n"
                << "Hash-full  : " << TT.hash_full ()                                   << "\n"
                << "Best Move  : " << move_to_san (root_moves[0][0], root_pos)          << "\n";
            if (    _ok (root_moves[0][0])
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
        if (   _ok (root_moves[0][0])
            && (root_moves[0].size () > 1 || root_moves[0].extract_ponder_move_from_tt (root_pos))
           )
        {
            std::cout << " ponder " << move_to_can (root_moves[0][1], Chess960);
        }
        std::cout << sync_endl;
    }
}
