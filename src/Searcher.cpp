#include "Searcher.h"

#include "Debugger.h"
#include "Evaluator.h"
#include "Notation.h"
#include "Polyglot.h"
#include "PRNG.h"
#include "TBsyzygy.h"
#include "Thread.h"
#include "Transposition.h"
#include "UCI.h"
#include "Zobrist.h"

using namespace std;
using namespace BitBoard;
using namespace Debugger;
using namespace Evaluator;
using namespace MoveGen;
using namespace Notation;
using namespace Polyglot;
using namespace TBSyzygy;
using namespace Threading;
using namespace Transposition;
using namespace UCI;
using namespace Zobrists;

// Extract ponder move from TT is called in case have no ponder move before exiting the search,
bool RootMove::extract_ponder_move_from_tt (Position &pos)
{
    assert(1 == size ());
    assert(MOVE_NONE != at (0));

    auto move = at (0);
    StateInfo si;
    pos.do_move (move, si);
    bool tt_hit;
    const auto *tte = TT.probe (pos.si->posi_key, tt_hit);
    Move ponder_move;
    if (   tt_hit
        && MOVE_NONE != (ponder_move = tte->move ()) // Local copy to be SMP safe
        && pos.pseudo_legal (ponder_move)
        && pos.legal (ponder_move))
    {
        assert(MoveList<GenType::LEGAL> (pos).contains (ponder_move));
        *this += ponder_move;
    }
    pos.undo_move (move);
    return 1 < size ();
}

RootMove::operator string () const
{
    ostringstream oss;
    for (auto move : *this)
    {
        assert(MOVE_NONE != move);
        oss << ' ' << move_to_can (move);
    }
    return oss.str ();
}

RootMoves::operator std::string () const
{
    std::ostringstream oss;
    for (const auto &rm : *this)
    {
        oss << rm << '\n';
    }
    return oss.str ();
}

namespace {

    const i32 MaxValue = 1 << 28;

    enum Stage : u08
    {
        NATURAL_TT, NATURAL, GOOD_CAPTURE, QUIET, BAD_CAPTURE,
        EVASION_TT, EVASION, ALL_EVASION,
        PROBCUT_CAPTURE_TT, PROBCUT_CAPTURE, ALL_PROBCUT_CAPTURE,
        Q_CHECK_TT, Q_CHECK, Q_CHECK_CAPTURE, Q_CHECK_QUIET,
        Q_NO_CHECK_TT, Q_NO_CHECK, Q_NO_CHECK_CAPTURE,
        Q_RECAPTURE_TT, Q_RECAPTURE, Q_ALL_RECAPTURE,
    };
}

// MovePicker class constructors. As arguments pass information to help
// it to return the (presumably) good moves first, to decide which moves to return
// (in the quiescence search, for instance, only want to search captures, promotions, and some checks)
// and about how important good move ordering is at the current node.

MovePicker::MovePicker (const Position &pos, Move ttm, const Stack *const &ss)
    : _pos (pos)
    , _ss (ss)
    , _tt_move (ttm)
    , _recap_sq (SQ_NO)
    , _threshold (VALUE_ZERO)
    , _depth (0)
    , _index (0)
{
    assert(MOVE_NONE == ttm
        || (pos.pseudo_legal (ttm)
         && pos.legal (ttm)));

    if (0 == _pos.si->checkers)
    {
        _stage = Stage::NATURAL_TT;

        _killer_moves.assign (_ss->killer_moves, _ss->killer_moves + MaxKillers);
    }
    else
    {
        _stage = Stage::EVASION_TT;
    }
    
    if (MOVE_NONE == _tt_move)
    {
        ++_stage;
    }
}
MovePicker::MovePicker (const Position &pos, Move ttm, const Stack *const &ss, i16 d)
    : _pos (pos)
    , _ss (ss)
    , _tt_move (ttm)
    , _recap_sq (SQ_NO)
    , _threshold (VALUE_ZERO)
    , _depth (d)
    , _index (0)
{
    assert(d <= 0);
    assert(MOVE_NONE == ttm
        || (pos.pseudo_legal (ttm)
         && pos.legal (ttm)));

    if (0 != _pos.si->checkers)
    {
        _stage = Stage::EVASION_TT;
    }
    else
    if (_depth >= 0)
    {
        _stage = Stage::Q_CHECK_TT;
    }
    else
    if (_depth > -5)
    {
        _stage = Stage::Q_NO_CHECK_TT;
    }
    else
    {
        _stage = Stage::Q_RECAPTURE_TT;
        _recap_sq = dst_sq ((_ss-1)->current_move);
        if (   MOVE_NONE != _tt_move
            && !(   _pos.capture (_tt_move)
                 && dst_sq (_tt_move) == _recap_sq))
        {
            _tt_move = MOVE_NONE;
        }
    }
    if (MOVE_NONE == _tt_move)
    {
        ++_stage;
    }
}
MovePicker::MovePicker (const Position &pos, Move ttm, Value thr)
    : _pos (pos)
    , _ss (nullptr)
    , _tt_move (ttm)
    , _recap_sq (SQ_NO)
    , _threshold (thr)
    , _depth (0)
    , _index (0)
{
    assert(0 == pos.si->checkers);
    assert(MOVE_NONE == ttm
        || (pos.pseudo_legal (ttm)
         && pos.legal (ttm)));

    _stage = Stage::PROBCUT_CAPTURE_TT;

    // In ProbCut we generate captures with SEE greater than or equal to the given threshold
    if (   MOVE_NONE != _tt_move
        && !(   _pos.capture (_tt_move)
             && _pos.see_ge (_tt_move, _threshold)))
    {
        _tt_move = MOVE_NONE;
    }
    if (MOVE_NONE == _tt_move)
    {
        ++_stage;
    }
}

// Assigns a numerical move ordering score to each move in a move list.
// The moves with highest scores will be picked first.

// Winning and equal captures in the main search are ordered by MVV/LVA, preferring captures near our home rank.
// Surprisingly, this appears to perform slightly better than SEE-based move ordering,
// exchanging big pieces before capturing a hanging piece probably helps to reduce the subtree size.
// In the main search push captures with negative SEE values to the bad captures vector,
// but instead of doing it now we delay until the move has been picked up,
// saving some SEE calls in case of a cutoff.
template<> void MovePicker::value<GenType::CAPTURE> ()
{
    for (auto &vm : _moves)
    {
        assert(_pos.pseudo_legal (vm.move)
            && _pos.legal (vm.move)
            && _pos.capture_or_promotion (vm.move));

        vm.value =
              PieceValues[MG][_pos.cap_type (vm.move)]
            - ptype (_pos[org_sq (vm.move)])
            - 200 * rel_rank (_pos.active, dst_sq (vm.move));
    }
}
template<> void MovePicker::value<GenType::QUIET> ()
{
    const auto &history = _pos.thread->history;

    const auto &smh1 = *(_ss-1)->m_history;
    const auto &smh2 = *(_ss-2)->m_history;
    const auto &smh4 = *(_ss-4)->m_history;

    for (auto &vm : _moves)
    {
        assert(_pos.pseudo_legal (vm.move)
            && _pos.legal (vm.move));

        vm.value =
              history[_pos.active][move_pp (vm.move)]
            + smh1[_pos[org_sq (vm.move)]][dst_sq (vm.move)]
            + smh2[_pos[org_sq (vm.move)]][dst_sq (vm.move)]
            + smh4[_pos[org_sq (vm.move)]][dst_sq (vm.move)];
    }
}
// First captures ordered by MVV/LVA, then non-captures ordered by stats heuristics
template<> void MovePicker::value<GenType::EVASION> ()
{
    const auto &history = _pos.thread->history;

    for (auto &vm : _moves)
    {
        assert(_pos.pseudo_legal (vm.move)
            && _pos.legal (vm.move));
        
        if (_pos.capture (vm.move))
        {
            vm.value =
                  PieceValues[MG][_pos.cap_type (vm.move)]
                - ptype (_pos[org_sq (vm.move)])
                + MaxValue;
        }
        else
        {
            vm.value =
                  history[_pos.active][move_pp (vm.move)];
        }
    }
}

// Finds the best move in the range [beg, end) and moves it to front,
// it is faster than sorting all the moves in advance when there are few moves (e.g. the possible captures).
ValMove& MovePicker::swap_best_move (u08 i)
{
    auto itr = _moves.begin () + i;
    std::swap (*itr, *std::max_element (itr, _moves.end ()));
    return *itr;
}
// Returns a new legal move every time it is called, until there are no more moves left.
// It picks the move with the biggest value from a list of generated moves.
Move MovePicker::next_move (bool skip_quiets)
{
    switch (_stage)
    {

    case Stage::NATURAL_TT:
    case Stage::EVASION_TT:
    case Stage::PROBCUT_CAPTURE_TT:
    case Stage::Q_CHECK_TT:
    case Stage::Q_NO_CHECK_TT:
    case Stage::Q_RECAPTURE_TT:
        ++_stage;
        return _tt_move;
        break;

    case Stage::NATURAL:
        ++_stage;
        generate<GenType::CAPTURE> (_moves, _pos);
        filter_illegal (_moves, _pos);
        if (MOVE_NONE != _tt_move)
        {
            _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
        }
        _index = 0;
        if (1 < _moves.size ())
        {
            value<GenType::CAPTURE> ();
        }
        /* fallthrough */
    case Stage::GOOD_CAPTURE:
        while (_index < _moves.size ())
        {
            auto &vm = swap_best_move (_index++);
            if (_pos.see_ge (vm.move))
            {
                return vm.move;
            }
            // Losing capture, add it to the capture moves
            _capture_moves.push_back (vm.move);
        }
        ++_stage;
        generate<GenType::QUIET> (_moves, _pos);
        filter_illegal (_moves, _pos);
        if (MOVE_NONE != _tt_move)
        {
            _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
        }
        _index = 0;
        if (1 < _moves.size ())
        {
            value<GenType::QUIET> ();
        }
        // Killers to top of quiet move
        {
            auto m = (_ss-1)->current_move;
            if (_ok (m))
            {
                auto cm = _pos.thread->counter_moves[_pos[fix_dst_sq (m)]][dst_sq (m)];
                if (   MOVE_NONE != cm
                    && _tt_move != cm
                    && std::find (_killer_moves.begin (), _killer_moves.end (), cm) == _killer_moves.end ())
                {
                    _killer_moves.push_back (cm);
                }
            }
            _killer_moves.erase (std::remove_if (_killer_moves.begin (),
                                                 _killer_moves.end (),
                                                 [&](Move mm)
                                                 {
                                                     return mm == MOVE_NONE
                                                         || mm == _tt_move;
                                                 }),
                                 _killer_moves.end ());

            i32 k = 0;
            for (auto km : _killer_moves)
            {
                auto itr = std::find (_moves.begin (), _moves.end (), km);
                if (itr != _moves.end ())
                {
                    assert(_pos.pseudo_legal (km)
                        && _pos.legal (km));
                    itr->value = MaxValue - k++;
                }
            }
        }
        /* fallthrough */
    case Stage::QUIET:
        while (_index < _moves.size ())
        {
            auto &vm = swap_best_move (_index++);
            if (   skip_quiets
                && vm.value < VALUE_ZERO)
            {
                break;
            }
            return vm.move;
        }
        ++_stage;
        _index = 0;
        /* fallthrough */
    case Stage::BAD_CAPTURE:
        while (_index < _capture_moves.size ())
        {
            return _capture_moves[_index++];
        }
        break;

    case Stage::EVASION:
        assert(0 != _pos.si->checkers);
        ++_stage;
        generate<GenType::EVASION> (_moves, _pos);
        filter_illegal (_moves, _pos);
        if (MOVE_NONE != _tt_move)
        {
            _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
        }
        _index = 0;
        if (1 < _moves.size ())
        {
            value<GenType::EVASION> ();
        }
        /* fallthrough */
    case Stage::ALL_EVASION:
        while (_index < _moves.size ())
        {
            return swap_best_move (_index++).move;
        }
        break;

    case Stage::PROBCUT_CAPTURE:
        ++_stage;
        generate<GenType::CAPTURE> (_moves, _pos);
        filter_illegal (_moves, _pos);
        if (MOVE_NONE != _tt_move)
        {
            _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
        }
        _index = 0;
        if (1 < _moves.size ())
        {
            value<GenType::CAPTURE> ();
        }
        /* fallthrough */
    case Stage::ALL_PROBCUT_CAPTURE:
        while (_index < _moves.size ())
        {
            auto &vm = swap_best_move (_index++);
            if (_pos.see_ge (vm.move, _threshold))
            {
                return vm.move;
            }
        }
        break;

    case Stage::Q_CHECK:
        ++_stage;
        generate<GenType::CAPTURE> (_moves, _pos);
        filter_illegal (_moves, _pos);
        if (MOVE_NONE != _tt_move)
        {
            _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
        }
        _index = 0;
        if (1 < _moves.size ())
        {
            value<GenType::CAPTURE> ();
        }
        /* fallthrough */
    case Stage::Q_CHECK_CAPTURE:
        while (_index < _moves.size ())
        {
            return swap_best_move (_index++).move;
        }
        ++_stage;
        generate<GenType::QUIET_CHECK> (_moves, _pos);
        filter_illegal (_moves, _pos);
        if (MOVE_NONE != _tt_move)
        {
            _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
        }
        _index = 0;
        if (1 < _moves.size ())
        {
            value<GenType::QUIET> ();
        }
        /* fallthrough */
    case Stage::Q_CHECK_QUIET:
        while (_index < _moves.size ())
        {
            return swap_best_move (_index++).move;
        }
        break;

    case Stage::Q_NO_CHECK:
        ++_stage;
        generate<GenType::CAPTURE> (_moves, _pos);
        filter_illegal (_moves, _pos);
        if (MOVE_NONE != _tt_move)
        {
            _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
        }
        _index = 0;
        if (1 < _moves.size ())
        {
            value<GenType::CAPTURE> ();
        }
        /* fallthrough */
    case Stage::Q_NO_CHECK_CAPTURE:
        while (_index < _moves.size ())
        {
            return swap_best_move (_index++).move;
        }
        break;

    case Stage::Q_RECAPTURE:
        ++_stage;
        generate<GenType::CAPTURE> (_moves, _pos);
        filter_illegal (_moves, _pos);
        if (MOVE_NONE != _tt_move)
        {
            _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
        }
        _index = 0;
        if (1 < _moves.size ())
        {
            value<GenType::CAPTURE> ();
        }
        /* fallthrough */
    case Stage::Q_ALL_RECAPTURE:
        assert(SQ_NO != _recap_sq);
        while (_index < _moves.size ())
        {
            auto &vm = swap_best_move (_index++);
            if (dst_sq (vm.move) == _recap_sq)
            {
                return vm.move;
            }
        }
        break;

    default:
        assert(false);
        break;
    }
    return MOVE_NONE;
}

namespace Searcher {

    Limit  Limits;

    u16    MultiPV = 1;
    //i32    MultiPV_cp = 0;

    i16    FixedContempt = 0
        ,  ContemptTime = 30
        ,  ContemptValue = 50;

    string HashFile = "Hash.dat";

    bool   OwnBook = false;
    string BookFile = "Book.bin";
    bool   BookMoveBest = true;
    i16    BookUptoMove = 20;

    i16    TBProbeDepth = 1;
    i32    TBLimitPiece = 6;
    bool   TBUseRule50 = true;
    bool   TBHasRoot = false;
    Value  TBValue = VALUE_ZERO;

    string OutputFile = Empty;

    namespace {

        const i16 MaxRazorDepth = 4;
        // RazorMargins[depth]
        Value RazorMargins[MaxRazorDepth] = { Value(0), Value(570), Value(602), Value(554) };

        const i16 MaxFutilityDepth = 16;
        // FutilityMoveCounts[improving][depth]
        u08 FutilityMoveCounts[2][MaxFutilityDepth];

        const i16 MaxReductionDepth = 64;
        const u08 MaxReductionMoveCount = 64;
        // ReductionDepths[pv][improving][depth][move_count]
        i16 ReductionDepths[2][2][MaxReductionDepth][MaxReductionMoveCount];
        i16 reduction_depth (bool pv, bool imp, i16 d, u08 mc)
        {
            return ReductionDepths[pv ? 1 : 0]
                                  [imp ? 1 : 0]
                                  [std::min (d, i16(MaxReductionDepth-1))]
                                  [std::min (mc, u08(MaxReductionMoveCount-1))];
        }

        Value DrawValue[CLR_NO]
            , BaseContempt[CLR_NO];

        ofstream OutputStream;

        // Stats bonus, based on depth
        i32 stat_bonus (i16 depth)
        {
            return depth <= 17 ? depth*(depth + 2) - 2 : 0;
        }

        // Updates countermoves and followupmoves history stats
        void update_cm_stats (Stack *const &ss, Piece pc, Square s, i32 value)
        {
            for (auto i : {1, 2, 4})
            {
                if (_ok ((ss-i)->current_move))
                {
                    (ss-i)->m_history->update (pc, s, value);
                }
            }
        }
        // Updates move sorting heuristics
        void update_stats (Stack *const &ss, const Position &pos, Move move, i32 value)
        {
            if (ss->killer_moves[0] != move)
            {
                ss->killer_moves[1] = ss->killer_moves[0];
                ss->killer_moves[0] = move;
            }
            assert(1 == std::count (ss->killer_moves, ss->killer_moves + MaxKillers, move));

            auto &th = pos.thread;
            auto m = (ss-1)->current_move;
            if (_ok (m))
            {
                th->counter_moves[pos[fix_dst_sq (m)]][dst_sq (m)] = move;
            }

            th->history.update (pos.active, move, value);
            update_cm_stats (ss, pos[org_sq (move)], dst_sq (move), value);
        }

        // Appends the move and child pv
        void update_pv (Moves &pv, Move move, const Moves &child_pv)
        {
            pv.clear ();
            pv.push_back (move);
            for (auto m : child_pv)
            {
                pv.push_back (m);
            }
        }

        // It adjusts a mate score from "plies to mate from the root" to
        // "plies to mate from the current position". Non-mate scores are unchanged.
        // The function is called before storing a value to the transposition table.
        Value value_to_tt (Value v, i32 ply)
        {
            assert(VALUE_NONE != v);
            return v + ((v >= +VALUE_MATE_MAX_PLY) - (v <= -VALUE_MATE_MAX_PLY)) * ply;
        }
        // It adjusts a mate score from "plies to mate from the current position" to "plies to mate from the root".
        // Non-mate scores are unchanged.
        // The function is called after retrieving a value of the transposition table.
        Value value_of_tt (Value v, i32 ply)
        {
            return v - (VALUE_NONE != v ?
                       ((v >= +VALUE_MATE_MAX_PLY) - (v <= -VALUE_MATE_MAX_PLY)) * ply : 0);
        }

        // Formats PV information according to UCI protocol.
        // UCI requires that all (if any) unsearched PV lines are sent using a previous search score.
        string multipv_info (Thread *const &th, i16 depth, Value alfa, Value beta)
        {
            auto elapsed_time = std::max (Threadpool.main_thread ()->time_mgr.elapsed_time (), 1LL);
            
            const auto &root_moves = th->root_moves;

            auto total_nodes = Threadpool.nodes ();
            auto tb_hits = Threadpool.tb_hits () + (TBHasRoot ? root_moves.size () : 0);

            ostringstream oss;
            u16 i = 0;
            while (true)
            {
                bool updated = 
                       i <= th->pv_index
                    && -VALUE_INFINITE != root_moves[i].new_value;

                auto d = updated ? depth : i16(depth - 1);

                if (d <= 0)
                {
                    if (i == Threadpool.pv_limit - 1)
                    {
                        break;
                    }
                    continue;
                }

                auto v =
                    updated ?
                        root_moves[i].new_value :
                        root_moves[i].old_value;
                bool tb =
                       TBHasRoot
                    && abs (v) < +VALUE_MATE - i32(MaxPlies);

                oss << "info"
                    << " multipv "  << i + 1
                    << " depth "    << d
                    << " seldepth " << th->max_ply
                    << " score "    << to_string (tb ? TBValue : v)
                    << (   !tb
                        && i == th->pv_index ?
                            beta <= v ? " lowerbound" :
                                v <= alfa ? " upperbound" : "" : "")
                    << " nodes "    << total_nodes
                    << " time "     << elapsed_time
                    << " nps "      << total_nodes * 1000 / elapsed_time
                    << " tbhits "   << tb_hits;
                if (elapsed_time > 1000)
                {
                    oss << " hashfull " << TT.hash_full ();
                }
                oss << " pv"        << root_moves[i];
                if (i++ == Threadpool.pv_limit - 1)
                {
                    break;
                }
                oss << '\n';
            }
            return oss.str ();
        }

        // The quiescence search function, which is called by the main depth limited search function
        // when the remaining depth is less than equal to 0.
        template<bool PVNode>
        Value quien_search (Position &pos, Stack *const &ss, Value alfa, Value beta, i16 depth = 0)
        {
            assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            assert(PVNode || (alfa == beta-1));
            assert(0 >= depth);
            assert(ss->ply > 1
                && ss->ply == (ss-1)->ply + 1
                && ss->ply <= MaxPlies);

            Value old_alfa;

            if (PVNode)
            {
                old_alfa = alfa; // To flag BOUND_EXACT when eval above alpha and no available moves
                ss->pv.clear ();
            }

            bool in_check = 0 != pos.si->checkers;
            // Check for an immediate draw or maximum ply reached.
            if (   ss->ply >= MaxPlies
                || pos.draw (ss->ply))
            {
                return ss->ply >= MaxPlies
                    && !in_check ?
                        evaluate (pos) :
                        DrawValue[pos.active];
            }

            //auto &th = pos.thread;

            Move move;
            // Transposition table lookup.
            bool tt_hit;
            auto *tte = TT.probe (pos.si->posi_key, tt_hit);
            auto tt_move =
                   tt_hit
                && MOVE_NONE != (move = tte->move ())
                && pos.pseudo_legal (move)
                && pos.legal (move) ?
                    move :
                    MOVE_NONE;
            assert(MOVE_NONE == tt_move
                || (pos.pseudo_legal (tt_move)
                 && pos.legal (tt_move)));
            auto tt_value =
                   tt_hit
                && tte->move () == tt_move ?
                    value_of_tt (tte->value (), ss->ply) :
                    VALUE_NONE;

            ss->current_move = MOVE_NONE;

            // Decide whether or not to include checks.
            // Fixes also the type of TT entry depth that are going to use.
            // Note that in quien_search use only 2 types of depth: (0) or (-1).
            i16 qs_depth = in_check || 0 <= depth ? 0 : -1;

            if (   !PVNode
                && VALUE_NONE != tt_value // Only in case of TT access race
                && tte->depth () >= qs_depth
                && BOUND_NONE != (tte->bound () & (tt_value >= beta ? BOUND_LOWER : BOUND_UPPER)))
            {
                return tt_value;
            }

            Value best_value
                , futility_base;

            // Evaluate the position statically.
            if (in_check)
            {
                ss->static_eval = VALUE_NONE;
                // Starting from the worst case which is checkmate
                best_value =
                futility_base = mated_in (ss->ply);
            }
            else
            {
                Value tt_eval;
                if (tt_hit)
                {
                    // Never assume anything on values stored in TT.
                    ss->static_eval = tt_eval =
                        VALUE_NONE != tte->eval () ?
                            tte->eval () :
                            evaluate (pos);
                    // Can tt_value be used as a better position evaluation?
                    if (   VALUE_NONE != tt_value
                        && BOUND_NONE != (tte->bound () & (tt_value > tt_eval ? BOUND_LOWER : BOUND_UPPER)))
                    {
                        tt_eval = tt_value;
                    }
                }
                else
                {
                    ss->static_eval = tt_eval =
                        (ss-1)->current_move != MOVE_NULL ?
                            evaluate (pos) :
                            -(ss-1)->static_eval + Tempo*2;
                }

                if (alfa < tt_eval)
                {
                    // Stand pat. Return immediately if static value is at least beta.
                    if (tt_eval >= beta)
                    {
                        if (!tt_hit)
                        {
                            tte->save (pos.si->posi_key,
                                       MOVE_NONE,
                                       value_to_tt (tt_eval, ss->ply),
                                       ss->static_eval,
                                       -6,
                                       BOUND_LOWER);
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
                futility_base = best_value + 128;
            }

            auto best_move = MOVE_NONE;

            u08 move_count = 0;

            // Initialize move picker (2) for the current position.
            MovePicker mp (pos, tt_move, ss, depth);
            StateInfo si;
            // Loop through the moves until no moves remain or a beta cutoff occurs.
            while (MOVE_NONE != (move = mp.next_move ()))
            {
                assert(pos.pseudo_legal (move)
                    && pos.legal (move));

                ++move_count;

                bool gives_check = pos.gives_check (move);

                // Futility pruning
                if (   !in_check
                    && futility_base <= alfa
                    && futility_base > -VALUE_KNOWN_WIN
                    && !gives_check
                    //&& 0 == Limits.mate
                        // Advance pawn push
                    && !(   PAWN == ptype (pos[org_sq (move)])
                         && rel_rank (pos.active, org_sq (move)) > R_4))
                {
                    // Futility pruning parent node
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
                    if (!pos.see_ge (move, VALUE_ONE))
                    {
                        if (best_value < futility_base)
                        {
                            best_value = futility_base;
                        }
                        continue;
                    }
                }

                // Don't search moves with negative SEE values
                if (   (   !in_check
                        // Evasion Prunable: Detect non-capture evasions that are candidate to be pruned
                        || (   (   0 != depth
                                || 2 < move_count)
                            && best_value > -VALUE_MATE_MAX_PLY
                            && !pos.capture (move)))
                    && PROMOTE != mtype (move)
                    //&& 0 == Limits.mate
                    && !pos.see_ge (move))
                {
                    continue;
                }

                // Speculative prefetch as early as possible
                prefetch (TT.cluster_entry (pos.move_posi_key (move)));

                ss->current_move = move;

                // Make the move
                pos.do_move (move, si, gives_check);

                auto value = -quien_search<PVNode> (pos, ss+1, -beta, -alfa, depth - 1);

                // Undo the move
                pos.undo_move (move);

                assert(-VALUE_INFINITE < value && value < +VALUE_INFINITE);

                // Check for new best move
                if (best_value < value)
                {
                    best_value = value;

                    if (alfa < value)
                    {
                        // Update pv even in fail-high case
                        if (PVNode)
                        {
                            update_pv (ss->pv, move, (ss+1)->pv);
                        }
                        // Fail high
                        if (value >= beta)
                        {
                            tte->save (pos.si->posi_key,
                                       move,
                                       value_to_tt (value, ss->ply),
                                       ss->static_eval,
                                       qs_depth,
                                       BOUND_LOWER);

                            assert(-VALUE_INFINITE < value && value < +VALUE_INFINITE);
                            return value;
                        }
                        assert(value < beta);

                        // Update alfa! Always alfa < beta
                        if (PVNode)
                        {
                            alfa = value;
                            best_move = move;
                        }
                    }
                }
            }

            tte->save (pos.si->posi_key,
                       best_move,
                       value_to_tt (best_value, ss->ply),
                       ss->static_eval,
                       qs_depth,
                          PVNode
                       && best_value > old_alfa ?
                           BOUND_EXACT :
                           BOUND_UPPER);

            assert(-VALUE_INFINITE < best_value && best_value < +VALUE_INFINITE);
            return best_value;
        }
        // The main depth limited search function for both PV and non-PV nodes.
        template<bool PVNode>
        Value depth_search (Position &pos, Stack *const &ss, Value alfa, Value beta, i16 depth, bool cut_node, bool prun_node, Move exclude_move = MOVE_NONE)
        {
            assert(-VALUE_INFINITE <= alfa && alfa < beta && beta <= +VALUE_INFINITE);
            assert(PVNode || (alfa == beta-1));
            assert(!(PVNode && cut_node));
            assert(0 < depth && depth < MaxPlies);
            assert(ss->ply >= 1
                && ss->ply == (ss-1)->ply + 1
                && ss->ply <= MaxPlies);

            // Step 1. Initialize node.
            auto &th = pos.thread;
            
            if (PVNode)
            {
                // Update max_ply.
                if (th->max_ply < ss->ply)
                {
                    th->max_ply = ss->ply;
                }
            }

            // Check for the available remaining limit.
            if (Threadpool.main_thread () == th)
            {
                Threadpool.main_thread ()->check_limits ();
            }

            bool root_node = 1 == ss->ply;
            bool in_check = 0 != pos.si->checkers;

            if (!root_node)
            {
                // Step 2. Check for aborted search, immediate draw or maximum ply reached.
                if (   Threadpool.force_stop.load (memory_order_relaxed)
                    || ss->ply >= MaxPlies
                    || pos.draw (ss->ply))
                {
                    return ss->ply >= MaxPlies
                        && !in_check ?
                            evaluate (pos) :
                            DrawValue[pos.active];
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

            ss->statistics = 0;
            ss->move_count = 0;
            ss->current_move = MOVE_NONE;
            ss->m_history = &th->cm_history[NO_PIECE][0];
            std::fill_n ((ss+2)->killer_moves, MaxKillers, MOVE_NONE);

            bool exclusion = MOVE_NONE != exclude_move;

            Move move;
            // Step 4. Transposition table lookup
            // Don't want the score of a partial search to overwrite a previous full search
            // TT value, so use a different position key in case of an excluded move.
            bool tt_hit = false;
            auto *tte =
                exclusion ?
                    nullptr :
                    TT.probe (pos.si->posi_key, tt_hit);
            auto tt_move =
                root_node ?
                    th->root_moves[th->pv_index][0] :
                       tt_hit
                    && MOVE_NONE != (move = tte->move ())
                    && pos.pseudo_legal (move)
                    && pos.legal (move) ?
                        move :
                        MOVE_NONE;
            assert(MOVE_NONE == tt_move
                || (pos.pseudo_legal (tt_move)
                 && pos.legal (tt_move)));

            auto tt_value =
                   tt_hit
                && (   root_node
                    || tte->move () == tt_move) ?
                    value_of_tt (tte->value (), ss->ply) :
                    VALUE_NONE;

            // At non-PV nodes we check for an early TT cutoff.
            if (   !PVNode
                && VALUE_NONE != tt_value // Only in case of TT access race.
                && tte->depth () >= depth
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
                            update_stats (ss, pos, tt_move, stat_bonus (depth));
                        }
                        // Extra penalty for a quiet tt_move in previous ply when it gets refuted.
                        auto m = (ss-1)->current_move;
                        if (   1 == (ss-1)->move_count
                            && _ok (m)
                            && NONE == pos.si->capture
                            && !pos.si->promotion)
                        {
                            update_cm_stats (ss-1, pos[fix_dst_sq (m)], dst_sq (m), -stat_bonus (depth + 1));
                        }
                    }
                    else
                    {
                        // Penalty for a quiet tt_move that fails low.
                        if (!pos.capture_or_promotion (tt_move))
                        {
                            auto penalty = -stat_bonus (depth);
                            th->history.update (pos.active, tt_move, penalty);
                            update_cm_stats (ss, pos[org_sq (tt_move)], dst_sq (tt_move), penalty);
                        }
                    }
                }
                return tt_value;
            }

            // Step 4A. Tablebase probe
            if (   !root_node
                && !exclusion
                && 0 != TBLimitPiece)
            {
                auto piece_count = pos.count<NONE> ();

                if (   (   piece_count < TBLimitPiece
                        || (   piece_count == TBLimitPiece
                            && depth >= TBProbeDepth))
                    && 0 == pos.si->clock_ply
                    && !pos.has_castleright (CR_ANY))
                {
                    ProbeState state;
                    WDLScore wdl = probe_wdl (pos, state);

                    if (ProbeState::PB_FAILURE != state)
                    {
                        th->tb_hits.fetch_add (1, std::memory_order::memory_order_relaxed);

                        auto draw = TBUseRule50 ? 1 : 0;

                        auto value = wdl < -draw ? -VALUE_MATE + i32(MaxPlies + ss->ply) :
                                     wdl > +draw ? +VALUE_MATE - i32(MaxPlies + ss->ply) :
                                                    VALUE_ZERO + 2 * draw * wdl;

                        tte->save (pos.si->posi_key,
                                   MOVE_NONE,
                                   value_to_tt (value, ss->ply),
                                   VALUE_NONE,
                                   std::min<i16> (depth + 6, MaxPlies - 1),
                                   BOUND_EXACT);

                        return value;
                    }
                }
            }

            StateInfo si;

            // Step 5. Evaluate the position statically.
            if (in_check)
            {
                ss->static_eval = VALUE_NONE;
            }
            else
            {
                Value tt_eval;
                if (tt_hit)
                {
                    // Never assume anything on values stored in TT.
                    ss->static_eval = tt_eval =
                        VALUE_NONE != tte->eval () ?
                            tte->eval () :
                            evaluate (pos);
                    // Can tt_value be used as a better position evaluation?
                    if (   VALUE_NONE != tt_value
                        && BOUND_NONE != (tte->bound () & (tt_value > tt_eval ? BOUND_LOWER : BOUND_UPPER)))
                    {
                        tt_eval = tt_value;
                    }
                }
                else
                {
                    ss->static_eval = tt_eval =
                        (ss-1)->current_move != MOVE_NULL ?
                            evaluate (pos) :
                            -(ss-1)->static_eval + Tempo*2;

                    if (!exclusion)
                    {
                        tte->save (pos.si->posi_key,
                                   MOVE_NONE,
                                   VALUE_NONE,
                                   ss->static_eval,
                                   -6,
                                   BOUND_NONE);
                    }
                }

                if (prun_node)
                {
                    assert(!exclusion);

                    // Step 6. Razoring sort of forward pruning where rather than
                    // skipping an entire subtree, search it to a reduced depth.
                    if (   !PVNode
                        && MaxRazorDepth > depth
                        //&& 0 == Limits.mate
                        && tt_eval + RazorMargins[depth] <= alfa)
                    {
                        if (1 >= depth)
                        {
                            return quien_search<false> (pos, ss, alfa, alfa+1);
                        }
                        auto alfa_margin = alfa - RazorMargins[depth];
                        assert(alfa_margin >= -VALUE_INFINITE);
                        auto value = quien_search<false> (pos, ss, alfa_margin, alfa_margin+1);
                        if (value <= alfa_margin)
                        {
                            return value;
                        }
                    }

                    // Step 7. Futility pruning: child node
                    // Betting that the opponent doesn't have a move that will reduce
                    // the score by more than futility margins [depth] if do a null move.
                    if (   !root_node
                        && 7 > depth
                        && tt_eval < +VALUE_KNOWN_WIN
                        && tt_eval - 150*depth >= beta
                        //&& 0 == Limits.mate
                        && pos.si->non_pawn_material (pos.active) > VALUE_ZERO)
                    {
                        return tt_eval;
                    }

                    // Step 8. Null move search with verification search.
                    if (   !PVNode
                        && tt_eval >= beta
                        //&& 0 == Limits.mate
                        && (   12 < depth
                            || ss->static_eval >= beta - 35*(depth - 6))
                        && pos.si->non_pawn_material (pos.active) > VALUE_ZERO)
                    {
                        // Speculative prefetch as early as possible.
                        prefetch (TT.cluster_entry (  pos.si->posi_key
                                                    ^ RandZob.color_key
                                                    ^ (SQ_NO != pos.si->en_passant_sq ? RandZob.en_passant_keys[_file (pos.si->en_passant_sq)] : 0)));

                        ss->current_move = MOVE_NULL;
                        ss->m_history = &th->cm_history[NO_PIECE][0];

                        pos.do_null_move (si);

                        // Null move dynamic reduction based on depth and static evaluation.
                        auto reduced_depth = i16(depth - (67*depth + 823) / 256 + std::min (i16(tt_eval - beta)/VALUE_MG_PAWN, 3));

                        auto null_value =
                            reduced_depth <= 0 ?
                                -quien_search<false> (pos, ss+1, -beta, -beta+1) :
                                -depth_search<false> (pos, ss+1, -beta, -beta+1, reduced_depth, !cut_node, false);
                        pos.undo_null_move ();

                        if (null_value >= beta)
                        {
                            bool unproven = null_value >= +VALUE_MATE_MAX_PLY;

                            // Don't do verification search at low depths.
                            if (   12 > depth
                                && abs (beta) < +VALUE_KNOWN_WIN)
                            {
                                // Don't return unproven mates.
                                return unproven ?
                                        beta : null_value;
                            }

                            // Do verification search at high depths.
                            auto value =
                                reduced_depth <= 0 ?
                                    quien_search<false> (pos, ss, beta-1, beta) :
                                    depth_search<false> (pos, ss, beta-1, beta, reduced_depth, false, false);

                            if (value >= beta)
                            {
                                // Don't return unproven mates.
                                return unproven ?
                                        beta : null_value;
                            }
                        }
                    }

                    // Step 9. ProbCut
                    // If good enough capture and a reduced search returns a value much above beta,
                    // then can (almost) safely prune the previous move.
                    if (   !PVNode
                        && 4 < depth
                        //&& 0 == Limits.mate
                        && abs (beta) < +VALUE_MATE_MAX_PLY)
                    {
                        auto beta_margin = std::min (beta + 200, +VALUE_INFINITE);
                        
                        assert(_ok ((ss-1)->current_move));

                        // Initialize move picker (3) for the current position.
                        MovePicker mp (pos, tt_move, beta_margin - ss->static_eval);
                        // Loop through all legal moves until no moves remain or a beta cutoff occurs.
                        while (MOVE_NONE != (move = mp.next_move ()))
                        {
                            assert(pos.pseudo_legal (move)
                                && pos.legal (move));
                            assert(pos.capture_or_promotion (move));

                            // Speculative prefetch as early as possible.
                            prefetch (TT.cluster_entry (pos.move_posi_key (move)));

                            ss->current_move = move;
                            ss->m_history = &th->cm_history[pos[org_sq (move)]][dst_sq (move)];

                            pos.do_move (move, si);

                            auto value = -depth_search<false> (pos, ss+1, -beta_margin, -beta_margin+1, i16(depth - 4), !cut_node, true);

                            pos.undo_move (move);

                            if (value >= beta_margin)
                            {
                                return value;
                            }
                        }
                    }

                    // Step 10. Internal iterative deepening (IID).
                    if (   MOVE_NONE == tt_move
                        && 4 < depth
                        && (   PVNode
                            || ss->static_eval + 256 >= beta))
                    {
                        depth_search<PVNode> (pos, ss, alfa, beta, 3*depth/4 - 2, cut_node, false);

                        tte = TT.probe (pos.si->posi_key, tt_hit);
                        tt_move =
                               tt_hit
                            && MOVE_NONE != (move = tte->move ())
                            && pos.pseudo_legal (move)
                            && pos.legal (move) ?
                                move :
                                MOVE_NONE;
                        tt_value =
                               tt_hit
                            && tte->move () == tt_move ?
                                value_of_tt (tte->value (), ss->ply) :
                                VALUE_NONE;
                    }
                }
            }

            auto best_value = -VALUE_INFINITE;
            auto value = -VALUE_INFINITE;

            auto best_move = MOVE_NONE;

            bool singular_ext_node =
                   !root_node
                && tt_hit
                && MOVE_NONE != tt_move
                && VALUE_NONE != tt_value
                && !exclusion // Recursive singular search is not allowed.
                && 7 < depth
                && tte->depth () + 4 > depth
                && BOUND_NONE != (tte->bound () & BOUND_LOWER);

            bool improving =
                   (ss-2)->static_eval <= (ss-0)->static_eval
                || (ss-2)->static_eval == VALUE_NONE;

            const auto &smh1 = *(ss-1)->m_history;
            const auto &smh2 = *(ss-2)->m_history;
            const auto &smh4 = *(ss-4)->m_history;

            u08 move_count = 0;

            Moves quiet_moves;
            quiet_moves.reserve (16);

            bool  skip_quiets = false
                , ttm_capture = false;

            // Initialize move picker (1) for the current position.
            MovePicker mp (pos, tt_move, ss);
            // Step 11. Loop through moves
            // Loop through all legal moves until no moves remain or a beta cutoff occurs.
            while (MOVE_NONE != (move = mp.next_move (skip_quiets)))
            {
                assert(pos.pseudo_legal (move)
                    && pos.legal (move));

                if (   // Skip exclusion move
                       (move == exclude_move)
                       // At root obey the "searchmoves" option and skip moves not listed in
                       // RootMove list, as a consequence any illegal move is also skipped.
                       // In MultiPV mode also skip PV moves which have been already searched.
                    || (   root_node
                        && std::find (th->root_moves.begin () + th->pv_index,
                                      th->root_moves.end (), move) ==
                                      th->root_moves.end ()))
                {
                    continue;
                }

                ss->move_count = ++move_count;

                bool gives_check = pos.gives_check (move);
                bool capture_or_promotion = pos.capture_or_promotion (move);
                bool move_count_pruning =
                       MaxFutilityDepth > depth
                    && FutilityMoveCounts[improving][depth] <= move_count;

                auto mpc = pos[org_sq (move)];
                assert(NO_PIECE != mpc);

                if (   root_node
                    && Threadpool.main_thread () == th)
                {
                    auto elapsed_time = Threadpool.main_thread ()->time_mgr.elapsed_time ();
                    if (elapsed_time > 3000)
                    {
                        sync_cout
                            << "info"
                            << " currmove "       << move_to_can (move)
                            << " currmovenumber " << th->pv_index + move_count
                            << " maxmoves "       << th->root_moves.size ()
                            << " depth "          << depth
                            << " time "           << elapsed_time
                            << sync_endl;
                    }
                }

                if (PVNode)
                {
                    (ss+1)->pv.clear ();
                }

                // Calculate new depth for this move
                i16 new_depth = depth - 1;

                // Step 12. Extensions

                // Singular extension (SE)
                // We extend the TT move if its value is much better than its siblings.
                // If all moves but one fail low on a search of (alfa-s, beta-s),
                // and just one fails high on (alfa, beta), then that move is singular and should be extended.
                // To verify this do a reduced search on all the other moves but the tt_move,
                // if result is lower than tt_value minus a margin then extend tt_move.
                if (   singular_ext_node
                    && move == tt_move)
                {
                    auto beta_margin = std::max (tt_value - 2*depth, -VALUE_MATE);
                    value = depth_search<false> (pos, ss, beta_margin-1, beta_margin, depth/2, cut_node, false, move);

                    if (value < beta_margin)
                    {
                        new_depth += 1;
                    }
                }
                else
                // Check extension (CE)
                if (   gives_check
                    && !move_count_pruning
                    && pos.see_ge (move))
                {
                    new_depth += 1;
                }

                // Step 13. Pruning at shallow depth.
                if (   !root_node
                    //&& 0 == Limits.mate
                    && best_value > -VALUE_MATE_MAX_PLY
                    && pos.si->non_pawn_material (pos.active) > VALUE_ZERO)
                {
                    if (   !capture_or_promotion
                        && !gives_check
                            // Advance pawn push.
                        && !(   PAWN == ptype (pos[org_sq (move)])
                             && rel_rank (pos.active, org_sq (move)) > R_4
                             && pos.si->non_pawn_material () < VALUE_MG_QUEN*2))
                    {
                        // Move count based pruning.
                        if (move_count_pruning)
                        {
                            skip_quiets = true;
                            continue;
                        }

                        // Reduced depth of the next LMR search.
                        auto lmr_depth = i16(std::max (new_depth - reduction_depth (PVNode, improving, depth, move_count), 0));
                        if (    // Counter moves value based pruning.
                               (   3 > lmr_depth
                                && smh1[mpc][dst_sq (move)] < 0
                                && smh2[mpc][dst_sq (move)] < 0)
                                // Futility pruning: parent node.
                            || (   7 > lmr_depth
                                && !in_check
                                && ss->static_eval + 200*lmr_depth + 256 <= alfa)
                                // SEE based pruning
                            || (   8 > lmr_depth
                                && !pos.see_ge (move, -Value(35*lmr_depth*lmr_depth))))
                        {
                            continue;
                        }
                    }
                    else
                    // SEE based pruning.
                    if (   7 > depth
                        && new_depth < depth
                        && !pos.see_ge (move, -Value(i32(VALUE_EG_PAWN)*depth)))
                    {
                        continue;
                    }
                }

                if (   capture_or_promotion
                    && move == tt_move)
                {
                    ttm_capture = true;
                }

                // Speculative prefetch as early as possible.
                prefetch (TT.cluster_entry (pos.move_posi_key (move)));

                // Update the current move (this must be done after singular extension search).
                ss->current_move = move;
                ss->m_history = &th->cm_history[mpc][dst_sq (move)];

                // Step 14. Make the move.
                pos.do_move (move, si, gives_check);

                bool full_depth_search;
                // Step 15. Reduced depth search (LMR).
                // If the move fails high will be re-searched at full depth.
                if (   2 < depth
                    && 1 < move_count
                    && (   move_count_pruning
                        || !capture_or_promotion))
                {
                    auto reduce_depth = reduction_depth (PVNode, improving, depth, move_count);

                    if (capture_or_promotion)
                    {
                        reduce_depth -= 1;
                    }
                    else
                    {
                        assert(PROMOTE != mtype (move));

                        // Increase reduction if tt_move is a capture
                        if (ttm_capture)
                        {
                            reduce_depth += 1;
                        }

                        // Increase reduction for cut nodes
                        if (cut_node)
                        {
                            reduce_depth += 2;
                        }
                        else
                        // Decrease reduction for moves that escape a capture in no-cut nodes.
                        // Filter out castling moves, because they are coded as "king captures rook" and hence break mk_move().
                        if (   NORMAL == mtype (move)
                            && !pos.see_ge (mk_move<NORMAL> (dst_sq (move), org_sq (move))))
                        {
                            reduce_depth -= 2;
                        }

                        ss->statistics =
                              th->history[~pos.active][move_pp (move)]
                            + smh1[mpc][dst_sq (move)]
                            + smh2[mpc][dst_sq (move)]
                            + smh4[mpc][dst_sq (move)]
                            - 4000; // Correction factor

                        // Decrease/Increase reduction by comparing opponent's stat value
                        if (   (ss  )->statistics > 0
                            && (ss-1)->statistics < 0)
                        {
                            reduce_depth -= 1;
                        }
                        else
                        if (   (ss  )->statistics < 0
                            && (ss-1)->statistics > 0)
                        {
                            reduce_depth += 1;
                        }

                        // Decrease/Increase reduction for moves with +/-ve history value
                        reduce_depth -= i16((ss)->statistics / 20000);
                    }

                    if (reduce_depth < 0)
                    {
                        reduce_depth = 0;
                    }
                    else
                    if (reduce_depth > new_depth - 1)
                    {
                        reduce_depth = new_depth - 1;
                    }

                    value = -depth_search<false> (pos, ss+1, -alfa-1, -alfa, new_depth - reduce_depth, true, true);

                    full_depth_search = alfa < value
                                     && reduce_depth != 0;
                }
                else
                {
                    full_depth_search = !PVNode
                                     || 1 < move_count;
                }

                // Step 16. Full depth search when LMR is skipped or fails high.
                if (full_depth_search)
                {
                    value =
                        new_depth <= 0 ?
                            -quien_search<false> (pos, ss+1, -alfa-1, -alfa) :
                            -depth_search<false> (pos, ss+1, -alfa-1, -alfa, new_depth, !cut_node, true);
                }

                // Do a full PV search on:
                // - 'full depth move count' move
                // - 'fail high' move (search only if value < beta)
                // otherwise let the parent node fail low with alfa >= value and try another move.
                if (   PVNode
                    && (   1 == move_count
                        || (   alfa < value
                            && (   root_node
                                || value < beta))))
                {
                    (ss+1)->pv.clear ();

                    value =
                        new_depth <= 0 ?
                            -quien_search<true> (pos, ss+1, -beta, -alfa) :
                            -depth_search<true> (pos, ss+1, -beta, -alfa, new_depth, false, true);
                }

                // Step 17. Undo move.
                pos.undo_move (move);

                assert(-VALUE_INFINITE < value && value < +VALUE_INFINITE);

                // Step 18. Check for the new best move.
                // Finished searching the move. If a stop or a cutoff occurred,
                // the return value of the search cannot be trusted,
                // and return immediately without updating best move, PV and TT.
                if (Threadpool.force_stop.load (memory_order_relaxed))
                {
                    return VALUE_ZERO;
                }

                if (root_node)
                {
                    auto &root_move = *std::find (th->root_moves.begin (), th->root_moves.end (), move);
                    // First PV move or new best move?
                    if (   1 == move_count
                        || alfa < value)
                    {
                        root_move.resize (1);
                        for (auto m : (ss+1)->pv)
                        {
                            root_move += m;
                        }
                        root_move.new_value = value;

                        // Record how often the best move has been changed in each iteration.
                        // This information is used for time management:
                        // When the best move changes frequently, allocate some more time.
                        if (   1 < move_count
                            && Limits.use_time_management ()
                            && Threadpool.main_thread () == th)
                        {
                            ++Threadpool.main_thread ()->best_move_change;
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

                // Step 19. Check best value.
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
                        // Fail high
                        if (value >= beta)
                        {
                            break;
                        }
                        assert(value < beta);

                        // Update alfa! Always alfa < beta.
                        if (PVNode)
                        {
                            alfa = value;
                        }
                    }
                }

                if (   !capture_or_promotion
                    && move != best_move)
                {
                    quiet_moves.push_back (move);
                }
            }

            assert(!in_check
                || 0 != move_count
                || exclusion
                || 0 == MoveList<GenType::LEGAL> (pos).size ());

            // Step 20. Check for checkmate and stalemate.
            // If all possible moves have been searched and if there are no legal moves,
            // If in a singular extension search then return a fail low score (alfa).
            // Otherwise it must be a checkmate or a stalemate, so return value accordingly.
            if (0 == move_count)
            {
                best_value =
                    exclusion ?
                        alfa :
                        in_check ?
                            mated_in (ss->ply) :
                            DrawValue[pos.active];
            }
            else
            {
                auto m = (ss-1)->current_move;
                // Quiet best move: update move sorting heuristics.
                if (MOVE_NONE != best_move)
                {
                    if (!pos.capture_or_promotion (best_move))
                    {
                        auto bonus = stat_bonus (depth);
                        update_stats (ss, pos, best_move, bonus);
                        // Decrease all the other played quiet moves.
                        for (auto qm : quiet_moves)
                        {
                            pos.thread->history.update (pos.active, qm, -bonus);
                            update_cm_stats (ss, pos[org_sq (qm)], dst_sq (qm), -bonus);
                        }
                    }
                    // Penalty for a quiet best move in previous ply when it gets refuted.
                    if (   1 == (ss-1)->move_count
                        && _ok (m)
                        && NONE == pos.si->capture
                        && !pos.si->promotion)
                    {
                        update_cm_stats (ss-1, pos[fix_dst_sq (m)], dst_sq (m), -stat_bonus (depth + 1));
                    }
                }
                else
                // Bonus for prior countermove that caused the fail low.
                if (   2 < depth
                    && _ok (m)
                    && NONE == pos.si->capture
                    && !pos.si->promotion)
                {
                    update_cm_stats (ss-1, pos[fix_dst_sq (m)], dst_sq (m), stat_bonus (depth));
                }
            }
            
            if (!exclusion)
            {
                tte->save (pos.si->posi_key,
                           best_move,
                           value_to_tt (best_value, ss->ply),
                           ss->static_eval,
                           depth,
                           best_value >= beta ?
                               BOUND_LOWER :
                                  PVNode
                               && MOVE_NONE != best_move ?
                                   BOUND_EXACT :
                                   BOUND_UPPER);
            }
            assert(-VALUE_INFINITE < best_value && best_value < +VALUE_INFINITE);
            return best_value;
        }
    }

    // Utility to verify move generation.
    // All the leaf nodes up to the given depth are generated, and the sum is returned.
    template<bool RootNode>
    u64 perft (Position &pos, i16 depth)
    {
        u64 leaf_nodes = 0;
        i16 move_count = 0;
        for (const auto &vm : MoveList<GenType::LEGAL> (pos))
        {
            u64 cum_nodes;
            if (   RootNode
                && 1 >= depth)
            {
                cum_nodes = 1;
            }
            else
            {
                StateInfo si;
                pos.do_move (vm.move, si);
                cum_nodes =
                    2 < depth ?
                        perft<false> (pos, depth - 1) :
                        MoveList<GenType::LEGAL> (pos).size ();
                pos.undo_move (vm.move);
            }

            if (RootNode)
            {
                sync_cout
                    << std::right
                    << std::setfill ('0')
                    << std::setw (2)
                    << ++move_count
                    << ' '
                    << std::left
                    << std::setfill (' ')
                    << std::setw (7)
                    << 
                       //move_to_can (vm.move)
                       move_to_san (vm.move, pos)
                    << std::right
                    << std::setfill ('.')
                    << std::setw (16)
                    << cum_nodes
                    << std::setfill (' ')
                    << std::left
                    << sync_endl;
            }

            leaf_nodes += cum_nodes;
        }
        return leaf_nodes;
    }
    // Explicit template instantiations
    template u64 perft<false> (Position&, i16);
    template u64 perft<true > (Position&, i16);

    // Initialize lookup tables during startup.
    void initialize ()
    {
        for (i16 d = 0; d < MaxFutilityDepth; ++d)
        {
            FutilityMoveCounts[0][d] = u08(0.74 * std::pow (d, 1.78) + 2.4);
            FutilityMoveCounts[1][d] = u08(1.00 * std::pow (d, 2.00) + 5.0);
        }
        for (u08 imp = 0; imp < 2; ++imp)
        {
            for (i16 d = 1; d < MaxReductionDepth; ++d)
            {
                for (u08 mc = 1; mc < MaxReductionMoveCount; ++mc)
                {
                    auto r = log (d) * log (mc) / 1.95;
                    ReductionDepths[0][imp][d][mc] = i16(std::round (r));
                    ReductionDepths[1][imp][d][mc] = i16(std::max (ReductionDepths[0][imp][d][mc] - 1, 0));
                    if (   0 == imp
                        && ReductionDepths[0][imp][d][mc] >= 2)
                    {
                        ReductionDepths[0][imp][d][mc] += 1;
                    }
                }
            }
        }
    }
    // Resets search state to its initial value, to obtain reproducible results.
    void clear ()
    {
        TT.clear ();
        Threadpool.clear ();
    }
}

namespace Threading {

    using namespace Searcher;

    const u08 SkipIndex = 20;
    const u08 SkipSize [SkipIndex] = { 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4 };
    const u08 SkipPhase[SkipIndex] = { 0, 1, 0, 1, 2, 3, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 6, 7 };

    // Thread iterative deepening loop function.
    // It calls depth_search() repeatedly with increasing depth until
    // - the force stop requested.
    // - the allocated thinking time has been consumed.
    // - the maximum search depth is reached.
    void Thread::search ()
    {
        Stack stacks[MaxPlies + 7]; // To allow referencing (ss-4) and (ss+2)
        for (auto s = stacks; s < stacks + MaxPlies + 7; ++s)
        {
            s->ply = i16(s - stacks - 3);
            s->current_move = MOVE_NONE;
            std::fill_n (s->killer_moves, MaxKillers, MOVE_NONE);
            s->static_eval = VALUE_ZERO;
            s->statistics = 0;
            s->move_count = 0;
            s->m_history = &this->cm_history[NO_PIECE][0];
        }

        auto *main_thread = Threadpool.main_thread ();

        auto best_value = VALUE_ZERO
           , window = VALUE_ZERO
           , alfa = -VALUE_INFINITE
           , beta = +VALUE_INFINITE;

        // Iterative deepening loop until requested to stop or the target depth is reached.
        while (   ++running_depth < MaxPlies
               && !Threadpool.force_stop
               && (   0 == Limits.depth
                   || main_thread != this
                   || running_depth <= Limits.depth))
        {
            if (main_thread == this)
            {
                assert(main_thread == this);
                if (Limits.use_time_management ())
                {
                    main_thread->failed_low = false;
                    // Age out PV variability metric
                    main_thread->best_move_change *= 0.505;
                }
            }
            else
            {
                // Distribute search depths across the threads.
                assert(0 != index);
                int i = (index - 1) % SkipIndex;
                if (0 != ((running_depth + root_pos.ply + SkipPhase[i]) / SkipSize[i]) % 2)
                {
                    continue;
                }
            }

            // Save the last iteration's values before first PV line is searched.
            for (auto &rm : root_moves)
            {
                rm.old_value = rm.new_value;
            }

            // MultiPV loop. Perform a full root search for each PV line.
            for (   pv_index = 0;
                    !Threadpool.force_stop
                 && pv_index < Threadpool.pv_limit;
                    ++pv_index)
            {
                // Reset aspiration window starting size.
                if (running_depth > 4)
                {
                    window = Value(18);
                    alfa = std::max (root_moves[pv_index].old_value - window, -VALUE_INFINITE);
                    beta = std::min (root_moves[pv_index].old_value + window, +VALUE_INFINITE);
                }

                // Start with a small aspiration window and, in case of fail high/low,
                // research with bigger window until not failing high/low anymore.
                while (true)
                {
                    best_value = depth_search<true> (root_pos, stacks+4, alfa, beta, running_depth, false, true);

                    // Bring the best move to the front. It is critical that sorting is
                    // done with a stable algorithm because all the values but the first
                    // and eventually the new best one are set to -VALUE_INFINITE and
                    // want to keep the same order for all the moves but the new PV
                    // that goes to the front. Note that in case of MultiPV search
                    // the already searched PV lines are preserved.
                    std::stable_sort (root_moves.begin () + pv_index, root_moves.end ());

                    // If search has been stopped, break immediately.
                    // Sorting and writing PV back to TT is safe becuase
                    // root moves is still valid, although refers to the previous iteration.
                    if (Threadpool.force_stop)
                    {
                        break;
                    }

                    if (main_thread == this)
                    {
                        // Give some update before to re-search.
                        if (   1 == Threadpool.pv_limit
                            && (best_value <= alfa || beta <= best_value)
                            && main_thread->time_mgr.elapsed_time () > 3000)
                        {
                            sync_cout << multipv_info (this, running_depth, alfa, beta) << sync_endl;
                        }
                    }
                    // If failing low/high set new bounds, otherwise exit the loop.
                    if (best_value <= alfa)
                    {
                        beta = (alfa + beta) / 2;
                        alfa = std::max (best_value - window, -VALUE_INFINITE);

                        if (main_thread == this)
                        {
                            if (Limits.use_time_management ())
                            {
                                main_thread->failed_low = true;
                            }
                            Threadpool.ponderhit_stop = false;
                        }
                    }
                    else
                    if (beta <= best_value)
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

                // Sort the PV lines searched so far and update the GUI.
                std::stable_sort (root_moves.begin (), root_moves.begin () + pv_index + 1);

                if (main_thread == this)
                {
                    if (   Threadpool.force_stop
                        || Threadpool.pv_limit == pv_index + 1
                        || main_thread->time_mgr.elapsed_time () > 3000)
                    {
                        sync_cout << multipv_info (this, running_depth, alfa, beta) << sync_endl;
                    }
                }
            }

            if (!Threadpool.force_stop)
            {
                finished_depth = running_depth;
            }

            //if (0 != ContemptValue)
            //{
            //    auto valued_contempt = Value(i32(root_moves[0].new_value)/ContemptValue);
            //    DrawValue[ root_pos.active] = BaseContempt[ root_pos.active] - valued_contempt;
            //    DrawValue[~root_pos.active] = BaseContempt[~root_pos.active] + valued_contempt;
            //}

            if (main_thread == this)
            {
                // If skill level is enabled and can pick move, pick a sub-optimal best move.
                if (   main_thread->skill_mgr.enabled ()
                    && main_thread->skill_mgr.can_pick (running_depth))
                {
                    main_thread->skill_mgr.clear ();
                    main_thread->skill_mgr.pick_best_move (main_thread->root_moves);
                }

                if (OutputStream.is_open ())
                {
                    OutputStream << pretty_pv_info (this) << std::endl;
                }

                if (   !Threadpool.force_stop
                    && !Threadpool.ponderhit_stop)
                {
                    // Stop the search early:
                    bool stop = false;

                    // Have time for the next iteration? Can stop searching now?
                    if (Limits.use_time_management ())
                    {
                        auto &root_move = root_moves[0];
                        // Stop the search
                        // -If there is only one legal move available
                        // -If all of the available time has been used
                        // -If matched an easy move from the previous search and just did a fast verification.
                        if (   1 == root_moves.size ()
                            || (  main_thread->time_mgr.elapsed_time () >
                                  main_thread->time_mgr.optimum_time
                                        // Unstable factor
                                        * (1.0 + main_thread->best_move_change)
                                        // Improving factor
                                        * std::min (1.1385,
                                          std::max (0.3646,
                                                    0.5685
                                                  + 0.1895 * (main_thread->failed_low ? 1 : 0)
                                                  - 0.0096 * (VALUE_NONE != main_thread->last_value ? best_value - main_thread->last_value : 0))))
                            || (main_thread->easy_played =
                                    (   root_move == main_thread->easy_move
                                     && main_thread->best_move_change < 0.030
                                     && main_thread->time_mgr.elapsed_time () >
                                        main_thread->time_mgr.optimum_time * 0.1136), main_thread->easy_played))
                        {
                            stop = true;
                        }

                        if (3 <= root_move.size ())
                        {
                            main_thread->move_mgr.update (root_pos, root_move);
                        }
                        else
                        {
                            main_thread->move_mgr.clear ();
                        }
                    }
                    else
                    // Have found a "mate in <x>"?
                    if (   0 != Limits.mate
                        && best_value >= +VALUE_MATE - 2*Limits.mate)
                    {
                        stop = true;
                    }

                    if (stop)
                    {
                        // If allowed to ponder do not stop the search now but
                        // keep pondering until GUI sends "ponderhit" or "stop".
                        if (Limits.ponder)
                        {
                            Threadpool.ponderhit_stop = true;
                        }
                        else
                        {
                            Threadpool.force_stop = true;
                        }
                    }
                }
            }
        }
    }
    // Main thread function.
    // It searches from root position and outputs the "bestmove"/"ponder".
    void MainThread::search ()
    {
        static Book book; // Defined static to initialize the PRNG only once
        assert(Threadpool.main_thread () == this
            && index == 0);

        check_count = 0;

        if (!white_spaces (OutputFile))
        {
            OutputStream.open (OutputFile, ios_base::out|ios_base::app);
            if (OutputStream.is_open ())
            {
                OutputStream
                    << std::boolalpha
                    << "RootPos  : " << root_pos.fen ()                    << '\n'
                    << "RootSize : " << root_moves.size ()                 << '\n'
                    << "Infinite : " << Limits.infinite                    << '\n'
                    << "Ponder   : " << Limits.ponder                      << '\n'
                    << "ClockTime: " << Limits.clock[root_pos.active].time << '\n'
                    << "Increment: " << Limits.clock[root_pos.active].inc  << '\n'
                    << "MoveTime : " << Limits.movetime                    << '\n'
                    << "MovesToGo: " << u16(Limits.movestogo)              << '\n'
                    << "Depth    : " << Limits.depth                       << '\n'
                    << " Depth Score    Time       Nodes  PV\n"
                    << "-----------------------------------------------------------"
                    << std::noboolalpha << std::endl;
            }
        }

        // When playing in 'Nodes as Time' mode, then convert from time to nodes, and use values in time management.
        // WARNING: Given NodesTime (nodes per millisecond) must be much lower then the real engine speed to avoid time losses.
        if (0 != NodesTime)
        {
            // Only once at game start
            if (0 == time_mgr.available_nodes)
            {
                time_mgr.available_nodes = NodesTime * Limits.clock[root_pos.active].time;
            }
            // Convert from millisecs to nodes
            Limits.clock[root_pos.active].time = time_mgr.available_nodes;
            Limits.clock[root_pos.active].inc *= NodesTime;
            Limits.clear_nontime ();
        }

        if (Limits.use_time_management ())
        {
            // Initialize the time manager before searching.
            time_mgr.initialize (root_pos.active, root_pos.ply);
        }

        Transposition::Entry::Generation = u08(((root_pos.ply + 1) << 2) & 0xFC);

        bool voting = false;

        if (root_moves.empty ())
        {
            root_moves += RootMove ();

            sync_cout
                << "info"
                << " depth " << 0
                << " score " << to_string (0 != root_pos.si->checkers ? -VALUE_MATE : VALUE_DRAW)
                << " time "  << 0
                << sync_endl;
        }
        else
        {
            // Check if can play with own book.
            if (   OwnBook
                && !white_spaces (BookFile)
                && (   0 == BookUptoMove
                    || root_pos.move_num () <= BookUptoMove)
                && 0 == Limits.mate
                && !Limits.infinite)
            {
                book.open (BookFile, ios_base::in);
                bool found = false;
                auto book_best_move = book.probe_move (root_pos, BookMoveBest);
                if (MOVE_NONE != book_best_move)
                {
                    auto itr = std::find (root_moves.begin (), root_moves.end (), book_best_move);
                    if (itr != root_moves.end ())
                    {
                        auto &root_move = root_moves[0];
                        std::swap (root_move, *itr);
                        StateInfo si;
                        root_pos.do_move (book_best_move, si);
                        auto book_ponder_move = book.probe_move (root_pos, BookMoveBest);
                        root_move += book_ponder_move;
                        root_pos.undo_move (book_best_move);
                        found = true;
                    }
                }
                book.close ();
                if (found)
                {
                    goto finish;
                }
            }

            i16 timed_contempt = 0;
            i64 diff_time;
            if (   Limits.use_time_management ()
                && 0 != ContemptTime
                && 0 != (diff_time = i64(  Limits.clock[ root_pos.active].time
                                         - Limits.clock[~root_pos.active].time)/1000))
            {
                timed_contempt = i16(diff_time/ContemptTime);
            }

            auto contempt = cp_to_value ((FixedContempt + timed_contempt) / 100.0);
            DrawValue[ root_pos.active] = BaseContempt[ root_pos.active] = VALUE_DRAW - contempt;
            DrawValue[~root_pos.active] = BaseContempt[~root_pos.active] = VALUE_DRAW + contempt;

            if (Limits.use_time_management ())
            {
                easy_move = move_mgr.easy_move (root_pos.si->posi_key);
                move_mgr.clear ();
                easy_played = false;
                failed_low = false;
                best_move_change = 0.0;
            }
            if (skill_mgr.enabled ())
            {
                skill_mgr.clear ();
            }

            // Have to play with skill handicap?
            // In this case enable MultiPV search by skill pv size
            // that will use behind the scenes to get a set of possible moves.
            Threadpool.pv_limit = std::min (std::max (MultiPV, u16(skill_mgr.enabled () ? 4 : 1)), u16(root_moves.size ()));

            voting = true;

            for (auto *th : Threadpool)
            {
                if (th != this)
                {
                    th->start_searching (false);
                }
            }

            Thread::search (); // Let's start searching !

            // Clear any candidate easy move
            if (   Limits.use_time_management ()
                && (// Prevents consecutive fast moves
                       easy_played
                    // Unstable for the last search iterations
                    || move_mgr.stable_count < 6))
            {
                move_mgr.clear ();
            }
            // Swap best PV line with the sub-optimal one if skill level is enabled
            if (skill_mgr.enabled ())
            {
                skill_mgr.pick_best_move (root_moves);
                if (MOVE_NONE != skill_mgr.best_move)
                {
                    auto itr = std::find (root_moves.begin (), root_moves.end (), skill_mgr.best_move);
                    if (itr != root_moves.end ())
                    {
                        std::swap (root_moves[0], *itr);
                    }
                }
            }
        }

    finish:

        // When playing in 'Nodes as Time' mode, update the time manager after searching.
        if (0 != NodesTime)
        {
            time_mgr.available_nodes += Limits.clock[root_pos.active].inc - Threadpool.nodes ();
        }

        // When reach max depth arrive here even without Force Stop is raised,
        // but if are pondering or in infinite search, according to UCI protocol,
        // shouldn't print the best move before the GUI sends a "stop" or "ponderhit" command.
        // Simply wait here until GUI sends one of those commands (that raise Force Stop).
        if (   !Threadpool.force_stop
            && (   Limits.infinite
                || Limits.ponder))
        {
            Threadpool.ponderhit_stop = true;
            wait_until (Threadpool.force_stop);
        }

        Thread *best_thread = this;
        if (voting)
        {
            // Stop the threads if not already stopped.
            Threadpool.force_stop = true;
            // Wait until all threads have finished.
            for (auto *th : Threadpool)
            {
                if (th != this)
                {
                    th->wait_while_searching ();
                }
            }
            // Check if there are deeper thread than main thread.
            if (   1 == Threadpool.pv_limit
                && !easy_played
                && 0 == Limits.depth // Depth limit search don't use deeper thread
                && MOVE_NONE != root_moves[0][0]
                && !skill_mgr.enabled ())
            {
                best_thread = Threadpool.best_thread ();
                // If best thread is not main thread send new PV.
                if (best_thread != this)
                {
                    sync_cout << multipv_info (best_thread, best_thread->finished_depth, -VALUE_INFINITE, +VALUE_INFINITE) << sync_endl;
                }
            }
        }

        assert(!best_thread->root_moves.empty ()
            && !best_thread->root_moves[0].empty ());

        auto &root_move = best_thread->root_moves[0];

        if (Limits.use_time_management ())
        {
            last_value = root_move.new_value;
        }

        auto best_move = root_move[0];
        auto ponder_move = MOVE_NONE != best_move
                        && (   root_move.size () > 1
                            || root_move.extract_ponder_move_from_tt (root_pos)) ?
                           root_move[1] : MOVE_NONE;

        if (OutputStream.is_open ())
        {
            auto total_nodes = Threadpool.nodes ();
            auto elapsed_time = std::max (time_mgr.elapsed_time (), 1LL);
            OutputStream
                << "Nodes (N)  : " << total_nodes                       << '\n'
                << "Time (ms)  : " << elapsed_time                      << '\n'
                << "Speed (N/s): " << total_nodes * 1000 / elapsed_time << '\n'
                << "Hash-full  : " << TT.hash_full ()                   << '\n'
                << "Best Move  : " << move_to_san (best_move, root_pos) << '\n';
            if (MOVE_NONE != best_move)
            {
                StateInfo si;
                root_pos.do_move (best_move, si);
                OutputStream
                    << "Ponder Move: " << move_to_san (ponder_move, root_pos) << '\n';
                root_pos.undo_move (best_move);
            }
            OutputStream << std::endl;
            OutputStream.close ();
        }

        // Best move could be MOVE_NONE when searching on a stalemate position.
        sync_cout << "bestmove " << move_to_can (best_move);
        if (MOVE_NONE != best_move)
        {
            std::cout << " ponder " << move_to_can (ponder_move);
        }
        std::cout << sync_endl;
    }

    // Used to detect when out of available limits and thus stop the search, also print debug info.
    void MainThread::check_limits ()
    {
        static TimePoint last_time = now ();

        if (--check_count >= 0)
        {
            return;
        }
        // At low node count increase the checking rate otherwise use a default value.
        check_count = i16(0 != Limits.nodes ? std::min (std::max (i32(std::round ((double) Limits.nodes / 0x1000)), 1), 0x1000) : 0x1000);
        assert(0 != check_count);

        auto elapsed_time = time_mgr.elapsed_time ();
        TimePoint tick = Limits.start_time + elapsed_time;

        if (last_time <= tick - 1000)
        {
            last_time = tick;
            
            dbg_print ();
        }

        // Do not stop until told so by the GUI.
        if (   Limits.infinite
            || Limits.ponder)
        {
            return;
        }

        if (   (   Limits.use_time_management ()
                && elapsed_time > time_mgr.maximum_time - 10) // 2*TimerResolution
            || (   0 != Limits.movetime
                && elapsed_time >= Limits.movetime)
            || (   0 != Limits.nodes
                && Threadpool.nodes () >= Limits.nodes))
        {
            Threadpool.force_stop = true;
        }
    }
}
