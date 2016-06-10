#include "Thread.h"

#include <cfloat>
#include "UCI.h"
#include "Searcher.h"
#include "TBsyzygy.h"

double MoveSlowness = 0.90; // Move Slowness, in %age.
u32    NodesTime    =    0; // 'Nodes as Time' mode
bool   Ponder       = true; // Whether or not the engine should analyze when it is the opponent's turn.

using namespace std;
using namespace UCI;
using namespace Searcher;
using namespace TBSyzygy;

namespace {

    const u08 MaximumMoveHorizon = 50; // Plan time management at most this many moves ahead, in num of moves.
    const u08 ReadyMoveHorizon   = 40; // Be prepared to always play at least this many moves, in num of moves.
    const u32 OverheadClockTime  = 60; // Attempt to keep at least this much time at clock, in milliseconds.
    const u32 OverheadMoveTime   = 30; // Attempt to keep at least this much time for each remaining move, in milliseconds.
    const u32 MinimumMoveTime    = 20; // No matter what, use at least this much time before doing the move, in milliseconds.

    // move_importance() is a skew-logistic function based on naive statistical
    // analysis of "how many games are still undecided after n half-moves".
    // Game is considered "undecided" as long as neither side has >275cp advantage.
    // Data was extracted from the CCRL game database with some simple filtering criteria.
    double move_importance (i16 ply)
    {
        //                                      PlyShift / PlyScale, PlySkewRate
        return std::max (pow (1.0 + exp ((ply - 58.400) / 7.640), -0.183), DBL_MIN); // Ensure non-zero
    }

    template<bool Maximum>
    // remaining_time<>() calculate the time remaining
    TimePoint remaining_time (TimePoint time, u08 movestogo, i16 ply)
    {
        // When in trouble, can step over reserved time with this ratio
        const auto  StepRatio = Maximum ? 7.09 : 1.00;
        // However must not steal time from remaining moves over this ratio
        const auto StealRatio = Maximum ? 0.35 : 0.00;

        auto  this_move_imp = std::max (move_importance (ply) * MoveSlowness, DBL_MIN);
        auto other_move_imp = 0.0;
        for (u08 i = 1; i < movestogo; ++i)
        {
            other_move_imp += move_importance (ply + 2 * i);
        }

        auto  step_time_ratio = (this_move_imp * StepRatio + other_move_imp * 0.00      ) / (this_move_imp * StepRatio + other_move_imp);
        auto steal_time_ratio = (this_move_imp * 1.00      + other_move_imp * StealRatio) / (this_move_imp * 1.00      + other_move_imp);

        return TimePoint(std::round (time * std::min (step_time_ratio, steal_time_ratio))); // Intel C++ asks for an explicit cast
    }
}

TimePoint TimeManager::elapsed_time () const
{
    return TimePoint(NodesTime != 0 ? Threadpool.game_nodes () : now () - Limits.start_time);
}
// TimeManager::initialize() is called at the beginning of the search.
// It calculates the allowed thinking time out of the time control and current game ply.
void TimeManager::initialize (Color c, i16 ply)
{
    // If we have to play in 'Nodes as Time' mode, then convert from time
    // to nodes, and use resulting values in time management formulas.
    // WARNING: Given npms (nodes per millisecond) must be much lower then
    // the real engine speed to avoid time losses.
    if (NodesTime != 0)
    {
        // Only once at game start
        if (available_nodes == 0)
        {
            available_nodes = NodesTime * Limits.clock[c].time; // Time is in msec
        }
        // Convert from millisecs to nodes
        Limits.clock[c].time = available_nodes;
        Limits.clock[c].inc *= NodesTime;
    }

    optimum_time =
    maximum_time =
        std::max (Limits.clock[c].time, TimePoint(MinimumMoveTime));

    const auto MaxMovesToGo = Limits.movestogo != 0 ? std::min (Limits.movestogo, MaximumMoveHorizon) : MaximumMoveHorizon;
    // Calculate optimum time usage for different hypothetic "moves to go" and choose the
    // minimum of calculated search time values. Usually the greatest hyp_movestogo gives the minimum values.
    for (u08 hyp_movestogo = 1; hyp_movestogo <= MaxMovesToGo; ++hyp_movestogo)
    {
        // Calculate thinking time for hypothetic "moves to go"
        auto hyp_time = std::max (
            + Limits.clock[c].time
            + Limits.clock[c].inc * (hyp_movestogo-1)
            - OverheadClockTime
            - OverheadMoveTime * std::min (hyp_movestogo, ReadyMoveHorizon), TimePoint(0));

        optimum_time = std::min (remaining_time<false> (hyp_time, hyp_movestogo, ply) + MinimumMoveTime, optimum_time);
        maximum_time = std::min (remaining_time<true > (hyp_time, hyp_movestogo, ply) + MinimumMoveTime, maximum_time);
    }

    if (Ponder)
    {
        optimum_time = TimePoint(optimum_time * 1.25);
    }
    // Make sure that optimum time is not over maximum time
    if (optimum_time > maximum_time)
    {
        optimum_time = maximum_time;
    }
}
// TimeManager::update() is called at the end of the search.
// It updates the allowed thinking time.
void TimeManager::update (Color c)
{
    // When playing in 'Nodes as Time' mode,
    // subtract the searched nodes from the available ones.
    if (NodesTime != 0)
    {
        available_nodes += Limits.clock[c].inc - Threadpool.game_nodes ();
    }
}

// ------------------------------------

// When playing with a strength handicap, choose best move among the first 'candidates'
// RootMoves using a statistical rule dependent on 'level'. Idea by Heinz van Saanen.
Move SkillManager::pick_best_move (u16 pv_limit)
{
    const auto &root_moves = Threadpool.main ()->root_moves;
    assert(!root_moves.empty ());
    static PRNG prng (now ()); // PRNG sequence should be non-deterministic

    if (_best_move == MOVE_NONE)
    {
        // RootMoves are already sorted by value in descending order
        auto weakness   = Value(MaxPlies - 4 * _skill_level);
        auto max_value  = root_moves[0].new_value;
        auto diversity  = std::min (max_value - root_moves[pv_limit - 1].new_value, VALUE_MG_PAWN);
        auto best_value = -VALUE_INFINITE;
        // Choose best move. For each move score add two terms, both dependent on weakness.
        // One is deterministic with weakness, and one is random with diversity.
        // Then choose the move with the resulting highest value.
        for (u16 i = 0; i < pv_limit; ++i)
        {
            auto value = root_moves[i].new_value
                        // This is magic formula for push
                       + (  weakness  * i32(max_value - root_moves[i].new_value)
                          + diversity * i32(prng.rand<u32> () % weakness)) / 128;

            if (best_value < value)
            {
                best_value = value;
                _best_move = root_moves[i][0];
            }
        }
    }
    return _best_move;
}

namespace Threading {

    // Thread constructor launches the thread and then waits until it goes to sleep in idle_loop().
    Thread::Thread ()
        : _alive (true)
        , _searching (true)
        , max_ply (0)
        , count (0)
        , reset_count (false)
    {
        index = u16(Threadpool.size ()); // Starts from 0
        history_values.clear ();
        counter_moves.clear ();

        std::unique_lock<Mutex> lk (_mutex);
        _native_thread = std::thread (&Thread::idle_loop, this);
        _sleep_condition.wait (lk, [&] { return !_searching; });
        lk.unlock ();
    }
    // Thread destructor waits for thread termination before returning.
    Thread::~Thread ()
    {
        _alive = false;
        std::unique_lock<Mutex> lk (_mutex);
        _sleep_condition.notify_one ();
        lk.unlock ();
        _native_thread.join ();
    }

    // ------------------------------------

    MainThread::MainThread ()
        : easy_played (false)
        , failed_low (false)
        , best_move_change (0.0)
        , previous_value (+VALUE_NONE)
    {}

    // ------------------------------------

    // ThreadPool::game_nodes() returns the total game nodes searched
    u64 ThreadPool::game_nodes () const
    {
        u64 nodes = 0;
        for (const auto *th : *this)
        {
            nodes += th->root_pos.game_nodes ();
        }
        return nodes;
    }
    // ThreadPool::configure() updates internal threads parameters from the corresponding
    // UCI options and creates/destroys threads to match the requested number.
    // Thread objects are dynamically allocated to avoid creating in advance all possible
    // threads, with included pawns and material tables, if only few are used.
    void ThreadPool::configure (size_t threads)
    {
        if (threads == 0)
        {
            threads = thread::hardware_concurrency ();
        }

        while (size () < threads)
        {
            push_back (new Thread);
        }
        while (size () > threads)
        {
            delete back ();
            pop_back ();
        }
        shrink_to_fit ();
        sync_cout << "info string Thread(s) used " << threads << sync_endl;
    }
    // ThreadPool::initialize() creates and launches requested threads, that will go immediately to sleep.
    // Cannot use a constructor becuase threadpool is a static object and require a fully initialized engine.
    void ThreadPool::initialize ()
    {
        assert(empty ());
        push_back (new MainThread);
        configure (i32(Options["Threads"]));
    }
    // ThreadPool::deinitialize() cleanly terminates the threads before the program exits.
    // Cannot be done in destructor because threads must be terminated before deleting any
    // static objects related to search while still in main().
    void ThreadPool::deinitialize ()
    {
        ForceStop = true;
        wait_while_thinking ();
        assert(!empty ());
        while (!empty ())
        {
            delete back ();
            // Get rid of stale pointer
            pop_back ();
        }
    }

    // ThreadPool::start_thinking() wakes up the main thread sleeping in Thread::idle_loop()
    // and starts a new search, then returns immediately.
    void ThreadPool::start_thinking (Position &root_pos, StateListPtr &states, const Limit &limits)
    {
        wait_while_thinking ();

        // After ownership transfer 'states' becomes empty, so if we stop the search
        // and call 'go' again without setting a new position states.get() == NULL.
        assert(states.get () != nullptr || setup_states.get () != nullptr);

        if (states.get () != nullptr)
        {
            // Ownership transfer, states is now empty
            setup_states = std::move (states);
            assert(states.get () == nullptr);
        }

        Limits  = limits;

        RootMoveVector root_moves;
        root_moves.initialize (root_pos, limits.search_moves);

        TBDepthLimit = i32(Options["SyzygyDepthLimit"])*DEPTH_ONE;
        TBPieceLimit = i32(Options["SyzygyPieceLimit"]);
        TBUseRule50  = bool(Options["SyzygyUseRule50"]);
        TBHits       = 0;
        TBHasRoot    = false;

        // Skip TB probing when no TB found: !MaxPieceLimit -> !TB::PieceLimit
        if (TBPieceLimit > MaxPieceLimit)
        {
            TBPieceLimit = MaxPieceLimit;
            TBDepthLimit = DEPTH_ZERO;
        }

        // Filter root moves
        if (   TBPieceLimit >= root_pos.count<NONE> ()
            && root_pos.can_castle (CR_ANY) == CR_NONE)
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
                    && ProbeValue <= VALUE_DRAW)
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

        const auto tmp_si = setup_states->back ();
        for (auto *th : *this)
        {
            th->max_ply = 0;
            th->root_depth = DEPTH_ZERO;
            th->root_pos.setup (root_pos.fen (Chess960), setup_states->back (), th, Chess960);
            th->root_moves = root_moves;
        }
        // Restore si->ptr, cleared by Position::setup()
        setup_states->back () = tmp_si;

        ForceStop       = false;
        PonderhitStop   = false;
        main ()->start_searching (false);
    }
    // ThreadPool::wait_while_thinking() waits for the main thread while searching.
    void ThreadPool::wait_while_thinking ()
    {
        main ()->wait_while_searching ();
    }
}

Threading::ThreadPool Threadpool;