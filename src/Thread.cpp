#include "Thread.h"

#include <cfloat>
#include "UCI.h"

u08     MaximumMoveHorizon =   50; // Plan time management at most this many moves ahead, in num of moves.
u08     ReadyMoveHorizon   =   40; // Be prepared to always play at least this many moves, in num of moves.
u32     OverheadClockTime  =   60; // Attempt to keep at least this much time at clock, in milliseconds.
u32     OverheadMoveTime   =   30; // Attempt to keep at least this much time for each remaining move, in milliseconds.
u32     MinimumMoveTime    =   20; // No matter what, use at least this much time before doing the move, in milliseconds.
double  MoveSlowness       = 1.20; // Move Slowness, in %age.
u32     NodesTime          =    0; // 'Nodes as Time' mode
bool    Ponder             = true; // Whether or not the engine should analyze when it is the opponent's turn.

Threading::ThreadPool Threadpool; // Global ThreadPool

using namespace std;
using namespace Searcher;

namespace {

    enum TimeType : u08
    {
        TT_OPTIMUM,
        TT_MAXIMUM,
    };

    // move_importance() is a skew-logistic function based on naive statistical
    // analysis of "how many games are still undecided after 'n' half-moves".
    // Game is considered "undecided" as long as neither side has >275cp advantage.
    // Data was extracted from CCRL game database with some simple filtering criteria.
    double move_importance (i16 ply)
    {
        //                          PlyShift / PlyScale  SkewRate
        return pow ((1 + exp ((ply - 58.400) / 7.640)), -0.183) + DBL_MIN; // Ensure non-zero
    }

    template<TimeType TT>
    // remaining_time<>() calculate the time remaining
    TimePoint remaining_time (TimePoint time, u08 movestogo, i16 ply)
    {
        const auto  StepRatio = TT == TT_OPTIMUM ? 1.00 : 7.09; // When in trouble, can step over reserved time with this ratio
        const auto StealRatio = TT == TT_MAXIMUM ? 0.00 : 0.35; // However must not steal time from remaining moves over this ratio

        auto this_move_imp = move_importance (ply) * MoveSlowness;
        auto remain_move_imp = 0.0;
        for (u08 i = 1; i < movestogo; ++i)
        {
            remain_move_imp += move_importance (ply + 2 * i);
        }

        auto  step_time_ratio = (0.0           +   this_move_imp *  StepRatio) / (this_move_imp * StepRatio + remain_move_imp);
        auto steal_time_ratio = (this_move_imp + remain_move_imp * StealRatio) / (this_move_imp * 1.0       + remain_move_imp);

        return TimePoint(time * std::min (step_time_ratio, steal_time_ratio));
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
    // real engine speed to avoid time losses.
    if (NodesTime != 0)
    {
        // Only once at game start
        if (available_nodes == U64(0))
        {
            available_nodes = NodesTime * Limits.clock[c].time; // Time is in msec
        }
        // Convert from millisecs to nodes
        Limits.clock[c].time = available_nodes;
        Limits.clock[c].inc *= NodesTime;
    }

    _instability_factor = 1.0;

    _optimum_time =
    _maximum_time =
        std::max (Limits.clock[c].time, TimePoint(MinimumMoveTime));

    const auto MaxMovesToGo = Limits.movestogo != 0 ? std::min (Limits.movestogo, MaximumMoveHorizon) : MaximumMoveHorizon;
    // Calculate optimum time usage for different hypothetic "moves to go"-values and choose the
    // minimum of calculated search time values. Usually the greatest hyp_movestogo gives the minimum values.
    for (u08 hyp_movestogo = 1; hyp_movestogo <= MaxMovesToGo; ++hyp_movestogo)
    {
        // Calculate thinking time for hypothetic "moves to go"-value
        auto hyp_time = std::max (
            + Limits.clock[c].time
            + Limits.clock[c].inc * (hyp_movestogo-1)
            - OverheadClockTime
            - OverheadMoveTime * std::min (hyp_movestogo, ReadyMoveHorizon), TimePoint(0));

        auto opt_time = remaining_time<TT_OPTIMUM> (hyp_time, hyp_movestogo, ply) + MinimumMoveTime;
        auto max_time = remaining_time<TT_MAXIMUM> (hyp_time, hyp_movestogo, ply) + MinimumMoveTime;

        if (_optimum_time > opt_time)
        {
            _optimum_time = opt_time;
        }
        if (_maximum_time > max_time)
        {
            _maximum_time = max_time;
        }
    }

    if (Ponder)
    {
        _optimum_time += _optimum_time / 4;
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

namespace Threading {

    // Thread constructor launchs the thread and then wait until it goes to sleep in idle_loop().
    Thread::Thread ()
        : _alive (true)
        , _searching (true)
        , max_ply (0)
        , chk_count (0)
        , reset_check (false)
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

    // ThreadPool::game_nodes() counts total game nodes
    u64 ThreadPool::game_nodes () const
    {
        u64 nodes = U64(0);
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
    void ThreadPool::configure ()
    {
        size_t threads = i32(Options["Threads"]);
        assert(threads > 0);

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
    // ThreadPool::initialize() is called at startup to create and launch
    // requested threads, that will go immediately to sleep.
    // Cannot use a constructor becuase threadpool is a static object
    // and require a fully initialized engine.
    void ThreadPool::initialize ()
    {
        assert(empty ());
        push_back (new MainThread);
        configure ();
    }
    // ThreadPool::deinitialize() cleanly terminates the threads before the program exits
    // Cannot be done in destructor because threads must be terminated before freeing ThreadPool.
    void ThreadPool::deinitialize ()
    {
        assert(!empty ());
        while (!empty ())
        {
            delete back ();
            pop_back ();    // Get rid of stale pointer
        }
    }

    // ThreadPool::start_thinking() wakes up the main thread sleeping in
    // Thread::idle_loop() and starts a new search, then returns immediately.
    void ThreadPool::start_thinking (const Position &pos, const Limit &limits, StateStackPtr &states)
    {
        wait_while_thinking ();

        ForceStop       = false;
        PonderhitStop   = false;

        Limits = limits;
        main ()->root_pos = pos;
        main ()->root_moves.initialize (pos, limits.moves);
        if (states.get () != nullptr) // If don't set a new position, preserve current state
        {
            SetupStates = std::move (states); // Ownership transfer here
            assert(states.get () == nullptr);
        }

        main ()->start_searching (false);
    }
    // ThreadPool::wait_while_thinking() waits for the main thread while searching.
    void ThreadPool::wait_while_thinking ()
    {
        main ()->wait_while_searching ();
    }
}
