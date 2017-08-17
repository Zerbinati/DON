#include "Thread.h"

#include <cfloat>
#include "Option.h"
#include "Searcher.h"
#include "TBsyzygy.h"

using namespace std;
using namespace Searcher;
using namespace TBSyzygy;

u32 OverheadMoveTime = 30;  // Attempt to keep at least this much time for each remaining move, in milli-seconds.
u32 MinimumMoveTime =  20;  // No matter what, use at least this much time before doing the move, in milli-seconds.

double MoveSlowness = 0.89; // Move Slowness, in %age.
u32    NodesTime =    0;    // 'Nodes as Time' mode.
bool   Ponder =       true; // Whether or not the engine should analyze when it is the opponent's turn.

Threading::ThreadPool Threadpool;

namespace {

    u08 MaximumMoveHorizon =  50;  // Plan time management at most this many moves ahead, in num of moves.
    u08 ReadyMoveHorizon =    40;  // Be prepared to always play at least this many moves, in num of moves.
    u32 OverheadClockTime =   60;  // Attempt to keep at least this much time at clock, in milli-seconds.

    /// Skew-logistic function based on naive statistical analysis of
    /// "how many games are still undecided after n half-moves".
    /// Game is considered "undecided" as long as neither side has >275cp advantage.
    /// Data was extracted from the CCRL game database with some simple filtering criteria.
    double move_importance (i16 ply)
    {
        //                                       Shift    Scale    Skew
        return std::pow (1.0 + std::exp ((ply - 58.400) / 7.640), -0.183) + DBL_MIN; // Ensure non-zero
    }

    template<bool Optimum>
    u64 remaining_time (u64 time, u08 movestogo, i16 ply)
    {
        const auto  StepRatio = Optimum ? 1.00 : 7.09; // When in trouble, can step over reserved time with this ratio
        const auto StealRatio = Optimum ? 0.00 : 0.35; // However must not steal time from remaining moves over this ratio

        auto move_imp1 = move_importance (ply) * MoveSlowness;
        auto move_imp2 = 0.0;
        for (u08 i = 1; i < movestogo; ++i)
        {
            move_imp2 += move_importance (ply + 2 * i);
        }

        auto time_ratio1 = (move_imp1 * StepRatio + move_imp2 * 0.00      ) / (move_imp1 * StepRatio + move_imp2 * 1.00);
        auto time_ratio2 = (move_imp1 * 1.00      + move_imp2 * StealRatio) / (move_imp1 * 1.00      + move_imp2 * 1.00);

        return u64(time * std::min (time_ratio1, time_ratio2));
    }
}

/// TimeManager::elapsed_time()
u64 TimeManager::elapsed_time () const
{
    return 0 != NodesTime ?
            Threadpool.nodes () :
            now () - Limits.start_time;
}
/// TimeManager::initialize() calculates the allowed thinking time out of the time control and current game ply.
/// Support four different kind of time controls, passed in 'limits':
///
/// moves_to_go = 0, increment = 0 means: x basetime                             ['sudden death' time control]
/// moves_to_go = 0, increment > 0 means: x basetime + z increment
/// moves_to_go > 0, increment = 0 means: x moves in y basetime                  ['standard' time control]
/// moves_to_go > 0, increment > 0 means: x moves in y basetime + z increment
void TimeManager::initialize (Color c, i16 ply)
{
    optimum_time =
    maximum_time =
        std::max (Limits.clock[c].time, u64(MinimumMoveTime));

    const auto MaxMovesToGo =
        0 == Limits.movestogo ?
            MaximumMoveHorizon :
            std::min (Limits.movestogo, MaximumMoveHorizon);
    // Calculate optimum time usage for different hypothetic "moves to go" and choose the
    // minimum of calculated search time values. Usually the greatest hyp_movestogo gives the minimum values.
    for (u08 hyp_movestogo = 1; hyp_movestogo <= MaxMovesToGo; ++hyp_movestogo)
    {
        // Calculate thinking time for hypothetic "moves to go"
        auto hyp_time = std::max (
                        + Limits.clock[c].time
                        + Limits.clock[c].inc * (hyp_movestogo-1)
                        - OverheadClockTime
                        - OverheadMoveTime * std::min (hyp_movestogo, ReadyMoveHorizon), 0ULL);

        optimum_time = std::min (remaining_time<true > (hyp_time, hyp_movestogo, ply) + MinimumMoveTime, optimum_time);
        maximum_time = std::min (remaining_time<false> (hyp_time, hyp_movestogo, ply) + MinimumMoveTime, maximum_time);
    }

    if (Ponder)
    {
        optimum_time += optimum_time / 4;
    }
    //// Make sure that optimum time is not over maximum time
    //if (optimum_time > maximum_time)
    //{
    //    optimum_time = maximum_time;
    //}
}

void MoveManager::update (Position &pos, const Moves &new_pv)
{
    assert (new_pv.size () >= 3);

    if (new_pv[2] == pv[2])
    {
        ++stable_count;
    }
    else
    {
        stable_count = 0;
    }

    if (!std::equal (new_pv.begin (), new_pv.begin () + 3, pv))
    {
        std::copy (new_pv.begin (), new_pv.begin () + 3, pv);

        // Update expected posi key
        u08 ply = 0;
        StateInfo si[2];
        do
        {
            pos.do_move (pv[ply], si[ply]);
        }
        while (2 > ++ply);

        exp_posi_key = pos.si->posi_key;

        while (0 != ply)
        {
            pos.undo_move (pv[--ply]);
        }
    }
}

/// SkillManager::pick_best_move() chooses best move among a set of RootMoves when playing with a strength handicap,
/// using a statistical rule dependent on 'level'. Idea by Heinz van Saanen.
void SkillManager::pick_best_move (const RootMoves &root_moves)
{
    static PRNG prng (now ()); // PRNG sequence should be non-deterministic.

    assert(!root_moves.empty ());
    if (MOVE_NONE == best_move)
    {
        // RootMoves are already sorted by value in descending order
        auto max_new_value = root_moves[0].new_value;
        i32  weakness = MaxPlies - 8 * level;
        i32  diversity = std::min (max_new_value - root_moves[Threadpool.pv_limit - 1].new_value, VALUE_MG_PAWN);
        // First for each move score add two terms, both dependent on weakness.
        // One is deterministic with weakness, and one is random with weakness.
        // Then choose the move with the highest value.
        auto best_value = -VALUE_INFINITE;
        for (u08 i = 0; i < Threadpool.pv_limit; ++i)
        {
            auto &root_move = root_moves[i];
            auto value = root_move.new_value
                        // This is magic formula for push
                       + (  weakness  * i32(max_new_value - root_move.new_value)
                          + diversity * i32(prng.rand<u32> () % weakness)) / MaxPlies;

            if (best_value < value)
            {
                best_value = value;
                best_move = root_move[0];
            }
        }
    }
}

namespace Threading {

    /// Win Processors Group
    /// Under Windows it is not possible for a process to run on more than one logical processor group.
    /// This usually means to be limited to use max 64 cores.
    /// To overcome this, some special platform specific API should be called to set group affinity for each thread.
    /// Original code from Texel by Peter Österlund.
    namespace { 
    
        /// bind_thread() set the group affinity for the thread index.
        void bind_thread (size_t index);

    #if defined(_WIN32)
        /// get_group() retrieves logical processor information using Windows specific
        /// API and returns the best group id for the thread index.
        i32 get_group (size_t index)
        {
            // Early exit if the needed API is not available at runtime
            auto kernel32 = GetModuleHandle("Kernel32.dll");
            if (nullptr == kernel32)
            {
                return -1;
            }
            auto fun1 = (fun1_t)GetProcAddress (kernel32, "GetLogicalProcessorInformationEx");
            if (nullptr == fun1)
            {
                return -1;
            }
            DWORD length = 0;
            // First call to get length. We expect it to fail due to null buffer
            if (fun1 (LOGICAL_PROCESSOR_RELATIONSHIP::RelationAll, nullptr, &length))
            {
                return -1;
            }

            // Once we know length, allocate the buffer
            auto *buffer = (SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*) malloc (length);
            if (nullptr == buffer)
            {
                return -1;
            }

            // Second call, now we expect to succeed
            if (!fun1 (LOGICAL_PROCESSOR_RELATIONSHIP::RelationAll, buffer, &length))
            {
                free (buffer);
                return -1;
            }

            auto *ptr = buffer;
            DWORD byte_offset = 0;
            i32 nodes = 0;
            i32 cores = 0;
            i32 threads = 0;
            while (   ptr->Size > 0
                   && ptr->Size + byte_offset <= length)
            {
                switch (ptr->Relationship)
                {
                case LOGICAL_PROCESSOR_RELATIONSHIP::RelationNumaNode:
                    ++nodes;
                    break;
                case LOGICAL_PROCESSOR_RELATIONSHIP::RelationProcessorCore:
                    ++cores;
                    threads += (ptr->Processor.Flags == LTP_PC_SMT) ? 2 : 1;
                    break;
                default:
                    break;
                }

                byte_offset += ptr->Size;
                ptr = (SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*) ((char*)ptr + ptr->Size);
            }
            free (buffer);

            std::vector<i32> groups;
            // Run as many threads as possible on the same node until core limit is
            // reached, then move on filling the next node.
            for (i32 n = 0; n < nodes; ++n)
            {
                for (i32 i = 0; i < cores / nodes; ++i)
                {
                    groups.push_back (n);
                }
            }
            // In case a core has more than one logical processor (we assume 2) and we
            // have still threads to allocate, then spread them evenly across available
            // nodes.
            for (i32 t = 0; t < threads - cores; ++t)
            {
                groups.push_back (t % nodes);
            }

            // If we still have more threads than the total number of logical processors
            // then return -1 and let the OS to decide what to do.
            return index < groups.size () ? groups[index] : -1;
        }

        void bind_thread (size_t index)
        {
            // If OS already scheduled us on a different group than 0 then don't overwrite
            // the choice, eventually we are one of many one-threaded processes running on
            // some Windows NUMA hardware, for instance in fishtest.
            // To make it simple, just check if running threads are below a threshold,
            // in this case all this NUMA machinery is not needed.
            if (Threadpool.size () < 8)
            {
                return;
            }

            // Use only local variables to be thread-safe
            auto group = get_group (index);
            if (-1 == group)
            {
                return;
            }
            // Early exit if the needed API are not available at runtime
            auto kernel32 = GetModuleHandle("Kernel32.dll");
            if (nullptr == kernel32)
            {
                return;
            }
            auto fun2 = (fun2_t) GetProcAddress (kernel32, "GetNumaNodeProcessorMaskEx");
            auto fun3 = (fun3_t) GetProcAddress (kernel32, "SetThreadGroupAffinity");
            if (   nullptr == fun2
                || nullptr == fun3)
            {
                return;
            }

            GROUP_AFFINITY affinity;
            if (fun2 (USHORT(group), &affinity))
            {
                auto current_thread = GetCurrentThread ();
                if (nullptr != current_thread)
                {
                    PGROUP_AFFINITY ptr = nullptr;
                    fun3 (current_thread, &affinity, ptr);
                }
            }
        }
    #else
        void bind_thread (size_t index)
        {}
    #endif

    }

    /// Thread constructor launches the thread and waits until it goes to sleep in idle_loop().
    /// Note that 'searching' and 'dead' should be already set.
    Thread::Thread (u08 n)
        : index (n)
        , std_thread (&Thread::idle_loop, this)
    {
        wait_while_busy ();
        clear ();
    }
    /// Thread destructor wakes up the thread in idle_loop() and
    /// waits for its termination.
    /// Thread should be already waiting.
    Thread::~Thread ()
    {
        assert(!busy);
        dead = true;
        start_searching ();
        std_thread.join ();
    }
    /// Thread::start_searching() wakes up the thread that will start the search.
    void Thread::start_searching ()
    {
        std::lock_guard<Mutex> lk (mutex);
        busy = true;
        condition_var.notify_one (); // Wake up the thread in idle_loop()
    }
    /// Thread::wait_while_busy() blocks on the condition variable while the thread is busy.
    void Thread::wait_while_busy ()
    {
        std::unique_lock<Mutex> lk (mutex);
        condition_var.wait (lk, [&] { return !busy; });
    }
    /// Thread::idle_loop() is where the thread is parked.
    /// Blocked on the condition variable, when it has no work to do.
    void Thread::idle_loop ()
    {
        bind_thread (index);

        while (true)
        {
            std::unique_lock<Mutex> lk (mutex);
            busy = false;
            condition_var.notify_one (); // Wake up anyone waiting for search finished
            condition_var.wait (lk, [&] { return busy; });
            if (dead)
            {
                break;
            }
            lk.unlock ();

            search ();
        }
    }
    /// Thread::clear() clears all the thread related stuff.
    void Thread::clear ()
    {
        nodes = 0;
        tb_hits = 0;

        counter_moves.fill (MOVE_NONE);
        butterfly.fill (0);
        for (auto &pd : continuation)
        {
            for (auto &piece_destiny : pd)
            {
                piece_destiny.fill (0);
            }
        }
        continuation[NO_PIECE][0].fill (CounterMovePruneThreshold - 1);

        pawn_table.fill (Pawns::Entry ());
        matl_table.fill (Material::Entry ());
    }

    /// MainThread constructor
    MainThread::MainThread (u08 n)
        : Thread (n)
        , check_count (0)
        , easy_played (false)
        , failed_low (false)
        , best_move_change (0.0)
        , easy_move (MOVE_NONE)
        , last_value (VALUE_NONE)
    {}
    /// MainThread::clear()
    void MainThread::clear ()
    {
        Thread::clear();
        if (Limits.use_time_management ())
        {
            move_mgr.clear ();
            last_value = VALUE_NONE;
        }
    }

    /// ThreadPool::clear() clears the threadpool
    void ThreadPool::clear ()
    {
        for (auto *th : *this)
        {
            th->clear ();
        }
    }
    /// ThreadPool::configure() creates/destroys threads to match the requested number.
    void ThreadPool::configure (u32 threads)
    {
        while (size () < threads)
        {
            push_back (new Thread (u08(size ())));
        }
        while (size () > threads)
        {
            delete back ();
            pop_back (); // Get rid of stale pointer
        }
        shrink_to_fit ();
        sync_cout << "info string Thread(s) used " << threads << sync_endl;
    }
    /// ThreadPool::start_thinking() wakes up main thread waiting in idle_loop() and returns immediately.
    /// Main thread will wake up other threads and start the search.
    void ThreadPool::start_thinking (Position &root_pos, StateListPtr &states, const Limit &limits, const Moves &search_moves, bool ponde)
    {
        stop = false;
        stop_on_ponderhit = false;
        ponder = ponde;

        Limits = limits;

        RootMoves root_moves;
        root_moves.initialize (root_pos, search_moves);

        TBProbeDepth = i16(i32(Options["SyzygyProbeDepth"]));
        TBLimitPiece = i32(Options["SyzygyLimitPiece"]);
        TBUseRule50 = bool(Options["SyzygyUseRule50"]);
        TBHasRoot = false;
        // Skip TB probing when no TB found: !MaxLimitPiece -> !TBLimitPiece.
        if (TBLimitPiece > MaxLimitPiece)
        {
            TBLimitPiece = MaxLimitPiece;
            TBProbeDepth = 0;
        }
        // Filter root moves.
        if (   0 != TBLimitPiece
            && TBLimitPiece >= root_pos.count<NONE> ()
            && !root_moves.empty ()
            && !root_pos.has_castleright (CR_ANY))
        {
            // If the current root position is in the tablebases,
            // then RootMoves contains only moves that preserve the draw or the win.
            TBHasRoot = root_probe_dtz (root_pos, root_moves, TBValue);

            if (TBHasRoot)
            {
                // Do not probe tablebases during the search
                TBLimitPiece = 0;
            }
            // If DTZ tables are missing, use WDL tables as a fallback.
            else
            {
                // Filter out moves that do not preserve the draw or the win.
                TBHasRoot = root_probe_wdl (root_pos, root_moves, TBValue);
                // Only probe during search if winning.
                if (   TBHasRoot
                    && TBValue <= VALUE_DRAW)
                {
                    TBLimitPiece = 0;
                }
            }

            if (   TBHasRoot
                && !TBUseRule50)
            {
                TBValue = TBValue > VALUE_DRAW ? +VALUE_MATE - i32(MaxPlies - 1) :
                          TBValue < VALUE_DRAW ? -VALUE_MATE + i32(MaxPlies + 1) : VALUE_DRAW;
            }
        }

        // After ownership transfer 'states' becomes empty, so if we stop the search
        // and call 'go' again without setting a new position states.get() == nullptr.
        assert(nullptr != states.get ()
            || nullptr != setup_states.get ());

        if (nullptr != states.get ())
        {
            setup_states = std::move (states); // Ownership transfer, states is now empty
        }

        // We use setup() to set root position across threads.
        // So we need to save and later to restore last stateinfo, cleared by setup().
        // Note that states is shared by threads but is accessed in read-only mode.
        const auto back_si = setup_states->back ();
        for (auto *th : *this)
        {
            th->root_pos.setup (root_pos.fen (), setup_states->back (), th);
            th->root_moves = root_moves;

            th->nodes = 0;
            th->tb_hits = 0;
            th->running_depth = 0;
            th->finished_depth = 0;
        }
        setup_states->back () = back_si;

        main_thread ()->start_searching ();
    }
    void ThreadPool::start_thinking (Position &root_pos, StateListPtr &states, const Limit &limits, bool ponde)
    {
        const Moves search_moves;
        start_thinking (root_pos, states, limits, search_moves, ponde);
    }
    /// ThreadPool::wait_while_thinking() waits for the main thread while searching.
    void ThreadPool::wait_while_thinking ()
    {
        main_thread ()->wait_while_busy ();
    }
    /// ThreadPool::initialize() creates and launches requested threads, that will go immediately to sleep.
    /// Cannot use a constructor because threadpool is a static object and require a fully initialized engine (due to allocation of Tables in the Thread).
    void ThreadPool::initialize (u32 threads)
    {
        assert(empty ());
        push_back (new MainThread (0));
        configure (threads);
    }
    /// ThreadPool::deinitialize() cleanly terminates the threads before the program exits.
    /// Cannot use a destructor because threads must be terminated before deleting any static objects.
    void ThreadPool::deinitialize ()
    {
        wait_while_thinking ();
        assert(!empty ());
        configure (0);
    }

}
