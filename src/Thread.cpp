#include "Thread.h"

#include <cfloat>
#include "Option.h"
#include "Searcher.h"
#include "TBsyzygy.h"

using namespace std;
using namespace Searcher;
using namespace TBSyzygy;

u16  OverheadMoveTime = 100;// Attempt to keep at least this much time for each remaining move, in milli-seconds.
u16  NodesTime = 0;         // 'Nodes as Time' mode.
bool Ponder = true;         // Whether or not the engine should analyze when it is the opponent's turn.

Threading::ThreadPool Threadpool;

namespace {

    /// remaining_time()
    u64 remaining_time (Color c, i16 move_num, bool optimum)
    {
        if (Limits.clock[c].time == 0)
        {
            return 0;
        }
        // Increment of time
        double inc = Limits.clock[c].inc
                     // quadratic function with the maximum around move 25 
                   * std::max (120.0 - 0.12 * std::pow (move_num - 25, 2), 55.0);
        // Ratio of time
        double ratio = std::min (0 == Limits.movestogo ?
                                      // y+z
                                      (optimum ? 0.017 : 0.07)
                                    * ((1 + 0.04 * move_num / (1 + 0.002 * move_num)) + inc / Limits.clock[c].time) :
                                      // x/y+z
                                      std::min (  (optimum ? 1.0 : 6.0)
                                                * (move_num <= 40 ?
                                                    // quadratic function with the maximum around move 20 for case less then 40 moves in y time.
                                                    1.1 - 0.001 * std::pow (move_num - 20, 2) :
                                                    // constant function.
                                                    1.5)
                                                / std::min (Limits.movestogo, u08(50)),
                                                1 < Limits.movestogo ? 0.75 : 1.5)
                                    * (1 + inc / (Limits.clock[c].time * 8.5)),
                                 1.0);
        if (   optimum
            && Ponder)
        {
            ratio *= 1.25;
        }
        return u64(std::max (Limits.clock[c].time - OverheadMoveTime, 0ULL) * ratio);
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
/// increment == 0, moves to go == 0 => y basetime                             ['sudden death']
/// increment != 0, moves to go == 0 => y basetime + z increment
/// increment == 0, moves to go != 0 => x moves in y basetime                  ['standard']
/// increment != 0, moves to go != 0 => x moves in y basetime + z increment
void TimeManager::initialize (Color c, i16 ply)
{
    i16 move_num = (ply + 1) / 2;
    optimum_time = remaining_time (c, move_num, true);
    maximum_time = remaining_time (c, move_num, false);
}

/// MoveManager::clear() 
void MoveManager::clear ()
{
    stable_count = 0;
    exp_posi_key = 0;
    std::fill_n (pv, 3, MOVE_NONE);
}
/// MoveManager::update()
void MoveManager::update (Position &pos, const vector<Move> &new_pv)
{
    assert(new_pv.size () >= 3
        && new_pv[2] != MOVE_NONE);

    if (new_pv[2] == pv[2])
    {
        if (++stable_count >= 4)
        {
            if (!std::equal (new_pv.begin (), new_pv.begin () + 3, pv))
            {
                std::copy (new_pv.begin (), new_pv.begin () + 3, pv);

                StateInfo si[2];
                pos.do_move (new_pv[0], si[0]);
                pos.do_move (new_pv[1], si[1]);
                exp_posi_key = pos.si->posi_key;
                pos.undo_move (new_pv[1]);
                pos.undo_move (new_pv[0]);
            }
        }
    }
    else
    {
        stable_count = 0;
        exp_posi_key = 0;
        std::fill_n (pv, 2, MOVE_NONE);
        pv[2] = new_pv[2];
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
        auto max_value = root_moves[0].new_value;
        auto min_value = root_moves[Threadpool.pv_limit - 1].new_value;
        i32  weakness = MaxPlies - 8 * level;
        i32  diversion = std::min (max_value - min_value, VALUE_MG_PAWN);
        // First for each move score add two terms, both dependent on weakness.
        // One is deterministic with weakness, and one is random with weakness.
        // Then choose the move with the highest value.
        auto best_value = -VALUE_INFINITE;
        for (u08 i = 0; i < Threadpool.pv_limit; ++i)
        {
            auto &root_move = root_moves[i];
            auto cur_value = root_move.new_value;
            auto value = cur_value
                        // This is magic formula for push
                       + (  weakness  * i32(max_value - cur_value)
                          + diversion * i32(prng.rand<u32> () % weakness)) / MaxPlies;

            if (best_value <= value)
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
                case LOGICAL_PROCESSOR_RELATIONSHIP::RelationProcessorCore:
                    ++cores;
                    threads += (ptr->Processor.Flags == LTP_PC_SMT) ? 2 : 1;
                    break;
                case LOGICAL_PROCESSOR_RELATIONSHIP::RelationNumaNode:
                    ++nodes;
                    break;
                default:
                    break;
                }

                byte_offset += ptr->Size;
                ptr = (SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*) ((char*)ptr + ptr->Size);
            }
            free (buffer);

            vector<i32> groups;
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
                fun3 (GetCurrentThread (), &affinity, nullptr);
            }
        }
    #else
        void bind_thread (size_t index)
        {}
    #endif

    }

    /// Thread constructor launches the thread and waits until it goes to sleep in idle_loop().
    /// Note that 'searching' and 'dead' should be already set.
    Thread::Thread (size_t n)
        : index (n)
        , std_thread (&Thread::idle_loop, this)
    {
        wait_while_busy ();
        clear (); // Init
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
        lock_guard<Mutex> lk (mutex);
        busy = true;
        condition_var.notify_one (); // Wake up the thread in idle_loop()
    }
    /// Thread::wait_while_busy() blocks on the condition variable while the thread is busy.
    void Thread::wait_while_busy ()
    {
        unique_lock<Mutex> lk (mutex);
        condition_var.wait (lk, [&] { return !busy; });
    }
    /// Thread::idle_loop() is where the thread is parked.
    /// Blocked on the condition variable, when it has no work to do.
    void Thread::idle_loop ()
    {
        bind_thread (index);

        while (true)
        {
            unique_lock<Mutex> lk (mutex);
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
    MainThread::MainThread (size_t n)
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

    /// ThreadPool::best_thread()
    Thread* ThreadPool::best_thread () const
    {
        auto *best_th = front ();
        for (auto *th : *this)
        {
            if (   best_th->root_moves[0].new_value < th->root_moves[0].new_value
                && (   best_th->finished_depth <= th->finished_depth
                    || VALUE_MATE_MAX_PLY <= th->root_moves[0].new_value))
            {
                best_th = th;
            }
        }
        return best_th;
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
            push_back (new Thread (size ()));
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
    void ThreadPool::start_thinking (Position &pos, StateListPtr &states, const Limit &limits, const vector<Move> &search_moves, bool ponde)
    {
        stop = false;
        stop_on_ponderhit = false;
        ponder = ponde;

        Limits = limits;

        RootMoves root_moves;
        root_moves.initialize (pos, search_moves);

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
        if (   !root_moves.empty ()
            && 1 == MultiPV
            && 0 != TBLimitPiece
            && TBLimitPiece >= pos.count<NONE> ()
            && !pos.has_castleright (CR_ANY))
        {
            // If the current root position is in the tablebases,
            // then RootMoves contains only moves that preserve the draw or the win.
            TBHasRoot = root_probe_dtz (pos, root_moves, TBValue);

            if (TBHasRoot)
            {
                // Do not probe tablebases during the search
                TBLimitPiece = 0;
            }
            // If DTZ tables are missing, use WDL tables as a fallback.
            else
            {
                // Filter out moves that do not preserve the draw or the win.
                TBHasRoot = root_probe_wdl (pos, root_moves, TBValue);
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
            th->root_pos.setup (pos.fen (), setup_states->back (), th);
            th->root_moves = root_moves;

            th->nodes = 0;
            th->tb_hits = 0;
            th->running_depth = 0;
            th->finished_depth = 0;
        }
        setup_states->back () = back_si;

        main_thread ()->start_searching ();
    }
    void ThreadPool::start_thinking (Position &pos, StateListPtr &states, const Limit &limits, bool ponde)
    {
        const vector<Move> search_moves;
        start_thinking (pos, states, limits, search_moves, ponde);
    }
    /// ThreadPool::stop_thinking()
    void ThreadPool::stop_thinking ()
    {
        // If allowed to ponder do not stop the search now but
        // keep pondering until GUI sends "stop"/"ponderhit".
        if (ponder)
        {
            stop_on_ponderhit = true;
        }
        else
        {
            stop = true;
        }
    }
    /// ThreadPool::initialize() creates and launches requested threads, that will go immediately to sleep.
    /// Cannot use a constructor because threadpool is a static object and require a fully initialized engine (due to allocation of Tables in the Thread).
    void ThreadPool::initialize (u32 threads)
    {
        push_back (new MainThread (0));
        configure (threads);
    }
    /// ThreadPool::deinitialize() cleanly terminates the threads before the program exits.
    /// Cannot use a destructor because threads must be terminated before deleting any static objects.
    void ThreadPool::deinitialize ()
    {
        main_thread ()->wait_while_busy ();
        configure (0);
    }

}
