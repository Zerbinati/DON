#include "Thread.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include "Option.h"
#include "Searcher.h"
#include "TBsyzygy.h"

using namespace std;
using namespace Searcher;
using namespace TBSyzygy;

u16 OverheadMoveTime = 30;  // Attempt to keep at least this much time for each remaining move, in milli-seconds.
u16 MinimumMoveTime = 20;   // No matter what, use at least this much time before doing the move, in milli-seconds.

i32 MoveSlowness = 84;      // Move Slowness, in %age.
u16 NodesTime = 0;          // 'Nodes as Time' mode.
bool Ponder = true;         // Whether or not the engine should analyze when it is the opponent's turn.

ThreadPool Threadpool;

namespace {

#if defined(_WIN32)

    /// Win Processors Group
    /// Under Windows it is not possible for a process to run on more than one logical processor group.
    /// This usually means to be limited to use max 64 cores.
    /// To overcome this, some special platform specific API should be called to set group affinity for each thread.
    /// Original code from Texel by Peter Österlund.

    /// The needed Windows API for processor groups could be missed from old Windows versions,
    /// so instead of calling them directly (forcing the linker to resolve the calls at compile time),
    /// try to load them at runtime. To do this first define the corresponding function pointers.
    extern "C"
    {
        typedef bool (*GLPIE)(LOGICAL_PROCESSOR_RELATIONSHIP LogicalProcRelationship, PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX PtrSysLogicalProcInfo, PDWORD PtrLength);
        typedef bool (*GNNPME)(USHORT Node, PGROUP_AFFINITY PtrGroupAffinity);
        typedef bool (*STGA)(HANDLE Thread, CONST GROUP_AFFINITY *GroupAffinity, PGROUP_AFFINITY PtrGroupAffinity);
    }

#else


#endif

    u16 OverheadClockTime = 2*OverheadMoveTime; // Attempt to keep at least this much time at clock, in milli-seconds.
    u08 MaximumMoveHorizon = 50;                // Plan time management at most this many moves ahead, in num of moves.
    u08 ReadyMoveHorizon = 40;                  // Be prepared to always play at least this many moves, in num of moves.

    // Skew-logistic function based on naive statistical analysis of
    // "how many games are still undecided after n half-moves".
    // Game is considered "undecided" as long as neither side has >275cp advantage.
    // Data was extracted from the CCRL game database with some simple filtering criteria.
    double move_importance (i16 ply)
    {
        //                                      Shift    Scale   Skew
        return std::pow (1.00 + std::exp ((ply - 64.50) / 6.85), -0.171) + DBL_MIN; // Ensure non-zero
    }

    template<bool Optimum>
    u64 remaining_time (u64 time, u08 movestogo, i16 ply)
    {
        constexpr auto  step_ratio = Optimum ? 1.00 : 7.30; // When in trouble, can step over reserved time with this ratio
        constexpr auto steal_ratio = Optimum ? 0.00 : 0.34; // However must not steal time from remaining moves over this ratio

        auto move_imp1 = move_importance (ply) * MoveSlowness / 100.0;
        auto move_imp2 = 0.0;
        for (u08 i = 1; i < movestogo; ++i)
        {
            move_imp2 += move_importance (ply + 2 * i);
        }

        auto time_ratio1 = (move_imp1 * step_ratio /* + move_imp2 * 0.00*/ ) / (move_imp1 * step_ratio + move_imp2 /* * 1.00*/);
        auto time_ratio2 = (move_imp1 /* * 1.00*/ + move_imp2 * steal_ratio) / (move_imp1 /* * 1.00*/  + move_imp2 /* * 1.00*/);

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
/// increment == 0, moves to go == 0 => y basetime                             ['sudden death']
/// increment != 0, moves to go == 0 => y basetime + z increment
/// increment == 0, moves to go != 0 => x moves in y basetime                  ['standard']
/// increment != 0, moves to go != 0 => x moves in y basetime + z increment
void TimeManager::initialize (Color c, i16 ply)
{
    optimum_time =
    maximum_time = std::max (Limits.clock[c].time, u64(MinimumMoveTime));

    const auto Max_movestogo = 0 == Limits.movestogo ?
                                MaximumMoveHorizon :
                                std::min (Limits.movestogo, MaximumMoveHorizon);
    // Calculate optimum time usage for different hypothetic "moves to go" and choose the
    // minimum of calculated search time values. Usually the greatest hyp_movestogo gives the minimum values.
    for (u08 hyp_movestogo = 1; hyp_movestogo <= Max_movestogo; ++hyp_movestogo)
    {
        // Calculate thinking time for hypothetic "moves to go"
        auto hyp_time = std::max (
                        + Limits.clock[c].time
                        + Limits.clock[c].inc * (hyp_movestogo-1)
                        - OverheadClockTime
                        - OverheadMoveTime * std::min (hyp_movestogo, ReadyMoveHorizon), u64(0));

        optimum_time = std::min (optimum_time, MinimumMoveTime + remaining_time<true > (hyp_time, hyp_movestogo, ply));
        maximum_time = std::min (maximum_time, MinimumMoveTime + remaining_time<false> (hyp_time, hyp_movestogo, ply));
    }

    if (Ponder)
    {
        optimum_time += optimum_time / 4;
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
            auto &rm = root_moves[i];
            auto value = rm.new_value
                        // This is magic formula for push
                       + (  weakness  * i32(max_value - rm.new_value)
                          + diversion * i32(prng.rand<u32> () % weakness)) / MaxPlies;

            if (best_value <= value)
            {
                best_value = value;
                best_move = rm[0];
            }
        }
    }
}

/// Thread constructor launches the thread and waits until it goes to sleep in idle_loop().
/// Note that 'busy' and 'dead' should be already set.
Thread::Thread (size_t idx)
    : index (idx)
    , std_thread (&Thread::idle_loop, this)
{
    wait_while_busy ();
}
/// Thread destructor wakes up the thread in idle_loop() and waits for its termination.
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
    // If OS already scheduled us on a different group than 0 then don't overwrite
    // the choice, eventually we are one of many one-threaded processes running on
    // some Windows NUMA hardware, for instance in fishtest. To make it simple,
    // just check if running threads are below a threshold, in this case all this
    // NUMA machinery is not needed.
    auto threads = i32(Options["Threads"]);
    if (0 == threads)
    {
        threads = thread::hardware_concurrency ();
    }
    if (8 <= threads)
    {
        ThreadPool::bind (index);
    }

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
    running_depth = 0;
    finished_depth = 0;
    nodes = 0;
    tb_hits = 0;
    nmp_ply = 0;
    nmp_odd = false;
    counter_moves.fill (MOVE_NONE);
    butterfly_history.fill (0);
    capture_history.fill (0);
    for (auto &pd : continuation_history)
    {
        for (auto &piece_destiny : pd)
        {
            piece_destiny.get ()->fill (0);
        }
    }
    continuation_history[NO_PIECE][0].get ()->fill (CounterMovePruneThreshold - 1);

    pawn_table.fill (Pawns::Entry ());
    matl_table.fill (Material::Entry ());
}

/// MainThread constructor
MainThread::MainThread (size_t idx)
    : Thread (idx)
    , check_count (0)
    , failed_low (false)
    , best_move_change (0.0)
    , last_value (VALUE_NONE)
    , last_time_reduction (1.00)
{}
/// MainThread::clear()
void MainThread::clear ()
{
    Thread::clear();

    time_mgr.available_nodes = 0;
    last_value = VALUE_NONE;
    last_time_reduction = 1.00;
}

vector<i16> ThreadPool::Groups;
/// initialize() retrieves logical processor information using specific API
void ThreadPool::initialize ()
{
#if defined(_WIN32)
    // Early exit if the needed API is not available at runtime
    const auto kernel32 = GetModuleHandle ("Kernel32.dll");
    if (nullptr == kernel32)
    {
        return;
    }
    const auto GetLogicalProcessorInformationEx = (GLPIE) GetProcAddress (kernel32, "GetLogicalProcessorInformationEx");
    if (nullptr == GetLogicalProcessorInformationEx)
    {
        return;
    }

    DWORD length = 0;
    // First call to get length. We expect it to fail due to null buffer
    if (GetLogicalProcessorInformationEx (LOGICAL_PROCESSOR_RELATIONSHIP::RelationAll, nullptr, &length))
    {
        return;
    }

    // Once we know length, allocate the buffer
    auto *ptrSysLogicalProcInfoBase = reinterpret_cast<SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*> (malloc (length));
    if (nullptr == ptrSysLogicalProcInfoBase)
    {
        return;
    }

    // Second call, now we expect to succeed
    if (!GetLogicalProcessorInformationEx (LOGICAL_PROCESSOR_RELATIONSHIP::RelationAll, ptrSysLogicalProcInfoBase, &length))
    {
        free (ptrSysLogicalProcInfoBase);
        return;
    }

    auto *ptrSysLogicalProcInfoCurr = ptrSysLogicalProcInfoBase;
    DWORD offset = 0;
    u16 nodes = 0;
    u16 cores = 0;
    u16 threads = 0;
    while (   ptrSysLogicalProcInfoCurr->Size > 0
           && ptrSysLogicalProcInfoCurr->Size + offset <= length)
    {
        switch (ptrSysLogicalProcInfoCurr->Relationship)
        {
        case LOGICAL_PROCESSOR_RELATIONSHIP::RelationProcessorCore:
            ++cores;
            threads += ptrSysLogicalProcInfoCurr->Processor.Flags == LTP_PC_SMT ? 2 : 1;
            break;
        case LOGICAL_PROCESSOR_RELATIONSHIP::RelationNumaNode:
            ++nodes;
            break;
        default:
            break;
        }

        offset += ptrSysLogicalProcInfoCurr->Size;
        ptrSysLogicalProcInfoCurr = reinterpret_cast<SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*> (reinterpret_cast<BYTE*> (ptrSysLogicalProcInfoCurr) + ptrSysLogicalProcInfoCurr->Size);
    }
    free (ptrSysLogicalProcInfoBase);

    // Run as many threads as possible on the same node until core limit is
    // reached, then move on filling the next node.
    for (u16 n = 0; n < nodes; ++n)
    {
        for (u16 i = 0; i < cores / nodes; ++i)
        {
            Groups.push_back (n);
        }
    }
    // In case a core has more than one logical processor (we assume 2) and we
    // have still threads to allocate, then spread them evenly across available
    // nodes.
    for (u16 t = 0; t < threads - cores; ++t)
    {
        Groups.push_back (t % nodes);
    }

#else
    
#endif
}
/// bind() set the group affinity for the thread index.
void ThreadPool::bind (size_t index)
{
    // If we still have more threads than the total number of logical processors then let the OS to decide what to do.
    if (index >= Groups.size ())
    {
        return;
    }
    u16 group = Groups[index];

#if defined(_WIN32)

    const auto kernel32 = GetModuleHandle ("Kernel32.dll");
    if (nullptr == kernel32)
    {
        return;
    }

    const auto GetNumaNodeProcessorMaskEx = (GNNPME) GetProcAddress (kernel32, "GetNumaNodeProcessorMaskEx");
    if (nullptr == GetNumaNodeProcessorMaskEx)
    {
        return;
    }
    GROUP_AFFINITY group_affinity;
    if (GetNumaNodeProcessorMaskEx (group, &group_affinity))
    {
        const auto SetThreadGroupAffinity = (STGA) GetProcAddress (kernel32, "SetThreadGroupAffinity");
        if (nullptr == SetThreadGroupAffinity)
        {
            return;
        }
        SetThreadGroupAffinity (GetCurrentThread (), &group_affinity, nullptr);
    }

#else
    
#endif
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
/// Created and launched threads will go immediately to sleep in idle_loop.
/// Upon resizing, threads are recreated to allow for binding if necessary.
void ThreadPool::configure (u32 threads)
{
    // Destroy any existing thread(s)
    if (!empty ())
    {
        main_thread ()->wait_while_busy ();
        while (!empty ())
        {
            delete back ();
            pop_back ();
        }
    }
    // Create new thread(s)
    if (0 < threads)
    {
        assert(empty ());
        push_back (new MainThread (size ()));
        while (size () < threads)
        {
            push_back (new Thread (size ()));
        }
        assert(!empty ());
        
        sync_cout << "info string Thread(s) used " << threads << sync_endl;

        clear ();
    }
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
        TBProbeDepth = DepthZero;
    }
    // Filter root moves.
    if (   !root_moves.empty ()
        && 1 == MultiPV
        && 0 != TBLimitPiece
        && TBLimitPiece >= pos.count ()
        && !pos.si->can_castle (CR_ANY))
    {
        // If the current root position is in the tablebases,
        // then RootMoves contains only moves that preserve the draw or the win.
        TBHasRoot = root_probe_dtz (pos, root_moves, TBValue);

        if (TBHasRoot)
        {
            // Do not probe tablebases during the search
            TBLimitPiece = 0;
        }
        // If DTZ tables are missing, use WDL tables as a fall back.
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
            TBValue = TBValue > VALUE_DRAW ? +VALUE_MATE - (MaxPlies - 1) :
                      TBValue < VALUE_DRAW ? -VALUE_MATE + (MaxPlies + 1) :
                                              VALUE_DRAW;
        }

        // Reset root move scores to -VALUE_INFINITE, Since root_probe_dtz() and root_probe_wdl() dirty them.
        for (auto &rm : root_moves)
        {
            rm.new_value = -VALUE_INFINITE;
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
    const auto fen = pos.fen ();
    const auto back_si = setup_states->back ();
    for (auto *th : *this)
    {
        th->running_depth = 0;
        th->finished_depth = 0;
        th->nodes = 0;
        th->tb_hits = 0;
        th->nmp_ply = 0;
        th->nmp_odd = false;

        th->root_pos.setup (fen, setup_states->back (), th);
        th->root_moves = root_moves;
    }
    setup_states->back () = back_si;

    main_thread ()->start_searching ();
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
