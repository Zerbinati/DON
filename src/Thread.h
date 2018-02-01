#ifndef _THREAD_H_INC_
#define _THREAD_H_INC_

#include <thread>
#include <atomic>
#include "Material.h"
#include "Pawns.h"
#include "Position.h"
#include "PRNG.h"
#include "Searcher.h"
#include "thread_win32.h"
#include "Type.h"

extern u16 OverheadMoveTime;
extern u16 MinimumMoveTime;

extern double MoveSlowness;
extern u16 NodesTime;
extern bool Ponder;

/// TimeManager class is used to computes the optimal time to think depending on the
/// maximum available time, the move game number and other parameters.
class TimeManager
{
public:

    u64 optimum_time;
    u64 maximum_time;
    // Used in 'Nodes as Time' mode
    u64 available_nodes;

    TimeManager () = default;
    TimeManager (const TimeManager&) = delete;
    TimeManager& operator= (const TimeManager&) = delete;

    u64 elapsed_time () const;

    void initialize (Color, i16);
};

/// Skill Manager class is used to implement strength limit
class SkillManager
{
public:
    // MaxLevel should be <= MaxPlies/8
    static const u08 MaxLevel = 12;

    u08  level;
    Move best_move;

    explicit SkillManager (u08 lvl = MaxLevel)
        : level (lvl)
        , best_move (MOVE_NONE)
    {}
    SkillManager (const SkillManager&) = delete;
    SkillManager& operator= (const SkillManager&) = delete;

    bool enabled () const
    {
        return level < MaxLevel;
    }

    bool can_pick (i16 depth) const
    {
        return depth == level + 1;
    }

    void pick_best_move (const RootMoves&);
};

/// Thread class keeps together all the thread-related stuff.
/// It use pawn and material hash tables so that once get a pointer to
/// an entry its life time is unlimited and we don't have to care about
/// someone changing the entry under our feet.
class Thread
{
protected:

    Mutex mutex;
    ConditionVariable condition_var;
    bool dead = false
       , busy = true;

    size_t index;
    std::thread std_thread;

public:

    Position root_pos;
    RootMoves root_moves;

    i16   running_depth
        , finished_depth
        , sel_depth;

    i16  nmp_ply;
    bool nmp_odd;

    size_t pv_index;
        
    std::atomic<u64> nodes
        ,            tb_hits;

    PieceDestinyMoveHistory counter_moves;
    ButterflyHistory butterfly_history;
    CapturePieceDestinyHistory capture_history;
    ContinuationHistory continuation_history;

    Pawns::Table pawn_table;
    Material::Table matl_table;

    explicit Thread (size_t);
    Thread () = delete;
    Thread (const Thread&) = delete;
    Thread& operator= (const Thread&) = delete;

    virtual ~Thread ();

    void start_searching ();
    void wait_while_busy ();

    void idle_loop ();

    virtual void clear ();
    virtual void search ();
};

/// MainThread class is derived from Thread class used specific for main thread.
class MainThread
    : public Thread
{
public:

    i16    check_count;

    bool   failed_low;

    double best_move_change;

    Value  last_value;
    double last_time_reduction;

    TimeManager  time_mgr;
    SkillManager skill_mgr;

    explicit MainThread (size_t);
    MainThread () = delete;
    MainThread (const MainThread&) = delete;
    MainThread& operator= (const MainThread&) = delete;

    void clear () override;
    void search () override;

    void check_limits ();
};

/// ThreadPool class handles all the threads related stuff like,
/// initializing & deinitializing, starting, parking & launching a thread
/// All the access to shared thread data is done through this class.
class ThreadPool
    : public std::vector<Thread*>
{
private:

    static std::vector<i16> Groups;

    StateListPtr setup_states;

    u64 accumulate (std::atomic<u64> Thread::*member) const
    {
        u64 sum = 0;
        for (const auto *th : *this)
        {
            sum += (th->*member).load (std::memory_order::memory_order_relaxed);
        }
        return sum;
    }

public:

    size_t pv_limit;

    std::atomic<bool> stop                // Stop search
        ,             stop_on_ponderhit   // Stop search on ponderhit
        ,             ponder;             // Search on ponder move until the "stop"/"ponderhit" command

    ThreadPool () = default;
    ThreadPool (const ThreadPool&) = delete;
    ThreadPool& operator= (const ThreadPool&) = delete;

    static void initialize ();
    static void bind (size_t);

    MainThread* main_thread () const { return static_cast<MainThread*> (front ()); }
    u64 nodes () const { return accumulate (&Thread::nodes); }
    u64 tb_hits () const { return accumulate (&Thread::tb_hits); }

    Thread* best_thread () const;

    void clear ();
    void configure (u32);

    void start_thinking (Position&, StateListPtr&, const Limit&, const std::vector<Move>&, bool = false);
    void stop_thinking ();

};


enum OutputState : u08
{
    OS_LOCK,
    OS_UNLOCK,
};

/// Used to serialize access to std::cout to avoid multiple threads writing at the same time.
inline std::ostream& operator<< (std::ostream &os, OutputState state)
{
    static Mutex mutex;
    if (OutputState::OS_LOCK == state)
    {
        mutex.lock ();
    }
    else
    if (OutputState::OS_UNLOCK == state)
    {
        mutex.unlock ();
    }
    return os;
}

#define sync_cout std::cout << OS_LOCK
#define sync_endl std::endl << OS_UNLOCK

// Global ThreadPool
extern ThreadPool Threadpool;

#endif // _THREAD_H_INC_
