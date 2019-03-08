#ifndef _THREAD_H_INC_
#define _THREAD_H_INC_

#include <thread>
#include <atomic>
#include "Material.h"
#include "Option.h"
#include "Pawns.h"
#include "Position.h"
#include "PRNG.h"
#include "Searcher.h"
#include "thread_win32.h"
#include "Type.h"

/// TimeManager class is used to computes the optimal time to think depending on the
/// maximum available time, the move game number and other parameters.
class TimeManager
{
private:
    u16 nodes_time;

public:
    TimePoint start_time;
    TimePoint optimum_time;
    TimePoint maximum_time;

    u64 available_nodes;

    TimeManager ()
    {
        initialize ();
    }
    TimeManager (const TimeManager&) = delete;
    TimeManager& operator= (const TimeManager&) = delete;

    TimePoint elapsed_time () const;

    void initialize ();
    void set (Color, i16, u16, TimePoint, TimePoint, double, bool);
    void update (Color);
};

// MaxLevel should be <= MaxDepth/4
const i16 MaxLevel = 24;

inline bool skill_mgr_enabled ()
{
    return i16(i32(Options["Skill Level"])) < MaxLevel;
}

/// Skill Manager class is used to implement strength limit
class SkillManager
{
public:
    static PRNG PRNG;

    Move best_move = MOVE_NONE;

    SkillManager () = default;
    SkillManager (const SkillManager&) = delete;
    SkillManager& operator= (const SkillManager&) = delete;

    void pick_best_move (i16);
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

    Position  root_pos;
    RootMoves root_moves;

    i16 running_depth
      , finished_depth
      , sel_depth;

    i16   nmp_ply;
    Color nmp_color;

    size_t pv_beg
         , pv_cur
         , pv_end;

    std::atomic<u64> nodes
        ,            tb_hits;
    Score contempt;

    MoveHistory move_history;
    ButterflyHistory butterfly_history;
    CaptureHistory capture_history;
    ContinuationHistory continuation_history;

    Pawns::Table pawn_table;
    Material::Table matl_table;

    explicit Thread (size_t);
    Thread () = delete;
    Thread (const Thread&) = delete;
    Thread& operator= (const Thread&) = delete;

    virtual ~Thread ();

    void start ();
    void wait_while_busy ();

    void idle_loop ();

    virtual void clear ();
    virtual void search ();
};

/// MainThread class is derived from Thread class used specific for main thread.
class MainThread
    : public Thread
{
private :
    
public:
    
    u64    check_count;

    bool   stop_on_ponderhit; // Stop search on ponderhit
    std::atomic<bool> ponder; // Search on ponder move until the "stop"/"ponderhit" command

    Value  last_value;
    double last_time_reduction;

    TimeManager  time_mgr;
    SkillManager skill_mgr;

    bool   failed_low;
    double best_move_change;
    Move   best_move;
    i16    best_move_depth;

    explicit MainThread (size_t);
    MainThread () = delete;
    MainThread (const MainThread&) = delete;
    MainThread& operator= (const MainThread&) = delete;

    void clear () override;
    void search () override;

    void set_check_count ();
    void tick ();
};

namespace WinProcGroup {

    extern std::vector<i16> Groups;

    extern void initialize ();
    extern void bind (size_t);
}

/// ThreadPool class handles all the threads related stuff like,
/// initializing & deinitializing, starting, parking & launching a thread
/// All the access to shared thread data is done through this class.
class ThreadPool
    : public std::vector<Thread*>
{
private:

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

    std::atomic<bool> stop; // Stop search forcefully

    ThreadPool () = default;
    ThreadPool (const ThreadPool&) = delete;
    ThreadPool& operator= (const ThreadPool&) = delete;

    MainThread* main_thread () const { return static_cast<MainThread*> (front ()); }
    u64 nodes () const { return accumulate (&Thread::nodes); }
    u64 tb_hits () const { return accumulate (&Thread::tb_hits); }

    void clear ();
    void configure (u32);

    void start_thinking (Position&, StateListPtr&, const Limit&, const std::vector<Move>&, bool = false);
};


enum OutputState : u08
{
    OS_LOCK,
    OS_UNLOCK,
};

extern Mutex OutputMutex;

/// Used to serialize access to std::cout to avoid multiple threads writing at the same time.
inline std::ostream& operator<< (std::ostream &os, OutputState state)
{
    switch (state)
    {
    case OutputState::OS_LOCK:
        OutputMutex.lock ();
        break;
    case OutputState::OS_UNLOCK:
        OutputMutex.unlock ();
        break;
    default:
        break;
    }
    return os;
}

#define sync_cout std::cout << OS_LOCK
#define sync_endl std::endl << OS_UNLOCK

// Global ThreadPool
extern ThreadPool Threadpool;

#endif // _THREAD_H_INC_
