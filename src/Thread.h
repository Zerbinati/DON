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

//extern u08 MaximumMoveHorizon;
//extern u08 ReadyMoveHorizon;
//extern u32 OverheadClockTime;
extern u32 OverheadMoveTime;
extern u32 MinimumMoveTime;

extern double MoveSlowness;
extern u32    NodesTime;
extern bool   Ponder;

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

/// MoveManager class is used to detect a so called 'easy move'.
/// When PV is stable across multiple search iterations engine can fast return the best move.
class MoveManager
{
private:
    Key  exp_posi_key;
    Move pv[3];

public:
    // Keep track of how many times in a row the 3rd ply remains stable
    u08  stable_count;

    MoveManager () = default;
    MoveManager (const MoveManager&) = delete;
    MoveManager& operator= (const MoveManager&) = delete;

    Move easy_move (Key posi_key) const
    {
        return posi_key == exp_posi_key ? pv[2] : MOVE_NONE;
    }

    void clear ()
    {
        exp_posi_key = 0;
        std::fill_n (pv, 3, MOVE_NONE);
        stable_count = 0;
    }

    void update (Position&, const Moves&);

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
    
    void clear ()
    {
        best_move = MOVE_NONE;
    }
    
    void pick_best_move (const RootMoves &root_moves);
};

namespace Threading {

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
        u08  index;
        std::thread std_thread;

    public:
        Position root_pos;
        RootMoves root_moves;

        i16  running_depth
           , finished_depth;

        i16  sel_depth;
        u08  pv_index;

        std::atomic<u64> nodes, tb_hits;

        PieceDestinyMoveTable counter_moves;
        ButterflyHistory butterfly;
        ContinuationStatTable continuation;

        Pawns   ::Table pawn_table;
        Material::Table matl_table;

        explicit Thread (u08 n);
        Thread () = delete;
        Thread (const Thread&) = delete;
        Thread& operator= (const Thread&) = delete;

        virtual ~Thread ();

        void clear ();
        
        void start_searching ();
        void wait_while_busy ();

        void idle_loop ();

        virtual void search ();
    };

    /// MainThread class is derived from Thread class used specific for main thread.
    class MainThread
        : public Thread
    {
    public:
        i16  check_count;

        bool easy_played
           , failed_low;

        double best_move_change;
        
        Move  easy_move;
        Value last_value;

        TimeManager  time_mgr;
        MoveManager  move_mgr;
        SkillManager skill_mgr;

        explicit MainThread (u08 n);
        MainThread () = delete;
        MainThread (const MainThread&) = delete;
        MainThread& operator= (const MainThread&) = delete;

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
        u08 pv_limit;
        
        std::atomic<bool>
                stop                // Stop search
            ,   stop_on_ponderhit   // Stop search on ponderhit
            ,   ponder;             // Search on ponder move until the "stop"/"ponderhit" command

        ThreadPool () = default;
        ThreadPool (const ThreadPool&) = delete;
        ThreadPool& operator= (const ThreadPool&) = delete;

        MainThread* main_thread () const { return static_cast<MainThread*> (front ()); }
        u64 nodes () const { return accumulate (&Thread::nodes); }
        u64 tb_hits () const { return accumulate (&Thread::tb_hits); }

        Thread* best_thread () const
        {
            auto *best_th = at (0);
            for (auto *th : *this)
            {
                if (   best_th->finished_depth < th->finished_depth
                    && best_th->root_moves[0].new_value <= th->root_moves[0].new_value)
                {
                    best_th = th;
                }
            }
            return best_th;
        }
        
        void clear ();
        void configure (u32);

        void start_thinking (Position&, StateListPtr&, const Limit&, const Moves&, bool = false);
        void start_thinking (Position&, StateListPtr&, const Limit&, bool = false);

        void wait_while_thinking ();

        /// No constructor and destructor, threads rely on globals that should
        /// be initialized and valid during the whole thread lifetime.
        void initialize (u32);
        void deinitialize ();
    };

}

enum OutputState : u08
{
    OS_LOCK,
    OS_UNLOCK,
};

/// Used to serialize access to std::cout to avoid multiple threads writing at the same time.
inline std::ostream& operator<< (std::ostream &os, const OutputState state)
{
    static Mutex mutex;

    switch (state)
    {
    case OS_LOCK  :
        mutex.lock ();
        break;
    case OS_UNLOCK:
        mutex.unlock ();
        break;
    default:
        assert(false);
        break;
    }
    return os;
}

#define sync_cout std::cout << OS_LOCK
#define sync_endl std::endl << OS_UNLOCK

// Global ThreadPool
extern Threading::ThreadPool Threadpool;

#endif // _THREAD_H_INC_
