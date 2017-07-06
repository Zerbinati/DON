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

extern double MoveSlowness;
extern u32    NodesTime;
extern bool   Ponder;

// TimeManager class computes the optimal time to think depending on the
// maximum available time, the move game number and other parameters.
// Support four different kind of time controls, passed in 'limits':
//
// moves_to_go = 0, increment = 0 means: x basetime                             ['sudden death' time control]
// moves_to_go = 0, increment > 0 means: x basetime + z increment
// moves_to_go > 0, increment = 0 means: x moves in y basetime                  ['standard' time control]
// moves_to_go > 0, increment > 0 means: x moves in y basetime + z increment
class TimeManager
{
public:
    TimePoint optimum_time;
    TimePoint maximum_time;
    // Used in 'Nodes as Time' mode
    u64 available_nodes;

    TimeManager () = default;
    TimeManager (const TimeManager&) = delete;
    TimeManager& operator= (const TimeManager&) = delete;

    TimePoint elapsed_time () const;

    void initialize (Color c, i16 ply);
};

// MoveManager class is used to detect a so called 'easy move'.
// When PV is stable across multiple search iterations engine can fast return the best move.
class MoveManager
{
private:
    Key  _posi_key;
    Move _pv[3];

public:
    // Keep track of how many times in a row the 3rd ply remains stable
    u08  stable_count;

    MoveManager () = default;
    MoveManager (const MoveManager&) = delete;
    MoveManager& operator= (const MoveManager&) = delete;

    Move easy_move (Key posi_key) const
    {
        return
            posi_key == _posi_key ?
                _pv[2] :
                MOVE_NONE;
    }

    void clear ()
    {
        _posi_key = 0;
        std::fill_n (_pv, 3, MOVE_NONE);
        stable_count = 0;
    }

    void update (Position &pos, const Moves &pv)
    {
        assert(pv.size () >= 3);

        if (pv[2] == _pv[2])
        {
            ++stable_count;
        }
        else
        {
            stable_count = 0;
        }

        if (!std::equal (pv.begin (), pv.begin () + 3, _pv))
        {
            std::copy (pv.begin (), pv.begin () + 3, _pv);

            // Update posi key
            u08 ply = 0;
            StateInfo si[2];
            do
            {
                pos.do_move (pv[ply], si[ply]);
            } while (2 > ++ply);

            _posi_key = pos.si->posi_key;
            
            while (0 != ply)
            {
                pos.undo_move (pv[--ply]);
            }
        }
    }
};

// Skill Manager class is used to implement strength limit
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

    // Thread class keeps together all the thread related stuff like.
    // It also use pawn and material hash tables so that once get a pointer
    // to an entry its life time is unlimited and don't have to care about
    // someone changing the entry under its feet.
    class Thread
    {
    private:
        std::thread _native_thread;
        Mutex _mutex;
        ConditionVariable _sleep_condition;

        bool  _alive;

    public:
        u16   index
            , pv_index
            , max_ply;

        std::atomic<bool> searching;

        Position  root_pos;
        RootMoves root_moves;

        i16   running_depth
            , finished_depth;

        std::atomic<u64>
              nodes
            , tb_hits;

        Pawns   ::Table pawn_table;
        Material::Table matl_table;

        HistoryStats          history;
        MoveHistoryBoardStats cm_history;

        SquareMoveBoardStats  counter_moves;

        Thread ();
        Thread (const Thread&) = delete;
        Thread& operator= (const Thread&) = delete;

        virtual ~Thread ();

        void clear ()
        {
            max_ply = 0;
            nodes = 0;
            tb_hits = 0;

            pawn_table.clear ();
            matl_table.clear ();

            history.fill (0);
            for (auto &pc : cm_history)
            {
                for (auto &dst : pc)
                {
                    dst.fill (0);
                }
            }
            cm_history[NO_PIECE][0].fill (-1);
            counter_moves.fill (MOVE_NONE);
        }

        // Wakes up the thread that will start the search
        void start_searching (bool resume = false)
        {
            std::unique_lock<Mutex> lk (_mutex);
            if (!resume)
            {
                searching = true;
            }
            _sleep_condition.notify_one ();
        }
        // Waits on sleep condition until 'condition' turns true.
        void wait_until (const std::atomic<bool> &condition)
        {
            std::unique_lock<Mutex> lk (_mutex);
            _sleep_condition.wait (lk, [&] { return  bool(condition); });
        }
        // Waits on sleep condition until 'condition' turns false.
        void wait_while (const std::atomic<bool> &condition)
        {
            std::unique_lock<Mutex> lk (_mutex);
            _sleep_condition.wait (lk, [&] { return !bool(condition); });
        }

        void idle_loop ();

        virtual void search ();
    };

    // MainThread class is derived class used to characterize the the main one.
    class MainThread
        : public Thread
    {
    public:
        i16   check_count;

        bool  easy_played
            , failed_low;

        double best_move_change;
        
        Move  easy_move;
        Value last_value;

        TimeManager  time_mgr;
        MoveManager  move_mgr;
        SkillManager skill_mgr;

        MainThread () = default;
        MainThread (const MainThread&) = delete;
        MainThread& operator= (const MainThread&) = delete;

        virtual void search () override;

        void check_limits ();
    };

    // ThreadPool class handles all the threads related stuff like,
    // - initializing & deinitializing
    // - starting
    // - parking
    // - launching.
    // All the access to shared thread data is done through this.
    class ThreadPool
        : public std::vector<Thread*>
    {
    public:
        u16 pv_limit;
        
        std::atomic<bool>
                force_stop      // Stop on request
            ,   ponderhit_stop; // Stop on ponder-hit

        ThreadPool () = default;
        ThreadPool (const ThreadPool&) = delete;
        ThreadPool& operator= (const ThreadPool&) = delete;

        MainThread* main_thread () const
        {
            return static_cast<MainThread*> (at (0));
        }
        
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
        
        // Returns the total nodes searched.
        u64 nodes () const
        {
            u64 nodes = 0;
            for (const auto *th : *this)
            {
                nodes += th->nodes.load (std::memory_order::memory_order_relaxed);
            }
            return nodes;
        }
        // Returns the total TB hits.
        u64 tb_hits () const
        {
            u64 tb_hits = 0;
            for (const auto *th : *this)
            {
                tb_hits += th->tb_hits.load (std::memory_order::memory_order_relaxed);
            }
            return tb_hits;
        }

        void clear ();
        void configure (u32 threads);

        void start_thinking (Position &root_pos, StateList &states, const Limit &limits);
        void wait_while_thinking ();

        void initialize ();
        void deinitialize ();
    };

}

enum OutputState : u08
{
    OS_LOCK,
    OS_UNLOCK,
};

// Used to serialize access to std::cout to avoid multiple threads writing at the same time.
inline std::ostream& operator<< (std::ostream &os, const OutputState state)
{
    static Mutex mutex;

    switch (state)
    {
    case OS_LOCK  : mutex.lock ();   break;
    case OS_UNLOCK: mutex.unlock (); break;
    default:        assert(false);   break;
    }
    return os;
}

#define sync_cout std::cout << OS_LOCK
#define sync_endl std::endl << OS_UNLOCK

// Global ThreadPool
extern Threading::ThreadPool Threadpool;

#endif // _THREAD_H_INC_
