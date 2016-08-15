#ifndef _THREAD_H_INC_
#define _THREAD_H_INC_

#include <bitset>
#include <thread>
#include <iostream>

#include "thread_win32.h"
#include "PRNG.h"
#include "Position.h"
#include "Pawns.h"
#include "Material.h"
#include "MovePicker.h"

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
    TimePoint optimum_time = 0;
    TimePoint maximum_time = 0;

    u64 available_nodes = 0; // 'Nodes as Time' mode

    TimeManager () = default;
    TimeManager (const TimeManager&) = delete;
    TimeManager& operator= (const TimeManager&) = delete;

    TimePoint elapsed_time () const;

    void initialize (Color c, i16 ply);

    void update (Color c);
};

// MoveManager class is used to detect a so called 'easy move'.
// When PV is stable across multiple search iterations engine can fast return the best move.
class MoveManager
{
public:
    static const u08 PVSize = 3;
private:
    Key  _posi_key = 0;
    Move _pv[PVSize];

public:
    // Keep track of how many times in a row the 3rd ply remains stable
    u08  stable_count   = 0;

    MoveManager ()
    {
        clear ();
    }
    MoveManager (const MoveManager&) = delete;
    MoveManager& operator= (const MoveManager&) = delete;

    Move easy_move (Key posi_key) const
    {
        return posi_key == _posi_key ?
            _pv[PVSize-1] :
            MOVE_NONE;
    }

    void clear ()
    {
        _posi_key    = 0;
        stable_count = 0;
        std::fill_n (_pv, PVSize, MOVE_NONE);
    }
    void update (Position &pos, const MoveVector &pv)
    {
        assert(pv.size () >= PVSize);

        if (pv[PVSize-1] == _pv[PVSize-1])
        {
            ++stable_count;
        }
        else
        {
            stable_count = 0;
        }

        if (!std::equal (pv.begin (), pv.begin () + PVSize, _pv))
        {
            std::copy (pv.begin (), pv.begin () + PVSize, _pv);

            u08 ply = 0;
            StateInfo si[PVSize-1];
            while (ply < PVSize-1)
            {
                pos.do_move (pv[ply], si[ply], pos.gives_check (pv[ply], CheckInfo (pos)));
                ++ply;
            }
            _posi_key = pos.posi_key ();
            while (ply != 0)
            {
                pos.undo_move ();
                --ply;
            }
        }
    }
};

// Skill Manager class is used to implement strength limit
class SkillManager
{
public:
    // MaxSkillLevel should be <= MaxPlies/4
    static const u08 MaxSkillLevel = 32;
    static const u16 MinSkillPV    = 4;

private:
    u08  _skill_level = MaxSkillLevel;
    Move _best_move   = MOVE_NONE;

public:
    explicit SkillManager (u08 skill_level = MaxSkillLevel)
        : _skill_level (skill_level)
        , _best_move (MOVE_NONE)
    {}
    SkillManager (const SkillManager&) = delete;
    SkillManager& operator= (const SkillManager&) = delete;

    bool enabled () const
    {
        return _skill_level < MaxSkillLevel;
    }
    bool can_pick (Depth depth) const
    {
        return i32(depth) == _skill_level + 1;
    }

    void change_skill_level (u08 skill_level)
    {
        _skill_level = skill_level;
    }
    void clear ()
    {
        _best_move = MOVE_NONE;
    }

    Move pick_best_move (u16 pv_limit);
};

namespace Threading {

    // Thread class keeps together all the thread related stuff like.
    // It also use pawn and material hash tables so that once get a pointer
    // to an entry its life time is unlimited and don't have to care about
    // someone changing the entry under its feet.
    class Thread
    {
    private:
        std::thread       _native_thread;
        Mutex             _mutex;
        ConditionVariable _sleep_condition;

        bool  _alive     = true
            , _searching = false;

    public:
        u16   index    = 0
            , pv_index = 0
            , max_ply  = 0
            , count    = 0;

        Position root_pos;
        RootMoveVector root_moves;
        Depth running_depth  = DEPTH_0
            , finished_depth = DEPTH_0;

        Pawns   ::Table pawn_table;
        Material::Table matl_table;

        HValueStats history_values;
        OrgDstStats org_dst_values;
        MoveStats   counter_moves;

        std::atomic_bool reset_count { false };

        Thread ();
        Thread (const Thread&) = delete;
        Thread& operator= (const Thread&) = delete;

        virtual ~Thread ();

        // Wakes up the thread that will start the search
        void start_searching (bool resume = false)
        {
            std::unique_lock<Mutex> lk (_mutex);
            if (!resume)
            {
                _searching = true;
            }
            _sleep_condition.notify_one ();
            lk.unlock ();
        }
        // Waits on sleep condition until not searching
        void wait_while_searching ()
        {
            std::unique_lock<Mutex> lk (_mutex);
            _sleep_condition.wait (lk, [&] { return !_searching; });
            lk.unlock ();
        }

        // Waits on sleep condition until 'condition' turns true
        void wait_until (const std::atomic_bool &condition)
        {
            std::unique_lock<Mutex> lk (_mutex);
            _sleep_condition.wait (lk, [&] { return bool(condition); });
            lk.unlock ();
        }
        // Waits on sleep condition until 'condition' turns false
        void wait_while (const std::atomic_bool &condition)
        {
            std::unique_lock<Mutex> lk (_mutex);
            _sleep_condition.wait (lk, [&] { return !bool(condition); });
            lk.unlock ();
        }

        // Function where the thread is parked when it has no work to do
        void idle_loop ()
        {
            while (_alive)
            {
                std::unique_lock<Mutex> lk (_mutex);

                _searching = false;

                while (   _alive
                       && !_searching)
                {
                    _sleep_condition.notify_one (); // Wake up main thread if needed
                    _sleep_condition.wait (lk);
                }

                lk.unlock ();

                if (_alive)
                {
                    search ();
                }
            }
        }

        virtual void search ();
    };

    // MainThread class is derived class used to characterize the the main one
    class MainThread
        : public Thread
    {
    public:
        MainThread () = default;
        MainThread (const MainThread&) = delete;
        MainThread& operator= (const MainThread&) = delete;

        virtual void search () override;
    };

    // ThreadPool class handles all the threads related stuff like
    // - initializing & deinitializing
    // - starting
    // - parking
    // - launching.
    // All the access to shared thread data is done through this.
    class ThreadPool
        : public std::vector<Thread*>
    {
    private:
        StateListPtr _states;

    public:
        u16   pv_limit    = 1;

        bool  easy_played = false;
        bool  failed_low  = false;
        double best_move_change = 0.0;
        Move  easy_move   = MOVE_NONE;
        Value last_value  = VALUE_NONE;

        TimeManager     time_mgr;
        MoveManager     move_mgr;
        SkillManager    skill_mgr;

        ThreadPool () = default;
        ThreadPool (const ThreadPool&) = delete;
        ThreadPool& operator= (const ThreadPool&) = delete;

        MainThread* main_thread () const
        {
            static auto *main_th = static_cast<MainThread*> (at (0));
            return main_th;
        }
        Thread* best_thread () const;
        u64  nodes () const;

        void reset_counts ();
        void clear ();
        void configure (u32 threads);

        void start_thinking (Position &root_pos, StateListPtr &states, const Limit &limits);
        void wait_while_thinking ();

        // No constructor and destructor, Threadpool rely on globals
        // that should be initialized and valid during the whole thread lifetime.
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
inline std::ostream& operator<< (std::ostream &os, OutputState state)
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
extern Threading::ThreadPool  Threadpool;

#endif // _THREAD_H_INC_
