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
#include "Searcher.h"
#include "MovePicker.h"

extern u08      MaximumMoveHorizon;
extern u08      ReadyMoveHorizon;
extern u32      OverheadClockTime;
extern u32      OverheadMoveTime;
extern u32      MinimumMoveTime;
extern double   MoveSlowness;
extern u32      NodesTime;
extern bool     Ponder;

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
private:

    TimePoint _optimum_time = 0;
    TimePoint _maximum_time = 0;

public:

    u64 available_nodes = U64(0); // When in 'Nodes as Time' mode

    TimeManager () = default;
    TimeManager (const TimeManager&) = delete;
    TimeManager& operator= (const TimeManager&) = delete;

    TimePoint optimum_time () const { return _optimum_time; }
    TimePoint maximum_time () const { return _maximum_time; }
    TimePoint elapsed_time () const;

    void initialize (Color c, i16 ply);

    void update (Color c);
};

// EasyMoveManager class is used to detect a so called 'easy move'.
// When PV is stable across multiple search iterations engine can fast return the best move.
class EasyMoveManager
{
public:
    static const u08 PVSize = 3;

private:
    Key  _posi_key = U64(0);
    Move _pv[PVSize];

public:
    u08 stable_count = 0; // Keep track of how many times in a row the 3rd ply remains stable

    EasyMoveManager ()
    {
        clear ();
    }
    EasyMoveManager (const EasyMoveManager&) = delete;
    EasyMoveManager& operator= (const EasyMoveManager&) = delete;

    void clear ()
    {
        stable_count = 0;
        _posi_key = U64(0);
        std::fill (_pv, _pv + PVSize, MOVE_NONE);
    }

    Move easy_move (const Key posi_key) const
    {
        return posi_key == _posi_key ? _pv[PVSize-1] : MOVE_NONE;
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

            StateInfo si[PVSize-1];
            for (u08 i = 0; i < PVSize-1; ++i)
            {
                pos.do_move (_pv[i], si[i], pos.gives_check (_pv[i], CheckInfo (pos)));
            }
            _posi_key = pos.posi_key ();
            for (u08 i = 0; i < PVSize-1; ++i)
            {
                pos.undo_move ();
            }
        }
    }
};

class SkillManager
{

private:
    u08  _skill_level = MaxSkillLevel;
    Move _best_move   = MOVE_NONE;

public:
    // MaxSkillLevel should be <= MaxPlies/4
    // Skill Manager class is used to implement strength limit
    static const u08 MaxSkillLevel = 32;
    static const u16 MinMultiPV    = 4;

    explicit SkillManager (u08 skill_level = MaxSkillLevel)
        : _skill_level (skill_level)
    {}
    SkillManager (const SkillManager&) = delete;
    SkillManager& operator= (const SkillManager&) = delete;

    void change_skill_level (u08 skill_level)
    {
        _skill_level = skill_level;
    }

    void clear ()
    {
        _best_move = MOVE_NONE;
    }

    bool enabled () const
    {
        return _skill_level < MaxSkillLevel;
    }

    bool can_pick (Depth depth) const
    {
        return depth/DEPTH_ONE == (_skill_level + 1);
    }

    // When playing with a strength handicap, choose best move among the first 'candidates'
    // RootMoves using a statistical rule dependent on 'level'. Idea by Heinz van Saanen.
    Move pick_best_move (const Searcher::RootMoveVector &root_moves, u16 pv_limit)
    {
        assert (!root_moves.empty ());
        static PRNG prng (now ()); // PRNG sequence should be non-deterministic

        if (_best_move == MOVE_NONE)
        {
            // RootMoves are already sorted by value in descending order
            auto weakness   = Value (MaxPlies - 4 * _skill_level);
            auto max_value  = root_moves[0].new_value;
            auto diversity  = std::min (max_value - root_moves[pv_limit - 1].new_value, VALUE_MG_PAWN);
            auto best_value = -VALUE_INFINITE;
            // Choose best move. For each move score add two terms, both dependent on weakness.
            // One is deterministic with weakness, and one is random with diversity.
            // Then choose the move with the resulting highest value.
            for (u16 i = 0; i < pv_limit; ++i)
            {
                auto value = root_moves[i].new_value;
                // This is magic formula for push
                auto push  = (weakness  * i32 (max_value - value)
                              + diversity * i32 (prng.rand<u32> () % weakness)
                             ) / (i32 (VALUE_EG_PAWN) / 2);

                if (best_value < value + push)
                {
                    best_value = value + push;
                    _best_move = root_moves[i][0];
                }
            }
        }
        return _best_move;
    }
};

namespace Threading {

    const u16 MaxThreads = 128; // Maximum Threads

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
        Pawns   ::Table pawn_table;
        Material::Table matl_table;

        u16  index      = 0
           , pv_index   = 0
           , max_ply    = 0
           , chk_count  = 0;

        Position                    root_pos;
        Searcher::RootMoveVector    root_moves;
        Depth                       root_depth = DEPTH_ZERO
            ,                       leaf_depth = DEPTH_ZERO;
        MovePick::HValueStats       history_values;
        MovePick::MoveStats         counter_moves;

        std::atomic_bool            reset_check { false };

        Thread ();
        Thread (const Thread&) = delete;
        Thread& operator= (const Thread&) = delete;

        virtual ~Thread ();

        // Thread::start_searching() wakes up the thread that will start the search
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
        // Thread::wait_while_searching() waits on sleep condition until not searching
        void wait_while_searching ()
        {
            std::unique_lock<Mutex> lk (_mutex);
            _sleep_condition.wait (lk, [&] { return !_searching; });
            lk.unlock ();
        }

        // Thread::wait_until() waits on sleep condition until 'condition' turns true
        void wait_until (const std::atomic_bool &condition)
        {
            std::unique_lock<Mutex> lk (_mutex);
            _sleep_condition.wait (lk, [&] { return bool(condition); });
            lk.unlock ();
        }
        // Thread::wait_while() waits on sleep condition until 'condition' turns false
        void wait_while (const std::atomic_bool &condition)
        {
            std::unique_lock<Mutex> lk (_mutex);
            _sleep_condition.wait (lk, [&] { return !bool(condition); });
            lk.unlock ();
        }

        // Thread::idle_loop() is where the thread is parked when it has no work to do
        void idle_loop ()
        {
            while (_alive)
            {
                std::unique_lock<Mutex> lk (_mutex);

                _searching = false;

                while (_alive && !_searching)
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
        bool   easy_played      = false;
        bool   failed_low       = false;
        bool   time_mgr_used    = false;
        double best_move_change = 0.0;
        Value  previous_value   = +VALUE_INFINITE;

        TimeManager     time_mgr;
        EasyMoveManager easy_move_mgr;
        SkillManager    skill_mgr;

        MainThread ();
        MainThread (const MainThread&) = delete;
        MainThread& operator= (const MainThread&) = delete;
        //virtual ~MainThread ();

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
    public:
        ThreadPool () = default;
        ThreadPool (const ThreadPool&) = delete;
        ThreadPool& operator= (const ThreadPool&) = delete;

        MainThread* main () const
        {
            static auto *main_thread = static_cast<MainThread*> (at (0));

            return main_thread;
        }

        u64  game_nodes () const;

        void configure ();

        // No constructor and destructor, threadpool rely on globals
        // that should be initialized and valid during the whole thread lifetime.
        void initialize ();
        void deinitialize ();

        void start_thinking (const Position &pos, const Limit &limits, StateStackPtr &states);
        void wait_while_thinking ();
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
    default: assert(false);          break;
    }
    return os;
}

#define sync_cout std::cout << OS_LOCK
#define sync_endl std::endl << OS_UNLOCK


extern Threading::ThreadPool  Threadpool;

#endif // _THREAD_H_INC_
