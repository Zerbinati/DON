#ifndef _THREAD_H_INC_
#define _THREAD_H_INC_

#include <bitset>
#include <thread>
#include <iostream>

#include "thread_win32.h"
#include "Position.h"
#include "Pawns.h"
#include "Material.h"
#include "Searcher.h"
#include "MovePicker.h"

namespace Threading {

    const u16 MaxThreads = 128; // Maximum Threads

    // Thread struct keeps together all the thread related stuff like.
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

        std::atomic_bool reset_check { false };

        Thread ();
        virtual ~Thread ();

        // Thread::start_searching() wake up the thread that will start the search
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
        // Thread::wait_while_searching() wait on sleep condition until not searching
        void wait_while_searching ()
        {
            std::unique_lock<Mutex> lk (_mutex);
            _sleep_condition.wait (lk, [&] { return !_searching; });
            lk.unlock ();
        }

        // Thread::wait_until() set the thread to sleep until 'condition' turns true
        void wait_until (const std::atomic_bool &condition)
        {
            std::unique_lock<Mutex> lk (_mutex);
            _sleep_condition.wait (lk, [&] { return bool(condition); });
            lk.unlock ();
        }
        // Thread::wait_while() set the thread to sleep until 'condition' turns false
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

    extern u08  MaximumMoveHorizon;
    extern u08  ReadyMoveHorizon;
    extern u32  OverheadClockTime;
    extern u32  OverheadMoveTime;
    extern u32  MinimumMoveTime;
    extern u32  MoveSlowness;
    extern u32  NodesTime;
    extern bool Ponder;

    // TimeManager class computes the optimal time to think depending on the
    // maximum available time, the move game number and other parameters.
    // Support four different kind of time controls, passed in 'limits':
    //
    // moves_to_go = 0, increment = 0 means: x basetime [sudden death!]
    // moves_to_go = 0, increment > 0 means: x basetime + z increment
    // moves_to_go > 0, increment = 0 means: x moves in y basetime [regular clock]
    // moves_to_go > 0, increment > 0 means: x moves in y basetime + z increment
    class TimeManager
    {
    private:

        TimePoint   _optimum_time = 0;
        TimePoint   _maximum_time = 0;

        double      _instability_factor = 1.0;

    public:

        u64     available_nodes  = U64(0); // When in 'nodes as time' mode
        double  best_move_change = 0.0;

        TimePoint available_time () const { return TimePoint(_optimum_time * _instability_factor * 1.016); }

        TimePoint maximum_time () const { return _maximum_time; }

        TimePoint elapsed_time () const;

        void instability () { _instability_factor = 1.0 + best_move_change; }

        void initialize (Color c, i16 ply);

        void update (Color c);
    };

    // EasyMoveManager class is used to detect a so called 'easy move'; when PV is
    // stable across multiple search iterations engine can fast return the best move.
    class EasyMoveManager
    {
    public:
        static const u08 LineSize = 3;
    private:
        Key  _posi_key = U64(0);
        Move _pv[LineSize];

    public:
        u08 stable_count = 0; // Keep track of how many times in a row pv remains stable

        EasyMoveManager () { clear (); }

        void clear ()
        {
            stable_count = 0;
            _posi_key = U64(0);
            std::fill (_pv, _pv + LineSize, MOVE_NONE);
        }

        Move easy_move (const Key posi_key) const
        {
            return posi_key == _posi_key ? _pv[LineSize-1] : MOVE_NONE;
        }

        void update (Position &pos, const MoveVector &pv)
        {
            assert(pv.size () >= LineSize);
            if (std::equal (pv.begin (), pv.begin () + LineSize, _pv))
            {
                ++stable_count;
            }
            else
            {
                stable_count = 0;
                std::copy (pv.begin (), pv.begin () + LineSize, _pv);

                StateInfo si[LineSize-1];
                for (u08 i = 0; i < LineSize-1; ++i)
                {
                    pos.do_move (pv[i], si[i], pos.gives_check (pv[i], CheckInfo (pos)));
                }
                _posi_key = pos.posi_key ();
                for (u08 i = 0; i < LineSize-1; ++i)
                {
                    pos.undo_move ();
                }
            }
        }

    };

    // MainThread class is derived class used to characterize the the main one
    class MainThread
        : public Thread
    {
    public:
        bool easy_played    = false;
        bool failed_low     = false;
        bool time_mgr_used  = false;

        TimeManager     time_mgr;
        EasyMoveManager easy_move_mgr;

        virtual void search () override;
    };

    // ThreadPool struct handles all the threads related stuff like
    // - initializing
    // - starting
    // - parking
    // - launching a thread.
    // All the access to shared thread data is done through this.
    class ThreadPool
        : public std::vector<Thread*>
    {
    public:
        ThreadPool () = default;

        MainThread* main () const { return static_cast<MainThread*> (at (0)); }

        // No constructor and destructor, threadpool rely on globals
        // that should be initialized and valid during the whole thread lifetime.
        void initialize ();
        void deinitialize ();

        void start_thinking (const Position &pos, const Searcher::Limit &limit, StateStackPtr &states);
        void wait_while_thinking ();
        u64  game_nodes ();

        void configure ();
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
