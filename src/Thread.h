#ifndef _THREAD_H_INC_
#define _THREAD_H_INC_

#include <bitset>
#include <vector>
#include <mutex>
#include <condition_variable>
#include <thread>

#include "Position.h"
#include "Pawns.h"
#include "Material.h"
#include "MovePicker.h"
#include "Searcher.h"

namespace Threads {

    using namespace Searcher;

    const u08   MaxThreads           = 128; // Maximum threads
    const u08   MaxSplitPointThreads =   8; // Maximum threads per splitpoint
    const u08   MaxSplitDepth        =  15; // Maximum split depth
    
    extern void check_time ();
    extern void auto_save_hash ();

    template<class T>
    extern T* new_thread ();
    template<class T>
    extern void delete_thread (T *th);

    class Thread;

    // SplitPoint struct
    struct SplitPoint
    {

    public:
        // Const data after splitpoint has been setup
        const Stack    *ss;
        const Position *pos;

        Thread *master;
        Value   beta;
        Depth   depth;
        NodeT   node_type;
        bool    cut_node;
        std::mutex   mutex;

        // Const pointers to shared data
        MovePicker  *movepicker;
        SplitPoint  *parent_splitpoint;

        // Shared data
        std::bitset<MaxThreads> slaves_mask;
        volatile bool  slave_searching;
        volatile u08   legals;
        volatile Value alpha;
        volatile Value best_value;
        volatile Move  best_move;
        volatile u64   nodes;
        volatile bool  cut_off;
    };

    // ThreadBase class is the base of the hierarchy from where
    // derive all the specialized thread classes.
    class ThreadBase
    {
    protected:
        std::condition_variable sleep_condition;
        volatile bool exit;

    public:
        std::mutex   mutex;
        std::thread  native_thread;

        ThreadBase ()
            : exit (false)
        {}

        virtual ~ThreadBase () {}

        void quit () { exit = true; }
        void notify_one ();

        void wait_for (const volatile bool &condition);
      
        virtual void idle_loop () = 0;
    };

    const i32 TimerResolution = 5;

    // TimerThread is derived from ThreadBase class
    // It's used for special purpose: the recurring timer.
    class TimerThread
        : public ThreadBase
    {
    private:

    public:
        // This is the minimum interval in msec between two check_time() calls
        bool run;
        i32 resolution;
        void (*task) ();

        TimerThread () : run (false) {}

        void start () { run = true ; }
        void stop  () { run = false; }

        virtual void idle_loop ();

    };

    // Thread is derived from ThreadBase class
    // Thread class keeps together all the thread related stuff like locks, state
    // and especially splitpoints. Also use per-thread pawn-hash and material-hash tables
    // so that once get a pointer to a thread entry its life time is unlimited
    // and don't have to care about someone changing the entry under our feet.
    class Thread
        : public ThreadBase
    {

    public:
        SplitPoint splitpoints[MaxSplitPointThreads];
        
        Material::Table  material_table;
        Pawns   ::Table  pawns_table;

        Position *active_pos;

        u08     idx;

        SplitPoint* volatile active_splitpoint;
        volatile    u08      splitpoint_threads;
        volatile    bool     searching;

        Thread ();

        virtual void idle_loop ();

        bool cutoff_occurred () const;

        bool available_to (const Thread *master) const;

        void split (Position &pos, const Stack *ss, Value alpha, Value beta, Value &best_value, Move &best_move,
            Depth depth, u08 legals, MovePicker &movepicker, NodeT node_type, bool cut_node);

    };

    // MainThread is derived from Thread
    // It's used for special purpose: the main thread.
    class MainThread
        : public Thread
    {

    public:
        volatile bool thinking;

        MainThread () : thinking (true) {} // Avoid a race with start_thinking()

        virtual void idle_loop ();

    };

    // ThreadPool class handles all the threads related stuff like initializing,
    // starting, parking and, the most important, launching a slave thread
    // at a splitpoint.
    // All the access to shared thread data is done through this class.
    class ThreadPool
        : public std::vector<Thread*>
    {

    public:
        std::mutex   mutex;
        std::condition_variable   sleep_condition;

        TimerThread *timer;
        TimerThread *auto_save;

        Depth   split_depth;
        u08     max_ply;
        
        MainThread* main () { return static_cast<MainThread*> ((*this)[0]); }

        // No c'tor and d'tor, threads rely on globals that should
        // be initialized and valid during the whole thread lifetime.
        void   initialize ();
        void deinitialize ();

        void configure ();

        Thread* available_slave (const Thread *master) const;

        void start_thinking (const Position &pos, const LimitsT &limit, StateInfoStackPtr &states);

        void wait_for_think_finished ();

    };

}

inline u32 cpu_count ()
{
#if __cplusplus > 199711L
    // May return 0 when not able to detect
    return std::thread::hardware_concurrency ();

#else    

#   ifdef WIN32

    SYSTEM_INFO sys_info;
    GetSystemInfo (&sys_info);
    return sys_info.dwNumberOfProcessors;

#   elif MACOS

    u32 count;
    u32 len = sizeof (count);

    i32 nm[2];
    nm[0] = CTL_HW;
    nm[1] = HW_AVAILCPU;
    sysctl (nm, 2, &count, &len, nullptr, 0);
    if (count < 1)
    {
        nm[1] = HW_NCPU;
        sysctl (nm, 2, &count, &len, nullptr, 0);
        if (count < 1) count = 1;
    }
    return count;

#   elif _SC_NPROCESSORS_ONLN // LINUX, SOLARIS, & AIX and Mac OS X (for all OS releases >= 10.4)

    return sysconf (_SC_NPROCESSORS_ONLN);

#   elif __IRIX

    return sysconf (_SC_NPROC_ONLN);

#   elif __HPUX

    pst_dynamic psd;
    return (pstat_getdynamic (&psd, sizeof (psd), 1, 0) == -1)
        ? 1 : psd.psd_proc_cnt;

    //return mpctl (MPC_GETNUMSPUS, nullptr, nullptr);

#   else

    return 1;

#   endif

#endif
}

// Used to serialize access to std::cout to avoid multiple threads writing at the same time.
inline std::ostream& operator<< (std::ostream &os, const SyncT &sync)
{
    static std::mutex mutex;

    if (sync == IO_LOCK)
    {
        mutex.lock ();
    }
    else
    if (sync == IO_UNLOCK)
    {
        mutex.unlock ();
    }
    return os;
}

extern Threads::ThreadPool  Threadpool;

#endif // _THREAD_H_INC_
