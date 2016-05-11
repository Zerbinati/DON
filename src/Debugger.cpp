#include "Debugger.h"
#include "Thread.h"

namespace Debugger {

    using namespace std;

    namespace {

        u64 CondCount   = 0;
        u64 HitCount    = 0;

        u64 ItemCount   = 0;
        i64 ItemSum     = 0;
    }

    void dbg_hit_on (bool hit)
    {
        static Mutex mutex;

        mutex.lock ();
        ++CondCount;
        if (hit)
        {
            ++HitCount;
        }
        mutex.unlock ();
    }

    void dbg_hit_on (bool cond, bool hit)
    {
        if (cond)
        {
            dbg_hit_on (hit);
        }
    }

    void dbg_mean_of (i64 item)
    {
        static Mutex mutex;

        mutex.lock ();
        ++ItemCount;
        ItemSum += item;
        mutex.unlock ();
    }

    void dbg_print ()
    {
        if (CondCount != 0)
        {
            std::cerr << std::right
                << "---------------------------\n"
                << "Cond  :" << std::setw (20) << CondCount << "\n"
                << "Hit   :" << std::setw (20) << HitCount  << "\n"
                << "Rate  :" << std::setw (20) << std::setprecision (2) << std::fixed << (double) HitCount / CondCount * 100.0
                << std::left << std::endl;
        }

        if (ItemCount != 0)
        {
            std::cerr << std::right
                << "---------------------------\n"
                << "Count :" << std::setw (20) << ItemCount << "\n"
                << "Sum   :" << std::setw (20) << ItemSum   << "\n"
                << "Mean  :" << std::setw (20) << std::setprecision (2) << std::fixed << (double) ItemSum / ItemCount
                << std::left << std::endl;
        }
    }

}
