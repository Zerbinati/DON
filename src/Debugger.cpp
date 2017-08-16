#include "Debugger.h"

#include "Thread.h"

namespace Debugger {

    using namespace std;

    namespace {

        u64 CondCount = 0;
        u64 HitCount = 0;

        u64 ItemCount = 0;
        i64 ItemValueSum = 0;
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

    void dbg_mean_of (i64 item_value)
    {
        static Mutex mutex;

        mutex.lock ();
        ++ItemCount;
        ItemValueSum += item_value;
        mutex.unlock ();
    }

    void dbg_print ()
    {
        if (0 != CondCount)
        {
            std::cerr
                << std::right
                << "---------------------------\n"
                << "Cond  :" << std::setw (20) << CondCount << "\n"
                << "Hit   :" << std::setw (20) << HitCount  << "\n"
                << "Rate  :" << std::setw (20) << std::fixed << std::setprecision (2) << (double) HitCount / CondCount * 100.0
                << std::left
                << std::endl;
        }

        if (0 != ItemCount)
        {
            std::cerr
                << std::right
                << "---------------------------\n"
                << "Count :" << std::setw (20) << ItemCount << "\n"
                << "Sum   :" << std::setw (20) << ItemValueSum << "\n"
                << "Mean  :" << std::setw (20) << std::fixed << std::setprecision (2) << (double) ItemValueSum / ItemCount
                << std::left
                << std::endl;
        }
    }

}
