#include "Debugger.h"

namespace Debugger {

    using namespace std;

    namespace {

        u64 Hits[2],
            Mean[2];
    }

    void dbg_hits_on (bool h, bool c)
    {
        if (c) { ++Hits[0]; if (h) ++Hits[1]; }
    }
    void dbg_mean_of (u64 v)
    {
        ++Mean[0]; Mean[1] += v;
    }

    void dbg_print ()
    {
        if (Hits[0] != U64(0))
        {
            std::cout
                << "Total: "  << setw (4) << Hits[0]
                << " Hits: " << setw (4) << Hits[1]
                << " Hit-rate (%): " << setw (4) << setprecision (2) << fixed << 100.0 * Hits[1] / Hits[0]
                << std::endl;
        }

        if (Mean[0] != U64(0))
        {
            std::cout
                << "Total: "  << setw (4) << Mean[0]
                << " Mean: " << setw (4) << setprecision (2) << fixed << (double) Mean[1] / Mean[0]
                << std::endl;
        }
    }

}
