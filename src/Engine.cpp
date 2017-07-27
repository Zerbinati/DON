#include "Engine.h"

#include "BitBases.h"
#include "Endgame.h"
#include "Material.h"
#include "MemoryHandler.h"
#include "Pawns.h"
#include "PieceSquare.h"
#include "Searcher.h"
#include "TBsyzygy.h"
#include "Thread.h"
#include "UCI.h"
#include "Zobrist.h"

namespace Engine {

    using namespace std;

    namespace {

        i32 month_index (const string &month)
        {
            const i08 Months = 12;
            const string MonthStr[Months] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

            for (auto m = 0; m < Months; ++m)
            {
                if (month == MonthStr[m])
                {
                    return m+1;
                }
            }
            return 0;
        }

    }

    string info ()
    {
        ostringstream oss;

        oss << std::setfill ('0');
    #if defined(VER)
        oss << VER;
    #else
        if (white_spaces (Version))
        {
            // From compiler, format is "Sep 2 1982"
            istringstream iss (__DATE__);
            string month, day, year;
            iss >> month >> day >> year;
            oss << std::setw (2) << day
                << std::setw (2) << month_index (month)
                << std::setw (2) << year.substr (2);
        }
        else
        {
            oss << Version;
        }
    #endif
        oss << std::setfill (' ');

    #if defined(BIT64)
        oss << ".64";
    #else
        oss << ".32";
    #endif

    #if defined(BM2)
        oss << ".BM2";
    #elif defined(ABM)
        oss << ".ABM";
    #elif defined(POP)
        oss << ".POP";
    #endif

    #if defined(LPAGES)
        oss << ".LP";
    #endif

        return oss.str ();
    }

    void run (i32 argc, const char *const *argv)
    {
        std::cout << Name << " " << info () << " by " << Author << std::endl;
        std::cout << "info string Processor(s) detected " << std::thread::hardware_concurrency () << std::endl;

#if defined(LPAGES)
        Memory   ::initialize ();
#endif
        UCI      ::initialize ();
        BitBoard ::initialize ();
        PieceSquare::initialize ();
        Zobrists ::initialize ();
        BitBases ::initialize ();
        Pawns    ::initialize ();
        EndGame  ::initialize ();
        Threadpool.initialize ();
        Searcher ::initialize ();

        UCI::loop (argc, argv);
    }

    void stop (i32 code)
    {
        Threadpool.deinitialize ();
        EndGame  ::deinitialize ();
        UCI      ::deinitialize ();
#if defined(LPAGES)
        Memory   ::deinitialize ();
#endif
        exit (code);
    }

}