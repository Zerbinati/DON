#include "Engine.h"

#include "UCI.h"
#include "PieceSquare.h"
#include "BitBases.h"
#include "Zobrist.h"
#include "Pawns.h"
#include "Material.h"
#include "Evaluator.h"
#include "Endgame.h"
#include "Thread.h"
#include "Searcher.h"
#include "TBsyzygy.h"
#include "Transposition.h"

namespace Engine {

    using namespace std;

    namespace {

        // Version number. If Version is left empty, then show compile date in the format DD-MM-YY.
        const string Version = "";

        i32 month_index (const string &month)
        {
            static const i08 Months = 12;
            static const string MonthStr[Months] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

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

    string info (bool uci)
    {
        ostringstream oss;

        oss << (uci ? "id name " : "") << "DON ";

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

        oss << (uci ? "\nid author " : " by ") << "Ehsan Rashid";

        return oss.str ();
    }

    void run (i32 argc, const char *const *argv)
    {
        std::cout << info (false) << std::endl;
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
        Evaluator::initialize ();
        EndGame  ::initialize ();
        Threadpool.initialize ();
        Searcher ::initialize ();
        TBSyzygy ::initialize ();

        TT.auto_resize (i32(Options["Hash"]), true);

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