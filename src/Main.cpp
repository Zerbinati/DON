#include "Engine.h"
#include "LeakDetector.h"
#include "PGN.h"

using namespace std;

namespace {

    string strarg (i32 argc, const char *const *argv)
    {
        string arg;
        for (auto i = 1; i < argc; ++i)
        {
            arg += string(argv[i]) + " ";
        }
        return arg;
    }

}

i32 main (i32 argc, const char *const *argv)
{
    /*
    string arg = strarg (argc, argv);
    Engine::run (arg);

    atexit (report_leak);
    Engine::exit (EXIT_SUCCESS);
    */

    PGN pgn("test.pgn", std::ios::in);
    string text;

    text = pgn.read_text (1);
    cout << text << endl;
    text = pgn.read_text (2);
    cout << text << endl;
    text = pgn.read_text (3);
    cout << text << endl;

    system ("PAUSE");
    return EXIT_SUCCESS;
}
