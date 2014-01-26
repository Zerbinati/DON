#include <iostream>
#include "xstring.h"

#include "Engine.h"
//#include "LeakDetector.h"

using namespace std;

namespace {

    string args_str (size_t argc, const char* const argv[])
    {
        string args = "";
        for (size_t i = 1; i < argc; ++i)
        {
            args += whitespace (args) ? string (argv[i]) : " " + string (argv[i]);
        }
        return args;
    }

}

void main (int32_t argc, const char* const argv[])
{
    string args = args_str (argc, argv);

    Engine::run (args);
    //system ("pause");
    //atexit (report_leak);
    Engine::exit (EXIT_SUCCESS);
}
