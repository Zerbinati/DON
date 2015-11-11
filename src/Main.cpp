#include "LeakDetector.h"
#include "Type.h"
#include "Parser.h"

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
    Parser::parse ();
    
    system ("PAUSE");
    //atexit (report_leak);
    return EXIT_SUCCESS;
}
