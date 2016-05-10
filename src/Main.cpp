#include "Engine.h"

using namespace std;

i32 main (i32 argc, const char *const *argv)
{
    Engine::run (argc, argv);
    Engine::stop (EXIT_SUCCESS);
    return EXIT_SUCCESS;
}
