#ifndef _ENGINE_H_INC_
#define _ENGINE_H_INC_

#include "Type.h"

namespace Engine {

    extern std::string info (bool uci = false);

    extern void run (i32 argc, const char *const *argv);

    // Exit from engine with exit code. (in case of some crash)
    extern void stop (i32 code);

}

#endif // _ENGINE_H_INC_
