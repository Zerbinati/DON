#ifndef _ENGINE_H_INC_
#define _ENGINE_H_INC_

#include "Type.h"

namespace Engine {

    const std::string Name = "DON";
    // Version number. If version is left empty, then show compile date in the format DD-MM-YY.
    const std::string Version = "";
    const std::string Author = "Ehsan Rashid";

    extern std::string info ();

    extern void run (i32 argc, const char *const *argv);
    extern void stop (i32 code);

}

#endif // _ENGINE_H_INC_
