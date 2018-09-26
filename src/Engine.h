#ifndef _ENGINE_H_INC_
#define _ENGINE_H_INC_

#include "Type.h"

const std::string Name = "DON";
// Version number. If version is left empty, then show compile date in the format YY-MM-DD.
const std::string Version = "";
const std::string Author = "Ehsan Rashid";

extern std::string info ();

extern void run (int, const char *const *);
extern void stop (int);

#endif // _ENGINE_H_INC_
