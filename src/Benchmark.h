#ifndef _BENCHMARK_H_INC_
#define _BENCHMARK_H_INC_

#include <istream>
#include "Position.h"

extern void benchmark (std::istringstream&, const Position&);

extern void perft (std::istringstream&, const Position&);

#endif // _BENCHMARK_H_INC_
