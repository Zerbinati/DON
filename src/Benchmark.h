#ifndef _BENCHMARK_H_INC_
#define _BENCHMARK_H_INC_

#include <istream>
#include <vector>

#include "Position.h"

extern std::vector<std::string> setup_bench (std::istringstream&, const Position&);

extern void perft (std::istringstream&, const Position&);

#endif // _BENCHMARK_H_INC_
