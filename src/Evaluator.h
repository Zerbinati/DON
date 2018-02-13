#ifndef _EVALUATOR_H_INC_
#define _EVALUATOR_H_INC_

#include <atomic>
#include "Position.h"
#include "Type.h"

// Tempo bonus
const Value Tempo = Value(20);

extern std::atomic<Score> Contempt;

extern Value evaluate (const Position&);

extern std::string trace (const Position&);

#endif // _EVALUATOR_H_INC_
