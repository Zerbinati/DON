#ifndef _EVALUATOR_H_INC_
#define _EVALUATOR_H_INC_

#include "Position.h"
#include "Type.h"

// Tempo bonus
constexpr Value Tempo = Value(28);

extern Value evaluate(Position const&);

extern std::string trace(Position const&);

#endif // _EVALUATOR_H_INC_
