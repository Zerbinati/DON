#ifndef _EVALUATOR_H_INC_
#define _EVALUATOR_H_INC_

#include "Position.h"
#include "Type.h"

namespace Evaluator {

    // Tempo bonus
    const Value Tempo = Value(20);

    extern Value evaluate (const Position&);
    
    extern std::string trace_eval (const Position&);
}

#endif // _EVALUATOR_H_INC_
