#ifndef _EVALUATOR_H_INC_
#define _EVALUATOR_H_INC_

#include "Type.h"

#include "Position.h"

namespace Evaluator {

    // Tempo bonus
    const Value Tempo = Value(20);

    extern Value evaluate (const Position &pos);
    
    extern std::string trace (const Position &pos);
}

#endif // _EVALUATOR_H_INC_
