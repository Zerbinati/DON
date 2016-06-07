#ifndef _EVALUATOR_H_INC_
#define _EVALUATOR_H_INC_

#include "Type.h"

#include "Position.h"

namespace Evaluator {

    // Tempo bonus
    const Value Tempo = Value(20);

    template<bool Trace = false>
    // evaluate<>() is the main evaluation function.
    // It returns a static evaluation of the position from the point of view of the side to move.
    extern Value evaluate    (const Position &pos);
    // trace() is like evaluate() but instead of a value returns a string
    // (suitable to be print on stdout) that contains the detailed descriptions
    // and values of each evaluation term. Used for debugging.
    extern std::string trace (const Position &pos);
    // initialize() init tables
    extern void initialize ();
}

#endif // _EVALUATOR_H_INC_
