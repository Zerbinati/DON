#pragma once

#include "Position.h"
#include "Type.h"

// Tempo bonus
constexpr Value Tempo = Value(28);

extern Value evaluate(Position const&);

extern std::string trace(Position const&);
