#ifndef _PSQT_H_INC_
#define _PSQT_H_INC_

#include <array>
#include "Type.h"

class Position;

extern std::array<std::array<Score, SQ_NO>, MAX_PIECE> PSQ;

extern Score compute_psq(Position const&);

extern void psq_initialize();

#endif // _PSQT_H_INC_
