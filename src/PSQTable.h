#ifndef _PSQT_H_INC_
#define _PSQT_H_INC_

#include "Type.h"

class Position;

extern Score PSQ[MAX_PIECE][SQ_NO];

extern Score compute_psq (const Position&);

extern void psq_initialize ();

#endif // _PSQT_H_INC_
