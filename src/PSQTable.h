#ifndef _PSQT_H_INC_
#define _PSQT_H_INC_

#include "Type.h"

class Position;

extern Score PSQT[CLR_NO][NONE][SQ_NO];

extern Score compute_psq (const Position&);

extern void psqt_initialize ();

#endif // _PSQT_H_INC_
