#ifndef _PSQT_H_INC_
#define _PSQT_H_INC_

#include "Position.h"
#include "Type.h"

extern Score PST[CLR_NO][NONE][SQ_NO];

extern Score compute_psq (const Position&);

template<Color Own>
extern Value compute_npm (const Position&);

extern void psqt_initialize ();

#endif // _PSQT_H_INC_
