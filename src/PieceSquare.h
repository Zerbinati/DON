#ifndef _PSQ_H_INC_
#define _PSQ_H_INC_

#include "Type.h"

class Position;

namespace PSQTable {

    extern Score PSQ[CLR_NO][NONE][SQ_NO];

    extern Score compute_psq_score (const Position &pos);

    extern void initialize ();

}

#endif // _PSQ_H_INC_
