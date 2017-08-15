#ifndef _PIECE_SQUARE_H_INC_
#define _PIECE_SQUARE_H_INC_

#include "Type.h"

class Position;

namespace PSQT {

    extern Score PSQ[CLR_NO][NONE][SQ_NO];

    extern Score compute_psq (const Position &pos);

    template<Color Own>
    extern Value compute_npm (const Position &pos);

    extern void initialize ();

}

#endif // _PIECE_SQUARE_H_INC_
