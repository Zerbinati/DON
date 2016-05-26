#ifndef _PIECE_SQUARE_H_INC_
#define _PIECE_SQUARE_H_INC_

#include "Type.h"

class Position;

namespace PieceSquare {

    extern Score PSQ[CLR_NO][NONE][SQ_NO];

    extern Score compute_psq_score (const Position &pos);

    extern void initialize ();

}

#endif // _PIECE_SQUARE_H_INC_
