#ifndef _PST_H_INC_
#define _PST_H_INC_

#include "Type.h"

class Position;

// Piece Square Table
class PieceSquareTable
{
private:

    static const Score PSQ_Bonus[NONE][R_NO][F_NO/2];

public:

    static Score PSQ[CLR_NO][NONE][SQ_NO];


    static void initialize ();

    Score compute_psq_score (const Position &pos) const;

};

extern PieceSquareTable PSQTable;

#endif // _PST_H_INC_
