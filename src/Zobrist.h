#ifndef _ZOBRIST_H_INC_
#define _ZOBRIST_H_INC_

#include "Type.h"

class Position;

/// Zobrist
class Zobrist
{
public:
    // 2*6*64 + 4 + 8 + 1 = 781
    Key piece_square[CLR_NO][NONE][SQ_NO];  // [color][piece-type][square]
    Key castle_right[CR_NO];                // [castle-right]
    Key enpassant   [F_NO];                 // [enpassant file]
    Key color;                              // color

    Zobrist () = default;
    Zobrist (const Zobrist&) = delete;
    Zobrist& operator= (const Zobrist&) = delete;

    Key compute_matl_key (const Position&) const;
    Key compute_pawn_key (const Position&) const;
    Key compute_posi_key (const Position&) const;
    //Key compute_fen_key (const std::string&) const;
};

extern void zobrist_initialize ();

extern Zobrist RandZob;
extern Zobrist const PolyZob;

#endif // _ZOBRIST_H_INC_
