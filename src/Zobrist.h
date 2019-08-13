#ifndef _ZOBRIST_H_INC_
#define _ZOBRIST_H_INC_

#include <array>
#include "Type.h"

class Position;

/// Zobrist
class Zobrist
{
public:
    // 2*6*64 + 16 + 8 + 1 = 793
    //Key piece_square[CLR_NO][NONE][SQ_NO];
    std::array<std::array<std::array<Key, SQ_NO>, NONE>, CLR_NO> piece_square;
    std::array<Key, CR_NO> castle_right;
    std::array<Key, F_NO> enpassant;
    Key color;

    Zobrist() = default;
    Zobrist(Zobrist const&) = delete;
    Zobrist& operator=(Zobrist const&) = delete;

    Key compute_matl_key(Position const&) const;
    Key compute_pawn_key(Position const&) const;
    Key compute_posi_key(Position const&) const;
    //Key compute_fen_key (std::string const&) const;
};

extern void zobrist_initialize();

extern Zobrist RandZob;
extern Zobrist const PolyZob;

#endif // _ZOBRIST_H_INC_
