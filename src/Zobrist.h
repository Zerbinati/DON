#ifndef _ZOBRIST_H_INC_
#define _ZOBRIST_H_INC_

#include "Type.h"

class Position;

/// Zobrist
class Zobrist
{
public:
    // 2*6*64 + 2*2 + 8 + 1
    //    768 +   4 + 8 + 1
    //                  781
    Key piece_square_keys[+Color::NO][+PieceType::NONE][+Square::NO];	// [color][piece-type][square]
    Key castle_right_keys[+Color::NO][+CastleSide::NO];				// [color][castle-side]
    Key en_passant_keys  [+File::NO];					// [enpassant file]
    Key color_key;										// color

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
extern const Zobrist PolyZob;

#endif // _ZOBRIST_H_INC_
