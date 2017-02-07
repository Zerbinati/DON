#ifndef _ZOBRIST_H_INC_
#define _ZOBRIST_H_INC_

#include "Type.h"

class Position;

namespace Zobrists {

    class Zobrist
    {
    public:
        // 2*6*64 + 2*2 + 8 + 1 + 1
        //    768 +   4 + 8 + 1 + 1
        //                  781 + 1
        Key piece_square_keys[CLR_NO][NONE][SQ_NO];  // [color][piece-type][square]
        Key castle_right_keys[CLR_NO][CS_NO];        // [color][castle-side]
        Key en_passant_keys  [F_NO];                 // [enpassant file]
        Key color_key;                               // color

        Key no_pawn_key;

        Zobrist () = default;
        Zobrist (const Zobrist&) = delete;
        Zobrist& operator= (const Zobrist&) = delete;

        // Hash key of the material situation.
        Key compute_matl_key (const Position &pos) const;
        // Hash key of the pawn structure.
        Key compute_pawn_key (const Position &pos) const;
        // Hash key of the complete position.
        Key compute_posi_key (const Position &pos) const;
        //// Hash key of the FEN
        //Key compute_fen_key (const std::string &fen) const;
    };

    extern void initialize ();
}

extern Zobrists::Zobrist Zob;
extern const Zobrists::Zobrist PolyZob;

#endif // _ZOBRIST_H_INC_
