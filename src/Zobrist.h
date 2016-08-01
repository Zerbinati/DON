#ifndef _ZOBRIST_H_INC_
#define _ZOBRIST_H_INC_

#include "Type.h"

class Position;

namespace Zobrists {

    // Zobrist class
    class Zobrist
    {
    public:
        static Key ExclusionKey;

        // 2*6*64 + 2*2 + 8 + 1
        //=   768 +   4 + 8 + 1
        //=                 781
        Key piece_square[CLR_NO][NONE][SQ_NO];  // [color][piece-type][square]
        Key castle_right[CLR_NO][CS_NO];        // [color][castle-side]
        Key en_passant  [F_NO];                 // [enpassant file]
        Key active_color;                       // color

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
// Random Zobrist filled with randoms, used to compute position key
extern Zobrists::Zobrist Zob;
// Random Zobrist from Polyglot, used to compute polyglot book hash key
extern Zobrists::Zobrist PolyZob;

#endif // _ZOBRIST_H_INC_
