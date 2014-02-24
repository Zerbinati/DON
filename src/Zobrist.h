//#pragma once
#ifndef ZOBRIST_H_
#define ZOBRIST_H_

#include "Type.h"
#include <string>

class RKISS;
class Position;

namespace Zobrist {

    // 2*6*64 + 2*2 + 8 + 1
    //    768 +   4 + 8 + 1
    //                  781
    const uint16_t SIZE_RANDOM = 781;

    extern const Key MATL_KEY_PG; // = U64 (0xB76D8438E5D28230);
    extern const Key PAWN_KEY_PG; // = U64 (0x37FC40DA841E1692);
    extern const Key POSI_KEY_PG; // = U64 (0x463B96181691FC9C);

    extern RKISS Rkiss;
    extern Key   Exclusion;

    // Zobrist Random numbers
    typedef union Zob
    {
    public:
        Key random[SIZE_RANDOM];

        //CACHE_ALIGN (64)
        struct _
        {
            Key piecesq[CLR_NO][NONE][SQ_NO];// [COLOR][PIECE][SQUARE]
            Key castle_right[CLR_NO][CS_NO]; // [COLOR][CASTLE SIDE]
            Key en_passant[F_NO];            // [ENPASSANT FILE]
            Key mover_side;                  // COLOR

        } _;

    public:

        void initialize (RKISS rk);

    public:
        // Hash key of the material situation.
        Key compute_matl_key (const Position &pos) const;
        // Hash key of the pawn structure.
        Key compute_pawn_key (const Position &pos) const;
        // Hash key of the complete position.
        Key compute_posi_key (const Position &pos) const;

        // Hash key of the FEN
#ifndef NDEBUG
        Key compute_fen_key (const        char *fen, bool c960 = false) const;
#endif
        Key compute_fen_key (const std::string &fen, bool c960 = false) const;

    } Zob;

    extern void initialize ();

}

extern const Zobrist::Zob  ZobPG;
//extern       Zobrist::Zob  ZobRand;
extern const Zobrist::Zob &ZobGlob;

#endif
