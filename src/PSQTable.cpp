#include "PSQTable.h"

#include "Position.h"

namespace {

#   define S(mg, eg) mk_score (mg, eg)
    // HPSQ[piece-type][rank][file/2] table contains half Piece-Square scores.
    // It is defined for files A..D and white side,
    // It is symmetric for second half of the files and negative for black side.
    // For each piece type on a given square a (midgame, endgame) score pair is assigned.
    const Score HPSQ[NONE][R_NO][F_NO/2] =
    {
        { // Pawn
            { S(   0,   0), S(  0,  0), S(  0,  0), S(  0,  0) },
            { S( -11,  -3), S(  7, -1), S(  7,  7), S( 17,  2) },
            { S( -16,  -2), S( -3,  2), S( 23,  6), S( 23, -1) },
            { S( -14,   7), S( -7, -4), S( 20, -8), S( 24,  2) },
            { S(  -5,  13), S( -2, 10), S( -1, -1), S( 12, -8) },
            { S( -11,  16), S(-12,  6), S( -2,  1), S(  4, 16) },
            { S(  -2,   1), S( 20,-12), S(-10,  6), S( -2, 25) },
            { S(   0,   0), S(  0,  0), S(  0,  0), S(  0,  0) }
        },
        { // Knight
            { S(-161,-105), S(-96,-82), S(-80,-46), S(-73,-14) },
            { S( -83, -69), S(-43,-54), S(-21,-17), S(-10,  9) },
            { S( -71, -50), S(-22,-39), S(  0, -7), S(  9, 28) },
            { S( -25, -41), S( 18,-25), S( 43,  6), S( 47, 38) },
            { S( -26, -46), S( 16,-25), S( 38,  3), S( 50, 40) },
            { S( -11, -54), S( 37,-38), S( 56, -7), S( 65, 27) },
            { S( -63, -65), S(-19,-50), S(  5,-24), S( 14, 13) },
            { S(-195,-109), S(-67,-89), S(-42,-50), S(-29,-13) }
        },
        { // Bishop
            { S( -49, -58), S( -7,-31), S(-10,-37), S(-34,-19) },
            { S( -24, -34), S(  9, -9), S( 15,-14), S(  1,  4) },
            { S(  -9, -23), S( 22,  0), S( -3, -3), S( 12, 16) },
            { S(   4, -26), S(  9, -3), S( 18, -5), S( 40, 16) },
            { S(  -8, -26), S( 27, -4), S( 13, -7), S( 30, 14) },
            { S( -17, -24), S( 14, -2), S( -6,  0), S(  6, 13) },
            { S( -19, -34), S(-13,-10), S(  7,-12), S(-11,  6) },
            { S( -47, -55), S( -7,-32), S(-17,-36), S(-29,-17) }
        },
        { // Rook
            { S( -25,   0), S(-16,  0), S(-16,  0), S( -9,  0) },
            { S( -21,   0), S( -8,  0), S( -3,  0), S(  0,  0) },
            { S( -21,   0), S( -9,  0), S( -4,  0), S(  2,  0) },
            { S( -22,   0), S( -6,  0), S( -1,  0), S(  2,  0) },
            { S( -22,   0), S( -7,  0), S(  0,  0), S(  1,  0) },
            { S( -21,   0), S( -7,  0), S(  0,  0), S(  2,  0) },
            { S( -12,   0), S(  4,  0), S(  8,  0), S( 12,  0) },
            { S( -23,   0), S(-15,  0), S(-11,  0), S( -5,  0) }
        },
        { // Queen
            { S(   0, -71), S( -4,-56), S( -3,-42), S( -1,-29) },
            { S(  -4, -56), S(  6,-30), S(  9,-21), S(  8, -5) },
            { S(  -2, -39), S(  6,-17), S(  9, -8), S(  9,  5) },
            { S(  -1, -29), S(  8, -5), S( 10,  9), S(  7, 19) },
            { S(  -3, -27), S(  9, -5), S(  8, 10), S(  7, 21) },
            { S(  -2, -40), S(  6,-16), S(  8,-10), S( 10,  3) },
            { S(  -2, -55), S(  7,-30), S(  7,-21), S(  6, -6) },
            { S(  -1, -74), S( -4,-55), S( -1,-43), S(  0,-30) }
        },
        { // King
            { S( 272,   0), S(325, 41), S(273, 80), S(190, 93) },
            { S( 277,  57), S(305, 98), S(241,138), S(183,131) },
            { S( 198,  86), S(253,138), S(168,165), S(120,173) },
            { S( 169, 103), S(191,152), S(136,168), S(108,169) },
            { S( 145,  98), S(176,166), S(112,197), S( 69,194) },
            { S( 122,  87), S(159,164), S( 85,174), S( 36,189) },
            { S(  87,  40), S(120, 99), S( 64,128), S( 25,141) },
            { S(  64,   5), S( 87, 60), S( 49, 75), S(  0, 75) }
        }
    };
#   undef S
}

Score PSQ[CLR_NO][NONE][SQ_NO];

/// Computes the scores for the middle game and the endgame.
/// These functions are used to initialize the scores when a new position is set up,
/// and to verify that the scores are correctly updated by do_move and undo_move when the program is running in debug mode.
Score compute_psq (const Position &pos)
{
    auto psq = SCORE_ZERO;
    for (auto c : { WHITE, BLACK })
    {
        for (auto pt : { PAWN, NIHT, BSHP, ROOK, QUEN, KING })
        {
            for (auto s : pos.squares[c][pt])
            {
                psq += PSQ[c][pt][s];
            }
        }
    }
    return psq;
}

/// psq_initialize() initializes psq lookup tables.
void psq_initialize ()
{
    for (auto pt : { PAWN, NIHT, BSHP, ROOK, QUEN, KING })
    {
        const auto p = mk_score (PieceValues[MG][pt], PieceValues[EG][pt]);
        for (auto s : SQ)
        {
            const auto psq = p + HPSQ[pt][_rank (s)][std::min (_file (s), ~_file (s))];
            PSQ[WHITE][pt][ s] = +psq;
            PSQ[BLACK][pt][~s] = -psq;
        }
    }
}
