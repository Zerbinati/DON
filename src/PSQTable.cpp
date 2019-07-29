#include "PSQTable.h"

#include "Position.h"

namespace {

#   define S(mg, eg) mk_score (mg, eg)
    // PieceHalfSQ[piece-type][rank][file/2] table contains half Piece-Square scores (symmetric distribution).
    // It is defined for files A..D and white side,
    // It is symmetric for second half of the files and negative for black side.
    // For each piece type on a given square a (midgame, endgame) score pair is assigned.
    constexpr Score PieceHalfSQ[NONE][R_NO][F_NO/2] =
    {
        { },
        { // Knight
            { S(-169,-105), S(-96,-74), S(-80,-46), S(-79,-18) },
            { S( -79, -70), S(-39,-56), S(-24,-15), S( -9,  6) },
            { S( -64, -38), S(-20,-33), S(  4, -5), S( 19, 27) },
            { S( -28, -36), S(  5,  0), S( 41, 13), S( 47, 34) },
            { S( -29, -41), S( 13,-20), S( 42,  4), S( 52, 35) },
            { S( -11, -51), S( 28,-38), S( 63,-17), S( 55, 19) },
            { S( -67, -64), S(-21,-45), S(  6,-37), S( 37, 16) },
            { S(-200, -98), S(-80,-89), S(-53,-53), S(-32,-16) }
        },
        { // Bishop
            { S( -44, -63), S( -4,-30), S(-11,-35), S(-28, -8) },
            { S( -18, -38), S(  7,-13), S( 14,-14), S(  3,  0) },
            { S(  -8, -18), S( 24,  0), S( -3, -7), S( 15, 13) },
            { S(   1, -26), S(  8, -3), S( 26,  1), S( 37, 16) },
            { S(  -7, -24), S( 30, -6), S( 23,-10), S( 28, 17) },
            { S( -17, -26), S(  4,  2), S( -1,  1), S(  8, 16) },
            { S( -21, -34), S(-19,-18), S( 10, -7), S( -6,  9) },
            { S( -48, -51), S( -3,-40), S(-12,-39), S(-25,-20) }
        },
        { // Rook
            { S( -24,  -2), S(-13,-6), S(  -7, -3), S(  2, -2) },
            { S( -18, -10), S(-10,-7), S(  -5,  1), S(  9,  0) },
            { S( -21,  10), S( -7,-4), S(   3,  2), S( -1, -2) },
            { S( -13,  -5), S( -5, 2), S(  -4, -8), S( -6,  8) },
            { S( -24,  -8), S(-12, 5), S(  -1,  4), S(  6, -9) },
            { S( -24,   3), S( -4,-2), S(   4,-10), S( 10,  7) },
            { S(  -8,   1), S(  6, 2), S(  10, 17), S( 12, -8) },
            { S( -22,  12), S(-24,-6), S(  -6, 13), S(  4,  7) }
        },
        { // Queen
            { S(   3, -69), S( -5,-57), S( -5,-47), S(  4,-26) },
            { S(  -3, -55), S(  5,-31), S(  8,-22), S( 12, -4) },
            { S(  -3, -39), S(  6,-18), S( 13, -9), S(  7,  3) },
            { S(   4, -23), S(  5, -3), S(  9, 13), S(  8, 24) },
            { S(   0, -29), S( 14, -6), S( 12,  9), S(  5, 21) },
            { S(  -4, -38), S( 10,-18), S(  6,-12), S(  8,  1) },
            { S(  -5, -50), S(  6,-27), S( 10,-24), S(  8, -8) },
            { S(  -2, -75), S( -2,-52), S(  1,-43), S( -2,-36) }
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

    constexpr Score PawnFullSQ[R_NO][F_NO] =
    { // Pawn (asymmetric distribution)
        { S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0) },
        { S(  3,-10), S(  3, -6), S( 10, 10), S( 19,  0), S( 16, 14), S( 19,  7), S(  7, -5), S( -5,-19) },
        { S( -9,-10), S(-15,-10), S( 11,-10), S( 15,  4), S( 32,  4), S( 22,  3), S(  5, -6), S(-22, -4) },
        { S( -8,  6), S(-23, -2), S(  6, -8), S( 20, -4), S( 40,-13), S( 17,-12), S(  4,-10), S(-12, -9) },
        { S( 13,  9), S(  0,  4), S(-13,  3), S(  1,-12), S( 11,-12), S( -2, -6), S(-13, 13), S(  5,  8) },
        { S( -5, 28), S(-12, 20), S( -7, 21), S( 22, 28), S( -8, 30), S( -5,  7), S(-15,  6), S(-18, 13) },
        { S( -7,  0), S(  7,-11), S( -3, 12), S(-13, 21), S(  5, 25), S(-16, 19), S( 10,  4), S( -8,  7) },
        { S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0) }
    };

#   undef S
}

std::array<std::array<Score, SQ_NO>, MAX_PIECE> PSQ;

/// Computes the scores for the middle game and the endgame.
/// These functions are used to initialize the scores when a new position is set up,
/// and to verify that the scores are correctly updated by do_move and undo_move when the program is running in debug mode.
Score compute_psq (const Position &pos)
{
    auto psq = SCORE_ZERO;
    for (const auto &pc : { W_PAWN, W_NIHT, W_BSHP, W_ROOK, W_QUEN, W_KING,
                            B_PAWN, B_NIHT, B_BSHP, B_ROOK, B_QUEN, B_KING })
    {
        for (const auto &s : pos.squares[pc])
        {
            psq += PSQ[pc][s];
        }
    }
    return psq;
}

/// psq_initialize() initializes psq lookup tables.
void psq_initialize ()
{
    for (const auto &pt : { PAWN, NIHT, BSHP, ROOK, QUEN, KING })
    {
        Score score = mk_score (PieceValues[MG][pt], PieceValues[EG][pt]);
        for (const auto &s : SQ)
        {
            Score psq = score
                      + (PAWN == pt ?
                            PawnFullSQ[_rank (s)][_file (s)] :
                            PieceHalfSQ[pt][_rank (s)][std::min (_file (s), ~_file (s))]);
            PSQ[WHITE|pt][ s] = +psq;
            PSQ[BLACK|pt][~s] = -psq;
        }
    }
}
