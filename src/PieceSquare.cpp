#include "PieceSquare.h"

#include "Position.h"

namespace {

    #define S(mg, eg) mk_score (mg, eg)
    // HalfPSQ[piece-type][rank][file/2] contains half Piece-Square scores.
    // Table is defined for files A..D and white side,
    // It is symmetric for second half of the files and negative for black side.
    // For each piece type on a given square a (midgame, endgame) score pair is assigned.
    const Score HalfPSQ[NONE][R_NO][F_NO/2] =
    {
        { // Pawn
            { S(  0, 0), S( 0, 0), S( 0, 0), S( 0, 0) },
            { S(-11, 7), S( 6,-4), S( 7, 8), S( 3,-2) },
            { S(-18,-4), S(-2,-5), S(19, 5), S(24, 4) },
            { S(-17, 3), S(-9, 3), S(20,-8), S(35,-3) },
            { S( -6, 8), S( 5, 9), S( 3, 7), S(21,-6) },
            { S( -6, 8), S(-8,-5), S(-6, 2), S(-2, 4) },
            { S( -4, 3), S(20,-9), S(-8, 1), S(-4,18) },
            { S(  0, 0), S( 0, 0), S( 0, 0), S( 0, 0) }
        },
        { // Knight
            { S(-144, -98), S(-96,-82), S(-80,-46), S(-73,-14) },
            { S( -83, -69), S(-43,-54), S(-21,-17), S(-10,  9) },
            { S( -71, -50), S(-22,-39), S(  0, -7), S(  9, 28) },
            { S( -25, -41), S( 18,-25), S( 43,  6), S( 47, 38) },
            { S( -26, -46), S( 16,-25), S( 38,  3), S( 50, 40) },
            { S( -11, -54), S( 37,-38), S( 56, -7), S( 65, 27) },
            { S( -62, -65), S(-17,-50), S(  5,-24), S( 14, 13) },
            { S(-194,-109), S(-66,-89), S(-42,-50), S(-29,-13) }
        },
        { // Bishop
            { S(-44,-58), S(-13,-31), S(-25,-37), S(-34,-19) },
            { S(-20,-34), S( 20, -9), S( 12,-14), S(  1,  4) },
            { S( -9,-23), S( 27,  0), S( 21, -3), S( 11, 16) },
            { S(-11,-26), S( 28, -3), S( 21, -5), S( 10, 16) },
            { S(-11,-26), S( 24, -4), S( 16, -7), S(  9, 14) },
            { S(-17,-24), S( 16, -2), S( 12,  0), S(  2, 13) },
            { S(-23,-34), S( 17,-10), S(  6,-12), S( -2,  6) },
            { S(-35,-55), S(-11,-32), S(-19,-36), S(-29,-17) }
        },
        { // Rook
            { S(-25, 0), S(-16, 0), S(-16, 0), S(-9, 0) },
            { S(-21, 0), S( -8, 0), S( -3, 0), S( 0, 0) },
            { S(-21, 0), S( -9, 0), S( -4, 0), S( 2, 0) },
            { S(-22, 0), S( -6, 0), S( -1, 0), S( 2, 0) },
            { S(-22, 0), S( -7, 0), S(  0, 0), S( 1, 0) },
            { S(-21, 0), S( -7, 0), S(  0, 0), S( 2, 0) },
            { S(-12, 0), S(  4, 0), S(  8, 0), S(12, 0) },
            { S(-23, 0), S(-15, 0), S(-11, 0), S(-5, 0) }
        },
        { // Queen
            { S( 0,-71), S(-4,-56), S(-3,-42), S(-1,-29) },
            { S(-4,-56), S( 6,-30), S( 9,-21), S( 8, -5) },
            { S(-2,-39), S( 6,-17), S( 9, -8), S( 9,  5) },
            { S(-1,-29), S( 8, -5), S(10,  9), S( 7, 19) },
            { S(-3,-27), S( 9, -5), S( 8, 10), S( 7, 21) },
            { S(-2,-40), S( 6,-16), S( 8,-10), S(10,  3) },
            { S(-2,-55), S( 7,-30), S( 7,-21), S( 6, -6) },
            { S(-1,-74), S(-4,-55), S(-1,-43), S( 0,-30) }
        },
        { // King
            { S(267,  0), S(320, 48), S(270, 75), S(195, 84) },
            { S(264, 43), S(304, 92), S(238,143), S(180,132) },
            { S(200, 83), S(245,138), S(176,167), S(110,165) },
            { S(177,106), S(185,169), S(148,169), S(110,179) },
            { S(149,108), S(177,163), S(115,200), S( 66,203) },
            { S(118, 95), S(159,155), S( 84,176), S( 41,174) },
            { S( 86, 50), S(128, 99), S( 63,122), S( 18,139) },
            { S( 63,  9), S( 89, 55), S( 47, 80), S(  0, 90) }
        }
    };
    #undef S
}

namespace PieceSquare
{
    // PSQ[color][piece-type][square] scores.
    Score PSQ[CLR_NO][NONE][SQ_NO];

    // Computes the scores for the middle game and the endgame.
    // These functions are used to initialize the scores when a new position is set up,
    // and to verify that the scores are correctly updated by do_move and undo_move when the program is running in debug mode.
    Score compute_psq (const Position &pos)
    {
        auto psq_score = SCORE_ZERO;
        for (i08 c = WHITE; c <= BLACK; ++c)
        {
            for (i08 pt = PAWN; pt <= KING; ++pt)
            {
                for (i08 s : pos.squares[c][pt])
                {
                    psq_score += PSQ[c][pt][s];
                }
            }
        }
        return psq_score;
    }

    // Computes the non-pawn middle game material value for the given side.
    // Material values are updated incrementally during the search.
    template<Color Own> Value compute_npm (const Position &pos)
    {
        auto npm_value = VALUE_ZERO;
        for (i08 pt = NIHT; pt <= QUEN; ++pt)
        {
            npm_value += PieceValues[MG][pt] * pos.count (Own, PieceType(pt));
        }
        return npm_value;
    }
    template Value compute_npm<WHITE> (const Position&);
    template Value compute_npm<BLACK> (const Position&);

    // Initialize lookup tables during startup
    void initialize ()
    {
        for (i08 pt = PAWN; pt <= KING; ++pt)
        {
            auto p_score = mk_score (PieceValues[MG][pt], PieceValues[EG][pt]);
            for (i08 s = SQ_A1; s <= SQ_H8; ++s)
            {
                auto psq_score = p_score + HalfPSQ[pt][_rank (Square(s))][std::min (_file (Square(s)), F_H - _file (Square(s)))];
                PSQ[WHITE][pt][ Square(s)] = +psq_score;
                PSQ[BLACK][pt][~Square(s)] = -psq_score;
            }
        }
    }
}
