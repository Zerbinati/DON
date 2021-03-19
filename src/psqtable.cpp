#include "psqtable.h"

#include <algorithm>

#include "position.h"

Score PSQ[PIECES][SQUARES];

namespace {

    auto constexpr S = makeScore;

    // PieceScores[piece-type][rank][file/2] contains half Piece-Square scores (symmetric distribution).
    // It is defined for files A..D and white side,
    // It is symmetric for second half of the files and negative for black side.
    constexpr Score PieceScores[PIECE_TYPES][RANKS][FILES/2]{
        {},
        {},
        { // Knight
            { S(-175, -96), S(-92,-65), S(-74,-49), S(-73,-21) },
            { S( -77, -67), S(-41,-54), S(-27,-18), S(-15,  8) },
            { S( -61, -40), S(-17,-27), S(  6, -8), S( 12, 29) },
            { S( -35, -35), S(  8, -2), S( 40, 13), S( 49, 28) },
            { S( -34, -45), S( 13,-16), S( 44,  9), S( 51, 39) },
            { S(  -9, -51), S( 22,-44), S( 58,-16), S( 53, 17) },
            { S( -67, -69), S(-27,-50), S(  4,-51), S( 37, 12) },
            { S(-201,-100), S(-83,-88), S(-56,-56), S(-26,-17) }
        },
        { // Bishop
            { S(-37,-40), S( -4,-21), S( -6,-26), S(-16, -8) },
            { S(-11,-26), S(  6, -9), S( 13,-12), S(  3,  1) },
            { S( -5,-11), S( 15, -1), S( -4, -1), S( 12,  7) },
            { S( -4,-14), S(  8, -4), S( 18,  0), S( 27, 12) },
            { S( -8,-12), S( 20, -1), S( 15,-10), S( 22, 11) },
            { S(-11,-21), S(  4,  4), S(  1,  3), S(  8,  4) },
            { S(-12,-22), S(-10,-14), S(  4, -1), S(  0,  1) },
            { S(-34,-32), S(  1,-29), S(-10,-26), S(-16,-17) }
        },
        { // Rook
            { S(-31, -9), S(-20,-13), S(-14,-10), S( -5, -9) },
            { S(-21,-12), S(-13, -9), S( -8, -1), S(  6, -2) },
            { S(-25,  6), S(-11, -8), S( -1, -2), S(  3, -6) },
            { S(-13, -6), S( -5,  1), S( -4, -9), S( -6,  7) },
            { S(-27, -5), S(-15,  8), S( -4,  7), S(  3, -6) },
            { S(-22,  6), S( -2,  1), S(  6, -7), S( 12, 10) },
            { S( -2,  4), S( 12,  5), S( 16, 20), S( 18, -5) },
            { S(-17, 18), S(-19,  0), S( -1, 19), S(  9, 13) }
        },
        { // Queen
            { S(  3,-69), S( -5,-57), S( -5,-47), S(  4,-26) },
            { S( -3,-54), S(  5,-31), S(  8,-22), S( 12, -4) },
            { S( -3,-39), S(  6,-18), S( 13, -9), S(  7,  3) },
            { S(  4,-23), S(  5, -3), S(  9, 13), S(  8, 24) },
            { S(  0,-29), S( 14, -6), S( 12,  9), S(  5, 21) },
            { S( -4,-38), S( 10,-18), S(  6,-11), S(  8,  1) },
            { S( -5,-50), S(  6,-27), S( 10,-24), S(  8, -8) },
            { S( -2,-74), S( -2,-52), S(  1,-43), S( -2,-34) }
        },
        { // King
            { S(271,  1), S(327, 45), S(271, 85), S(198, 76) },
            { S(278, 53), S(303,100), S(234,133), S(179,135) },
            { S(195, 88), S(258,130), S(169,169), S(120,175) },
            { S(164,103), S(190,156), S(138,172), S( 98,172) },
            { S(154, 96), S(179,166), S(105,199), S( 70,199) },
            { S(123, 92), S(145,172), S( 81,184), S( 31,191) },
            { S( 88, 47), S(120,121), S( 65,116), S( 33,131) },
            { S( 59, 11), S( 89, 59), S( 45, 73), S( -1, 78) }
        }
    };

    // PawnScores contains full Pawn-Square score (asymmetric distribution)
    constexpr Score PawnScores[RANKS][FILES]{
        { S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0) },
        { S(  2, -8), S(  4, -6), S( 11,  9), S( 18,  5), S( 16, 16), S( 21,  6), S(  9, -6), S( -3,-18) },
        { S( -9, -9), S(-15, -7), S( 11,-10), S( 15,  5), S( 31,  2), S( 23,  3), S(  6, -8), S(-20, -5) },
        { S( -3,  7), S(-20,  1), S(  8, -8), S( 19, -2), S( 39,-14), S( 17,-13), S(  2,-11), S( -5, -6) },
        { S( 11, 12), S( -4,  6), S(-11,  2), S(  2, -6), S( 11, -5), S(  0, -4), S(-12, 14), S(  5,  9) },
        { S(  3, 27), S(-11, 18), S( -6, 19), S( 22, 29), S( -8, 30), S( -5,  9), S(-14,  8), S(-11, 14) },
        { S( -7, -1), S(  6,-14), S( -2, 13), S(-11, 22), S(  4, 24), S(-14, 17), S( 10,  7), S( -9,  7) },
        { S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0), S(  0,  0) }
    };

}

namespace PSQT {
    /// initialize() initializes piece-square tables
    void initialize() noexcept {

        for (PieceType const pt : { PAWN, NIHT, BSHP, ROOK, QUEN, KING }) {
            Score const score{ makeScore(PieceValues[MG][pt], PieceValues[EG][pt]) };

            for (Square s = SQ_A1; s <= SQ_H8; ++s) {

                PSQ[WHITE|pt][s] =
                    score
                  + (pt == PAWN ?
                        PawnScores[sRank(s)][sFile(s)] :
                        PieceScores[pt][sRank(s)][edgeDistance(sFile(s))]);

                PSQ[BLACK|pt][flip<Rank>(s)] = -PSQ[WHITE|pt][s];
            }
        }
    }

    /// Computes the scores for the middle game and the endgame.
    /// These functions are used to initialize the scores when a new position is set up,
    /// and to verify that the scores are correctly updated by do_move and undo_move when the program is running in debug mode.
    Score computePSQ(Position const &pos) noexcept {
        Score psq{ SCORE_ZERO };

        for (Piece const p : Pieces) {
            Bitboard bb{ pos.pieces(p) };
            while (bb != 0) {
                psq += PSQ[p][popLSq(bb)];
            }
        }
        return psq;
    }

}
