#include "Cuckoo.h"

#include "Bitboard.h"
#include "Zobrist.h"

Array<Cuckoo, CuckooSize> Cuckoos;

bool Cuckoo::empty() const {
    return NO_PIECE == piece
        || (SQ_A1 == s1 && SQ_A1 == s2);
}

Key Cuckoo::key() const {
    return RandZob.colorKey
         ^ RandZob.pieceSquareKey[piece][s1]
         ^ RandZob.pieceSquareKey[piece][s2];
}

namespace CucKoo {

    void initialize() {

#if !defined(NDEBUG)
        u16 count = 0;
#endif

        // Prepare the Cuckoo tables
        Cuckoos.fill({ NO_PIECE, SQ_A1, SQ_A1 });

        for (Piece p : Pieces) {
            // Pawn moves are not reversible
            if (PAWN == pType(p)) {
                continue;
            }

            for (Square s1 = SQ_A1; s1 <= SQ_H8 + WEST; ++s1) {
                for (Square s2 = s1 + EAST; s2 <= SQ_H8; ++s2) {

                    if (contains(PieceAttackBB[pType(p)][s1], s2)) {

                        Cuckoo cuckoo{ p, s1, s2 };

                        u16 h = hash<0>(cuckoo.key());
                        while (true) {

                            std::swap(Cuckoos[h], cuckoo);
                            // Arrived at empty slot ?
                            if (cuckoo.empty()) {
                                break;
                            }
                            // Push victim to alternative slot
                            h = h == hash<0>(cuckoo.key()) ?
                                hash<1>(cuckoo.key()) :
                                hash<0>(cuckoo.key());
                        }

#if !defined(NDEBUG)
                        ++count;
#endif
                    }
                }
            }
        }

        assert(3668 == count);
    }
}


