#include "Cuckoo.h"

#include "Bitboard.h"
#include "Zobrist.h"


bool Cuckoo::empty() const {
    return NO_PIECE == piece
        || SQ_NONE == s1
        || SQ_NONE == s2;
}

bool Cuckoo::operator==(Cuckoo const &ck) const {
    return piece == ck.piece
        && s1 == ck.s1
        && s2 == ck.s2;
}
bool Cuckoo::operator!=(Cuckoo const &ck) const {
    return piece != ck.piece
        || s1 != ck.s1
        || s2 != ck.s2;
}

Key Cuckoo::key() const {
    return empty() ?
           0 :
           RandZob.colorKey
         ^ RandZob.pieceSquareKey[piece][s1]
         ^ RandZob.pieceSquareKey[piece][s2];
}

namespace Cuckoos {

    Array<Cuckoo, CuckooSize> CuckooTable;

    bool lookup(Cuckoo const &cuckoo) {
        auto key = cuckoo.key();
        return CuckooTable[hash<0>(key)] == cuckoo
            || CuckooTable[hash<1>(key)] == cuckoo;
            //|| CuckooTable[hash<2>(key)] == cuckoo
            //|| CuckooTable[hash<3>(key)] == cuckoo;
    }

    bool place(Cuckoo &cuckoo) {

        if (lookup(cuckoo)) {
            return false;
        }

        u16 h = hash<0>(cuckoo.key());
        for (auto i = 0; i < 20; ++i) {

            /*
            std::swap(CuckooTable[h], cuckoo);
            // Arrived at empty slot ?
            if (cuckoo.empty()) {
                return true;
            }

            // Push victim to alternative slot
            h = h == hash<0>(cuckoo.key()) ?
                hash<1>(cuckoo.key()) :
                hash<0>(cuckoo.key());
            */

            // Arrived at empty slot ?
            if (CuckooTable[h].empty()) {
                CuckooTable[h] = cuckoo;
                //printf("%d ", i);
                return true;
            }
            else {
                std::swap(CuckooTable[h], cuckoo);
                // Push victim to alternative slot
                h = h == hash<0>(cuckoo.key()) ?
                    hash<1>(cuckoo.key()) :
                    hash<0>(cuckoo.key());
            }

        }

        // Insertion failed (need to rehash the table)
        return false;
    }
    /*
    bool place(Cuckoo &cuckoo, u16 count, u16 n) {

        if (count == n)
        {
            printf("Cycle present. REHASH.\n");
            return false;
        }

        if (lookup(cuckoo)) {
            return false;
        }

        u16 h;
        switch (count % 2) {
        case 0: h = hash<0>(cuckoo.key()); break;
        case 1: h = hash<1>(cuckoo.key()); break;
        //case 2: h = hash<2>(cuckoo.key()); break;
        //case 3: h = hash<3>(cuckoo.key()); break;
        default: break;
        }

        // Arrived at empty slot ?
        if (CuckooTable[h].empty()) {
            CuckooTable[h] = cuckoo;
            return true;
        }
        else {
            std::swap(CuckooTable[h], cuckoo);
            // Push victim to alternative slot
            place(cuckoo, count + 1, n);
            return false;
        }
    }
    */

    bool lookup(Key key, Cuckoo &cuckoo) {
        return ((cuckoo = CuckooTable[hash<0>(key)]).key() == key)
            || ((cuckoo = CuckooTable[hash<1>(key)]).key() == key);
            //|| ((cuckoo = CuckooTable[hash<2>(key)]).key() == key)
            //|| ((cuckoo = CuckooTable[hash<3>(key)]).key() == key);
    }

    void initialize() {

        std::vector<Cuckoo> cuckoos;
        for (Piece p : Pieces) {
            // Pawn moves are not reversible
            if (PAWN == pType(p)) {
                continue;
            }
            for (Square s1 = SQ_A1; s1 <= SQ_H8 + WEST; ++s1) {
                for (Square s2 = s1 + EAST; s2 <= SQ_H8; ++s2) {

                    if (contains(PieceAttackBB[pType(p)][s1], s2)) {
                        cuckoos.push_back({ p, s1, s2 });
                    }
                }
            }
        }
        assert(3668 == cuckoos.size()); // (168+280+448+728+210)*2 = 7336 / 2

        u16 count = 0;
        // Prepare the Cuckoo table
        CuckooTable.fill({ NO_PIECE, SQ_NONE, SQ_NONE });
        for (auto cuckoo : cuckoos) {
            if (place(cuckoo)) {
            //if (place(cuckoo, 0, cuckoos.size())) {
                ++count;
            }
        }
        assert(count == cuckoos.size());

    }
}
