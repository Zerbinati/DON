#pragma once

#include "Position.h"
#include "Type.h"

namespace Pawns {

    /// Pawns::Entry contains information about Pawn structure.
    struct Entry {

        Key key;

        i32 complexity;
        bool pawnNotBothFlank;

        Array<Score, COLORS> score;

        Array<Bitboard, COLORS> sglAttacks;
        Array<Bitboard, COLORS> dblAttacks;
        Array<Bitboard, COLORS> attacksSpan;
        Array<Bitboard, COLORS> passPawns;

        i32 passedCount() const;

        template<Color>
        void evaluate(Position const&);

    };

    using Table = HashTable<Entry>;

    extern Entry* probe(Position const&);
}
