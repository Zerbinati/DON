#pragma once

#include <algorithm>
#include <cassert>

#include "Position.h"
#include "Type.h"

namespace King {

    /// King::Entry contains information about King & Pawn structure.
    struct Entry {

        Key key;

        Array<Score, COLORS> pawnDist;

        Array<u08  , COLORS> castleSide;
        Array<Score, COLORS> pawnSafety;

        template<Color>
        Score evaluateSafety(Position const&, Bitboard);

        template<Color>
        void evaluate(Position const&);
    };

    using Table = HashTable<Entry>;

    extern Entry* probe(Position const&);

}
