#include "Material.h"

#include "Thread.h"

namespace Material {

    using namespace std;
    using namespace BitBoard;
    using namespace EndGame;

    namespace {

        // Polynomial material imbalance parameters

        const i32 OwnQuadratic[][NONE] =
        {
            //          Own Pieces
            //  P     N     B     R     Q    BP
            {    0,    0,    0,    0,    0,   +40 }, // P
            { +255,   -3,    0,    0,    0,   +32 }, // N
            { +104,   +4,    0,    0,    0,     0 }, // B     Own Pieces
            {   -2,  +47, +105, -149,    0,   -26 }, // R
            {  +24, +122, +137, -134,    0,  -185 }, // Q
            {    0,    0,    0,    0,    0, +1667 }  // BP
        };

        const i32 OppQuadratic[][NONE] =
        {
            //          Opp Pieces
            //  P     N     B     R     Q    BP
            {    0,    0,    0,    0,    0,  +36 }, // P
            {  +63,    0,    0,    0,    0,   +9 }, // N
            {  +65,  +42,    0,    0,    0,  +59 }, // B     Own Pieces
            {  +39,  +24,  -24,    0,    0,  +46 }, // R
            { +100,  -37, +141, +268,    0, +101 }, // Q
            {    0,    0,    0,    0,    0,    0 }  // BP
        };

        // PawnsSet[count] contains a bonus/malus indexed by number of pawns
        const int PawnsSet[] = { 24, -32, 107, -51, 117, -9, -126, -21, 31 };

        // Endgame evaluation and scaling functions are accessed direcly and not through
        // the function maps because they correspond to more than one material hash key.
        Endgame<KXK>     EvaluateKXK [] = { Endgame<KXK>     (WHITE), Endgame<KXK>     (BLACK) };

        Endgame<KBPsKPs> ScaleKBPsKPs[] = { Endgame<KBPsKPs> (WHITE), Endgame<KBPsKPs> (BLACK) };
        Endgame<KQKRPs>  ScaleKQKRPs [] = { Endgame<KQKRPs>  (WHITE), Endgame<KQKRPs>  (BLACK) };
        Endgame<KPsK>    ScaleKPsK   [] = { Endgame<KPsK>    (WHITE), Endgame<KPsK>    (BLACK) };
        Endgame<KPKP>    ScaleKPKP   [] = { Endgame<KPKP>    (WHITE), Endgame<KPKP>    (BLACK) };

        // Calculates the imbalance by comparing the piece count of each piece type for both colors.
        // NOTE:: King == Bishop Pair
        template<Color Own>
        i32 imbalance (const i32 count[][NONE])
        {
            const auto Opp = Own == WHITE ? BLACK : WHITE;

            i32 value = PawnsSet[count[Own][PAWN]];
            // "The Evaluation of Material Imbalances in Chess"
            // Second-degree polynomial material imbalance by Tord Romstad
            for (auto pt1 = PAWN; pt1 <= QUEN; ++pt1)
            {
                if (0 != count[Own][pt1])
                {
                    i32 v = 0;

                    for (auto pt2 = PAWN; pt2 <= pt1; ++pt2)
                    {
                        v += count[Own][pt2] * OwnQuadratic[pt1][pt2]
                           + count[Opp][pt2] * OppQuadratic[pt1][pt2];
                    }
                    v += count[Own][KING] * OwnQuadratic[pt1][KING]
                       + count[Opp][KING] * OppQuadratic[pt1][KING];

                    value += count[Own][pt1] * v;
                }
            }
            if (0 != count[Own][KING])
            {
                value += count[Own][KING] * OwnQuadratic[KING][KING]
                       + count[Opp][KING] * OppQuadratic[KING][KING];
            }
            return value;
        }
    }

    // Looks up a MaterialEntry object, and returns a pointer to it.
    // The pointer is also stored in a hash table.
    Entry* probe (const Position &pos)
    {
        auto *e = pos.thread->matl_table[pos.si->matl_key];

        if (e->key == pos.si->matl_key)
        {
            return e;
        }

        std::memset (e, 0x00, sizeof (*e));
        e->key = pos.si->matl_key;
        e->scale[WHITE] =
        e->scale[BLACK] = SCALE_NORMAL;

        // Let's look if have a specialized evaluation function for this
        // particular material configuration. First look for a fixed
        // configuration one, then a generic one if previous search failed.
        if (nullptr != (e->value_func = EndGames->probe<Value> (pos.si->matl_key)))
        {
            return e;
        }
        // Generic evaluation
        for (auto c = WHITE; c <= BLACK; ++c)
        {
            if (   pos.si->non_pawn_material ( c) >= VALUE_MG_ROOK
                && pos.count<NONE> (~c) == 1)
            {
                e->value_func = &EvaluateKXK[c];
                return e;
            }
        }

        // Didn't find any special evaluation function for the current
        // material configuration. Is there a suitable scaling function?
        //
        // Face problems when there are several conflicting applicable
        // scaling functions and need to decide which one to use.
        EndgameBase<Scale> *scale_func;
        if (nullptr != (scale_func = EndGames->probe<Scale> (pos.si->matl_key)))
        {
            e->scale_func[scale_func->strong_color] = scale_func;
            return e;
        }

        // Didn't find any specialized scaling function, so fall back on
        // generic scaling functions that refer to more than one material distribution.
        // Note that these ones don't return after setting the function.
        for (auto c = WHITE; c <= BLACK; ++c)
        {
            if (   pos.si->non_pawn_material ( c) == VALUE_MG_BSHP
                && pos.count<BSHP> ( c) == 1
                && pos.count<PAWN> ( c) != 0)
            {
                e->scale_func[c] = &ScaleKBPsKPs[c];
            }
            else
            if (   pos.si->non_pawn_material ( c) == VALUE_MG_QUEN
                && pos.count<QUEN> ( c) == 1
                && pos.count<PAWN> ( c) == 0
                && pos.si->non_pawn_material (~c) == VALUE_MG_ROOK
                && pos.count<ROOK> (~c) == 1
                && pos.count<PAWN> (~c) != 0)
            {
                e->scale_func[c] = &ScaleKQKRPs[c];
            }
            else
            // Only pawns on the board
            if (   pos.si->non_pawn_material () == VALUE_ZERO
                && pos.pieces (PAWN) != 0)
            {
                switch (pos.count<PAWN> (~c))
                {
                case 0:
                    assert(pos.count<PAWN> ( c) > 1);
                    e->scale_func[c] = &ScaleKPsK[c];
                    break;
                case 1:
                    if (pos.count<PAWN> ( c) == 1)
                    {
                        e->scale_func[c] = &ScaleKPKP[c];
                    }
                    break;
                }
            }

            // Zero or just one pawn makes it difficult to win, even with a material advantage.
            // This catches some trivial draws like KK, KBK and KNK and gives a very drawish
            // scale for cases such as KRKBP and KmmKm (except for KBBKN).
            if (abs (  pos.si->non_pawn_material ( c)
                     - pos.si->non_pawn_material (~c)) <= VALUE_MG_BSHP)
            {
                switch (pos.count<PAWN> ( c))
                {
                case 0:
                    e->scale[c] =
                        pos.si->non_pawn_material ( c) <  VALUE_MG_ROOK ?
                            SCALE_DRAW :
                            pos.si->non_pawn_material (~c) <= VALUE_MG_BSHP ?
                                Scale(4) :
                                Scale(14);
                    break;
                case 1:
                    e->scale[c] = SCALE_ONEPAWN;
                    break;
                }
            }
        }
        // Evaluate the material imbalance.
        // Use KING as a place holder for the bishop pair "extended piece",
        // this allow us to be more flexible in defining bishop pair bonuses.
        const i32 piece_count[CLR_NO][NONE] =
        {
            {
                pos.count<PAWN> (WHITE), pos.count<NIHT> (WHITE), pos.count<BSHP> (WHITE),
                pos.count<ROOK> (WHITE), pos.count<QUEN> (WHITE), pos.paired_bishop (WHITE) ? 1 : 0
            },
            {
                pos.count<PAWN> (BLACK), pos.count<NIHT> (BLACK), pos.count<BSHP> (BLACK),
                pos.count<ROOK> (BLACK), pos.count<QUEN> (BLACK), pos.paired_bishop (BLACK) ? 1 : 0
            }
        };

        auto value = (  imbalance<WHITE> (piece_count)
                      - imbalance<BLACK> (piece_count)) / 16;
        e->imbalance = mk_score (value, value);

        return e;
    }
}
