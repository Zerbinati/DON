#include "Material.h"

#include "Thread.h"

namespace Material {

    using namespace std;
    using namespace EndGame;

    namespace {

        // Polynomial material imbalance parameters

        const i32 OwnQuadratic[NONE][NONE] =
        {
            //          Own Pieces
            //  P     N     B     R     Q    BP
            {    0,    0,    0,    0,    0,   +40 }, // P
            { +255,   -3,    0,    0,    0,   +32 }, // N
            { +104,   +4,    0,    0,    0,     0 }, // B     Own Pieces
            {   -2,  +47, +105, -149,    0,   -26 }, // R
            {  +24, +117, +133, -134,    0,  -189 }, // Q
            {    0,    0,    0,    0,    0, +1667 }  // BP
        };

        const i32 OppQuadratic[NONE][NONE] =
        {
            //          Opp Pieces
            //  P     N     B     R     Q    BP
            {    0,    0,    0,    0,    0,  +36 }, // P
            {  +63,    0,    0,    0,    0,   +9 }, // N
            {  +65,  +42,    0,    0,    0,  +59 }, // B     Own Pieces
            {  +39,  +24,  -24,    0,    0,  +46 }, // R
            { +100,  -42, +137, +268,    0,  +97 }, // Q
            {    0,    0,    0,    0,    0,    0 }  // BP
        };

        // Endgame evaluation and scaling functions are accessed direcly and not through
        // the function maps because they correspond to more than one material hash key.
        Endgame<KXK>     ValueKXK[+Color::NO] = { Endgame<KXK> (Color::WHITE), Endgame<KXK> (Color::BLACK) };

        Endgame<KPKP>    ScaleKPKP[+Color::NO] = { Endgame<KPKP> (Color::WHITE), Endgame<KPKP> (Color::BLACK) };
        Endgame<KPsK>    ScaleKPsK[+Color::NO] = { Endgame<KPsK> (Color::WHITE), Endgame<KPsK> (Color::BLACK) };
        Endgame<KBPsKP> ScaleKBPsKP[+Color::NO] = { Endgame<KBPsKP> (Color::WHITE), Endgame<KBPsKP> (Color::BLACK) };
        Endgame<KQKRPs>  ScaleKQKRPs[+Color::NO] = { Endgame<KQKRPs> (Color::WHITE), Endgame<KQKRPs> (Color::BLACK) } ;

        /// imbalance() calculates the imbalance by the piece count of each piece type for both colors.
        /// NOTE:: KING == BISHOP PAIR
        template<Color Own>
        i32 imbalance (const i32 (*count)[NONE])
        {
            const auto Opp = Color::WHITE == Own ? Color::BLACK : Color::WHITE;

            i32 value = 0;
            // "The Evaluation of Material Imbalances in Chess"
            // Second-degree polynomial material imbalance by Tord Romstad
            for (auto pt1 : { PAWN, NIHT, BSHP, ROOK, QUEN })
            {
                if (0 != count[+Own][pt1])
                {
                    i32 v = 0;

                    for (auto pt2 = PAWN; pt2 <= pt1; ++pt2)
                    {
                        v += count[+Own][pt2] * OwnQuadratic[pt1][pt2]
                           + count[+Opp][pt2] * OppQuadratic[pt1][pt2];
                    }
                    v += count[+Own][KING] * OwnQuadratic[pt1][KING]
                       + count[+Opp][KING] * OppQuadratic[pt1][KING];

                    value += count[+Own][pt1] * v;
                }
            }
            if (0 != count[+Own][KING])
            {
                value += count[+Own][KING] * OwnQuadratic[KING][KING]
                       + count[+Opp][KING] * OppQuadratic[KING][KING];
            }

            return value;
        }
    }

    /// Material::probe() looks up a current position's material configuration in the material hash table
    /// and returns a pointer to it if found, otherwise a new Entry is computed and stored there.
    Entry* probe (const Position &pos)
    {
        auto *e = pos.thread->matl_table.get (pos.si->matl_key);

        if (e->key == pos.si->matl_key)
        {
            return e;
        }

        std::memset (e, 0x00, sizeof (*e));
        e->key = pos.si->matl_key;

        // Calculates the phase interpolating total non-pawn material between endgame and midgame limits.
        e->phase = +(  std::min (Value::MIDGAME,
                       std::max (Value::ENDGAME, pos.si->non_pawn_material ()))
                     - Value::ENDGAME)
                 * PhaseResolution
                 / +(Value::MIDGAME - Value::ENDGAME);
        e->scale[+Color::WHITE] =
        e->scale[+Color::BLACK] = SCALE_NORMAL;

        // Let's look if have a specialized evaluation function for this
        // particular material configuration. First look for a fixed
        // configuration one, then a generic one if previous search failed.
        if (nullptr != (e->value_func = EndGames->probe<Value> (pos.si->matl_key)))
        {
            return e;
        }
        // Generic evaluation
        for (auto c : { Color::WHITE, Color::BLACK })
        {
            if (   pos.si->non_pawn_material ( c) >= Value::MG_ROOK
                && pos.count (~c) == 1)
            {
                e->value_func = &ValueKXK[+c];
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
            e->scale_func[+scale_func->strong_color] = scale_func;
            return e;
        }

        // Didn't find any specialized scaling function, so fall back on
        // generic scaling functions that refer to more than one material distribution.
        // Note that these ones don't return after setting the function.
        for (auto c : { Color::WHITE, Color::BLACK })
        {
            if (   pos.si->non_pawn_material ( c) == Value::MG_BSHP
                && pos.count ( c, BSHP) == 1
                && pos.count ( c, PAWN) != 0)
            {
                e->scale_func[+c] = &ScaleKBPsKP[+c];
            }
            else
            if (   pos.si->non_pawn_material ( c) == Value::MG_QUEN
                && pos.count ( c, QUEN) == 1
                && pos.count ( c, PAWN) == 0
                && pos.si->non_pawn_material (~c) == Value::MG_ROOK
                && pos.count (~c, ROOK) == 1
                && pos.count (~c, PAWN) != 0)
            {
                e->scale_func[+c] = &ScaleKQKRPs[+c];
            }

            // Zero or just one pawn makes it difficult to win, even with a material advantage.
            // This catches some trivial draws like KK, KBK and KNK and gives a very drawish
            // scale for cases such as KRKBP and KmmKm (except for KBBKN).
            if (abs (+(  pos.si->non_pawn_material ( c)
                       - pos.si->non_pawn_material (~c))) <= +Value::MG_BSHP)
            {
                if (0 == pos.count ( c, PAWN))
                {
                    e->scale[+c] = pos.si->non_pawn_material ( c) <  Value::MG_ROOK ?
                                    SCALE_DRAW :
                                    pos.si->non_pawn_material (~c) <= Value::MG_BSHP ?
                                        Scale(4) :
                                        Scale(14);
                }
                else
                if (1 == pos.count ( c, PAWN))
                {
                    e->scale[+c] = SCALE_ONEPAWN;
                }
            }
        }

        // Only pawns left
        if (   pos.si->non_pawn_material () == Value::ZERO
            && pos.pieces (PAWN) != 0)
        {
            if (0 == pos.pieces (Color::BLACK, PAWN))
            {
                assert(2 <= pos.count (Color::WHITE, PAWN));
                e->scale_func[+Color::WHITE] = &ScaleKPsK[+Color::WHITE];
            }
            else
            if (0 == pos.pieces (Color::WHITE, PAWN))
            {
                assert(2 <= pos.count (Color::BLACK, PAWN));
                e->scale_func[+Color::BLACK] = &ScaleKPsK[+Color::BLACK];
            }
            else 
            if (   1 == pos.count (Color::WHITE, PAWN)
                && 1 == pos.count (Color::BLACK, PAWN))
            {
                e->scale_func[+Color::WHITE] = &ScaleKPKP[+Color::WHITE];
                e->scale_func[+Color::BLACK] = &ScaleKPKP[+Color::BLACK];
            }
        }

        // Evaluate the material imbalance.
        // Use KING as a place holder for the bishop pair "extended piece",
        // this allow us to be more flexible in defining bishop pair bonuses.
        const i32 piece_count[+Color::NO][NONE] =
        {
            {
                pos.count (Color::WHITE, PAWN), pos.count (Color::WHITE, NIHT), pos.count (Color::WHITE, BSHP),
                pos.count (Color::WHITE, ROOK), pos.count (Color::WHITE, QUEN), pos.paired_bishop (Color::WHITE) ? 1 : 0
            },
            {
                pos.count (Color::BLACK, PAWN), pos.count (Color::BLACK, NIHT), pos.count (Color::BLACK, BSHP),
                pos.count (Color::BLACK, ROOK), pos.count (Color::BLACK, QUEN), pos.paired_bishop (Color::BLACK) ? 1 : 0
            }
        };

        i32 imb[+Color::NO] =
        {
            imbalance<Color::WHITE> (piece_count),
            imbalance<Color::BLACK> (piece_count)
        };
        
        auto value = (imb[+Color::WHITE] - imb[+Color::BLACK]) / 16; // Imbalance Resolution
        e->imbalance = mk_score (value, value);

        return e;
    }
}
