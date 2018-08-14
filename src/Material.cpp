#include "Material.h"

#include "Thread.h"

namespace Material {

    using namespace std;
    using namespace EndGame;

    namespace {

        // Polynomial material imbalance parameters

        constexpr i32 OwnQuadratic[NONE][NONE] =
        {
            //          Own Pieces
            //  P    N    B     R  Q    BP
            {   0,   0,   0,    0, 0,   40 }, // P
            { 255, -67,   0,    0, 0,   32 }, // N
            { 104,   4,   0,    0, 0,    0 }, // B     Own Pieces
            {  -2,  47, 105, -221, 0,  -26 }, // R
            {  24, 117, 133, -134, 0, -189 }, // Q
            {   0,   0,   0,    0, 0, 1443 }  // BP
        };

        constexpr i32 OppQuadratic[NONE][NONE] =
        {
            //          Opp Pieces
            //  P    N    B     R  Q    BP
            {   0,   0,   0,    0, 0,   36 }, // P
            {  63,   0,   0,    0, 0,    9 }, // N
            {  65,  42,   0,    0, 0,   59 }, // B     Own Pieces
            {  39,  24, -24,    0, 0,   46 }, // R
            { 100, -42, 137,  268, 0,   97 }, // Q
            {   0,   0,   0,    0, 0,    0 }  // BP
        };

        constexpr int PawnBonus[9] = { 0, 304,  144, -320, -560, -704, -672, -464, -320 };


        // Endgame evaluation and scaling functions are accessed direcly and not through
        // the function maps because they correspond to more than one material hash key.
        Endgame<KXK> ValueKXK[CLR_NO] =
        {
            Endgame<KXK> (WHITE),
            Endgame<KXK> (BLACK)
        };

        Endgame<KPKP> ScaleKPKP[CLR_NO] =
        {
            Endgame<KPKP> (WHITE),
            Endgame<KPKP> (BLACK)
        };
        Endgame<KPsK> ScaleKPsK[CLR_NO] =
        {
            Endgame<KPsK> (WHITE),
            Endgame<KPsK> (BLACK)
        };
        Endgame<KBPsKP> ScaleKBPsKP[CLR_NO] =
        {
            Endgame<KBPsKP> (WHITE),
            Endgame<KBPsKP> (BLACK)
        };
        Endgame<KQKRPs> ScaleKQKRPs[CLR_NO] =
        {
            Endgame<KQKRPs> (WHITE),
            Endgame<KQKRPs> (BLACK)
        } ;

        /// imbalance() calculates the imbalance by the piece count of each piece type for both colors.
        /// NOTE:: KING == BISHOP PAIR
        template<Color Own>
        i32 imbalance (const i32 (*count)[NONE])
        {
            constexpr auto Opp = WHITE == Own ? BLACK : WHITE;

            i32 value = PawnBonus[count[Own][PAWN]];
            // "The Evaluation of Material Imbalances in Chess"
            // Second-degree polynomial material imbalance by Tord Romstad
            for (auto pt1 : { PAWN, NIHT, BSHP, ROOK, QUEN })
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

    /// Material::probe() looks up a current position's material configuration in the material hash table
    /// and returns a pointer to it if found, otherwise a new Entry is computed and stored there.
    Entry* probe (const Position &pos)
    {
        auto *e = pos.thread->matl_table[pos.si->matl_key];

        if (e->key == pos.si->matl_key)
        {
            return e;
        }

        std::memset (e, 0x00, sizeof (*e));
        e->key = pos.si->matl_key;
        std::fill_n (e->scale, CLR_NO, SCALE_NORMAL);
        // Calculates the phase interpolating total non-pawn material between endgame and midgame limits.
        e->phase = i32(  std::min (VALUE_MIDGAME,
                         std::max (VALUE_ENDGAME, pos.si->non_pawn_material ()))
                       - VALUE_ENDGAME)
                 * PhaseResolution
                 / (VALUE_MIDGAME - VALUE_ENDGAME);

        // Let's look if have a specialized evaluation function for this
        // particular material configuration. First look for a fixed
        // configuration one, then a generic one if previous search failed.
        if (nullptr != (e->value_func = EndGames->probe<Value> (pos.si->matl_key)))
        {
            return e;
        }
        // Generic evaluation
        for (auto c : { WHITE, BLACK })
        {
            if (   pos.si->non_pawn_material ( c) >= VALUE_MG_ROOK
                && pos.count (~c) == 1)
            {
                e->value_func = &ValueKXK[c];
                return e;
            }
        }

        // Didn't find any special evaluation function for the current
        // material configuration. Is there a suitable scaling function?
        //
        // Face problems when there are several conflicting applicable
        // scaling functions and need to decide which one to use.
        const EndgameBase<Scale> *scale_func;
        if (nullptr != (scale_func = EndGames->probe<Scale> (pos.si->matl_key)))
        {
            e->scale_func[scale_func->strong_color] = scale_func;
            return e;
        }

        // Didn't find any specialized scaling function, so fall back on
        // generic scaling functions that refer to more than one material distribution.
        // Note that these ones don't return after setting the function.
        for (auto c : { WHITE, BLACK })
        {
            if (   pos.si->non_pawn_material ( c) == VALUE_MG_BSHP
                && pos.count ( c, BSHP) == 1
                && pos.count ( c, PAWN) != 0)
            {
                e->scale_func[c] = &ScaleKBPsKP[c];
            }
            else
            if (   pos.si->non_pawn_material ( c) == VALUE_MG_QUEN
                && pos.count ( c, QUEN) == 1
                && pos.count ( c, PAWN) == 0
                && pos.si->non_pawn_material (~c) == VALUE_MG_ROOK
                && pos.count (~c, ROOK) == 1
                && pos.count (~c, PAWN) != 0)
            {
                e->scale_func[c] = &ScaleKQKRPs[c];
            }

            // Zero or just one pawn makes it difficult to win, even with a material advantage.
            // This catches some trivial draws like KK, KBK and KNK and gives a very drawish
            // scale for cases such as KRKBP and KmmKm (except for KBBKN).
            if (   0 == pos.count (c, PAWN)
                && abs (  pos.si->non_pawn_material ( c)
                        - pos.si->non_pawn_material (~c)) <= VALUE_MG_BSHP)
            {
                e->scale[c] = pos.si->non_pawn_material ( c) <  VALUE_MG_ROOK ?
                                SCALE_DRAW :
                                Scale(pos.si->non_pawn_material (~c) <= VALUE_MG_BSHP ?
                                    4 :
                                    14);
            }
        }

        // Only pawns left
        if (   pos.si->non_pawn_material () == VALUE_ZERO
            && pos.pieces (PAWN) != 0)
        {
            if (0 == pos.pieces (BLACK, PAWN))
            {
                assert(2 <= pos.count (WHITE, PAWN));
                e->scale_func[WHITE] = &ScaleKPsK[WHITE];
            }
            else
            if (0 == pos.pieces (WHITE, PAWN))
            {
                assert(2 <= pos.count (BLACK, PAWN));
                e->scale_func[BLACK] = &ScaleKPsK[BLACK];
            }
            else
            if (   1 == pos.count (WHITE, PAWN)
                && 1 == pos.count (BLACK, PAWN))
            {
                e->scale_func[WHITE] = &ScaleKPKP[WHITE];
                e->scale_func[BLACK] = &ScaleKPKP[BLACK];
            }
        }

        // Evaluate the material imbalance.
        // Use KING as a place holder for the bishop pair "extended piece",
        // this allow us to be more flexible in defining bishop pair bonuses.
        i32 piece_count[CLR_NO][NONE] =
        {
            {
                pos.count (WHITE, PAWN), pos.count (WHITE, NIHT),
                pos.count (WHITE, BSHP), pos.count (WHITE, ROOK),
                pos.count (WHITE, QUEN), pos.paired_bishop (WHITE) ? 1 : 0
            },
            {
                pos.count (BLACK, PAWN), pos.count (BLACK, NIHT),
                pos.count (BLACK, BSHP), pos.count (BLACK, ROOK),
                pos.count (BLACK, QUEN), pos.paired_bishop (BLACK) ? 1 : 0
            }
        };

        i32 imb[CLR_NO] =
        {
            imbalance<WHITE> (piece_count),
            imbalance<BLACK> (piece_count)
        };

        auto value = (imb[WHITE] - imb[BLACK]) / 16; // Imbalance Resolution
        e->imbalance = mk_score (value, value);

        return e;
    }
}
