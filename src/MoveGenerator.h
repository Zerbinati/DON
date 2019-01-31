#ifndef _MOVE_GENERATOR_H_INC_
#define _MOVE_GENERATOR_H_INC_

#include "Position.h"
#include "Type.h"

enum GenType : u08
{
    NATURAL,
    EVASION,
    CAPTURE,
    QUIET,
    CHECK,
    QUIET_CHECK,
    LEGAL,
};

template<GenType>
extern void generate (ValMoves&, const Position&);

extern void filter_illegal (ValMoves&, const Position&);

template<GenType GT, PieceType PT = NONE>
class MoveList
    : public ValMoves
{
public:

    MoveList () = delete;
    //MoveList (const MoveList&) = delete;

    explicit MoveList (const Position &pos)
    {
        generate<GT> (*this, pos);
        //if (NONE != PT)
        //{
        //    erase (std::remove_if (begin (),
        //                           end (),
        //                           [&pos] (const ValMove &vm)
        //                           {
        //                               return PT != ptype (pos[org_sq (vm)]);
        //                           }),
        //            end ());
        //}
    }

    bool contains (Move move) const
    {
        return std::find (begin (), end (), move) != end ();
    }
};

struct Perft
{
    i16 moves;
    u64 all;
    u64 capture;
    u64 enpassant;
    u64 any_check;
    u64 dsc_check;
    //u64 dbl_check;
    u64 castle;
    u64 promote;

    Perft ()
    {
        moves = 0;
        all = 0;
        capture = 0;
        enpassant = 0;
        any_check = 0;
        dsc_check = 0;
        //dbl_check = 0;
        castle = 0;
        promote = 0;
    }

    void operator+= (Perft &p)
    {
        all       += p.all;
        capture   += p.capture;
        enpassant += p.enpassant;
        any_check += p.any_check;
        dsc_check += p.dsc_check;
        //dbl_check += p.dbl_check;
        castle    += p.castle;
        promote   += p.promote;
    }
    void operator-= (Perft &p)
    {
        all       -= p.all;
        capture   -= p.capture;
        enpassant -= p.enpassant;
        any_check -= p.any_check;
        dsc_check -= p.dsc_check;
        //dbl_check -= p.dbl_check;
        castle    -= p.castle;
        promote   -= p.promote;
    }
};

template<bool RootNode>
extern Perft perft (Position&, i16);

#endif // _MOVE_GENERATOR_H_INC_
