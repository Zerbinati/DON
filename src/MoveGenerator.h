#ifndef _MOVE_GENERATOR_H_INC_
#define _MOVE_GENERATOR_H_INC_

#include "Type.h"
#include "Position.h"

const u16 MaxMoves  = 256;  // Maximum Moves

enum GenType : u08
{
    RELAX,
    EVASION,
    CAPTURE,
    QUIET,
    CHECK,
    QUIET_CHECK,
    LEGAL,
};

namespace MoveGen {

    template<GenType GT>
    extern void generate (const Position &pos, std::vector<ValMove> &moves);

    extern void filter_illegal (const Position &pos, std::vector<ValMove> &moves);

    template<GenType GT, PieceType PT = NONE>
    class MoveList
        : public std::vector<ValMove>
    {

    public:
        MoveList () = delete;
        explicit MoveList (const Position &pos)
        {
            generate<GT> (pos, *this);
            //if (PT != NONE)
            //{
            //    auto *cur_move = _beg_move;
            //    while (cur_move < _end_move)
            //    {
            //        if (PT != ptype (pos[org_sq (cur_move->move)]))
            //        {
            //            cur_move->move = (--_end_move)->move;
            //            continue;
            //        }
            //        ++cur_move;
            //    }
            //}
        }

        bool contains (Move move) const
        {
            return std::find (begin (), end (), move) != end ();
        }
    };
}

#endif // _MOVE_GENERATOR_H_INC_
