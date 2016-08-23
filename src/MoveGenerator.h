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
    extern ValMove* generate (ValMove *moves, const Position &pos);

    extern ValMove* filter_illegal (const Position &pos, ValMove *cur_move, ValMove *end_move);

    // The MoveList class is a simple wrapper around generate().
    template<GenType GT, PieceType PT = NONE>
    class MoveList
    {
    private:
        ValMove _beg_move[MaxMoves]
            ,  *_end_move;

    public:
        MoveList () = delete;
        explicit MoveList (const Position &pos)
            : _end_move (generate<GT> (_beg_move, pos))
        {
            if (PT != NONE)
            {
                auto *cur_move = _beg_move;
                while (cur_move < _end_move)
                {
                    if (PT != ptype (pos[org_sq (cur_move->move)]))
                    {
                        cur_move->move = (--_end_move)->move;
                        continue;
                    }
                    ++cur_move;
                }
            }
        }

        const ValMove* begin () const { return _beg_move; }
        const ValMove* end   () const { return _end_move; }

        u16 size () const { return u16(_end_move - _beg_move); }
        
        bool contains (Move move) const
        {
            for (const auto &vm : *this)
            {
                if (vm.move == move)
                {
                    return true;
                }
            }
            return false;
        }
    };

}

#endif // _MOVE_GENERATOR_H_INC_
