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

    extern ValMove* filter_illegal (const Position &pos, ValMove *beg_move, ValMove *end_move);

    template<GenType GT>
    extern ValMove* generate (ValMove *moves, const Position &pos);

    // The MoveList<T> class is a simple wrapper around generate().
    // It sometimes comes in handy to use this class instead of
    // the low level generate() function.
    template<GenType GT, PieceType PT = NONE>
    class MoveList
    {

    private:
        ValMove  _beg_move[MaxMoves]
              , *_end_move = _beg_move;

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
                        *cur_move = *(--_end_move);
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

        //explicit operator std::string () const;
    };

    //template<class CharT, class Traits, GenType GT>
    //inline std::basic_ostream<CharT, Traits>&
    //    operator<< (std::basic_ostream<CharT, Traits> &os, MoveList<GT> &movelist)
    //{
    //    for (const auto &vm : movelist)
    //    {
    //        os << vm.move << std::endl;
    //    }
    //    return os;
    //}
}

#endif // _MOVE_GENERATOR_H_INC_
