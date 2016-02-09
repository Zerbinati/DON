#ifndef _MOVE_GENERATOR_H_INC_
#define _MOVE_GENERATOR_H_INC_

#include "Type.h"
#include "Position.h"

// Generator Type
enum GenType : u08
{
    // PSEUDO-LEGAL MOVES
    RELAX,       // Normal.
    EVASION,     // Save the friendly king from check.
    CAPTURE,     // Change material balance where an enemy piece is captured.
    QUIET,       // Do not capture pieces but under-promotion is allowed.
    CHECK,       // Checks the enemy King in any way possible.
    QUIET_CHECK, // Do not change material and only checks the enemy King (no capture or promotion).
                 // ------------------------
    LEGAL,       // Legal.
};

namespace MoveGen {

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
            //if (PT != NONE)
            //{
            //    auto *cur_move = _beg_move;
            //    while (cur_move < _end_move)
            //    {
            //        if (PT != ptype (pos[org_sq (*cur_move)]))
            //        {
            //            *cur_move = *(--_end_move);
            //            continue;
            //        }
            //        ++cur_move;
            //    }
            //}
        }

        const ValMove* begin () const { return _beg_move; }
        const ValMove* end   () const { return _end_move; }

        size_t size () const { return size_t(_end_move - _beg_move); }
        
        bool contains (Move move) const
        {
            for (const auto &vm : *this)
            {
                if (vm.move == move) return true;
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
