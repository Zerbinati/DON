#ifndef _MOVE_GENERATOR_H_INC_
#define _MOVE_GENERATOR_H_INC_

#include "Type.h"
#include "Position.h"

namespace MoveGen {

    const u16 MaxMoves = 0x100; // Maximum Moves

    struct ValMove
    {
    public:
        Move  move  = MOVE_NONE;
        Value value = VALUE_ZERO;

        ValMove& operator= (const ValMove&) = default;

        operator Move () const  { return move; }
        void operator= (Move  m) { move  = m; }
        //explicit operator Value () const { return value; }
        //void operator= (Value v) { value = v; }

        // Ascending sort
        bool operator<  (const ValMove &vm) const { return value <  vm.value; }
        bool operator>  (const ValMove &vm) const { return value >  vm.value; }
        bool operator<= (const ValMove &vm) const { return value <= vm.value; }
        bool operator>= (const ValMove &vm) const { return value >= vm.value; }
        bool operator== (const ValMove &vm) const { return value == vm.value; }
        bool operator!= (const ValMove &vm) const { return value != vm.value; }
    };

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

    template<GenType GT>
    extern ValMove* generate (ValMove *moves, const Position &pos);

    // The MoveList<T> class is a simple wrapper around generate().
    // It sometimes comes in handy to use this class instead of
    // the low level generate() function.
    template<GenType GT, PieceType PT = NONE>
    class MoveList
    {

    private:
        ValMove  _moves_beg[MaxMoves]
              , *_moves_end = _moves_beg;

    public:

        MoveList () = delete;

        explicit MoveList (const Position &pos)
            : _moves_end (generate<GT> (_moves_beg, pos))
        {
            //if (PT != NONE)
            //{
            //    auto *moves_cur = _moves_beg;
            //    while (moves_cur < _moves_end)
            //    {
            //        if (PT != ptype (pos[org_sq (*moves_cur)]))
            //        {
            //            *moves_cur = *(--_moves_end);
            //            continue;
            //        }
            //        ++moves_cur;
            //    }
            //}
        }

        const ValMove* begin () const { return _moves_beg; }
        const ValMove* end   () const { return _moves_end; }

        size_t size () const { return size_t(_moves_end - _moves_beg); }
        
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
