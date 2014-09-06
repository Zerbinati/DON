#ifndef _MOVE_GENERATOR_H_INC_
#define _MOVE_GENERATOR_H_INC_

#include "Type.h"

class Position;

const u08   MaxMoves   = 255;

struct ValMove
{
public:
    Move    move;
    Value   value;

    // Unary predicate functor used by std::partition to split positive(+ve) scores from
    // remaining ones so to sort separately the two sets, and with the second sort delayed.
    inline bool operator() (const ValMove &vm) { return (vm.value > VALUE_ZERO); }

    friend bool operator<  (const ValMove &vm1, const ValMove &vm2) { return (vm1.value <  vm2.value); }
    friend bool operator>  (const ValMove &vm1, const ValMove &vm2) { return (vm1.value >  vm2.value); }
    friend bool operator<= (const ValMove &vm1, const ValMove &vm2) { return (vm1.value <= vm2.value); }
    friend bool operator>= (const ValMove &vm1, const ValMove &vm2) { return (vm1.value >= vm2.value); }
    friend bool operator== (const ValMove &vm1, const ValMove &vm2) { return (vm1.value == vm2.value); }
    friend bool operator!= (const ValMove &vm1, const ValMove &vm2) { return (vm1.value != vm2.value); }

};

namespace MoveGenerator {

    // Types of Generator
    enum GenT : u08
    {
        // PSEUDO-LEGAL MOVES
        RELAX,       // Normal moves.
        EVASION,     // Save the friendly king from check
        CAPTURE,     // Change material balance where an enemy piece is captured.
        QUIET,       // Do not capture pieces but under-promotion is allowed.
        CHECK,       // Checks the enemy King in any way possible.
        QUIET_CHECK, // Do not change material and only checks the enemy King (no capture or promotion).

        // ------------------------
        LEGAL        // Legal moves

    };

    template<GenT GT>
    extern ValMove* generate (ValMove *moves, const Position &pos);

    // The MoveList struct is a simple wrapper around generate(). It sometimes comes
    // in handy to use this class instead of the low level generate() function.
    template<GenT GT>
    class MoveList
    {

    private:
        ValMove _moves[MaxMoves]
              , *_cur
              , *_end;

    public:

        explicit MoveList (const Position &pos)
            : _cur (_moves)
            , _end (generate<GT> (_moves, pos))
        {
            _end->move = MOVE_NONE;
        }

        inline const ValMove* begin() const { return _cur; }
        inline const ValMove* end  () const { return _end; }

        inline void operator++ () { ++_cur; }
        inline void operator-- () { --_cur; }

        inline Move operator* () const { return _cur->move; }

        inline u16 size       () const { return u16(_end - _cur); }

        bool contains (Move m) const
        {
            for (const ValMove &ms : *this)
            {
                if (ms.move == m) return true;
            }
            return false;
        }

        //template<class CharT, class Traits, GenT GT>
        //friend std::basic_ostream<CharT, Traits>&
        //    operator<< (std::basic_ostream<CharT, Traits> &os, MoveList<GT> &movelist);
    };

    //template<class CharT, class Traits, GenT GT>
    //inline std::basic_ostream<CharT, Traits>&
    //    operator<< (std::basic_ostream<CharT, Traits> &os, MoveList<GT> &movelist)
    //{
    //    ValMove *cur = movelist._cur;
    //    for ( ; *movelist; ++movelist)
    //    {
    //        os << *movelist << std::endl;
    //    }
    //    movelist._cur = cur;
    //    return os;
    //}
}

#endif // _MOVE_GENERATOR_H_INC_
