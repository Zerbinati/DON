#ifndef _MOVE_GENERATOR_H_INC_
#define _MOVE_GENERATOR_H_INC_

#include "Type.h"
#include "Position.h"

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

namespace MoveGen {

    template<GenType GT>
    extern void generate (std::vector<ValMove> &moves, const Position &pos);

    extern void filter_illegal (std::vector<ValMove> &moves, const Position &pos);

    template<GenType GT, PieceType PT = NONE>
    class MoveList
        : public std::vector<ValMove>
    {
    public:
        MoveList () = delete;
        explicit MoveList (const Position &pos)
        {
            generate<GT> (*this, pos);
            if (PT != NONE)
            {
                erase (std::remove_if (begin (),
                                       end (),
                                       [&pos] (const ValMove &vm)
                                       {
                                            return PT != ptype (pos[org_sq (vm.move)]);
                                       }),
                       end ());
            }
        }

        bool contains (Move move) const
        {
            return std::find (begin (), end (), move) != end ();
        }
    };
}

#endif // _MOVE_GENERATOR_H_INC_
