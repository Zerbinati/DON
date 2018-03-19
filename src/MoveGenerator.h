#ifndef _MOVE_GENERATOR_H_INC_
#define _MOVE_GENERATOR_H_INC_

#include "Position.h"
#include "Type.h"

enum class GenType : u08
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

template<GenType GT, PieceType PT = PieceType::NONE>
class MoveList
    : public ValMoves
{
public:

    MoveList () = delete;
    //MoveList (const MoveList&) = delete;

    explicit MoveList (const Position &pos)
    {
        generate<GT> (*this, pos);
        if (PieceType::NONE != PT)
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

template<bool RootNode>
extern u64 perft (Position&, i16);

#endif // _MOVE_GENERATOR_H_INC_
