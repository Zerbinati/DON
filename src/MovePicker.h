#ifndef _MOVE_PICKER_H_INC_
#define _MOVE_PICKER_H_INC_

#include "Type.h"
#include "Position.h"
#include "MoveGenerator.h"
#include "Searcher.h"

namespace MovePick {

    // MovePicker class is used to pick one legal moves from the current position.
    // next_move() which returns a new legal move each time it is called, until there are no moves left,
    class MovePicker
    {
    private:
        const Position &_pos;
        const Stack *const _ss = nullptr;

        std::vector<ValMove> _moves;
        std::vector<ValMove> _bad_cap_moves;
        
        i32     _index      = 0;
        u08     _stage      = 0;

        Move    _tt_move    = MOVE_NONE;
        Square  _recap_sq   = SQ_NO;
        Value   _threshold  = VALUE_ZERO;

        // value() assign a numerical move ordering score to each move in a move list.
        // The moves with highest scores will be picked first.
        template<GenType GT>
        void value ();

        void generate_next_stage ();

        ValMove& pick_best_move (i32 i);

    public:

        MovePicker () = delete;
        MovePicker (const MovePicker&) = delete;
        MovePicker& operator= (const MovePicker&) = delete;

        MovePicker (const Position&, Move, const Stack *const&);
        MovePicker (const Position&, Move, i16, Move);
        MovePicker (const Position&, Move, Value);

        Move next_move ();
    };

}

#endif // _MOVE_PICKER_H_INC_
