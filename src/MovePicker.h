#ifndef _MOVE_PICKER_H_INC_
#define _MOVE_PICKER_H_INC_

#include "Type.h"
#include "Position.h"
#include "MoveGenerator.h"
#include "Searcher.h"

namespace MovePick {

    // MovePicker class is used to pick one pseudo legal move at a time from the
    // current position. The most important method is next_move(), which returns a
    // new pseudo legal move each time it is called, until there are no moves left,
    // when MOVE_NONE is returned. In order to improve the efficiency of the
    // alfa-beta algorithm, MovePicker attempts to return the moves which are most
    // likely to get a cut-off first.
    class MovePicker
    {
    private:
        std::vector<ValMove> _moves;
        std::vector<ValMove> _bad_cap_moves;
        i32 _index = 0;

        const Position &_pos;
        const Stack *const _ss = nullptr;

        u08     _stage      = 0;

        Move    _tt_move    = MOVE_NONE;
        Square  _recap_sq   = SQ_NO;
        Value   _threshold  = VALUE_ZERO;

        // value() assign a numerical move ordering score to each move in a move list.
        // The moves with highest scores will be picked first.
        template<GenType GT>
        void value ();

        void generate_next_stage ();

        ValMove& pick_best (i32 i);

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
