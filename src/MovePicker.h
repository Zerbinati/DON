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

        ValMove  _beg_move[MaxMoves]
            ,   *_cur_move         = _beg_move
            ,   *_end_move         = _beg_move
            ,   *_beg_bad_cap_move = _beg_move+MaxMoves;

        const Position        &_pos;
        const Searcher::Stack *_ss = nullptr;

        Move    _tt_move        = MOVE_NONE;
        Move    _counter_move   = MOVE_NONE;
        Square  _recapture_sq   = SQ_NO;
        Value   _threshold      = VALUE_ZERO;
        Depth   _depth          = DEPTH_0;
        u08     _stage          = 0;

        ValMove _killer_moves[Killers + 1];

        // value() assign a numerical move ordering score to each move in a move list.
        // The moves with highest scores will be picked first.
        template<GenType GT>
        void value ();

        void generate_next_stage ();

    public:

        MovePicker () = delete;
        MovePicker (const MovePicker&) = delete;
        MovePicker& operator= (const MovePicker&) = delete;

        MovePicker (const Position&, Move, Depth, Searcher::Stack*);
        MovePicker (const Position&, Move, Depth, Square);
        MovePicker (const Position&, Move, Value);

        ValMove* begin () { return _beg_move; }
        ValMove* end   () { return _end_move; }

        Move next_move ();

    };

}

#endif // _MOVE_PICKER_H_INC_
