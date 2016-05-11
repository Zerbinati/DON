#include "MovePicker.h"

#include "Thread.h"

namespace MovePick {

    using namespace std;
    using namespace BitBoard;
    using namespace MoveGen;
    using namespace Searcher;
    using namespace Threading;

    namespace {

        enum Stage : u08
        {
            S_MAIN    , S_GOOD_CAPTURE, S_KILLER, S_QUIET, S_BAD_CAPTURE,
            S_EVASION , S_ALL_EVASION,
            S_QSEARCH_WITH_CHECK   , S_QCAPTURE_1, S_QUIET_CHECK,
            S_QSEARCH_WITHOUT_CHECK, S_QCAPTURE_2,
            S_PROBCUT  , S_PROBCUT_CAPTURE,
            S_RECAPTURE, S_ALL_RECAPTURE,
            S_STOP
        };

        const Value PieceCapValues[MAX_PTYPE] =
        {
            VALUE_MG_PAWN, VALUE_MG_NIHT, VALUE_MG_BSHP, VALUE_MG_ROOK, VALUE_MG_QUEN, VALUE_MG_QUEN+1, VALUE_ZERO
        };

        // pick_best() finds the best move in the range [beg, end) and moves it to front,
        // it is faster than sorting all the moves in advance when there are few moves
        // e.g. the possible captures.
        ValMove& pick_best (ValMove *beg, ValMove *end)
        {
            std::swap (*beg, *std::max_element (beg, end));
            return *beg;
        }

    }

    // Constructors of the MovePicker class. As arguments pass information to help
    // it to return the (presumably) good moves first, to decide which moves to return
    // (in the quiescence search, for instance, only want to search captures, promotions, and some checks)
    // and about how important good move ordering is at the current node.

    MovePicker::MovePicker (const Position &pos, Move ttm, Depth d, Stack *ss)
        : _pos (pos)
        , _ss (ss)
        , _tt_move (ttm)
        , _depth (d)
    {
        assert(_tt_move == MOVE_NONE || (_pos.pseudo_legal (_tt_move) && _pos.legal (_tt_move)));
        assert(_depth > DEPTH_ZERO);

        _counter_move = _pos.thread ()->counter_moves[_ok ((ss-1)->current_move) ? _pos[dst_sq ((ss-1)->current_move)] : NO_PIECE][dst_sq ((ss-1)->current_move)];

        _stage = _pos.checkers () != 0 ? S_EVASION : S_MAIN;

        if (   _tt_move != MOVE_NONE
            && !_pos.pseudo_legal (_tt_move)
           )
        {
            _tt_move = MOVE_NONE;
        }
        _end_move += _tt_move != MOVE_NONE ? 1 : 0;
    }

    MovePicker::MovePicker (const Position &pos, Move ttm, Depth d, Square dst_sq)
        : _pos (pos)
        , _tt_move (ttm)
        , _depth (d)
    {
        assert(_depth <= DEPTH_ZERO);
        assert(_tt_move == MOVE_NONE || (_pos.pseudo_legal (_tt_move) && _pos.legal (_tt_move)));

        if (_pos.checkers () != 0)
        {
            _stage = S_EVASION;
        }
        else
        if (_depth >= DEPTH_QS_CHECK)
        {
            _stage = S_QSEARCH_WITH_CHECK;
        }
        else
        if (_depth > DEPTH_QS_RECAPTURE)
        {
            _stage = S_QSEARCH_WITHOUT_CHECK;
        }
        else
        {
            _stage = S_RECAPTURE;
            _recapture_sq = dst_sq;
            _tt_move = MOVE_NONE;
        }

        if (   _tt_move != MOVE_NONE
            && !_pos.pseudo_legal (_tt_move)
           )
        {
            _tt_move = MOVE_NONE;
        }
        _end_move += _tt_move != MOVE_NONE ? 1 : 0;
    }

    MovePicker::MovePicker (const Position &pos, Move ttm, Value thr)
        : _pos (pos)
        , _tt_move (ttm)
        , _threshold (thr)
    {
        assert(_pos.checkers () == 0);
        assert(_tt_move == MOVE_NONE || (_pos.pseudo_legal (_tt_move) && _pos.legal (_tt_move)));

        _stage = S_PROBCUT;

        // In ProbCut generate captures with SEE higher than the given threshold
        if (   _tt_move != MOVE_NONE
            && (   !_pos.pseudo_legal (_tt_move)
                || !_pos.capture (_tt_move)
                || _pos.see (_tt_move) <= _threshold
               )
           )
        {
            _tt_move = MOVE_NONE;
        }
        _end_move += _tt_move != MOVE_NONE ? 1 : 0;
    }

    // value() assigns a numerical move ordering score to each move in a move list.
    // The moves with highest scores will be picked first.

    template<>
    // Winning and equal captures in the main search are ordered by MVV/LVA.
    // In the main search push captures with negative SEE values to the bad-captures[],
    // but instead of doing it now we delay until the move has been picked up,
    // saving some SEE calls in case of a cutoff.
    void MovePicker::value<CAPTURE> ()
    {
        for (auto &vm : *this)
        {
            assert(_pos.pseudo_legal (vm.move));
            vm.value = PieceCapValues[_pos.en_passant (vm.move) ? PAWN : ptype (_pos[dst_sq (vm.move)])]
                     - PieceCapValues[ptype (_pos[org_sq (vm.move)])];
        }
    }

    template<>
    void MovePicker::value<QUIET> ()
    {
        const auto &history_values = _pos.thread ()->history_values;
        const auto *const &cmv  = (_ss-1)->counter_move_values;
        const auto *const &fmv1 = (_ss-2)->counter_move_values;
        const auto *const &fmv2 = (_ss-4)->counter_move_values;

        for (auto &vm : *this)
        {
            assert(_pos.pseudo_legal (vm.move));
            vm.value = history_values[_pos[org_sq (vm.move)]][dst_sq (vm.move)]
                + (cmv  != nullptr ? (*cmv )[_pos[org_sq (vm.move)]][dst_sq (vm.move)] : VALUE_ZERO)
                + (fmv1 != nullptr ? (*fmv1)[_pos[org_sq (vm.move)]][dst_sq (vm.move)] : VALUE_ZERO)
                + (fmv2 != nullptr ? (*fmv2)[_pos[org_sq (vm.move)]][dst_sq (vm.move)] : VALUE_ZERO);
        }
    }

    template<>
    // First try winning and equal captures, ordered by SEE value,
    // then non-captures if destination square is not under attack, ordered by history values,
    // then bad-captures and quiet moves with a negative SEE, ordered by SEE value.
    void MovePicker::value<EVASION> ()
    {
        const auto &history_values = _pos.thread ()->history_values;
        const auto *const &cmv  = _ss != nullptr ? (_ss-1)->counter_move_values : nullptr;
        const auto *const &fmv1 = _ss != nullptr ? (_ss-2)->counter_move_values : nullptr;
        const auto *const &fmv2 = _ss != nullptr ? (_ss-4)->counter_move_values : nullptr;

        for (auto &vm : *this)
        {
            assert(_pos.pseudo_legal (vm.move));
            auto see_value = _pos.see (vm.move);
            if (see_value < VALUE_ZERO)
            {
                vm.value = see_value - MaxStatsValue;
            }
            else
            if (_pos.capture (vm.move))
            {
                vm.value = see_value + MaxStatsValue;
            }
            else
            {
                vm.value = history_values[_pos[org_sq (vm.move)]][dst_sq (vm.move)]
                    + (cmv  != nullptr ? (*cmv )[_pos[org_sq (vm.move)]][dst_sq (vm.move)] : VALUE_ZERO)
                    + (fmv1 != nullptr ? (*fmv1)[_pos[org_sq (vm.move)]][dst_sq (vm.move)] : VALUE_ZERO)
                    + (fmv2 != nullptr ? (*fmv2)[_pos[org_sq (vm.move)]][dst_sq (vm.move)] : VALUE_ZERO);
            }
        }
    }

    // generate_next_stage() generates, scores, and sorts the next bunch of moves,
    // when there are no more moves to try for the current stage.
    void MovePicker::generate_next_stage ()
    {
        assert(_stage != S_STOP);
        switch (++_stage)
        {

        case S_GOOD_CAPTURE:
        case S_QCAPTURE_1:
        case S_QCAPTURE_2:
        case S_PROBCUT_CAPTURE:
        case S_ALL_RECAPTURE:
            _cur_move = _beg_move;
            _end_move = generate<CAPTURE> (_beg_move, _pos);
            if (_cur_move < _end_move-1)
            {
                value<CAPTURE> ();
            }
            break;

        case S_KILLER:
            std::copy (_ss->killer_moves, _ss->killer_moves + Killers, _killer_moves);
            _killer_moves[Killers] = _counter_move;
            _cur_move = _killer_moves;
            _end_move = _killer_moves + Killers + (std::find (_killer_moves, _killer_moves + Killers, _counter_move) == _killer_moves + Killers);
            break;

        case S_QUIET:
            _cur_move = _beg_move;
            _end_move = generate<QUIET> (_beg_move, _pos);
            if (_cur_move < _end_move-1)
            {
                value<QUIET> ();
            }
            break;

        case S_BAD_CAPTURE:
            _cur_move = _beg_bad_cap_move;
            _end_move = _beg_move+MaxMoves;
            break;

        case S_ALL_EVASION:
            assert(_pos.checkers () != 0);
            _cur_move = _beg_move;
            _end_move = filter_illegal (_pos, _beg_move, generate<EVASION> (_beg_move, _pos));
            if (_cur_move < _end_move-1)
            {
                value<EVASION> ();
            }
            break;

        case S_QUIET_CHECK:
            _cur_move = _beg_move;
            _end_move = generate<QUIET_CHECK> (_beg_move, _pos);
            break;

        case S_EVASION:
        case S_QSEARCH_WITH_CHECK:
        case S_QSEARCH_WITHOUT_CHECK:
        case S_PROBCUT:
        case S_RECAPTURE:
        case S_STOP:
            _stage = S_STOP;
            break;

        default:
            assert(false);
            break;
        }
    }

    // next_move() is the most important method of the MovePicker class.
    // It returns a new pseudo legal move every time it is called, until there are no more moves left.
    // It picks the move with the biggest value from a list of generated moves
    // taking care not to return the tt-move if it has already been searched.
    Move MovePicker::next_move ()
    {
        do {
            while (   _stage != S_STOP
                   && _cur_move == _end_move
                  )
            {
                generate_next_stage ();
            }

            switch (_stage)
            {

            case S_MAIN:
            case S_EVASION:
            case S_QSEARCH_WITH_CHECK:
            case S_QSEARCH_WITHOUT_CHECK:
            case S_PROBCUT:
                ++_cur_move;
                return _tt_move;
                break;

            case S_GOOD_CAPTURE:
                do {
                    auto move = pick_best (_cur_move++, _end_move).move;
                    if (move != _tt_move)
                    {
                        auto see_value = _pos.see_sign (move);
                        if (see_value >= VALUE_ZERO)
                        {
                            assert(move != MOVE_NONE);
                            return move;
                        }
                        // Losing capture, move it to the tail of the array
                        --_beg_bad_cap_move;
                        _beg_bad_cap_move->move  = move;
                        _beg_bad_cap_move->value = see_value;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_KILLER:
                do {
                    auto move = (*_cur_move++).move;
                    if (   move != MOVE_NONE
                        && move != _tt_move
                        && _pos.pseudo_legal (move)
                        && !_pos.capture (move)
                       )
                    {
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_QUIET:
                do {
                    auto move = pick_best (_cur_move++, _end_move).move;
                    if (   move != _tt_move
                        && std::find (_killer_moves, _killer_moves + Killers + 1, move) == _killer_moves + Killers + 1 // Not killer move
                       )
                    {
                        assert(move != MOVE_NONE);
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_BAD_CAPTURE:
                {
                    return pick_best (_cur_move++, _end_move).move;
                }
                break;

            case S_ALL_EVASION:
            case S_QCAPTURE_1:
            case S_QCAPTURE_2:
                do {
                    auto move = pick_best (_cur_move++, _end_move).move;
                    if (move != _tt_move)
                    {
                        assert(move != MOVE_NONE);
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_QUIET_CHECK:
                do {
                    auto move = (*_cur_move++).move;
                    if (move != _tt_move)
                    {
                        assert(move != MOVE_NONE);
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_PROBCUT_CAPTURE:
                do {
                    auto move = pick_best (_cur_move++, _end_move).move;
                    if (   move != _tt_move
                        && _pos.see (move) > _threshold
                       )
                    {
                        assert(move != MOVE_NONE);
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_ALL_RECAPTURE:
                do {
                    auto move = pick_best (_cur_move++, _end_move).move;
                    if (dst_sq (move) == _recapture_sq)
                    {
                        assert(move != MOVE_NONE);
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_STOP:
                return MOVE_NONE;
                break;

            default:
                assert(false);
                break;
            }
        } while (true);
    }

}
