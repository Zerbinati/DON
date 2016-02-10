#include "MovePicker.h"

namespace MovePick {

    using namespace std;
    using namespace MoveGen;
    using namespace BitBoard;

    namespace {

        enum Stage : u08
        {
            S_MAIN    , S_GOOD_CAPTURE, S_KILLER, S_GOOD_QUIET, S_BAD_QUIET, S_BAD_CAPTURE,
            S_EVASION , S_ALL_EVASION,
            S_QSEARCH_WITH_CHECK   , S_QCAPTURE_1, S_QUIET_CHECK,
            S_QSEARCH_WITHOUT_CHECK, S_QCAPTURE_2,
            S_PROBCUT  , S_PROBCUT_CAPTURE,
            S_RECAPTURE, S_ALL_RECAPTURE,
            S_STOP
        };

        // Insertion sort in the range [beg, end), which is guaranteed to be stable, as it should be
        void insertion_sort (ValMove *beg, ValMove *end)
        {
            for (auto *p = beg+1; p < end; ++p)
            {
                ValMove t = *p, *q;
                for (q = p; q != beg && *(q-1) < t; --q)
                {
                    *q = *(q-1);
                }
                *q = t;
            }
        }

        // Generic insertion sort
        //template<class Iterator, class BinaryPredicate = greater<>>
        //void insertion_sort (Iterator beg, Iterator end, BinaryPredicate pred = BinaryPredicate{})
        //{
        //    for (auto it = beg; it != end; ++it)
        //    {
        //        rotate (std::upper_bound (beg, it, *it, pred), it, std::next (it));
        //    }
        //}

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

    MovePicker::MovePicker (const Position &pos, const HValueStats &hv, const CMValueStats &cmv, const Move *ss_km, Move ttm, Move cm)
        : _pos (pos)
        , _history_values (hv)
        , _counter_moves_values (&cmv)
        , _ss_killer_moves (ss_km)
        , _counter_move (cm)
    {
        _stage = _pos.checkers () != U64(0) ? S_EVASION : S_MAIN;
        
        _tt_move =   ttm != MOVE_NONE
                  && _pos.pseudo_legal (ttm) ?
                        ttm : MOVE_NONE;

        _end_move += _tt_move != MOVE_NONE ? 1 : 0;
    }

    MovePicker::MovePicker (const Position &pos, const HValueStats &hv, Move ttm, Square dst_sq, Depth depth)
        : _pos (pos)
        , _history_values (hv)
    {
        assert(depth <= DEPTH_ZERO);

        if (_pos.checkers () != U64(0))
        {
            _stage = S_EVASION;
        }
        else
        if (depth > DEPTH_QS_NO_CHECKS)
        {
            _stage = S_QSEARCH_WITH_CHECK;
        }
        else
        if (depth > DEPTH_QS_RECAPTURES)
        {
            _stage = S_QSEARCH_WITHOUT_CHECK;
        }
        else
        {
            _stage = S_RECAPTURE;
            _recapture_sq = dst_sq;
            ttm = MOVE_NONE;
        }

        _tt_move =   ttm != MOVE_NONE
                  && _pos.pseudo_legal (ttm) ?
                        ttm : MOVE_NONE;

        _end_move += _tt_move != MOVE_NONE ? 1 : 0;
    }

    MovePicker::MovePicker (const Position &pos, const HValueStats &hv, Move ttm, Value thr)
        : _pos (pos)
        , _history_values (hv)
        , _threshold (thr)
    {
        assert(_pos.checkers () == U64(0));

        _stage = S_PROBCUT;

        // In ProbCut generate captures with SEE higher than the given threshold
        _tt_move =   ttm != MOVE_NONE
                  && _pos.capture (ttm)
                  && _pos.pseudo_legal (ttm)
                  && _pos.see (ttm) > _threshold ?
                        ttm : MOVE_NONE;

        _end_move += _tt_move != MOVE_NONE ? 1 : 0;
    }

    // value() assigns a numerical move ordering score to each move in a move list.
    // The moves with highest scores will be picked first.

    template<>
    // Winning and equal captures in the main search are ordered by MVV/LVA.
    // In the main search we want to push captures with negative SEE values to the
    // badCaptures[] array, but instead of doing it now we delay until the move
    // has been picked up, saving some SEE calls in case we get a cutoff.
    void MovePicker::value<CAPTURE> ()
    {
        for (auto &vm : *this)
        {
            vm.value = PieceValues[MG][_pos.en_passant (vm.move) ? PAWN : ptype (_pos[dst_sq (vm.move)])]
              - Value(ptype (_pos[org_sq (vm.move)]) + 1);
        }
    }

    template<>
    void MovePicker::value<QUIET> ()
    {
        for (auto &vm : *this)
        {
            vm.value = _history_values[_pos[org_sq (vm.move)]][dst_sq (vm.move)]
              + (*_counter_moves_values)[_pos[org_sq (vm.move)]][dst_sq (vm.move)];
        }
    }

    template<>
    // Try winning and equal captures ordered by MVV/LVA,
    // then non-captures if destination square is not under attack, ordered by _history_values,
    // then bad-captures and quiet moves with a negative SEE. This last group is ordered by the SEE value.
    void MovePicker::value<EVASION> ()
    {
        for (auto &vm : *this)
        {
            auto see_value = _pos.see_sign (vm.move);
            if (see_value < VALUE_ZERO)
            {
                vm.value = see_value - MaxStatsValue; // At the bottom
            }
            else
            if (_pos.capture (vm.move))
            {
                vm.value = PieceValues[MG][_pos.en_passant (vm.move) ? PAWN : ptype (_pos[dst_sq (vm.move)])]
                  - Value(ptype (_pos[org_sq (vm.move)]) + 1) + MaxStatsValue;
            }
            else
            {
                vm.value = _history_values[_pos[org_sq (vm.move)]][dst_sq (vm.move)];
            }
        }
    }

    // generate_next_stage() generates, scores, and sorts the next bunch of moves,
    // when there are no more moves to try for the current stage.
    void MovePicker::generate_next_stage ()
    {
        assert(_stage != S_STOP);

        _cur_move = _beg_move;

        switch (++_stage)
        {

        case S_GOOD_CAPTURE:
        case S_QCAPTURE_1:
        case S_QCAPTURE_2:
        case S_PROBCUT_CAPTURE:
        case S_ALL_RECAPTURE:
            _end_move = _stage != S_ALL_RECAPTURE || _recapture_sq != SQ_NO ?
                generate<CAPTURE> (_beg_move, _pos) : _beg_move;
            if (_cur_move < _end_move-1)
            {
                value<CAPTURE> ();
            }
            break;

        case S_KILLER:
            _cur_move = _killer_moves;
            _end_move = _killer_moves + Killers;
            std::copy (_ss_killer_moves, _ss_killer_moves + Killers, _killer_moves);
            *_end_move = MOVE_NONE;
            // Be sure countermoves are different from killer_moves
            if (_counter_move != MOVE_NONE && std::find (_cur_move, _end_move, _counter_move) == _end_move)
            {
                *_end_move++ = _counter_move;
            }
            break;

        case S_GOOD_QUIET:
            _end_move = _end_quiet = generate<QUIET> (_beg_move, _pos);
            value<QUIET> ();
            // Split positive(+ve) value from the list
            _end_move = std::partition (_cur_move, _end_move, [](const ValMove &m) { return m.value > VALUE_ZERO; });
            if (_cur_move < _end_move-1)
            {
                insertion_sort (_cur_move, _end_move);
            }
            break;

        case S_BAD_QUIET:
            _cur_move = _end_move;
            _end_move = _end_quiet;
            if (_cur_move < _end_move-1)
            {
                insertion_sort (_cur_move, _end_move);
            }
            break;

        case S_BAD_CAPTURE:
            // Just pick them in reverse order to get MVV/LVA ordering
            _cur_move = _beg_move+MaxMoves-1;
            _end_move = _end_bad_capture;
            break;

        case S_ALL_EVASION:
            _end_move = generate<EVASION> (_beg_move, _pos);
            if (_cur_move < _end_move-1)
            {
                value<EVASION> ();
            }
            break;

        case S_QUIET_CHECK:
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
    // taking care not to return the ttMove if it has already been searched.
    Move MovePicker::next_move ()
    {
        do
        {
            while (_cur_move == _end_move && _stage != S_STOP)
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
                do
                {
                    Move move = pick_best (_cur_move++, _end_move);
                    if (move != _tt_move)
                    {
                        if (_pos.see_sign (move) >= VALUE_ZERO)
                        {
                            return move;
                        }
                        // Losing capture, move it to the tail of the array
                        *_end_bad_capture-- = move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_KILLER:
                do
                {
                    Move move = *_cur_move++;
                    if (   move != MOVE_NONE
                        && move != _tt_move
                        && !_pos.capture (move)
                        && _pos.pseudo_legal (move)
                       )
                    {
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_GOOD_QUIET:
            case S_BAD_QUIET:
                do
                {
                    Move move = *_cur_move++;
                    if (   move != MOVE_NONE
                        && move != _tt_move
                        && std::find (_killer_moves, _killer_moves + Killers + 1, move) == _killer_moves + Killers + 1 // Not killer move
                       )
                    {
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_BAD_CAPTURE:
                return *_cur_move--;
                break;

            case S_ALL_EVASION:
            case S_QCAPTURE_1:
            case S_QCAPTURE_2:
                do
                {
                    Move move = pick_best (_cur_move++, _end_move);
                    if (move != _tt_move)
                    {
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_QUIET_CHECK:
                do
                {
                    Move move = *_cur_move++;
                    if (move != _tt_move)
                    {
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_PROBCUT_CAPTURE:
                do
                {
                    Move move = pick_best (_cur_move++, _end_move);
                    if (   move != _tt_move
                        && _pos.see (move) > _threshold
                       )
                    {
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_ALL_RECAPTURE:
                do
                {
                    Move move = pick_best (_cur_move++, _end_move);
                    if (dst_sq (move) == _recapture_sq)
                    {
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
