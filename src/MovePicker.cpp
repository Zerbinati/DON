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
            S_RELAX   , S_GOOD_CAPTURE, S_QUIET, S_BAD_CAPTURE,
            S_EVASION , S_ALL_EVASION,
            S_QSEARCH_WITH_CHECK   , S_QCAPTURE_1, S_QUIET_CHECK,
            S_QSEARCH_WITHOUT_CHECK, S_QCAPTURE_2,
            S_PROBCUT  , S_PROBCUT_CAPTURE,
            S_RECAPTURE, S_ALL_RECAPTURE,
            S_STOP
        };

        // Finds the best move in the range [beg, end) and moves it to front,
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

    MovePicker::MovePicker (const Position &pos, Move ttm, const Stack *ss)
        : _pos (pos)
        , _ss (ss)
        , _tt_move (ttm)
    {
        assert(   ttm == MOVE_NONE
               || (   pos.pseudo_legal (ttm)
                   && pos.legal (ttm)));

        _stage =
            pos.checkers () != 0 ?
                S_EVASION :
                S_RELAX;

        _end_move += _tt_move != MOVE_NONE ? 1 : 0;
    }

    MovePicker::MovePicker (const Position &pos, Move ttm, Depth d, Move lm)
        : _pos (pos)
        , _tt_move (ttm)
    {
        assert(d <= DEPTH_0);
        assert(   ttm == MOVE_NONE
               || (   pos.pseudo_legal (ttm)
                   && pos.legal (ttm)));

        if (pos.checkers () != 0)
        {
            _stage = S_EVASION;
        }
        else
        if (d == DEPTH_0)
        {
            _stage = S_QSEARCH_WITH_CHECK;
        }
        else
        if (d > DEPTH_5_)
        {
            _stage = S_QSEARCH_WITHOUT_CHECK;
        }
        else
        {
            assert(_ok (lm));

            _stage = S_RECAPTURE;
            _recap_sq = dst_sq (lm);
            _tt_move = MOVE_NONE;
        }

        _end_move += _tt_move != MOVE_NONE ? 1 : 0;
    }

    MovePicker::MovePicker (const Position &pos, Move ttm, Value thr)
        : _pos (pos)
        , _tt_move (ttm)
        , _threshold (thr)
    {
        assert(pos.checkers () == 0);
        assert(   ttm == MOVE_NONE
               || (   pos.pseudo_legal (ttm)
                   && pos.legal (ttm)));

        _stage = S_PROBCUT;

        // In ProbCut generate captures with SEE higher than the given threshold
        if (   _tt_move != MOVE_NONE
            && (   !_pos.capture (_tt_move)
                || _pos.see (_tt_move) <= thr))
        {
            _tt_move = MOVE_NONE;
        }
        _end_move += _tt_move != MOVE_NONE ? 1 : 0;
    }

    // Assigns a numerical move ordering score to each move in a move list.
    // The moves with highest scores will be picked first.

    // Winning and equal captures in the main search are ordered by MVV/LVA, preferring captures near our home rank.
    // Surprisingly, this appears to perform slightly better than SEE-based move ordering,
    // exchanging big pieces before capturing a hanging piece probably helps to reduce the subtree size.
    // In the main search push captures with negative SEE values to the bad-captures[],
    // but instead of doing it now we delay until the move has been picked up,
    // saving some SEE calls in case of a cutoff.
    template<>
    void MovePicker::value<CAPTURE> ()
    {
        for (auto &vm : *this)
        {
            assert(_pos.pseudo_legal (vm.move)
                && _pos.legal (vm.move));
            vm.value =
                  PieceValues[MG][_pos.en_passant (vm.move) ? PAWN : ptype (_pos[dst_sq (vm.move)])]
                - 200 * Value(rel_rank (_pos.active (), dst_sq (vm.move)))
                - Value(ptype (_pos[org_sq (vm.move)]) + 1);
        }
    }

    template<>
    void MovePicker::value<QUIET> ()
    {
        const auto &history_values = _pos.thread ()->history_values;
        const auto &org_dst_values = _pos.thread ()->org_dst_values;
        const auto *const &cmv  = (_ss-1)->counter_move_values;
        const auto *const &fmv1 = (_ss-2)->counter_move_values;
        const auto *const &fmv2 = (_ss-4)->counter_move_values;

        for (auto &vm : *this)
        {
            assert(_pos.pseudo_legal (vm.move)
                && _pos.legal (vm.move));
            vm.value =
                  history_values(_pos[org_sq (vm.move)], dst_sq (vm.move))
                + org_dst_values(_pos.active (), vm.move)
                + (cmv  != nullptr ? (*cmv )(_pos[org_sq (vm.move)], dst_sq (vm.move)) : VALUE_ZERO)
                + (fmv1 != nullptr ? (*fmv1)(_pos[org_sq (vm.move)], dst_sq (vm.move)) : VALUE_ZERO)
                + (fmv2 != nullptr ? (*fmv2)(_pos[org_sq (vm.move)], dst_sq (vm.move)) : VALUE_ZERO);
        }
    }

    // First try winning and equal captures, ordered by SEE value,
    // then non-captures if destination square is not under attack, ordered by history values,
    // then bad-captures and quiet moves with a negative SEE, ordered by SEE value.
    template<>
    void MovePicker::value<EVASION> ()
    {
        const auto &history_values = _pos.thread ()->history_values;
        const auto &org_dst_values = _pos.thread ()->org_dst_values;

        for (auto &vm : *this)
        {
            assert(_pos.pseudo_legal (vm.move)
                && _pos.legal (vm.move));
            auto cap_value = _pos.see_sign (vm.move);
            if (cap_value < VALUE_ZERO)
            {
                vm.value = cap_value - MaxStatsValue;
            }
            else
            if (_pos.capture (vm.move))
            {
                vm.value =
                      PieceValues[MG][_pos.en_passant (vm.move) ? PAWN : ptype (_pos[dst_sq (vm.move)])]
                    - Value(ptype (_pos[org_sq (vm.move)]) + 1) + MaxStatsValue;
            }
            else
            {
                vm.value =
                      history_values(_pos[org_sq (vm.move)], dst_sq (vm.move))
                    + org_dst_values(_pos.active (), vm.move);
            }
        }
    }

    // Generates and sorts the next bunch of moves,
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
            _end_move = filter_illegal (_pos, _beg_move, generate<CAPTURE> (_beg_move, _pos));
            if (_cur_move < _end_move-1)
            {
                value<CAPTURE> ();
            }
            break;

        case S_QUIET:
            _cur_move = _beg_move;
            _end_move = filter_illegal (_pos, _beg_move, generate<QUIET> (_beg_move, _pos));
            if (_cur_move < _end_move-1)
            {
                value<QUIET> ();
            }
            // Move killers to top of quiet move
            {
                MoveVector killer_moves (_ss->killer_moves, _ss->killer_moves + MaxKillers);
                killer_moves.push_back (_pos.thread ()->counter_moves(_ok ((_ss-1)->current_move) ?
                                                                        _pos[dst_sq ((_ss-1)->current_move)] :
                                                                        NO_PIECE,
                                                                      dst_sq ((_ss-1)->current_move)));
                //killer_moves.erase (std::remove (killer_moves.begin (), killer_moves.end (), MOVE_NONE), killer_moves.end ());
                for (u08 k = 0; k < killer_moves.size (); ++k)
                {
                    auto km = killer_moves[k];
                    if (km != MOVE_NONE)
                    {
                        // Remove duplicates
                        std::replace (killer_moves.begin () + k + 1, killer_moves.end (), km, MOVE_NONE);
                        // Increase move value
                        auto *itr = std::find (_beg_move, _end_move, km);
                        if (itr != _end_move)
                        {
                            assert(_pos.pseudo_legal (km)
                                && _pos.legal (km));
                            itr->value = MaxStatsValue - i32(k);
                        }
                    }
                }
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
            _end_move = filter_illegal (_pos, _beg_move, generate<QUIET_CHECK> (_beg_move, _pos));
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

    // Returns a new legal move every time it is called, until there are no more moves left.
    // It picks the move with the biggest value from a list of generated moves
    // taking care not to return the tt-move if it has already been searched.
    Move MovePicker::next_move ()
    {
        do {
            while (   _stage != S_STOP
                   && _cur_move == _end_move)
            {
                generate_next_stage ();
            }

            switch (_stage)
            {

            case S_RELAX:
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
                            return move;
                        }
                        // Losing capture, move it to the tail of the array
                        --_beg_bad_cap_move;
                        _beg_bad_cap_move->move  = move;
                        _beg_bad_cap_move->value = see_value;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_QUIET:
                do {
                    auto move = pick_best (_cur_move++, _end_move).move;
                    if (move != _tt_move)
                    {
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
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_QUIET_CHECK:
                do {
                    auto move = (*_cur_move++).move;
                    if (move != _tt_move)
                    {
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_PROBCUT_CAPTURE:
                do {
                    auto move = pick_best (_cur_move++, _end_move).move;
                    if (   move != _tt_move
                        && _pos.see (move) > _threshold)
                    {
                        return move;
                    }
                } while (_cur_move < _end_move);
                break;

            case S_ALL_RECAPTURE:
                do {
                    auto move = pick_best (_cur_move++, _end_move).move;
                    if (dst_sq (move) == _recap_sq)
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
