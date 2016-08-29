#include "MovePicker.h"

#include <set>

#include "Thread.h"

namespace MovePick {

    using namespace std;
    using namespace BitBoard;
    using namespace MoveGen;
    using namespace Searcher;
    using namespace Threading;

    namespace {

        const Value MaxValue = Value(1 << 28);

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

        // Remove duplicates keeping order unchanged
        template<typename T>
        void remove_duplicates (std::vector<T> &vec)
        {
            std::set<T> set;
            vec.erase (std::remove_if (vec.begin (),
                                       vec.end (),
                                       [&set](const T &item)
                                       {
                                           if (set.find (item) == set.end ())
                                           {
                                               set.insert (item);
                                               return false;
                                           }
                                           return true;
                                       }),
                       vec.end ());
        }

    }

    // Constructors of the MovePicker class. As arguments pass information to help
    // it to return the (presumably) good moves first, to decide which moves to return
    // (in the quiescence search, for instance, only want to search captures, promotions, and some checks)
    // and about how important good move ordering is at the current node.

    MovePicker::MovePicker (const Position &pos, Move ttm, const Stack *const &ss)
        : _pos (pos)
        , _ss (ss)
        , _tt_move (ttm)
    {
        assert(   ttm == MOVE_NONE
               || (   pos.pseudo_legal (ttm)
                   && pos.legal (ttm)));
        assert(_moves.empty ()
            && _bad_cap_moves.empty ());

        _stage =
            pos.checkers () != 0 ?
                S_EVASION :
                S_RELAX;
        if (_tt_move != MOVE_NONE)
        {
            _index = -1;
        }
    }

    MovePicker::MovePicker (const Position &pos, Move ttm, i16 d, Move lm)
        : _pos (pos)
        , _tt_move (ttm)
    {
        assert(d <= 0);
        assert(   ttm == MOVE_NONE
               || (   pos.pseudo_legal (ttm)
                   && pos.legal (ttm)));
        assert(_moves.empty ()
            && _bad_cap_moves.empty ());

        if (pos.checkers () != 0)
        {
            _stage = S_EVASION;
        }
        else
        if (d == 0)
        {
            _stage = S_QSEARCH_WITH_CHECK;
        }
        else
        if (d > -5)
        {
            _stage = S_QSEARCH_WITHOUT_CHECK;
        }
        else
        {
            assert(lm != MOVE_NONE);

            _stage = S_RECAPTURE;
            _recap_sq = dst_sq (lm);
            _tt_move = MOVE_NONE;
        }
        if (_tt_move != MOVE_NONE)
        {
            _index = -1;
        }
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
        assert(_moves.empty ()
            && _bad_cap_moves.empty ());

        _stage = S_PROBCUT;

        // In ProbCut generate captures with SEE higher than the given threshold
        if (   _tt_move != MOVE_NONE
            && (   !_pos.capture (_tt_move)
                || _pos.see (_tt_move) <= thr))
        {
            _tt_move = MOVE_NONE;
        }
        if (_tt_move != MOVE_NONE)
        {
            _index = -1;
        }
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
        for (auto &vm : _moves)
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
        for (auto &vm : _moves)
        {
            assert(_pos.pseudo_legal (vm.move)
                && _pos.legal (vm.move));

            vm.value =
                  _pos.thread ()->history_values(_pos[org_sq (vm.move)], vm.move)
                + ((_ss-1)->cm_history_values != nullptr ? (*(_ss-1)->cm_history_values)(_pos[org_sq (vm.move)], vm.move) : VALUE_ZERO)
                + ((_ss-2)->cm_history_values != nullptr ? (*(_ss-2)->cm_history_values)(_pos[org_sq (vm.move)], vm.move) : VALUE_ZERO)
                + ((_ss-4)->cm_history_values != nullptr ? (*(_ss-4)->cm_history_values)(_pos[org_sq (vm.move)], vm.move) : VALUE_ZERO);
        }
    }

    // First try winning and equal captures, ordered by SEE value,
    // then non-captures if destination square is not under attack, ordered by history values,
    // then bad-captures and quiet moves with a negative SEE, ordered by SEE value.
    template<>
    void MovePicker::value<EVASION> ()
    {
        for (auto &vm : _moves)
        {
            assert(_pos.pseudo_legal (vm.move)
                && _pos.legal (vm.move));

            if (_pos.capture (vm.move))
            {
                auto cap_value = _pos.see_sign (vm.move);
                if (cap_value < VALUE_ZERO)
                {
                    vm.value = cap_value - MaxValue;
                }
                else
                {
                    vm.value =
                          PieceValues[MG][_pos.en_passant (vm.move) ? PAWN : ptype (_pos[dst_sq (vm.move)])]
                        - 200 * Value(rel_rank (_pos.active (), dst_sq (vm.move)))
                        - Value(ptype (_pos[org_sq (vm.move)]) + 1) + MaxValue;
                }
            }
            else
            {
                vm.value =
                      _pos.thread ()->history_values(_pos[org_sq (vm.move)], vm.move);
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
            generate<CAPTURE> (_moves, _pos);
            filter_illegal (_moves, _pos);
            if (_tt_move != MOVE_NONE)
            {
                _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
            }
            _index = 0;
            if (_moves.size () > 1)
            {
                value<CAPTURE> ();
            }
            break;

        case S_QUIET:
            generate<QUIET> (_moves, _pos);
            filter_illegal (_moves, _pos);
            if (_tt_move != MOVE_NONE)
            {
                _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
            }
            _index = 0;
            if (_moves.size () > 1)
            {
                value<QUIET> ();
            }
            // Move killers to top of quiet move
            {
                MoveVector killer_moves (_ss->killer_moves, _ss->killer_moves + MaxKillers);
                if ((_ss-1)->cm_history_values != nullptr)
                {
                    auto cm = _pos.thread ()->counter_moves (_pos[fix_dst_sq ((_ss-1)->current_move)], (_ss-1)->current_move);
                    if (cm != MOVE_NONE)
                    {
                        killer_moves.push_back (cm);
                    }
                }
                //killer_moves.erase (std::remove (killer_moves.begin (), killer_moves.end (), MOVE_NONE), killer_moves.end ());
                remove_duplicates (killer_moves);
                i32 k = 0;
                for (auto km : killer_moves)
                {
                    auto itr = std::find (_moves.begin (), _moves.end (), km);
                    if (itr != _moves.end ())
                    {
                        assert(_pos.pseudo_legal (km)
                            && _pos.legal (km));
                        itr->value = MaxValue - k++;
                    }
                }
            }
            break;

        case S_BAD_CAPTURE:
            _moves = _bad_cap_moves;
            _index = 0;
            break;

        case S_ALL_EVASION:
            assert(_pos.checkers () != 0);
            generate<EVASION> (_moves, _pos);
            filter_illegal (_moves, _pos);
            if (_tt_move != MOVE_NONE)
            {
                _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
            }
            _index = 0;
            if (_moves.size () > 1)
            {
                value<EVASION> ();
            }
            break;

        case S_QUIET_CHECK:
            generate<QUIET_CHECK> (_moves, _pos);
            filter_illegal (_moves, _pos);
            if (_tt_move != MOVE_NONE)
            {
                _moves.erase (std::remove (_moves.begin (), _moves.end (), _tt_move), _moves.end ());
            }
            _index = 0;
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
                   && _index == i32(_moves.size ()))
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
                ++_index;
                assert(MoveList<LEGAL> (_pos).contains (_tt_move));
                return _tt_move;
                break;

            case S_GOOD_CAPTURE:
                {
                    auto move = pick_best_move (_index++).move;
                    assert(move != _tt_move);
                    auto see_value = _pos.see_sign (move);
                    if (see_value >= VALUE_ZERO)
                    {
                        return move;
                    }
                    // Losing capture, add it to the bad capture moves
                    _bad_cap_moves.push_back ({move, see_value});
                }
                break;

            case S_QUIET:
                return pick_best_move (_index++).move;
                break;

            case S_BAD_CAPTURE:
                return pick_best_move (_index++).move;
                break;

            case S_ALL_EVASION:
            case S_QCAPTURE_1:
            case S_QCAPTURE_2:
                return pick_best_move (_index++).move;
                break;

            case S_QUIET_CHECK:
                return _moves[_index++].move;
                break;

            case S_PROBCUT_CAPTURE:
                {
                    auto move = pick_best_move (_index++).move;
                    assert(move != _tt_move);
                    if (_pos.see (move) > _threshold)
                    {
                        return move;
                    }
                }
                break;

            case S_ALL_RECAPTURE:
                {
                    auto move = pick_best_move (_index++).move;
                    if (dst_sq (move) == _recap_sq)
                    {
                        return move;
                    }
                }
                break;

            case S_STOP:
                return MOVE_NONE;
                break;
            }
        } while (true);
    }

    // Finds the best move in the range [beg, end) and moves it to front,
    // it is faster than sorting all the moves in advance when there are few moves
    // e.g. the possible captures.
    ValMove& MovePicker::pick_best_move (i32 i)
    {
        auto itr = _moves.begin () + i;
        std::swap (*itr, *std::max_element (itr, _moves.end ()));
        return *itr;
    }

}
