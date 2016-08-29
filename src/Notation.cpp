#include "Notation.h"

#include "Position.h"
#include "MoveGenerator.h"

using namespace std;

namespace Notation {

    using namespace BitBoard;
    using namespace MoveGen;
    using namespace Threading;

    namespace {

        // Type of the Ambiguity
        enum AmbiguityType : u08
        {
            AMB_NONE,
            AMB_RANK,
            AMB_FILE,
            AMB_SQR,
        };

        // Ambiguity if more then one piece of same type can reach 'dst' with a legal move.
        // NOTE: for pawns it is not needed because 'org' file is explicit.
        AmbiguityType ambiguity (Move m, const Position &pos)
        {
            assert(pos.legal (m));

            auto org = org_sq (m);
            auto dst = dst_sq (m);

            // Disambiguation if have more then one piece with destination 'dst'
            // note that for pawns is not needed because starting file is explicit.

            auto amb = (attacks_bb (pos[org], dst, pos.pieces ()) & pos.pieces (pos.active (), ptype (pos[org]))) - org;
            auto pcs = amb; // & ~pos.abs_pinneds (pos.active ()); // If pinned piece is considered as ambiguous
            while (pcs != 0)
            {
                auto sq = pop_lsq (pcs);
                if (!pos.legal (mk_move<NORMAL> (sq, dst)))
                {
                    amb -= sq;
                }
            }
            if (amb != 0)
            {
                if ((amb & file_bb (org)) == 0) return AMB_RANK;
                if ((amb & rank_bb (org)) == 0) return AMB_FILE;
                return AMB_SQR;
            }
            return AMB_NONE;
        }

        // Value to string
        string pretty_value (Value v, Color c)
        {
            ostringstream oss;
            if (abs (v) < +VALUE_MATE - i32(MaxPlies))
            {
                oss << std::setprecision (2) << std::fixed << std::showpos << value_to_cp (c == WHITE ? +v : -v);
            }
            else
            {
                oss << "#" << std::showpos << i32(v > VALUE_ZERO ?
                                                    +(VALUE_MATE - v + 1) :
                                                    -(VALUE_MATE + v + 0)) / 2;
            }
            return oss.str ();
        }

        // Time to string
        string pretty_time (TimePoint time)
        {
            u32 hours  = u32(time / HourMilliSec);
            time      %= HourMilliSec;
            u32 minutes= u32(time / MinuteMilliSec);
            time      %= MinuteMilliSec;
            u32 seconds= u32(time / MilliSec);
            time      %= MilliSec;
            time      /= 10;

            ostringstream oss;
            oss << std::setfill ('0')
                << std::setw (2) << hours   << ":"
                << std::setw (2) << minutes << ":"
                << std::setw (2) << seconds << "."
                << std::setw (2) << time
                << std::setfill (' ');
            return oss.str ();
        }

    }

    // Converts a move to a string in coordinate algebraic notation representation.
    // The only special case is castling moves,
    //  - e1g1 notation in normal chess mode,
    //  - e1h1 notation in chess960 mode.
    // Internally castle moves are always coded as "king captures rook".
    string move_to_can (Move m)
    {
        if (m == MOVE_NONE) return "(none)";
        if (m == MOVE_NULL) return "(null)";

        auto can = to_string (org_sq (m))
                 + to_string (fix_dst_sq (m, Position::Chess960));
        if (mtype (m) == PROMOTE)
        {
            can += char(tolower (char(PieceChar[promote (m)])));
        }
        return can;
    }
    // Converts a string representing a move in coordinate algebraic notation
    // to the corresponding legal move, if any.
    Move move_from_can (string &can, const Position &pos)
    {
        if (   can.length () == 5
            && isupper (can[4]))
        {
            can[4] = char (tolower (can[4])); // Promotion piece in lowercase
        }
        for (const auto &vm : MoveList<LEGAL> (pos))
        {
            if (can == move_to_can (vm.move))
            {
                return vm.move;
            }
        }
        return MOVE_NONE;
    }

    // Converts a move to a string in short algebraic notation representation.
    string move_to_san (Move m, Position &pos)
    {
        if (m == MOVE_NONE) return "(none)";
        if (m == MOVE_NULL) return "(null)";
        assert(MoveList<LEGAL> (pos).contains (m));

        string san;
        auto org = org_sq (m);
        auto dst = dst_sq (m);

        if (mtype (m) != CASTLE)
        {
            auto pt = ptype (pos[org]);

            if (pt != PAWN)
            {
                san = PieceChar[pt];
                if (pt != KING)
                {
                    // Disambiguation if have more then one piece of type 'pt'
                    // that can reach 'dst' with a legal move.
                    switch (ambiguity (m, pos))
                    {
                    case AMB_NONE:                               break;
                    case AMB_RANK: san += to_char (_file (org)); break;
                    case AMB_FILE: san += to_char (_rank (org)); break;
                    case AMB_SQR:  san += to_string (org);       break;
                    default:       assert(false);                break;
                    }
                }
            }

            if (pos.capture (m))
            {
                if (pt == PAWN)
                {
                    san += to_char (_file (org));
                }
                san += "x";
            }

            san += to_string (dst);

            if (   pt == PAWN
                && mtype (m) == PROMOTE)
            {
                san += "=";
                san += PieceChar[promote (m)];
            }
        }
        else
        {
            san = (dst > org ? "O-O" : "O-O-O");
        }

        // Move marker for check & checkmate
        if (pos.gives_check (m))
        {
            StateInfo si;
            pos.do_move (m, si, true);
            san += (MoveList<LEGAL> (pos).size () != 0 ? "+" : "#");
            pos.undo_move ();
        }

        return san;
    }
    // Converts a string representing a move in short algebraic notation
    // to the corresponding legal move, if any.
    Move move_from_san (const string &san, Position &pos)
    {
        for (const auto &vm : MoveList<LEGAL> (pos))
        {
            if (san == move_to_san (vm.move, pos))
            {
                return vm.move;
            }
        }
        return MOVE_NONE;
    }

    //// Converts a move to a string in long algebraic notation representation.
    //string move_to_lan (Move m, Position &pos)
    //{
    //    string lan;
    //    return lan;
    //}
    //// Converts a string representing a move in long algebraic notation
    //// to the corresponding legal move, if any.
    //Move move_from_lan (const string &lan, Position &pos)
    //{
    //    for (const auto &vm : MoveList<LEGAL> (pos))
    //    {
    //        if (lan == move_to_lan (vm.move, pos))
    //        {
    //            return vm.move;
    //        }
    //    }
    //    return MOVE_NONE;
    //}

    // Returns formated human-readable search information,
    // typically to be appended to the search log file.
    // It uses the two helpers to pretty format the value and time respectively.
    string pretty_pv_info (Thread *const &thread)
    {
        static const u16 K = 1000;
        static const u32 M = K*K;

        ostringstream oss;

        u64 total_nodes = Threadpool.nodes ();
        oss << std::setw ( 4) << thread->running_depth
            << std::setw ( 8) << pretty_value (thread->root_moves[0].new_value, thread->root_pos.active ())
            << std::setw (12) << pretty_time (Threadpool.time_mgr.elapsed_time ());
        if (total_nodes < 1*M)
        {
            oss << std::setw (8) << total_nodes / 1;
        }
        else
        if (total_nodes < K*M)
        {
            oss << std::setw (7) << total_nodes / K << 'K';
        }
        else
        {
            oss << std::setw (7) << total_nodes / M << 'M';
        }
        oss << ' ';

        StateList states;
        u08 ply = 0;
        for (auto m : thread->root_moves[0])
        {
            oss <<
                //move_to_can (m)
                move_to_san (m, thread->root_pos) << ' ';
            states.push_back (StateInfo ());
            thread->root_pos.do_move (m, states.back (), thread->root_pos.gives_check (m));
            ++ply;
        }
        for (; ply != 0; --ply)
        {
            thread->root_pos.undo_move ();
            states.pop_back ();
        }

        return oss.str ();
    }
}
