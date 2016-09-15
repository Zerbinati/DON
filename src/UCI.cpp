#include "UCI.h"

#include <cstdarg>

#include "Engine.h"
#include "Searcher.h"
#include "Evaluator.h"
#include "MoveGenerator.h"
#include "Thread.h"
#include "Notation.h"
#include "Benchmark.h"

namespace UCI {

    using namespace std;
    using namespace Engine;
    using namespace Searcher;
    using namespace Evaluator;
    using namespace MoveGen;
    using namespace Notation;

    // Waits for a command from stdin, parses it and calls the appropriate function.
    // Also intercepts EOF from stdin to ensure gracefully exiting if the GUI dies unexpectedly.
    // When called with some command line arguments, e.g. to run 'bench',
    // once the command is executed the function returns immediately.
    // In addition to the UCI ones, also some additional debug commands are supported.
    void loop (i32 argc, const char *const *argv)
    {
        // Forsyth-Edwards Notation (FEN) is a standard notation for describing a particular board position of a chess game.
        // The purpose of FEN is to provide all the necessary information to restart a game from a particular position.
        static const string StartFEN ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

        // Stack to keep track of the position states along the setup moves
        // (from the start position to the position just before the search starts).
        // Needed by 'draw by repetition' detection.
        StateList states (1);

        Position root_pos;
        root_pos.setup (StartFEN, states.back (), Threadpool.main_thread (), true);
        // Join arguments
        string cmd;
        for (i32 i = 1; i < argc; ++i)
        {
            cmd += string (" ", !white_spaces (cmd) ? 1 : 0) + string (argv[i]);
        }

        string token;
        do {
            // Block here waiting for input or EOF
            if (   argc == 1
                && !std::getline (cin, cmd, '\n'))
            {
                cmd = "quit";
            }

            istringstream iss (cmd);
            token.clear (); // std::getline() could return empty or blank line
            iss >> skipws >> token;

            if (white_spaces (token))
            {
                continue;
            }

            if (token == "uci")
            {
                sync_cout
                    << info (true) << '\n'
                    << Options
                    << "uciok"
                    << sync_endl;
            }
            else
            if (token == "ucinewgame")
            {
                ForceStop = true;
                Threadpool.wait_while_thinking ();
                clear ();
                Threadpool.time_mgr.available_nodes = 0;
            }
            else
            if (token == "isready")
            {
                sync_cout << "readyok" << sync_endl;
            }
            else
            // This function updates the UCI option ("name") to the given value ("value").
            if (token == "setoption")
            {
                iss >> token; // Consume "name" token
                if (token == "name")
                {
                    string name;
                    // Read option-name (can contain spaces) also consume "value" token
                    while (   iss >> token
                           && !iss.fail ()
                           && token != "value")
                    {
                        name += string (" ", !white_spaces (name) ? 1 : 0) + token;
                    }

                    string value;
                    // Read option-value (can contain spaces)
                    while (   iss >> token
                           && !iss.fail ())
                    {
                        value += string (" ", !white_spaces (value) ? 1 : 0) + token;
                    }

                    if (Options.find (name) != Options.end ())
                    {
                        Options[name] = value;
                    }
                    else
                    {
                        sync_cout << "No such option: \'" << name << "\'" << sync_endl;
                    }
                }
            }
            // This sets up the position:
            //  - starting position ("startpos")
            //  - fen-string position ("fen")
            // and then makes the moves given in the following move list ("moves")
            // also saving the moves on stack.
            else
            if (token == "position")
            {
                string fen;

                iss >> token;  // Consume "startpos" or "fen" token
                if (token == "startpos")
                {
                    fen = StartFEN;
                    iss >> token;          // Consume "moves" token if any
                }
                else
                if (token == "fen")
                {
                    while (   iss >> token
                           && !iss.fail ()
                           && token != "moves") // Consume "moves" token if any
                    {
                        fen += string (" ", !white_spaces (fen) ? 1 : 0) + token;
                    }
                    assert(_ok (fen, true));
                }
                else
                {
                    assert(false);
                    continue;
                }

                states.resize (1);
                root_pos.setup (fen, states.back (), Threadpool.main_thread (), true);

                if (token == "moves")
                {
                    // Parse and validate moves (if any)
                    while (   iss >> token
                           && !iss.fail ())
                    {
                        auto m = move_from_can (token, root_pos);
                        if (m == MOVE_NONE)
                        {
                            std::cerr << "ERROR: Illegal Move '" + token << "'" << std::endl;
                            break;
                        }
                        states.push_back (StateInfo ());
                        root_pos.do_move (m, states.back (), root_pos.gives_check (m));
                    }
                }
            }
            // This sets the following parameters:
            //  - wtime and btime
            //  - winc and binc
            //  - movestogo
            //  - movetime
            //  - depth
            //  - nodes
            //  - mate
            //  - infinite
            //  - ponder
            // Then starts the search.
            else
            if (token == "go")
            {
                Limit limits;
                limits.start_time = now ();
                limits.elapsed_time = 0;
                i64 value;
                while (   iss >> token
                       && !iss.fail ())
                {
                    if (token == "wtime")
                    {
                        iss >> value;
                        limits.clock[WHITE].time = u64(abs (value));
                    }
                    else
                    if (token == "btime")
                    {
                        iss >> value;
                        limits.clock[BLACK].time = u64(abs (value));
                    }
                    else
                    if (token == "winc")
                    {
                        iss >> value;
                        limits.clock[WHITE].inc  = u64(abs (value));
                    }
                    else
                    if (token == "binc")
                    {
                        iss >> value;
                        limits.clock[BLACK].inc  = u64(abs (value));
                    }
                    else
                    if (token == "movetime")
                    {
                        iss >> value;
                        limits.movetime  = u64(abs (value));
                    }
                    else
                    if (token == "movestogo")
                    {
                        iss >> value;
                        limits.movestogo = u08(abs (value));
                    }
                    else
                    if (token == "depth")
                    {
                        iss >> value;
                        limits.depth     = i16(abs (value));
                    }
                    else
                    if (token == "nodes")
                    {
                        iss >> value;
                        limits.nodes     = u64(abs (value));
                    }
                    else
                    if (token == "mate")
                    {
                        iss >> value;
                        limits.mate      = u08(abs (value));
                    }
                    else
                    if (token == "infinite")
                    {
                        limits.infinite  = true;
                    }
                    else
                    if (token == "ponder")
                    {
                        limits.ponder    = true;
                    }
                    else
                    // Parse and Validate search-moves (if any)
                    if (token == "searchmoves")
                    {
                        while (   iss >> token
                               && !iss.fail ())
                        {
                            auto m = move_from_can (token, root_pos);
                            if (m == MOVE_NONE)
                            {
                                std::cerr << "ERROR: Illegal Move '" + token << "'" << std::endl;
                                continue;
                            }
                            limits.search_moves.push_back (m);
                        }
                        limits.search_moves.shrink_to_fit ();
                    }
                }
                ForceStop = true;
                Threadpool.wait_while_thinking ();
                Threadpool.start_thinking (root_pos, states, limits);
            }
            // GUI sends 'ponderhit' to tell us to ponder on the same move the
            // opponent has played. In case Ponderhit Stop stream set are
            // waiting for 'ponderhit' to stop the search (for instance because
            // already ran out of time), otherwise should continue searching but
            // switching from pondering to normal search.
            else
            if (   token == "quit"
               ||  token == "stop"
               || (token == "ponderhit" && PonderhitStop))
            {
                ForceStop = true;
                Threadpool.main_thread ()->start_searching (true); // Could be sleeping
            }
            else
            if (token == "ponderhit")
            {
                Limits.ponder = false;
            }
            //// Register an engin, with following tokens:
            //// - later: the user doesn't want to register the engine now.
            //// - name <x> code <y>: the engine should be registered with the name <x> and code <y>
            //// Example:
            ////   "register later"
            ////   "register name Stefan MK code 4359874324"
            //else
            //if (token == "register")
            //{
            //    iss >> token;
            //    if (token == "name")
            //    {
            //        string name;
            //        // Read "name" (can contain spaces), consume "code" token
            //        while (   iss >> token
            //               && !iss.fail ()
            //               && token != "code")
            //        {
            //            name += string (" ", !white_spaces (name) ? 1 : 0) + token;
            //        }
            //        string code;
            //        // Read "code" (can contain spaces)
            //        while (   iss >> token
            //               && !iss.fail ())
            //        {
            //            code += string (" ", !white_spaces (code) ? 1 : 0) + token;
            //        }
            //        //std::cout << name << '\n' << code << std::endl;
            //    }
            //    else
            //    if (token == "later")
            //    {
            //    }
            //}
            // Print the root position
            else
            if (token == "show")
            {
                sync_cout << root_pos << sync_endl;
            }
            // Print the root fen and keys
            else
            if (token == "keys")
            {
                sync_cout
                    << std::hex << std::uppercase << std::setfill ('0')
                    << "FEN: "                        << root_pos.fen (true)  << '\n'
                    << "Posi key: " << std::setw (16) << root_pos.posi_key () << '\n'
                    << "Poly key: " << std::setw (16) << root_pos.poly_key () << '\n'
                    << "Matl key: " << std::setw (16) << root_pos.matl_key () << '\n'
                    << "Pawn key: " << std::setw (16) << root_pos.pawn_key ()
                    << std::setfill (' ') << std::nouppercase << std::dec
                    << sync_endl;
            }
            else
            if (token == "moves")
            {
                sync_cout;
                i32 count;
                if (root_pos.checkers () != 0)
                {
                    std::cout << "\nEvasion moves: ";
                    count = 0;
                    for (const auto &vm : MoveList<EVASION> (root_pos))
                    {
                        if (root_pos.legal (vm.move))
                        {
                            std::cout << move_to_san (vm.move, root_pos) << ' ';
                            ++count;
                        }
                    }
                    std::cout << "(" << count << ")";
                }
                else
                {
                    std::cout << "\nQuiet moves: ";
                    count = 0;
                    for (const auto &vm : MoveList<QUIET> (root_pos))
                    {
                        if (root_pos.legal (vm.move))
                        {
                            std::cout << move_to_san (vm.move, root_pos) << ' ';
                            ++count;
                        }
                    }
                    std::cout << "(" << count << ")";

                    std::cout << "\nCheck moves: ";
                    count = 0;
                    for (const auto &vm : MoveList<CHECK> (root_pos))
                    {
                        if (root_pos.legal (vm.move))
                        {
                            std::cout << move_to_san (vm.move, root_pos) << ' ';
                            ++count;
                        }
                    }
                    std::cout << "(" << count << ")";

                    std::cout << "\nQuiet Check moves: ";
                    count = 0;
                    for (const auto &vm : MoveList<QUIET_CHECK> (root_pos))
                    {
                        if (root_pos.legal (vm.move))
                        {
                            std::cout << move_to_san (vm.move, root_pos) << ' ';
                            ++count;
                        }
                    }
                    std::cout << "(" << count << ")";

                    std::cout << "\nCapture moves: ";
                    count = 0;
                    for (const auto &vm : MoveList<CAPTURE> (root_pos))
                    {
                        if (root_pos.legal (vm.move))
                        {
                            std::cout << move_to_san (vm.move, root_pos) << ' ';
                            ++count;
                        }
                    }
                    std::cout << "(" << count << ")";
                }

                std::cout << "\nLegal moves: ";
                count = 0;
                for (const auto &vm : MoveList<LEGAL> (root_pos))
                {
                    std::cout << move_to_san (vm.move, root_pos) << ' ';
                    ++count;
                }
                std::cout << "(" << count << ")";

                std::cout << sync_endl;
            }
            else
            if (token == "flip")
            {
                root_pos.flip ();
            }
            else
            if (token == "eval")
            {
                sync_cout << trace (root_pos) << sync_endl;
            }
            else
            if (token == "perft")
            {
                i32    depth;
                string fen_fn;
                depth  = (iss >> depth)  && !iss.fail () ? depth  : 1;
                fen_fn = (iss >> fen_fn) && !iss.fail () ? fen_fn : "";

                istringstream ss(to_string (i32(Options["Hash"]))    + ' '
                               + to_string (i32(Options["Threads"])) + ' '
                               + to_string (depth) + " perft " + fen_fn);
                benchmark (ss, root_pos);
            }
            else
            if (token == "bench")
            {
                benchmark (iss, root_pos);
            }
            else
            {
                sync_cout << "Unknown command: \'" << cmd << "\'" << sync_endl;
            }
            
        } while (   argc == 1
                 && cmd != "quit");

        Threadpool.wait_while_thinking ();
    }

}
