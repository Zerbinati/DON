#include "UCI.h"

#include <cstdarg>

#include "Benchmark.h"
#include "Engine.h"
#include "Evaluator.h"
#include "MoveGenerator.h"
#include "Notation.h"
#include "Searcher.h"
#include "TBsyzygy.h"
#include "Thread.h"
#include "Transposition.h"

namespace UCI {

    using namespace std;
    using namespace Engine;
    using namespace Evaluator;
    using namespace MoveGen;
    using namespace Notation;
    using namespace Searcher;
    using namespace TBSyzygy;
    using namespace Threading;

    namespace {

        // On ucinewgame following steps are needed to reset the state
        void newgame ()
        {
            clear ();
            Threadpool.main_thread ()->time_mgr.available_nodes = 0;
            TBSyzygy::initialize ();
        }

    }

    // Waits for a command from stdin, parses it and calls the appropriate function.
    // Also intercepts EOF from stdin to ensure gracefully exiting if the GUI dies unexpectedly.
    // Single command line arguments is executed once and returns immediately, e.g. 'bench'.
    // In addition to the UCI ones, also some additional commands are supported.
    void loop (i32 argc, const char *const *argv)
    {
        // Forsyth-Edwards Notation (FEN) is a standard notation for describing a particular board position of a chess game.
        // The purpose of FEN is to provide all the necessary information to restart a game from a particular position.
        const string StartFEN ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        Position::Chess960 = false;

        newgame (); // Implied ucinewgame before the first position command

        // Stack to keep track of the position states along the setup moves
        // (from the start position to the position just before the search starts).
        // Needed by 'draw by repetition' detection.
        StateList states (1);

        Position root_pos;
        root_pos.setup (StartFEN, states.back (), Threadpool.main_thread ());
        // Join arguments
        string cmd;
        for (i32 i = 1; i < argc; ++i)
        {
            cmd += string (" ", !white_spaces (cmd) ? 1 : 0) + string (argv[i]);
        }

        string token;
        do
        {
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

            // GUI sends 'ponderhit' to tell us to ponder on the same move the opponent has played.
            // In case Threadpool.stop_on_ponderhit is set wait for 'ponderhit' to stop the search (for instance because already ran out of time),
            // otherwise should continue searching but switching from pondering to normal search.
            if (    token == "quit"
                ||  token == "stop"
                || (token == "ponderhit" && Threadpool.stop_on_ponderhit))
            {
                Threadpool.stop = true;
                Threadpool.main_thread ()->start_searching (true); // Could be sleeping
            }
            else
            if (token == "ponderhit" && Threadpool.ponder)
            {
                Threadpool.ponder = false;
            }
            else
            if (token == "isready")
            {
                sync_cout << "readyok" << sync_endl;
            }
            else
            if (token == "uci")
            {
                sync_cout
                    << "id name " << Name << " " << info () << "\n"
                    << "id author " << Author << "\n"
                    << Options
                    << "uciok"
                    << sync_endl;
            }
            // These commands can not be executed while a search is ongoing.
            else
            {
                // Wait for stop.
                Threadpool.wait_while_thinking ();

                if (token == "ucinewgame")
                {
                    newgame ();
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

                    bool ponder = false;    // Search on ponder move until the "stop" command
                    Moves search_moves;     // Restrict search to these root moves only

                    i64 value;
                    while (iss >> token)
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
                            limits.clock[WHITE].inc = u32(abs (value));
                        }
                        else
                        if (token == "binc")
                        {
                            iss >> value;
                            limits.clock[BLACK].inc = u32(abs (value));
                        }
                        else
                        if (token == "movestogo")
                        {
                            iss >> value;
                            limits.movestogo = u08(abs (value));
                        }
                        else
                        if (token == "movetime")
                        {
                            iss >> value;
                            limits.movetime = u64(abs (value));
                        }
                        else
                        if (token == "depth")
                        {
                            iss >> value;
                            limits.depth = i16(abs (value));
                        }
                        else
                        if (token == "nodes")
                        {
                            iss >> value;
                            limits.nodes = u64(abs (value));
                        }
                        else
                        if (token == "mate")
                        {
                            iss >> value;
                            limits.mate = u08(abs (value));
                        }
                        else
                        if (token == "infinite")
                        {
                            limits.infinite = true;
                        }
                        else
                        if (token == "ponder")
                        {
                            ponder = true;
                        }
                        else
                        // Parse and Validate search-moves (if any)
                        if (token == "searchmoves")
                        {
                            while (iss >> token)
                            {
                                auto m = move_from_can (token, root_pos);
                                if (MOVE_NONE == m)
                                {
                                    std::cerr << "ERROR: Illegal Rootmove '" + token << "'" << std::endl;
                                    continue;
                                }
                                search_moves.push_back (m);
                            }
                            search_moves.shrink_to_fit ();
                        }
                    }
                    Threadpool.start_thinking (root_pos, states, limits, search_moves, ponder);
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

                    iss >> token; // Consume "startpos" or "fen" token
                    if (token == "startpos")
                    {
                        fen = StartFEN;
                        iss >> token; // Consume "moves" token if any
                    }
                    else
                    if (token == "fen")
                    {
                        while (   iss >> token
                               && token != "moves") // Consume "moves" token if any
                        {
                            fen += token + " ";
                        }
                        assert(_ok (fen));
                    }
                    else
                    {
                        std::cerr << "ERROR: Illegal token : " + token << std::endl;
                        continue;
                    }

                    states.resize (1);
                    root_pos.setup (fen, states.back (), Threadpool.main_thread ());

                    if (token == "moves")
                    {
                        u16 count = 0;
                        // Parse and validate moves (if any)
                        while (iss >> token)
                        {
                            ++count;
                            auto m = move_from_can (token, root_pos);
                            if (MOVE_NONE == m)
                            {
                                std::cerr << "ERROR: Illegal Move '" + token << "' at " << count << std::endl;
                                break;
                            }
                            states.push_back (StateInfo ());
                            root_pos.do_move (m, states.back ());
                        }
                    }
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
                               && token != "value")
                        {
                            name += string (" ", !white_spaces (name) ? 1 : 0) + token;
                        }

                        string value;
                        // Read option-value (can contain spaces)
                        while (iss >> token)
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
                //               && token != "code")
                //        {
                //            name += string (" ", !white_spaces (name) ? 1 : 0) + token;
                //        }
                //        string code;
                //        // Read "code" (can contain spaces)
                //        while (iss >> token)
                //        {
                //            code += string (" ", !white_spaces (code) ? 1 : 0) + token;
                //        }
                //        //std::cout << name << "\n" << code << std::endl;
                //    }
                //    else
                //    if (token == "later")
                //    {
                //    }
                //}

                // Additional custom non-UCI commands, useful for debugging
                else
                {
                    if (token == "bench")
                    {
                        benchmark (iss, root_pos);
                    }
                    else
                    if (token == "perft")
                    {
                        perft (iss, root_pos);
                    }
                    else
                    if (token == "flip")
                    {
                        root_pos.flip ();
                    }
                    else
                    if (token == "mirror")
                    {
                        root_pos.mirror ();
                    }
                    // Print the root position
                    else
                    if (token == "show")
                    {
                        sync_cout << root_pos << sync_endl;
                    }
                    else
                    if (token == "eval")
                    {
                        sync_cout << trace_eval (root_pos) << sync_endl;
                    }
                    // Print the root fen and keys
                    else
                    if (token == "keys")
                    {
                        sync_cout
                            << std::hex << std::uppercase << std::setfill ('0')
                            << "FEN: "                        << root_pos.fen ()       << "\n"
                            << "Posi key: " << std::setw (16) << root_pos.si->posi_key << "\n"
                            << "Matl key: " << std::setw (16) << root_pos.si->matl_key << "\n"
                            << "Pawn key: " << std::setw (16) << root_pos.si->pawn_key << "\n"
                            << "PG key: " << std::setw (16) << root_pos.pg_key ()
                            << std::setfill (' ') << std::nouppercase << std::dec
                            << sync_endl;
                    }
                    else
                    if (token == "moves")
                    {
                        sync_cout;
                        i32 count;
                        if (0 != root_pos.si->checkers)
                        {
                            std::cout << "\nEvasion moves: ";
                            count = 0;
                            for (const auto &vm : MoveList<GenType::EVASION> (root_pos))
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
                            for (const auto &vm : MoveList<GenType::QUIET> (root_pos))
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
                            for (const auto &vm : MoveList<GenType::CHECK> (root_pos))
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
                            for (const auto &vm : MoveList<GenType::QUIET_CHECK> (root_pos))
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
                            for (const auto &vm : MoveList<GenType::CAPTURE> (root_pos))
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
                        for (const auto &vm : MoveList<GenType::LEGAL> (root_pos))
                        {
                            std::cout << move_to_san (vm.move, root_pos) << ' ';
                            ++count;
                        }
                        std::cout << "(" << count << ")";

                        std::cout << sync_endl;
                    }
                    else
                    {
                        sync_cout << "Unknown command: \'" << cmd << "\'" << sync_endl;
                    }
                }
            }
        }
        while (   argc == 1
               && cmd != "quit");

        Threadpool.wait_while_thinking ();
    }

}
