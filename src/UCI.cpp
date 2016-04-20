#include "UCI.h"

#include <cstdarg>

#include "Engine.h"
#include "Searcher.h"
#include "Evaluator.h"
#include "MoveGenerator.h"
#include "Notation.h"
#include "Debugger.h"
#include "Benchmark.h"
#include "Thread.h"

using namespace std;

namespace UCI {

    using namespace Engine;
    using namespace Searcher;
    using namespace Evaluator;
    using namespace MoveGen;
    using namespace Notation;
    using namespace Debugger;

    namespace {

        // Root position
        Position RootPos;

        // Stack to keep track of the position states along the setup moves
        // (from the start position to the position just before the search starts).
        // Needed by 'draw by repetition' detection.
        StateListPtr SetupStates(new StateList (1));

    }

    // loop() waits for a command from stdin, parses it and calls the appropriate
    // function. Also intercepts EOF from stdin to ensure gracefully exiting if the
    // GUI dies unexpectedly. When called with some command line arguments, e.g. to
    // run 'bench', once the command is executed the function returns immediately.
    // In addition to the UCI ones, also some additional debug commands are supported.
    void loop (const string &arg)
    {
        RootPos.setup (StartupFEN, SetupStates->back(), Threadpool.main (), false);

        bool running = white_spaces (arg);
        string cmd   = arg;
        string token;
        do
        {
            // Block here waiting for input or EOF
            if (running && !std::getline (cin, cmd, '\n')) cmd = "quit";

            istringstream iss (cmd);
            token.clear (); // std::getline() could return empty or blank line
            iss >> skipws >> token;

            if (white_spaces (token)) continue;
            else
            if (token == "uci")
            {
                sync_cout
                    << info (true) << "\n"
                    << Options
                    << "uciok"
                    << sync_endl;
            }
            else
            if (token == "ucinewgame")
            {
                clear ();
                Threadpool.main ()->time_mgr.available_nodes = 0;
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
                    while (iss >> token && !iss.fail () && token != "value")
                    {
                        name += string (" ", !white_spaces (name) ? 1 : 0) + token;
                    }

                    string value;
                    // Read option-value (can contain spaces)
                    while (iss >> token && !iss.fail ())
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
                    fen = StartupFEN;
                    iss >> token;          // Consume "moves" token if any
                }
                else
                if (token == "fen")
                {
                    while (iss >> token && !iss.fail () && token != "moves") // Consume "moves" token if any
                    {
                        fen += token + " ";
                    }
                    //assert(_ok (fen, Chess960, true));
                }
                else
                {
                    goto end_cmd;
                }

                SetupStates = StateListPtr (new StateList (1));
                RootPos.setup (fen, SetupStates->back(), Threadpool.main (), Chess960);

                if (token == "moves")
                {
                    while (iss >> token && !iss.fail ())   // Parse and validate game moves (if any)
                    {
                        auto m = move_from_can (token, RootPos);
                        if (m == MOVE_NONE)
                        {
                            std::cerr << "ERROR: Illegal Move '" + token << "'" << std::endl;
                            break;
                        }

                        SetupStates->push_back (StateInfo ());
                        RootPos.do_move (m, SetupStates->back (), RootPos.gives_check (m, CheckInfo (RootPos)));
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
                limits.start_time = now (); // As early as possible!
                i64 value;
                while (iss >> token && !iss.fail ())
                {
                    if (token == "wtime")      { iss >> value; limits.clock[WHITE].time = u64(abs (value)); }
                    else
                    if (token == "btime")      { iss >> value; limits.clock[BLACK].time = u64(abs (value)); }
                    else
                    if (token == "winc")       { iss >> value; limits.clock[WHITE].inc  = u64(abs (value)); }
                    else
                    if (token == "binc")       { iss >> value; limits.clock[BLACK].inc  = u64(abs (value)); }
                    else
                    if (token == "movetime")   { iss >> value; limits.movetime  = u64(abs (value)); }
                    else
                    if (token == "movestogo")  { iss >> value; limits.movestogo = u08(abs (value)); }
                    else
                    if (token == "depth")      { iss >> value; limits.depth     = u08(abs (value)); }
                    else
                    if (token == "nodes")      { iss >> value; limits.nodes     = u64(abs (value)); }
                    else
                    if (token == "mate")       { iss >> value; limits.mate      = u08(abs (value)); }
                    else
                    if (token == "infinite")   { limits.infinite  = true; }
                    else
                    if (token == "ponder")     { limits.ponder    = true; }
                    else
                    // Parse and Validate search-moves (if any)
                    if (token == "searchmoves")
                    {
                        while (iss >> token && !iss.fail ())
                        {
                            auto m = move_from_can (token, RootPos);
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
                Threadpool.start_thinking (RootPos, SetupStates, limits);
            }
            // GUI sends 'ponderhit' to tell us to ponder on the same move the
            // opponent has played. In case Ponderhit Stop stream set are
            // waiting for 'ponderhit' to stop the search (for instance because
            // already ran out of time), otherwise should continue searching but
            // switching from pondering to normal search.
            else
            if (   token == "quit"
               ||  token == "stop"
               || (token == "ponderhit" && PonderhitStop)
               )
            {
                ForceStop = true;
                Threadpool.main ()->start_searching (true); // Could be sleeping
            }
            else
            if (token == "ponderhit")
            {
                Limits.ponder = false;
            }
            // It is the command to try to register an engine or to tell the engine that registration
            // will be done later. This command should always be sent if the engine has sent "registration error"
            // at program startup.
            // The following tokens are allowed:
            // * later
            //   the user doesn't want to register the engine now.
            // * name <x>
            //   the engine should be registered with the name <x>
            // * code <y>
            //   the engine should be registered with the code <y>
            // Example:
            //   "register later"
            //   "register name Stefan MK code 4359874324"
            else
            if (token == "register")
            {
                iss >> token;
                if (token == "name")
                {
                    string name;
                    // Read name (can contain spaces)
                    // consume "value" token
                    while (iss >> token && !iss.fail () && token != "code")
                    {
                        name += string (" ", !white_spaces (name) ? 1 : 0) + token;
                    }

                    string code;
                    // Read code (can contain spaces)
                    while (iss >> token && !iss.fail ())
                    {
                        code += string (" ", !white_spaces (code) ? 1 : 0) + token;
                    }
                    //std::cout << name << "\n" << code << std::endl;
                }
                else
                if (token == "later")
                {
                }
            }
            else
            if (token == "debug")
            {
                iss >> token;
                if (token == "on")  Logger::instance ().start ();
                else
                if (token == "off") Logger::instance ().stop ();
                else
                    sync_cout << "Invalid option." << sync_endl;
            }
            // Print the root position
            else
            if (token == "show")
            {
                sync_cout << RootPos << sync_endl;
            }
            // Print the root fen and keys
            else
            if (token == "keys")
            {
                sync_cout
                    << std::hex << std::uppercase << std::setfill ('0')
                    << "FEN: "                        << RootPos.fen ()      << "\n"
                    << "Posi key: " << std::setw (16) << RootPos.posi_key () << "\n"
                    << "Poly key: " << std::setw (16) << RootPos.poly_key () << "\n"
                    << "Matl key: " << std::setw (16) << RootPos.matl_key () << "\n"
                    << "Pawn key: " << std::setw (16) << RootPos.pawn_key ()
                    << std::setfill (' ') << std::nouppercase << std::dec
                    << sync_endl;
            }
            else
            if (token == "moves")
            {
                sync_cout;

                auto pinneds = RootPos.pinneds (RootPos.active ());
                if (RootPos.checkers () != 0)
                {
                    std::cout << "\nEvasion moves: ";
                    for (const auto &vm : MoveList<EVASION> (RootPos))
                    {
                        if (RootPos.legal (vm.move, pinneds))
                        {
                            std::cout << move_to_san (vm.move, RootPos) << " ";
                        }
                    }
                }
                else
                {
                    std::cout << "\nQuiet moves: ";
                    for (const auto &vm : MoveList<QUIET> (RootPos))
                    {
                        if (RootPos.legal (vm.move, pinneds))
                        {
                            std::cout << move_to_san (vm.move, RootPos) << " ";
                        }
                    }

                    std::cout << "\nCheck moves: ";
                    for (const auto &vm : MoveList<CHECK> (RootPos))
                    {
                        if (RootPos.legal (vm.move, pinneds))
                        {
                            std::cout << move_to_san (vm.move, RootPos) << " ";
                        }
                    }

                    std::cout << "\nQuiet Check moves: ";
                    for (const auto &vm : MoveList<QUIET_CHECK> (RootPos))
                    {
                        if (RootPos.legal (vm.move, pinneds))
                        {
                            std::cout << move_to_san (vm.move, RootPos) << " ";
                        }
                    }

                    std::cout << "\nCapture moves: ";
                    for (const auto &vm : MoveList<CAPTURE> (RootPos))
                    {
                        if (RootPos.legal (vm.move, pinneds))
                        {
                            std::cout << move_to_san (vm.move, RootPos) << " ";
                        }
                    }
                }

                std::cout << "\nLegal moves: ";
                for (const auto &vm : MoveList<LEGAL> (RootPos))
                {
                    std::cout << move_to_san (vm.move, RootPos) << " ";
                }

                std::cout << sync_endl;
            }
            else
            if (token == "flip")
            {
                RootPos.flip ();
            }
            else
            if (token == "eval")
            {
                sync_cout << trace (RootPos) << sync_endl;
            }
            else
            if (token == "perft")
            {
                i32    depth;
                string fen_fn;
                depth  = (iss >> depth) && !iss.fail ()  ? depth : 1;
                fen_fn = (iss >> fen_fn) && !iss.fail () ? fen_fn : "";

                stringstream ss;
                ss  << i32(Options["Hash"])    << " "
                    << i32(Options["Threads"]) << " "
                    << depth << " perft " << fen_fn;

                benchmark (ss, RootPos);
            }
            else
            if (token == "bench")
            {
                benchmark (iss, RootPos);
            }
            else
            {
                sync_cout << "Unknown command: \'" << cmd << "\'" << sync_endl;
            }
            end_cmd:;
        } while (running && cmd != "quit");

        Threadpool.wait_while_thinking (); // Can't quit while the search is running
    }

}
