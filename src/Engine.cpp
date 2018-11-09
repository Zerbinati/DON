#include "Engine.h"

#include <cstdarg>
#include <iostream>
#include <fstream>
#include "BitBoard.h"
#include "BitBases.h"
#include "Endgame.h"
#include "Evaluator.h"
#include "Logger.h"
#include "Material.h"
#include "MemoryHandler.h"
#include "Notation.h"
#include "Option.h"
#include "Pawns.h"
#include "Polyglot.h"
#include "PSQTable.h"
#include "Searcher.h"
#include "TBsyzygy.h"
#include "Thread.h"
#include "Transposition.h"
#include "Zobrist.h"

using namespace std;

namespace {

    using namespace Searcher;
    using namespace TBSyzygy;

    /// Forsyth-Edwards Notation (FEN) is a standard notation for describing a particular board position of a chess game.
    /// The purpose of FEN is to provide all the necessary information to restart a game from a particular position.
    const string StartFEN ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

    const vector<string> DefaultCmds =
    {
        // ---Chess Normal---
        "setoption name UCI_Chess960 value false",
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 10",
        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 11",
        "4rrk1/pp1n3p/3q2pQ/2p1pb2/2PP4/2P3N1/P2B2PP/4RRK1 b - - 7 19",
        "rq3rk1/ppp2ppp/1bnpb3/3N2B1/3NP3/7P/PPPQ1PP1/2KR3R w - - 7 14 moves d4e6",
        "r1bq1r1k/1pp1n1pp/1p1p4/4p2Q/4Pp2/1BNP4/PPP2PPP/3R1RK1 w - - 2 14 moves g2g4",
        "r3r1k1/2p2ppp/p1p1bn2/8/1q2P3/2NPQN2/PPP3PP/R4RK1 b - - 2 15",
        "r1bbk1nr/pp3p1p/2n5/1N4p1/2Np1B2/8/PPP2PPP/2KR1B1R w kq - 0 13",
        "r1bq1rk1/ppp1nppp/4n3/3p3Q/3P4/1BP1B3/PP1N2PP/R4RK1 w - - 1 16",
        "4r1k1/r1q2ppp/ppp2n2/4P3/5Rb1/1N1BQ3/PPP3PP/R5K1 w - - 1 17",
        "2rqkb1r/ppp2p2/2npb1p1/1N1Nn2p/2P1PP2/8/PP2B1PP/R1BQK2R b KQ - 0 11",
        "r1bq1r1k/b1p1npp1/p2p3p/1p6/3PP3/1B2NN2/PP3PPP/R2Q1RK1 w - - 1 16",
        "3r1rk1/p5pp/bpp1pp2/8/q1PP1P2/b3P3/P2NQRPP/1R2B1K1 b - - 6 22",
        "r1q2rk1/2p1bppp/2Pp4/p6b/Q1PNp3/4B3/PP1R1PPP/2K4R w - - 2 18",
        "4k2r/1pb2ppp/1p2p3/1R1p4/3P4/2r1PN2/P4PPP/1R4K1 b - - 3 22",
        "3q2k1/pb3p1p/4pbp1/2r5/PpN2N2/1P2P2P/5PP1/Q2R2K1 b - - 4 26",
        "6k1/6p1/6Pp/ppp5/3pn2P/1P3K2/1PP2P2/3N4 b - - 0 1",
        "3b4/5kp1/1p1p1p1p/pP1PpP1P/P1P1P3/3KN3/8/8 w - - 0 1",
        "2K5/p7/7P/5pR1/8/5k2/r7/8 w - - 0 1 moves g5g6 f3e3 g6g5 e3f3",
        "8/6pk/1p6/8/PP3p1p/5P2/4KP1q/3Q4 w - - 0 1",
        "7k/3p2pp/4q3/8/4Q3/5Kp1/P6b/8 w - - 0 1",
        "8/2p5/8/2kPKp1p/2p4P/2P5/3P4/8 w - - 0 1",
        "8/1p3pp1/7p/5P1P/2k3P1/8/2K2P2/8 w - - 0 1",
        "8/pp2r1k1/2p1p3/3pP2p/1P1P1P1P/P5KR/8/8 w - - 0 1",
        "8/3p4/p1bk3p/Pp6/1Kp1PpPp/2P2P1P/2P5/5B2 b - - 0 1",
        "5k2/7R/4P2p/5K2/p1r2P1p/8/8/8 b - - 0 1",
        "6k1/6p1/P6p/r1N5/5p2/7P/1b3PP1/4R1K1 w - - 0 1",
        "1r3k2/4q3/2Pp3b/3Bp3/2Q2p2/1p1P2P1/1P2KP2/3N4 w - - 0 1",
        "6k1/4pp1p/3p2p1/P1pPb3/R7/1r2P1PP/3B1P2/6K1 w - - 0 1",
        "8/3p3B/5p2/5P2/p7/PP5b/k7/6K1 w - - 0 1",
        // 5-men positions
        "8/8/8/8/5kp1/P7/8/1K1N4 w - - 0 80",     // Kc2 - Mate
        "8/8/8/5N2/8/p7/8/2NK3k w - - 0 82",      // Na2 - Mate
        "8/3k4/8/8/8/4B3/4KB2/2B5 w - - 0 85",    // Draw
        // 6-men positions
        "8/8/1P6/5pr1/8/4R3/7k/2K5 w - - 0 92",   // Re5 - Mate
        "8/2p4P/8/kr6/6R1/8/8/1K6 w - - 0 94",    // Ka2 - Mate
        "8/8/3P3k/8/1p6/8/1P6/1K3n2 b - - 0 90",  // Nd2 - Draw
        // 7-men positions
        "8/R7/2q5/8/6k1/8/1P5p/K6R w - - 0 124", // Draw
        // Mate and stalemate positions
        "6k1/3b3r/1p1p4/p1n2p2/1PPNpP1q/P3Q1p1/1R1RB1P1/5K2 b - - 0 1",
        "r2r1n2/pp2bk2/2p1p2p/3q4/3PN1QP/2P3R1/P4PP1/5RK1 w - - 0 1",
        "8/8/8/8/8/6k1/6p1/6K1 w - - 0 1",
        "7k/7P/6K1/8/3B4/8/8/8 b - - 0 1",

        // ---Chess 960---
        "setoption name UCI_Chess960 value true",
        "bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w HFhf - 0 1 moves g2g3 d7d5 d2d4 c8h3 c1g5 e8d6 g5e7 f7f6",
    };

    i32 month_index (const string &month)
    {
        constexpr i08 Months = 12;
        const string MonthStr[Months] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

        for (auto m = 0; m < Months; ++m)
        {
            if (month == MonthStr[m])
            {
                return m+1;
            }
        }
        return 0;
    }

    /// setoption() function updates the UCI option ("name") to the given value ("value").
    void setoption (istringstream &iss)
    {
        string token;
        iss >> token; // Consume "name" token

        //if (token != "name") return;
        string name;
        // Read option-name (can contain spaces)
        while (   iss >> token
               && token != "value") // Consume "value" token if any
        {
            name += (white_spaces (name) ? "" : " ") + token;
        }

        //if (token != "value") return;
        string value;
        // Read option-value (can contain spaces)
        while (iss >> token)
        {
            value += (white_spaces (value) ? "" : " ") + token;
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

    /// position() sets up the starting position "startpos"/"fen <fenstring>" and then
    /// makes the moves given in the move list "moves" also saving the moves on stack.
    void position (istringstream &iss, Position &pos, StateListPtr &states)
    {
        string token;
        iss >> token; // Consume "startpos" or "fen" token

        string fen;
        if (token == "startpos")
        {
            fen = StartFEN;
            iss >> token; // Consume "moves" token if any
        }
        else
        //if (token == "fen")
        {
            while (iss >> token) // Consume "moves" token if any
            {
                if (token == "moves")
                {
                    break;
                }
                fen += token + " ";
                token.clear ();
            }
            //assert(_ok (fen));
        }
        assert(token == "" || token == "moves");

        states = StateListPtr (new std::deque<StateInfo> (1)); // Drop old and create a new one
        pos.setup (fen, states->back (), pos.thread);
        assert(pos.fen() == trim (fen));

        u16 count = 0;
        // Parse and validate moves (if any)
        while (iss >> token)
        {
            ++count;
            auto m = move_from_can (token, pos);
            if (MOVE_NONE == m)
            {
                std::cerr << "ERROR: Illegal Move '" << token << "' at " << count << std::endl;
                break;
            }
            states->emplace_back ();
            pos.do_move (m, states->back ());
        }
    }

    /// go() sets the thinking time and other parameters from the input string, then starts the search.
    void go (istringstream &iss, Position &pos, StateListPtr &states)
    {
        Threadpool.stop = true;
        Threadpool.main_thread ()->wait_while_busy ();

        DebugTime = 0;
        StartTime = now ();
        Limit limit;
        vector<Move> search_moves; // Restrict search to these root moves only
        bool ponder = false;

        string token;
        while (iss >> token)
        {
            if (token == "wtime")     iss >> limit.clock[WHITE].time;
            else
            if (token == "btime")     iss >> limit.clock[BLACK].time;
            else
            if (token == "winc")      iss >> limit.clock[WHITE].inc;
            else
            if (token == "binc")      iss >> limit.clock[BLACK].inc;
            else
            if (token == "movestogo") iss >> limit.movestogo;
            else
            if (token == "movetime")  iss >> limit.movetime;
            else
            if (token == "depth")     iss >> limit.depth;
            else
            if (token == "nodes")     iss >> limit.nodes;
            else
            if (token == "mate")      iss >> limit.mate;
            else
            if (token == "infinite")  limit.infinite = true;
            else
            if (token == "ponder")    ponder = true;
            else
            // Parse and Validate search-moves (if any)
            if (token == "searchmoves")
            {
                while (iss >> token)
                {
                    auto m = move_from_can (token, pos);
                    if (MOVE_NONE == m)
                    {
                        std::cerr << "ERROR: Illegal Rootmove '" << token << "'" << std::endl;
                        continue;
                    }
                    search_moves.push_back (m);
                }
            }
            else
            if (token == "ignoremoves")
            {
                for (const auto &vm : MoveList<GenType::LEGAL> (pos))
                {
                    search_moves.push_back (vm);
                }
                while (iss >> token)
                {
                    auto m = move_from_can (token, pos);
                    if (MOVE_NONE == m)
                    {
                        std::cerr << "ERROR: Illegal Rootmove '" << token << "'" << std::endl;
                        continue;
                    }
                    search_moves.erase (std::remove (search_moves.begin (), search_moves.end (), m), search_moves.end ());
                }
            }
            else
            if (token == "perft")
            {
                i16 depth = 0;
                iss >> depth;
                if (DepthZero != depth)
                {
                    auto nodes = perft<true> (pos, depth);
                    sync_cout << "\nTotal:    "
                              << std::right
                              << std::setfill ('.')
                              << std::setw (16)
                              << nodes
                              << std::setfill (' ')
                              << std::left 
                              << sync_endl;
                }
                return;
            }
            else
            {
                std::cerr << "ERROR: Illegal token : " << token << std::endl;
                return;
            }
        }
        Threadpool.start_thinking (pos, states, limit, search_moves, ponder);
    }

    /// setup_bench() builds a list of UCI commands to be run by bench.
    /// There are five parameters:
    /// - TT size in MB (default is 16)
    /// - Threads count (default is 1)
    /// - limit value spent for each position (default is 13)
    /// - limit type:
    ///     * depth (default)
    ///     * movetime
    ///     * nodes
    ///     * mate
    ///     * perft
    /// - FEN positions to be used in FEN format
    ///     * 'default' for builtin positions (default)
    ///     * 'current' for current position
    ///     * '<filename>' for file containing FEN positions
    /// example:
    /// bench -> search default positions up to depth 13
    /// bench 64 1 15 -> search default positions up to depth 15 (TT = 64MB)
    /// bench 64 4 5000 movetime current -> search current position with 4 threads for 5 sec (TT = 64MB)
    /// bench 64 1 100000 nodes -> search default positions for 100K nodes (TT = 64MB)
    /// bench 16 1 5 perft -> run perft 5 on default positions
    vector<string> setup_bench (istringstream &iss, const Position &pos)
    {
        string token;

        // Assign default values to missing arguments
        string hash    = (iss >> token) && !white_spaces (token) ? token : "16";
        string threads = (iss >> token) && !white_spaces (token) ? token : "1";
        string value   = (iss >> token) && !white_spaces (token) ? token : "13";
        string mode    = (iss >> token) && !white_spaces (token) ? token : "depth";
        string pos_fn  = (iss >> token) && !white_spaces (token) ? token : "default";

        vector<string> cmds;
        vector<string> uci_cmds;

        if (pos_fn == "current")
        {
            cmds.emplace_back (pos.fen ());
        }
        else
        if (pos_fn == "default")
        {
            cmds = DefaultCmds;
        }
        else
        {
            ifstream ifs (pos_fn, ios_base::in);
            if (!ifs.is_open ())
            {
                std::cerr << "ERROR: unable to open file ... \'" << pos_fn << "\'" << std::endl;
                return uci_cmds;
            }
            string cmd;
            while (std::getline (ifs, cmd, '\n'))
            {
                if (!white_spaces (cmd))
                {
                    cmds.emplace_back (cmd);
                }
            }
            ifs.close ();
        }

        bool chess960 = bool(Options["UCI_Chess960"]);

        uci_cmds.emplace_back ("setoption name Threads value " + threads);
        uci_cmds.emplace_back ("setoption name Hash value " + hash);
        uci_cmds.emplace_back ("setoption name Clear Hash");

        string go = "go " + mode + " " + value;

        for (const auto &cmd : cmds)
        {
            if (cmd.find ("setoption") != string::npos)
            {
                uci_cmds.emplace_back (cmd);
            }
            else
            {
                uci_cmds.emplace_back ("position fen " + cmd);
                uci_cmds.emplace_back (go);
            }
        }

        if (pos_fn != "current")
        {
            uci_cmds.emplace_back ("setoption name UCI_Chess960 value " + string(chess960 ? "true" : "false"));
            uci_cmds.emplace_back ("position fen " + pos.fen ());
        }
        return uci_cmds;
    }

    /// bench() setup list of UCI commands is setup according to bench parameters,
    /// then it is run one by one printing a summary at the end.
    void bench (istringstream &iss, Position &pos, StateListPtr &states)
    {
        auto uci_cmds = setup_bench (iss, pos);
        u16 total = u16(count_if (uci_cmds.begin (), uci_cmds.end (), [](string s) { return s.find ("go ") == 0; }));
        u16 count = 0;

        debug_init ();

        auto elapsed_time = now ();
        u64 total_nodes = 0;
        for (const auto &cmd : uci_cmds)
        {
            istringstream is (cmd);
            string token;
            token.clear ();
            is >> std::skipws >> token;

            if (white_spaces (token))
            {
                continue;
            }

            if (token == "position")
            {
                position (is, pos, states);
            }
            else
            if (token == "go")
            {
                std::cerr
                    << "\n---------------\n"
                    << "Position: " << std::right
                    << std::setw (2) << ++count << '/' << total << " "
                    << std::left << pos.fen () << std::endl;

                go (is, pos, states);
                Threadpool.main_thread ()->wait_while_busy ();
                total_nodes += Threadpool.nodes ();
            }
            else
            if (token == "setoption")
            {
                setoption (is);
            }
            else
            if (token == "ucinewgame")
            {
                clear ();
            }
            else
            {
                std::cerr << "Unknown command: \'" << token << "\'" << std::endl;
            }
        }

        elapsed_time = std::max (now () - elapsed_time, 1LL);

        debug_print (); // Just before exiting

        std::cerr << std::right
                    << "\n=================================\n"
                    << "Total time (ms) :" << std::setw (16) << elapsed_time << "\n"
                    << "Nodes searched  :" << std::setw (16) << total_nodes  << "\n"
                    << "Nodes/second    :" << std::setw (16) << total_nodes * 1000 / elapsed_time
                    << "\n---------------------------------\n"
                    << std::left << std::endl;
    }

    /// loop() Waits for a command from stdin, parses it and calls the appropriate function.
    /// Also intercepts EOF from stdin to ensure gracefully exiting if the GUI dies unexpectedly.
    /// Single command line arguments is executed once and returns immediately, e.g. 'bench'.
    /// In addition to the UCI ones, also some additional commands are supported.
    void loop (i32 argc, const char *const *argv)
    {
        // Join arguments
        string cmd;
        for (i32 i = 1; i < argc; ++i)
        {
            cmd += string(argv[i]) + " ";
        }

        debug_init ();

        Position pos;
        // Stack to keep track of the position states along the setup moves
        // (from the start position to the position just before the search starts).
        // Needed by 'draw by repetition' detection.
        StateListPtr states (new std::deque<StateInfo> (1));
        auto ui_thread = std::make_shared<Thread> (0);
        pos.setup (StartFEN, states->back (), ui_thread.get ());

        do
        {
            if (   1 == argc
                && !std::getline (std::cin, cmd, '\n')) // Block here waiting for input or EOF
            {
                cmd = "quit";
            }

            istringstream iss (cmd);
            string token;
            token.clear (); // Avoid a stale if getline() returns empty or blank line
            iss >> std::skipws >> token;

            if (white_spaces (token))
            {
                continue;
            }

            // GUI sends 'ponderhit' to tell that to ponder on the same move the opponent has played.
            // So 'ponderhit' will be sent if told to ponder on the same move the opponent has played.
            // We should continue searching but switch from pondering to normal search.
            // In case Threadpool.stop_on_ponderhit is set wait for 'ponderhit' to stop the search
            // (for instance because already ran out of time, max depth is reached),
            // otherwise should continue searching but switching from pondering to normal search.
            if (    token == "quit"
                ||  token == "stop"
                || (token == "ponderhit" && Threadpool.stop_on_ponderhit))
            {
                Threadpool.stop = true;
            }
            else
            if (token == "ponderhit")
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
                sync_cout << "id name " << Name << " " << info () << "\n"
                          << "id author " << Author << "\n"
                          << Options
                          << "uciok" << sync_endl;
            }
            else
            if (token == "ucinewgame")
            {
                clear ();
            }
            else
            if (token == "position")
            {
                position (iss, pos, states);
            }
            else
            if (token == "go")
            {
                go (iss, pos, states);
            }
            else
            if (token == "setoption")
            {
                setoption (iss);
            }
            // Additional custom non-UCI commands, useful for debugging
            else
            {
                if (token == "bench")
                {
                    bench (iss, pos, states);
                }
                else
                if (token == "flip")
                {
                    pos.flip ();
                }
                else
                if (token == "mirror")
                {
                    pos.mirror ();
                }
                // Print the root position
                else
                if (token == "show")
                {
                    sync_cout << pos << sync_endl;
                }
                else
                if (token == "eval")
                {
                    sync_cout << trace (pos) << sync_endl;
                }
                // Print the root fen and keys
                else
                if (token == "keys")
                {
                    sync_cout << std::hex << std::uppercase << std::setfill ('0')
                              << "FEN: "                        << pos.fen ()       << "\n"
                              << "Posi key: " << std::setw (16) << pos.si->posi_key << "\n"
                              << "Matl key: " << std::setw (16) << pos.si->matl_key << "\n"
                              << "Pawn key: " << std::setw (16) << pos.si->pawn_key << "\n"
                              << "PG key: " << std::setw (16) << pos.pg_key ()
                              << std::setfill (' ') << std::nouppercase << std::dec << sync_endl;
                }
                else
                if (token == "moves")
                {
                    sync_cout;
                    i32 count;
                    if (0 != pos.si->checkers)
                    {
                        std::cout << "\nEvasion moves: ";
                        count = 0;
                        for (const auto &vm : MoveList<GenType::EVASION> (pos))
                        {
                            if (pos.legal (vm))
                            {
                                std::cout << move_to_san (vm, pos) << " ";
                                ++count;
                            }
                        }
                        std::cout << "(" << count << ")";
                    }
                    else
                    {
                        std::cout << "\nQuiet moves: ";
                        count = 0;
                        for (const auto &vm : MoveList<GenType::QUIET> (pos))
                        {
                            if (pos.legal (vm))
                            {
                                std::cout << move_to_san (vm, pos) << " ";
                                ++count;
                            }
                        }
                        std::cout << "(" << count << ")";

                        std::cout << "\nCheck moves: ";
                        count = 0;
                        for (const auto &vm : MoveList<GenType::CHECK> (pos))
                        {
                            if (pos.legal (vm))
                            {
                                std::cout << move_to_san (vm, pos) << " ";
                                ++count;
                            }
                        }
                        std::cout << "(" << count << ")";

                        std::cout << "\nQuiet Check moves: ";
                        count = 0;
                        for (const auto &vm : MoveList<GenType::QUIET_CHECK> (pos))
                        {
                            if (pos.legal (vm))
                            {
                                std::cout << move_to_san (vm, pos) << " ";
                                ++count;
                            }
                        }
                        std::cout << "(" << count << ")";

                        std::cout << "\nCapture moves: ";
                        count = 0;
                        for (const auto &vm : MoveList<GenType::CAPTURE> (pos))
                        {
                            if (pos.legal (vm))
                            {
                                std::cout << move_to_san (vm, pos) << " ";
                                ++count;
                            }
                        }
                        std::cout << "(" << count << ")";
                    }

                    std::cout << "\nLegal moves: ";
                    count = 0;
                    for (const auto &vm : MoveList<GenType::LEGAL> (pos))
                    {
                        std::cout << move_to_san (vm, pos) << " ";
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
        while (   1 == argc
               && cmd != "quit");
    }
}

string info ()
{
    ostringstream oss;

    oss << std::setfill ('0');
#if defined(VER)
    oss << VER;
#else
    if (white_spaces (Version))
    {
        // From compiler, format is "Sep 2 1982"
        istringstream iss (__DATE__);
        string month, day, year;
        iss >> month >> day >> year;
        oss << std::setw (2) << year.substr (2)
            << std::setw (2) << month_index (month)
            << std::setw (2) << day;
    }
    else
    {
        oss << Version;
    }
#endif
    oss << std::setfill (' ');

    oss <<
#   if defined(BIT64)
        ".64"
#   else
        ".32"
#   endif
    ;

    oss <<
#   if defined(BM2)
        ".BM2"
#   elif defined(ABM)
        ".ABM"
#   else
        ""
#   endif
    ;

    oss <<
#   if defined(LPAGES)
        ".LP"
#   else
        ""
#   endif
    ;

    return oss.str ();
}

/// run() runs with command arguments
void run (int argc, const char *const *argv)
{
    std::cout << Name << " " << info () << " by " << Author << std::endl;
    std::cout << "info string Processor(s) detected " << std::thread::hardware_concurrency () << std::endl;

#if defined(LPAGES)
    Memory::initialize ();
#endif

    BitBoard::initialize ();
    BitBases::initialize ();
    psq_initialize ();
    zobrist_initialize ();
    Position::initialize ();
    UCI::initialize ();
    Pawns::initialize ();
    EndGame::initialize ();
    WinProcGroup::initialize ();
    TCluster::initialize ();
    Threadpool.configure (threads_option ());
    Book.initialize (string(Options["Book File"]));
    TBSyzygy::initialize (string(Options["SyzygyPath"]));
    Searcher::initialize ();
    Searcher::clear ();

    loop (argc, argv);
}

/// stop() exits with a code (in case of some crash).
void stop (int code)
{
    Threadpool.stop = true;
    Threadpool.configure (0);
    EndGame::deinitialize ();
    UCI::deinitialize ();
#if defined(LPAGES)
    Memory::deinitialize ();
#endif
    exit (code);
}
