#include "Benchmark.h"

#include <string>

#include "Debugger.h"
#include "Searcher.h"
#include "Thread.h"
#include "Option.h"

using namespace std;
using namespace Debugger;
using namespace Searcher;

namespace {

    const vector<string> DefaultFENs =
    {
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 10",
        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 11",
        "4rrk1/pp1n3p/3q2pQ/2p1pb2/2PP4/2P3N1/P2B2PP/4RRK1 b - - 7 19",
        "rq3rk1/ppp2ppp/1bnpb3/3N2B1/3NP3/7P/PPPQ1PP1/2KR3R w - - 7 14",
        "r1bq1r1k/1pp1n1pp/1p1p4/4p2Q/4Pp2/1BNP4/PPP2PPP/3R1RK1 w - - 2 14",
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
        "2K5/p7/7P/5pR1/8/5k2/r7/8 w - - 0 1",
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
        "8/8/8/8/5kp1/P7/8/1K1N4 w - - 0 1",     // Kc2 - Mate
        "8/8/8/5N2/8/p7/8/2NK3k w - - 0 1",      // Na2 - Mate
        "8/3k4/8/8/8/4B3/4KB2/2B5 w - - 0 1",    // Draw
        // 6-men positions
        "8/8/1P6/5pr1/8/4R3/7k/2K5 w - - 0 92",   // Re5 - Mate
        "8/2p4P/8/kr6/6R1/8/8/1K6 w - - 0 94",    // Ka2 - Mate
        "8/8/3P3k/8/1p6/8/1P6/1K3n2 b - - 0 90",  // Nd2 - Draw
        // 7-men positions
        "8/R7/2q5/8/6k1/8/1P5p/K6R w - - 0 124", // Draw
        // Mate and stalemate positions
        "8/8/8/8/8/6k1/6p1/6K1 w - - 0 100",
        "5k2/5P2/5K2/8/8/8/8/8 b - - 0 113",
        "8/8/8/8/8/4k3/4p3/4K3 w - - 0 145",
        "8/8/8/8/8/5K2/8/3Q1k2 b - - 0 137",
        "7k/7P/6K1/8/3B4/8/8/8 b - - 0 126",

    };
}

/// Runs a simple benchmark by letting engine analyze a set of positions for a given limit.
/// There are 5 optional parameters:
///  - Transposition table size (default is 16 MB)
///  - Threads count (default is 1 Thread)
///  - Limit value spent for each position (default is 13)
///  - Limit type:
///     * 'depth' (default)
///     * 'time'
///     * 'movetime'
///     * 'nodes'
///     * 'mate'
///  - FEN positions to be used:
///     * 'default' for builtin positions (default)
///     * 'current' for current position
///     * '<filename>' for file containing FEN positions
/// example: bench 32 1 10000 movetime default
void benchmark (istringstream &is, const Position &cur_pos)
{
    u32    hash;
    u16    threads;
    i64    value;
    string mode;
    string fen_fn;

    // Assign default values to missing arguments
    hash    = (is >> hash)    ? hash    : 16;
    threads = (is >> threads) ? threads : 1;
    value   = (is >> value)   ? value   : 13;
    mode    = (is >> mode)   && !white_spaces (mode)   ? mode   : "depth";
    fen_fn  = (is >> fen_fn) && !white_spaces (fen_fn) ? fen_fn : "default";

    auto *ui_thread = cur_pos.thread;

    Limit limits;
    if (mode == "time")
    {
        limits.clock[WHITE].time =
        limits.clock[BLACK].time = u64(abs (value));
    }
    else
    if (mode == "movetime")
    {
        limits.movetime = u64(abs (value));
    }
    else
    if (mode == "nodes")
    {
        limits.nodes = u64(abs (value));
    }
    else
    if (mode == "mate")
    {
        limits.mate = u08(abs (value));
    }
    else //mode=="depth"
    {
        limits.depth = i16(abs (value));
    }

    vector<string> fens;

    if (fen_fn == "default")
    {
        fens = DefaultFENs;
    }
    else
    if (fen_fn == "current")
    {
        fens.push_back (cur_pos.fen ());
    }
    else
    {
        ifstream ifs (fen_fn);
        if (!ifs.is_open ())
        {
            std::cerr << "ERROR: unable to open file ... \'" << fen_fn << "\'" << std::endl;
            return;
        }
        string fen;
        while (std::getline (ifs, fen))
        {
            if (!white_spaces (fen))
            {
                fens.push_back (fen);
            }
        }
        ifs.close ();
        fens.shrink_to_fit ();
    }

    clear ();
    Options["Hash"] = to_string (hash);
    Options["Threads"] = to_string (threads);

    u64  total_nodes = 0;
    Position pos;
    auto start_time = now ();
    for (u16 i = 0; i < fens.size (); ++i)
    {
        std::cerr
            << "\n---------------\n"
            << "Position: " << std::right
            << std::setw (2) << i+1 << "/" << fens.size () << " "
            << std::left << fens[i] << std::endl;

        StateListPtr states (new std::deque<StateInfo> (1));
        pos.setup (fens[i], states->back (), ui_thread);
        assert(pos.fen () == fens[i]);

        limits.start_time = now ();
        Threadpool.start_thinking (pos, states, limits);
        Threadpool.wait_while_thinking ();
        total_nodes += Threadpool.nodes ();
    }

    auto elapsed_time = std::max (now () - start_time, 1LL);

    dbg_print (); // Just before exit
    std::cerr
        << std::right
        << "\n=================================\n"
        << "Total time (ms) :" << std::setw (16) << elapsed_time << "\n"
        << "Nodes searched  :" << std::setw (16) << total_nodes  << "\n"
        << "Nodes/second    :" << std::setw (16) << total_nodes * 1000 / elapsed_time
        << "\n---------------------------------\n"    
        << std::left
        << std::endl;
}

/// Runs command perft
/// There are 2 optional parameters:
///  - Depth
///  - FEN positions to be used:
///     * 'default' for builtin positions (default)
///     * 'current' for current position
///     * '<filename>' for file containing FEN positions
void perft (istringstream &is, const Position &cur_pos)
{
    i16 depth;
    string fen_fn;

    // Assign default values to missing arguments
    depth  = (is >> depth) ? depth : 6;
    fen_fn = (is >> fen_fn) && !white_spaces (fen_fn) ? fen_fn : "default";

    auto *ui_thread = cur_pos.thread;

    vector<string> fens;
    if (fen_fn == "default")
    {
        fens = DefaultFENs;
    }
    else
    if (fen_fn == "current")
    {
        fens.push_back (cur_pos.fen ());
    }
    else
    {
        ifstream ifs (fen_fn);
        if (!ifs.is_open ())
        {
            std::cerr << "ERROR: unable to open file ... \'" << fen_fn << "\'" << std::endl;
            return;
        }
        string fen;
        while (std::getline (ifs, fen))
        {
            if (!white_spaces (fen))
            {
                fens.push_back (fen);
            }
        }
        ifs.close ();
        fens.shrink_to_fit ();
    }

    clear ();

    u64  total_nodes = 0;
    Position pos;
    auto start_time = now ();
    for (u16 i = 0; i < fens.size (); ++i)
    {
        std::cerr
            << "\n---------------\n"
            << "Position: " << std::right
            << std::setw (2) << i+1 << "/" << fens.size () << " "
            << std::left << fens[i] << std::endl;
        StateInfo si;
        pos.setup (fens[i], si, ui_thread);
        assert(pos.fen () == fens[i]);

        auto leaf_nodes = perft<true > (pos, depth);
        std::cerr
            << "\nLeaf nodes: "
            << leaf_nodes << std::endl;
        total_nodes += leaf_nodes;
    }

    auto elapsed_time = std::max (now () - start_time, 1LL);

    dbg_print (); // Just before exit
    std::cerr
        << std::right
        << "\n=================================\n"
        << "Total time (ms) :" << std::setw (16) << elapsed_time << "\n"
        << "Nodes searched  :" << std::setw (16) << total_nodes  << "\n"
        << "Nodes/second    :" << std::setw (16) << total_nodes * 1000 / elapsed_time
        << "\n---------------------------------\n"
        << std::left
        << std::endl;
}
