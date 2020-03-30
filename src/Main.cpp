#include <iostream>
#include <thread>

#include "Bitbase.h"
#include "Bitboard.h"
#include "Cuckoo.h"
#include "Endgame.h"
#include "Polyglot.h"
#include "PSQTable.h"
#include "Searcher.h"
#include "Thread.h"
#include "TimeManager.h"
#include "UCI.h"
#include "Zobrist.h"

/// clear() clears the stuffs in case of some crash.
void clear() {
    Threadpool.stop = true;
    Threadpool.setup(0);
    Threadpool.clear();
}

int main(int argc, char const *const *argv) {

    std::cout
        << Name << " " << engineInfo() << " by " << Author << "\n"
        << "info string Processor(s) detected " << std::thread::hardware_concurrency() << std::endl;

    BitBoard::initialize();
    BitBase::initialize();
    PSQT::initialize();
    Zobrists::initialize();
    Cuckoos::initialize();
    UCI::initialize();
    EndGame::initialize();
    Book.initialize(Options["Book File"]);
    Threadpool.setup(optionThreads());
    TimeMgr.clear();
    UCI::clear();

    std::atexit(clear);

    // Join arguments
    std::string cmdLine;
    for (int i = 1; i < argc; ++i) {
        cmdLine += std::string{ argv[i] } + " ";
    }

    UCI::handleCommands(cmdLine);

    std::exit(EXIT_SUCCESS);
    return EXIT_SUCCESS;
}
