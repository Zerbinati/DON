#pragma once

#include <streambuf>

// Fancy logging facility.
// The trick here is to replace cin.rdbuf() and cout.rdbuf() with two
// TieStreamBuf objects that tie std::cin and std::cout to a out file stream.
// Can toggle the logging of std::cout and std:cin at runtime whilst preserving
// usual I/O functionality, all without changing a single line of code!
// Idea from http://groups.google.com/group/comp.lang.c++/msg/1d941c0f26ea0d81
class TieStreamBuf
    : public std::streambuf {

private:

    int_type write(int_type ch, std::string const &prefix) {
        // Previous character
        static int_type pCh = '\n';

        if ('\n' == pCh) {
            //if (
            sbWrit->sputn(prefix.c_str(), prefix.length());
            //    != prefix.length()) return EOF;
        }
        return pCh = sbWrit->sputc(char(ch));
    }

public:

    std::streambuf *sbRead
        ,          *sbWrit;

    TieStreamBuf(
          std::streambuf *sbRd
        , std::streambuf *sbWr)
        : sbRead{ sbRd }
        , sbWrit{ sbWr }
    {}
    //TieStreamBuf(TieStreamBuf const&) = delete;
    //TieStreamBuf& operator=(TieStreamBuf const&) = delete;

    int sync() override {
        return sbWrit->pubsync(), sbRead->pubsync();
    }

    int_type overflow(int_type ch) override {
        return write(sbRead->sputc(char(ch)), "<< ");
    }

    int_type underflow() override {
        return sbRead->sgetc();
    }

    int_type uflow() override {
        return write(sbRead->sbumpc(), ">> ");
    }

};
