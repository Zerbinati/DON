#ifndef _PGNFILE_H_INC_
#define _PGNFILE_H_INC_

#include "Type.h"

//#define STRING_SIZE 256

struct pgn_t
{
    FILE *file;

    int char_hack;
    int char_line;
    int char_column;
    bool char_unread;
    bool char_first;

    std::string token;
    int token_type;
    int token_line;
    int token_column;
    bool token_unread;
    bool token_first;

    int games;
    std::string result;
    std::string fen;
    std::string white_elo;
    std::string black_elo;

    int moves;
    int move_line;
    int move_column;

    void open (const std::string &pgn_fn);
    void close ();

    bool next_game ();
    bool next_move (std::string &moves);

};

#endif