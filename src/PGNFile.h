#ifndef _PGNFILE_H_INC_
#define _PGNFILE_H_INC_

#include "Type.h"

//#define STRING_SIZE 256

typedef struct pgn_t
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

    std::string result;
    std::string fen;

    std::string white_elo;
    std::string black_elo;

    int move_line;
    int move_column;
    int game_nb;

    void open (const char *fn_pgn);
    void close ();

    bool next_game ();
    bool next_move (std::string &moves);

} pgn_t;

#endif