#ifndef _PGN_H_INC_
#define _PGN_H_INC_

#include "Type.h"

//#define STRING_SIZE 256

struct PGN
{
private:

    void _read_char ();
    void _unread_char ();

    void _read_skip_blanks ();

    void _read_token ();
    void _unread_token ();

    void _read_tok ();

    FILE *_file = nullptr;

    enum token_t
    {
        TOKEN_ERROR   = -1,
        TOKEN_EOF     = 256,
        TOKEN_SYMBOL  = 257,
        TOKEN_STRING  = 258,
        TOKEN_INTEGER = 259,
        TOKEN_NAG     = 260,
        TOKEN_RESULT  = 261
    };

public:

    int char_hack;
    i32 char_line;
    i32 char_column;
    bool char_unread;
    bool char_first;

    std::string token;
    token_t token_type;
    i32 token_line;
    i32 token_column;
    bool token_unread;
    bool token_first;

    u32 games;
    std::string result;
    std::string fen;
    std::string white_elo;
    std::string black_elo;

    u16 moves;
    i32 move_line;
    i32 move_column;

    void open (const std::string &pgn_fn);
    void close ();

    bool next_game ();
    bool next_move (std::string &moves);

};

#endif // _PGN_H_INC_