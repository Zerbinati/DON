#ifndef _PGN_H_INC_
#define _PGN_H_INC_

#include <fstream>
#include <vector>
#include <stack>

#include "Type.h"
#include "Game.h"

// PGN file with *.pgn extension
class PGN
    : private std::fstream
{

private:

    enum State : i08
    {
        S_ERR = -1,
        S_NEW = 0,

        S_TAG_NEW,
        S_TAG_BEG,
        S_TAG_END,

        S_MOV_NEW,
        S_MOV_LST,
        S_MOV_COM,

        S_VAR_LST,
        S_VAR_COM,

    };

    std::string _fn_pgn;
    std::ios_base::openmode _mode;

    u64 _size_pgn;

    std::vector<u64> _indexes_game;
    std::stack<char> _stk_char;

    void _reset ();
    void _build_indexes ();
    
    void _scan_index (const std::string &str, u64 &pos, State &state);
    void _add_index (u64 pos);

public:

    PGN (const PGN&) = delete;
    PGN& operator= (const PGN&) = delete;

    PGN ();

    // mode = std::ios_base::in|std::ios_base::out

    PGN (const std::string &fn_pgn, std::ios_base::openmode mode);

    ~PGN ();

    bool open (const std::string &fn_pgn, std::ios_base::openmode mode);

    void close ();

    u64 size ()
    {
        if (0 >= _size_pgn)
        {
            u64 pos_cur = tellg ();
            seekg (0L, std::ios_base::end);
            _size_pgn = tellg ();
            seekg (pos_cur, std::ios_base::beg);
            clear ();
        }
        return _size_pgn;
    }

    std::string filename () const { return _fn_pgn; }

    u64 game_count () const { return _indexes_game.size (); }

    std::string read_text (u64 index);
    std::string read_text (u64 index_beg, u64 index_end);
    u64 write_text (const std::string &text);

    Game read_game (u64 index);
    u64 write_game (const Game &game);

};

#endif // _PGN_H_INC_
