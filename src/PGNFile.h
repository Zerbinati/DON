#ifndef _PGNFILE_H_INC_
#define _PGNFILE_H_INC_

#include <fstream>
#include <vector>
#include <stack>

#include "Type.h"
#include "Game.h"

// PGN file with *.pgn extension
class PGNFile
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

    std::string _pgn_fn = "";
    openmode    _mode   = openmode(0);
    size_t      _size   = 0;

    std::vector<u64> _game_indexes;
    std::stack<char> _char_stack;

    void _reset ();
    void _build_indexes ();
    
    void _scan_index (const std::string &str, u64 &pos, State &state);
    void _add_index (u64 pos);

public:

    PGNFile () = default;
    PGNFile (const PGNFile&) = delete;
    PGNFile& operator= (const PGNFile&) = delete;
    
    PGNFile (const std::string &pgn_fn, std::ios_base::openmode mode);
    ~PGNFile ();

    bool open (const std::string &pgn_fn, std::ios_base::openmode mode);
    void close ();

    std::string pgn_fn () const { return _pgn_fn; }
    size_t size ()
    {
        if (_size != 0) return _size;

        u64 pos_cur = tellg ();
        seekg (0L, std::ios_base::end);
        _size = tellg ();
        seekg (pos_cur, std::ios_base::beg);
        clear ();
        return _size;
    }

    u64 game_count () const { return _game_indexes.size (); }

    std::string read_text (u64 index);
    std::string read_text (u64 beg_index, u64 end_index);
    
    u64 write_text (const std::string &text);

    Game read_game (u64 index);
    
    u64 write_game (const Game &game);

};

#endif // _PGNFILE_H_INC_
