#ifndef _GAME_H_INC_
#define _GAME_H_INC_

#include <map>
#include <vector>
#include "functor.h"
#include "Position.h"

enum Result : u08
{
    NO_RES  = 0,
    WIN_W   = 1,
    WIN_B   = 2,
    DRAW    = 3,

};

struct Tag
{
private:

private:
    std::string value;

public:

    i32 index;

    Tag ()
        : value ("")
        , index (0)
    {}
    Tag (std::string val, i32 idx)
        : value (val)
        , index (idx)
    {}

    operator std::string () const { return value; }

    std::string Tag::to_string () const { return *this; }

    template<class charT, class Traits>
    friend std::basic_ostream<charT, Traits>&
        operator<< (std::basic_ostream<charT, Traits> &os, const Tag &tag)
    {
        os << std::string (tag);
        return os;
    }

};


class Game
{
public:

    typedef std::map<std::string, Tag, std::no_case_less_comparer> TagMap;
    //typedef std::map<std::string, std::string, std::no_case_less_comparer> TagMap;

private:

    TagMap     _tag_map;

    MoveVector _move_list;
    StateStack _state_stk;

    Position   _current_pos;

    Result     _result;

    std::string print_tags () const;
    std::string print_moves () const;

public:

    Game ();
    explicit Game (i32 dummy);

    //Game (const        char *text);
    Game (const std::string &text);

    Game (const Game &game);
    ~Game ();
    Game& operator= (const Game &game);

    //Position Position () const { return _current_pos; }

    Result Result ()     const { return _result; }

    void add_tag (const Tag &tag);
    void add_tag (const std::string &name, const std::string &value);

    bool append_move (Move m);
    bool append_move (const std::string &smove);

    bool remove_move ();

    bool setup (const std::string &fen, bool c960 = false, bool full = true);

    void clear ();
    void reset ();

    std::string pgn () const;
    operator std::string () const;

    //static bool parse (Game &game, const        char *text);
    static bool parse (Game &game, const std::string &text);

    template<class charT, class Traits>
    friend std::basic_ostream<charT, Traits>&
        operator<< (std::basic_ostream<charT, Traits> &os, const Game &game)
    {
        os << std::string (game);
        return os;
    }

    template<class charT, class Traits>
    friend std::basic_istream<charT, Traits>&
        operator>> (std::basic_istream<charT, Traits> &is, Game &game)
    {
        //is >> std::string (game);
        return is;
    }


};

#endif // _GAME_H_INC_
