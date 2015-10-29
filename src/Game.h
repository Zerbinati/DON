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
public:

    std::string value;
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

    template<class charT, class Traits>
    friend std::basic_ostream<charT, Traits>&
        operator<< (std::basic_ostream<charT, Traits> &os, const Tag &tag)
    {
        os << std::string(tag);
        return os;
    }

};

typedef std::map<std::string, Tag, std::no_case_less_comparer> TagMap;

template<class CharT, class Traits>
inline std::basic_ostream<CharT, Traits>&
operator<< (std::basic_ostream<CharT, Traits> &os, const TagMap &tagmap)
{
    for (auto index = 0; index < tagmap.size (); ++index)
    {
        /*
        auto pair = tagmap.begin ();
        while (pair != tagmap.end ())
        {
            const auto &tag = pair->second;
            if (index == tag.index)
            {
                os << "[" << pair->first << " \"" << tag << "\"]" << endl;
            }
            ++pair;
        }
        */
        for (auto &pair : tagmap)
        {
            const auto &tag = pair.second;
            if (index == tag.index)
            {
                os << "[" << pair.first << " \"" << tag << "\"]" << std::endl;
                break;
            }
        }
    }
    return os;
}

class Game
{

private:

    bool setup (const std::string &fen, bool c960 = false, bool full = true);

public:

    TagMap      tags;
    MoveVector  moves;
    StateStack  states;
    Position    position;
    Result      result;

    Game ()
        : position (STARTUP_FEN)
        , result (NO_RES)
    {}
    Game (i32)
    {}
    Game (const std::string &text)
    {
        if (!parse (text)) clear ();
    }
    Game (const Game &game)
    {
        *this = game;
    }

    ~Game ()
    {}

    Result Result ()     const { return result; }

    void add_tag (const std::string &name, const Tag &tag);
    void add_tag (const std::string &name, const std::string &value);

    bool append_move (Move m);
    bool append_move (const std::string &smove);

    bool remove_last_move ();

    void clear ();
    void reset ();

    bool parse (const std::string &text);

    std::string print_moves (bool is_pos = false) const;

    std::string pgn () const;
    operator std::string () const;

    template<class charT, class Traits>
    friend std::basic_ostream<charT, Traits>&
        operator<< (std::basic_ostream<charT, Traits> &os, const Game &game)
    {
        os << std::string(game);
        return os;
    }

    template<class charT, class Traits>
    friend std::basic_istream<charT, Traits>&
        operator>> (std::basic_istream<charT, Traits> &is, Game &game)
    {
        //is >> game;
        return is;
    }


};

#endif // _GAME_H_INC_
