#include "Game.h"

#include <sstream>
#include <regex>
#include <iterator>

#include "Notation.h"

using namespace std;
using namespace Notation;

Game::Game ()
    : _current_pos (STARTUP_FEN)
    , _result (NO_RES)
{}

Game::Game (i32 dummy)
{}

//Game::Game (const char   *text)
//{
//    Game game (0);
//    if (parse (game, text))
//    {
//        *this = game;
//    }
//    else
//    {
//        clear ();
//    }
//}
Game::Game (const string &text)
{
    Game game (0);
    if (parse (game, text))
    {
        *this = game;
    }
    else
    {
        clear ();
    }
}

Game::Game (const Game &game)
{
    *this = game;
}
Game::~Game ()
{}
Game& Game::operator= (const Game &game)
{
    return *this;
}

void Game::add_tag (const Tag &tag)
{
}
void Game::add_tag (const string &name, const string &value)
{
    auto find = _tag_map.find (name);
    
    _tag_map[name] = Tag (value, find != _tag_map.end () ? distance (_tag_map.begin (), find) : _tag_map.size ());

    if (name == "FEN")
    {
        setup (value);
    }

    //p = _tag_map.find (); if (p != _tag_map.end ())
    //    //this line produce error for insert
    //    pair<map<string, string>::iterator, bool> test = _tag_map.insert (make_pair (name, value));
    //if (test.second)
    //{
    //    pair<string, string> tag = *(test.first);
    //    if (equals (tag.first, "fen"))
    //    {
    //        string &fen = tag.second;
    //        setup (fen);
    //    }
    //}

}

bool Game::append_move (Move m)
{
    // TODO:: check legal move
    if (_current_pos.legal (m))
    {
        StateInfo si;
        _state_stk.push (si);
        //_move_list.emplace_back (m);

        _current_pos.do_move (m, _state_stk.top (), _current_pos.gives_check (m, CheckInfo (_current_pos)));

        return true;
    }
    return false;
}
bool Game::append_move (const string &smove)
{
    // TODO::
    Move m = move_from_san (smove, _current_pos);
    
    if (MOVE_NONE == m)
    {
        cerr << "ERROR: Illegal Move '" + smove << "'" << endl;
        return false;
    }
    
    _move_list.push_back (m);
    _state_stk.push (StateInfo ());
    _current_pos.do_move (m, _state_stk.top (), _current_pos.gives_check (m, CheckInfo (_current_pos)));
    
    return true;
}

// Remove last move
bool Game::remove_move ()
{
    _current_pos.undo_move ();
    //Move m = _state_stk.top ().last_move;
    _state_stk.pop ();
    _move_list.pop_back ();
    return true;
}

bool Game::setup (const string &fen, bool c960, bool full)
{
    return _current_pos.setup (fen, nullptr, c960, full);
}

void Game::clear ()
{
    _tag_map.clear ();
    _move_list.clear ();
    _current_pos.clear ();
    _result = NO_RES;
}
void Game::reset ()
{
    size_t size = _move_list.size ();
    while (!_move_list.empty ())
    {
        if (!remove_move ())
        {
            //cout << "ERROR: Undo move " << _move_list.back ();
            break;
        }
    }

    if (size != _move_list.size ())
    {
        _result = NO_RES;
    }
}

string Game::print_tags () const
{
    ostringstream oss;
    for (auto idx = 0; idx < _tag_map.size (); ++idx)
    {
        auto itr = _tag_map.begin ();
        while (itr != _tag_map.end ())
        {
            const auto &tag = itr->second;
            if (idx == tag.index)
            {
                oss << "[" << itr->first << " \"" << tag << "\"]" << endl;
            }
            ++itr;
        }
    }
    return oss.str ();
}
string Game::print_moves () const
{
    ostringstream oss;
    string name = "FEN";
    Position pos (_tag_map.find (name) != _tag_map.end () ? _tag_map.at (name) : STARTUP_FEN, nullptr);
    
    StateStack states;
    u08 ply = 0;
    for (auto m : _move_list)
    {
        oss << move_to_san (m, pos) << " ";
        states.push (StateInfo ());
        pos.do_move (m, states.top (), pos.gives_check (m, CheckInfo (pos)));
        ++ply;
        ////---------------------------------
        //oss << move_to_can (m, pos.chess960 ()) << " ";
    }

    while (ply != 0)
    {
        pos.undo_move ();
        states.pop ();
        --ply;
    }

    return oss.str ();
}

string Game::pgn () const
{
    ostringstream oss;
    // pgn format
    oss << print_tags ();

    return oss.str ();
}

Game::operator string ()  const
{
    ostringstream oss;
    // tag list
    // starting fen
    // move list
    // last position
    oss << print_tags ();
    oss << print_moves ();

    return oss.str ();
}

/*
bool Game::parse (Game &game, const char   *text)
{
    bool is_ok = false;
    char *c = strdup (text);

    //char name[40];
    //char value[80];
    //i32 n;

    //// Tag section
    //do
    //{
    //    // " [ %s \"%[^\"]\" ] %n" or " [ %[^ ] \"%[^\"]\" ] %n"
    //    i08 read = _snscanf_s (c, 256, " [ %s \"%[^\"]\" ] %n", name, 40, value, 80, &n);
    //    if (read != 2) break;
    //    c += n;

    //    cout << name << " " << value << endl;
    //    //game.AddTag(name, value);
    //}
    //while ('\0' != *c);

    //char *m = c;
    //// modify he move section
    //do
    //{
    //    if (';' == *m)
    //    {
    //        while ('\n' != *m)
    //        {
    //            *m++ = ' ';
    //            if ('\0' == *m) goto end_modify;
    //        }
    //    }

    //    if ('\n' == *m)
    //    {
    //        *m++ = ' ';
    //        //while (' ' == *m) ++m;

    //        if ('%' == *m)
    //        {
    //            while ('\n' != *m)
    //            {
    //                *m++ = ' ';
    //                if ('\0' == *m) goto end_modify;
    //            }
    //            *m++ = ' ';
    //        }
    //    }
    //    ++m;
    //}
    //while ('\0' != *m);

    //end_modify:

    // Move section
    do
    {

        c++;
    }
    while ('\0' != *c);

    free (c);

    return is_ok;
}
*/
bool Game::parse (Game &game, const string &text)
{
    bool is_ok = false;

    // TODO::

    ////string seq("[Event \"Blitz 4m+2s\"]\n[Site \"?\"]\n[Date \"2001.12.05\"]\n[Round \"4\"]\n[White \"Deep Fritz 13\"]\n[Black \"aquil, muzaffar\"]\n[Result \"1/2-1/2\"]\n[ECO \"C80\"]\n[WhiteElo \"2839\"]\n[BlackElo \"2808\"]\n[PlyCount \"37\"]\n");
    //char *pat = "[Event \"Blitz 4m+2s\"]\n[Site \"?\"]\n1. e4 e5 2. Nf3 {a}  {b} Nc6 3. Bb5 a6 4...d5 1-0";
    //"11... Bxe3 12. Qxe3 Nxc3  13. Qxc3 {dfs} {sfsf} Qd7 14. Rad1 Nd8";
    //string seq (text);

    string tag_regexp  = "(?:^\\s*\\[\\s*(\\w+)\\s+\"([^\"]+)\"\\s*\\]\\s*)";
    string move_regexp = "(?:\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(\\d+)(\\.|\\.{3})\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:([NBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?)\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:([NBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?)\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*)?|\\s*(\\*|1-0|0-1|1\\/2-1\\/2)\\s*)";

    //string reg_esp =
    //    /// tag
    //    "(?:^\\s*\\[\\s*(\\w+)\\s+\"([^\"]+)\"\\s*\\]\\s*)";
    //    ///move
    //    // backtracking
    //    //"(?:\\s*(?:\\{([^\\}]*?)\\}\\s*)?\\s*(\\d+)(\\.|\\.{3})\\s*(?:\\{([^\\}]*?)\\}\\s*)?\\s*((?:([PNBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?))\\s*(?:\\{([^\\}]*?)\\}\\s*)?\\s*(?:\\s*(?:\\{([^\\}]*?)\\}\\s*)?\\s*((?:([PNBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?))\\s*(?:\\{([^\\}]*?)\\}\\s*)?\\s*)?)";
    //    // no backtracking
    //    //"(?:\\s*(?:\\{([^\\}]*?)\\}\\s*)?\\s*(\\d+)(\\.|\\.{3})\\s*(?:\\{([^\\}]*?)\\}\\s*)?\\s*(?:([PNBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?)\\s*(?:\\{([^\\}]*?)\\}\\s*)?\\s*(?:\\s*(?:\\{([^\\}]*?)\\}\\s*)?\\s*(?:([PNBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?)\\s*(?:\\{([^\\}]*?)\\}\\s*)?\\s*)?)";
    //    // no comment
    //    //"(?:\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(\\d+)(\\.|\\.{3})\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:([PNBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?)\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:([PNBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?)\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*)?)";
    //    // final
    //    //"(?:^\\s*\\[\\s*(\\w+)\\s+\"([^\"]+)\"\\s*\\]\\s*)|(?:\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(\\d+)(\\.|\\.{3})\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:([NBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?)\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:([NBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?)\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*)?|\\s*(\\*|1-0|0-1|1\\/2-1\\/2)\\s*)";
    //// endMarker
    ////\\s+(1\\-?0|0\\-?1|1\\/2\\-?1\\/2|\\*)\\s+

    regex tag_regex (tag_regexp);//, regex_constants::match_flag_type::match_continuous);
    for (sregex_iterator itr (text.begin (), text.end (), tag_regex), end; itr != end; ++itr)
    {
        /*
        bool first = true;
        for (auto x : (*itr))
        {
            if (first)
            {
                first = false;
                continue;
            }
            
            //game.add_tag (string(x.first), string(x.second));
            cout << x << "   ";
        }
        cout << endl;
        */

        game.add_tag ((*itr)[1].str (), (*itr)[2].str ());
    }

    regex move_regex (move_regexp);//, regex_constants::match_flag_type::match_continuous);
    for (sregex_iterator itr (text.begin (), text.end (), move_regex), end; itr != end; ++itr)
    {
        //cout << std::distance (itr, end);
        smatch match = *itr;
        //cout << match.size ();

        if (match.size () > 3 && match[3].matched && match[3].length () != 0)
        {
            cout << match[3];
            game.append_move (match[3]);
        }
        
        if (match.size () > 4 && match[4].matched && match[4].length () != 0)
        {
            cout << match[4];
            game.append_move (match[4]);
        }

        cout << endl;
    }

    //cout << "--------" << endl;

    //smatch match;
    //while (regex_search(seq, match, rgx, regex_constants::match_flag_type::match_not_null))
    //{
    //    bool first = false;
    //    
    //    for (sub_match<string::const_iterator> x : match)
    //    for (auto x : match)
    //    {
    //        if (first)
    //        {
    //            first = false;
    //            continue;
    //        }
    //        cout << x << "   ";
    //    }
    //    cout << endl;
    //    seq = match.suffix().str();
    //}

    return is_ok;
}
