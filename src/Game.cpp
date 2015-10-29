#include "Game.h"

#include <sstream>
#include <regex>
#include <iterator>

#include "MoveGenerator.h"
#include "Notation.h"

using namespace std;
using namespace MoveGen;
using namespace Notation;

void Game::add_tag (const string &name, const Tag &tag)
{
    tags[name] = tag;
    if (name == "FEN")
    {
        string set_name = "SetUp";
        string set_value = "1";
        auto find = tags.find (set_name);
        tags[set_name] = Tag (set_value, i32 (find != tags.end () ? distance (tags.begin (), find) : tags.size ()));

        setup (tag.value);
    }

    //p = tags.find (); if (p != tags.end ())
    //    //this line produce error for insert
    //    pair<map<string, string>::iterator, bool> test = tags.insert (make_pair (name, tag.value));
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
void Game::add_tag (const string &name, const string &value)
{
    auto find = tags.find (name);
    auto tag = Tag (value, i32 (find != tags.end () ? distance (tags.begin (), find) : tags.size ()));
    add_tag (name, tag);
}

bool Game::append_move (Move m)
{
    if (m != MOVE_NONE
        && MoveList<LEGAL> (position).contains (m)
       )
    {
        moves.push_back (m);
        states.push (StateInfo ());
        position.do_move (m, states.top (), position.gives_check (m, CheckInfo (position)));
        return true;
    }
    return false;
}
bool Game::append_move (const string &move)
{
    Move m = move_from_san (move, position);
    if (MOVE_NONE == m)
    {
        cerr << "ERROR: Illegal Move '" + move << "'" << endl;
        return false;
    }
    return append_move (m);
}

// Remove last move
bool Game::remove_last_move ()
{
    position.undo_move ();
    //Move m = states.top ().last_move;
    states.pop ();
    moves.pop_back ();
    return true;
}

bool Game::setup (const string &fen, bool c960, bool full)
{
    return position.setup (fen, nullptr, c960, full);
}

void Game::clear ()
{
    tags.clear ();
    moves.clear ();
    position.clear ();
    result = NO_RES;
}
void Game::reset ()
{
    size_t size = moves.size ();
    while (!moves.empty ())
    {
        if (!remove_last_move ())
        {
            //cout << "ERROR: Undo move " << moves.back ();
            break;
        }
    }

    if (size != moves.size ())
    {
        result = NO_RES;
    }
}


string Game::print_moves (bool is_pos) const
{
    ostringstream oss;

    string name = "FEN";
    Position pos (tags.find (name) != tags.end () ? tags.at (name) : STARTUP_FEN, nullptr);
    StateStack st;
    u08 ply = 0;
    for (auto m : moves)
    {
        oss << move_to_san (m, pos) << " ";
        st.push (StateInfo ());
        pos.do_move (m, st.top (), pos.gives_check (m, CheckInfo (pos)));
        ++ply;
        ////---------------------------------
        //oss << move_to_can (m, pos.chess960 ()) << " ";
    }

    if (is_pos) oss << "\n" << pos; // current position

    while (ply != 0)
    {
        pos.undo_move ();
        st.pop ();
        --ply;
    }

    return oss.str ();
}

string Game::pgn () const
{
    ostringstream oss;
    // pgn format
    oss << tags                 << "\n"
        << print_moves (false)  << "\n";
    return oss.str ();
}

Game::operator string ()  const
{
    ostringstream oss;
    oss << tags                 << "\n"  // tag list + starting fen
        << print_moves (true)   << "\n"; // move list
    return oss.str ();
}

namespace {

    vector<vector<string>> find_groups (const string &s, const string &regexp, bool case_sensitive = true)
    {
        regex *reg = nullptr;
        if (case_sensitive)
        {
            reg = new regex (regexp);
        }
        else
        {
            reg = new regex (regexp, regex_constants::syntax_option_type::icase);
        }

        vector<vector<string>> groups;
        vector<string> subgroups;

        const std::sregex_token_iterator end_i;
        for (std::sregex_token_iterator beg_i (s.cbegin (), s.cend (), *reg); beg_i != end_i; ++beg_i)
        {
            subgroups.clear ();
            string group = *beg_i;
            //cout << group << endl;
            smatch match;
            if (std::regex_search (group, match, *reg))
            {
                for (auto sm : match)
                    //for (unsigned i = 0; i < match.size (); ++i)
                {
                    //subgroups.push_back (match[i]);
                    subgroups.push_back (sm);
                }
                if (subgroups.size () > 0)
                {
                    groups.push_back (subgroups);
                }
            }
        }
        //groups.push_back (subgroups);

        if (reg != nullptr) delete reg;
        return groups;
    }

}

namespace {


    int char_row, char_col;

    char read_char ()
    {

        return ' ';
    }
}


//#define SKIP_WHITESPACE() do { if (length == index) goto finish; c = text[index++]; } while (isspace (c) && c != '\n')

/*
bool Game::parse (const char   *text)
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
    //    //AddTag(name, value);
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
bool Game::parse (const string &text)
{
    // TODO::

    ////string seq("[Event \"Blitz 4m+2s\"]\n[Site \"?\"]\n[Date \"2001.12.05\"]\n[Round \"4\"]\n[White \"Deep Fritz 13\"]\n[Black \"aquil, muzaffar\"]\n[Result \"1/2-1/2\"]\n[ECO \"C80\"]\n[WhiteElo \"2839\"]\n[BlackElo \"2808\"]\n[PlyCount \"37\"]\n");
    //char *pat = "[Event \"Blitz 4m+2s\"]\n[Site \"?\"]\n1. e4 e5 2. Nf3 {a}  {b} Nc6 3. Bb5 a6 4...d5 1-0";
    //"11... Bxe3 12. Qxe3 Nxc3  13. Qxc3 {dfs} {sfsf} Qd7 14. Rad1 Nd8";
    //string seq (text);

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


#pragma region final
    /*
    /// final code
    string tag_regexp  = "(?:^\\s*\\[\\s*(\\w+)\\s+\"([^\"]+)\"\\s*\\]\\s*)";
    //string move_regexp = "(?:\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(\\d+)(\\.|\\.{3})\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:([NBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?)\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:([NBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?(?:\\s*[!?]+)?)\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*)?|\\s*(\\*|1-0|0-1|1\\/2-1\\/2)\\s*)";
    string move_regexp = "(?:\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(\\d+)(\\.|\\.{3})\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:((?:[NBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?)(?:\\s*[!?]+)?)\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*(?:((?:[NBRQK]?[a-h]?[1-8]?x?[a-h][1-8](?:\\=[NBRQ])?|O(?:-?O){1,2})(?:[+][+]?|[#])?)(?:\\s*[!?]+)?)\\s*(?:\\{[^\\}]*?\\}\\s*)?\\s*)?|\\s*(\\*|1-0|0-1|1\\/2-1\\/2)\\s*)";

    regex tag_regex (tag_regexp);//, regex_constants::match_flag_type::match_continuous);
    for (sregex_iterator itr (text.begin (), text.end (), tag_regex), end; itr != end; ++itr)
    {

        //bool first = true;
        //for (auto x : (*itr))
        //{
        //    if (first)
        //    {
        //        first = false;
        //        continue;
        //    }
        //
        //    //add_tag (string(x.first), string(x.second));
        //    cout << x << "   ";
        //}
        //cout << endl;


        add_tag ((*itr)[1].str (), (*itr)[2].str ());
    }

    //regex move_regex (move_regexp);//, regex_constants::match_flag_type::match_continuous);
    //for (sregex_iterator itr (text.begin (), text.end (), move_regex), end; itr != end; ++itr)
    //{
    //    //cout << std::distance (itr, end);
    //    smatch match = *itr;
    //    //cout << match.size ();
    //    if (match.size () > 3 && match[3].matched && match[3].length () != 0)
    //    {
    //        //cout << match[3];
    //        append_move (match[3]);
    //    }
    //
    //    if (match.size () > 4 && match[4].matched && match[4].length () != 0)
    //    {
    //        //cout << match[4];
    //        append_move (match[4]);
    //    }
    //
    //    //cout << endl;
    //}

    auto groups = find_groups (text, move_regexp, true);

    for (auto group : groups)
        //for (auto itr = groups.begin (); itr != groups.end (); ++itr)
    {
        //auto group = *itr;

        if (group.size () > 3 && group[3].length () != 0)
        {
            //cout << group[3] << " ";
            append_move (group[3]);
        }
        if (group.size () > 4 && group[4].length () != 0)
        {
            //cout << group[4] << " ";
            append_move (group[4]);
        }
        if (group.size () > 5 && group[5].length () != 0)
        {
            cout << group[5] << "\n";
        }
    }
    */
#pragma endregion

    /// todo::
//    size_t index = 0;
//    auto length = text.length ();
//    while (index < length)
//    {
//
//        u08 c;
//
//        //SKIP_WHITESPACE ();
//
//        if (c != '[') break;
//
//
//
//    }
//
//finish:

    //string s = "1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. c3 Nf6 5. d4 exd4 6. cxd4 Bb4+ 7. Nc3 Nxe4 8. O-O Nxc3 9. bxc3 Bxc3 10. Qb3 Bxa1 11. Bxf7+ Kf8 12. Bg5 Ne7 13. Ne5 Bxd4 14. Bg6 d5 15. Qf3+ Bf5 16. Bxf5 Bxe5 17. Be6+ Bf6 18. Bxf6 Ke8 19. Bxg7 1-0";

    //copy (sregex_token_iterator (s.begin (), s.end (), move_regex, -1),
    //      sregex_token_iterator (),
    //      ostream_iterator<string> (cout, "\n"));

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

    

    return true;
}

