#include "Parser.h"

#include <iostream>
#include <string>
#include <regex>

#include "Position.h"
#include "MoveGenerator.h"
#include "Notation.h"
#include "PolyglotBook.h"
#include "PGN.h"
#include "PGNFile.h"

using namespace std;
using namespace MoveGen;
using namespace Notation;
using namespace OpeningBook;

void print_key (Key key)
{
    cout
        << hex << uppercase << setfill ('0')
        << "Posi key: " << setw (16) << key << "\n"
        << setfill (' ') << nouppercase << dec
        << endl;
}
void Parser::parse ()
{
    BitBoard::initialize ();
    //Position::initialize ();
    //std::string lines[] = { "Roses are #ff0000",
    //    "violets are #0000ff",
    //    "all of my base are belong to you" };

    //std::regex color_regex ("#([a-f0-9]{2})"
    //                        "([a-f0-9]{2})"
    //                        "([a-f0-9]{2})");

    //for (const auto &line : lines)
    //{
    //    std::cout << line << ": "
    //        << std::regex_search (line, color_regex) << '\n';
    //}

    //std::smatch color_match;
    //for (const auto &line : lines)
    //{
    //    std::regex_search (line, color_match, color_regex);
    //    std::cout << "matches for '" << line << "'\n";
    //    for (size_t i = 0; i < color_match.size (); ++i)
    //        std::cout << i << ": " << color_match[i] << '\n';
    //}

    /*
    //PGN pgn ("test.pgn", std::ios::in);
    PGN pgn ("book.pgn", std::ios::in);
    //PGN pgn ("5lac.pgn", std::ios::in);
    
    cout << pgn.game_count () << endl;
    
    string text;

    text = pgn.read_text (1);
    cout << text << endl;
    //text = pgn.read_text (500000);
    //cout << text << endl;
    //text = pgn.read_text (2);
    //cout << text << endl;
    //text = pgn.read_text (3);
    //cout << text << endl;

    //Game game;
    //game.parse (text);
    //cout << game;
    
    /*
    Tag tag1 ("Hello1", 1);
    Tag tag2 ("Hello2", 2);
    Tag tag3 ("Hello3", 3);

    TagMap tags;
    tags["Event1"] = tag1;
    tags["Event2"] = tag2;
    tags["Event3"] = tag3;
    //cout << tags;

    Game game;
    game.tags = tags;
    cout << game;
    */
    
    /*
    PolyglotBook book;
    book.open ("book.bin", ios_base::in|ios_base::binary);
    
    string move;
    StateStack states;
    Position pos(STARTUP_FEN, nullptr);

    print_key (pos.posi_key ());
    /*
    states.push (StateInfo ());
    move = "e2e4";
    pos.do_move (move, states.top ());
    print_key (pos.posi_key ());
 
    states.push (StateInfo ());
    move = "d7d5";
    pos.do_move (move, states.top ());
    print_key (pos.posi_key ());
    /*
    states.push (StateInfo ());
    move = "e4e5";
    pos.do_move (move, states.top ());
    print_key (pos.posi_key ());

    states.push (StateInfo ());
    move = "f7f5";
    pos.do_move (move, states.top ());
    print_key (pos.posi_key ());
    
    states.push (StateInfo ());
    move = "e1e2";
    pos.do_move (move, states.top ());
    print_key (pos.posi_key ());

    //auto book_move = book.probe_move (pos, false);
    //cout << book_move << endl;
    //cout << book.read_entries (pos);
    */
    

    const char fn_pgn[] = "book.pgn";

    pgn_t pgn[1];
    open_pgn (pgn, fn_pgn);
    pgn->game_nb = 0;
    while (next_game_pgn (pgn))
    {
        Position pos(STARTUP_FEN, nullptr);
        //pgn->game_nb++;
    
        std::string move;
        StateStack states;
        while (next_move_pgn (pgn, move))
        {
            Move m = move_from_san (move, pos);

            
            cout << m << " ";

            states.push (StateInfo ());
            pos.do_move (m, states.top (), pos.gives_check (m, CheckInfo (pos)));
            
        }


        cout << endl << endl;
    }

    cout << pgn->game_nb << endl;

}
