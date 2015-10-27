#include "Parser.h"

#include "Position.h"
#include "Notation.h"
#include "PolyglotBook.h"
#include "PGN.h"

using namespace std;
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

    Game game;
    game.parse (text);
    cout << game;
    */
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
    //MoveVector moves;
    //moves.push_back (Move(634));
    //cout << moves;

    PolyglotBook book;
    book.open ("book.bin", ios_base::in|ios_base::binary);
    
    string move;
    StateStack states;
    Position pos(STARTUP_FEN, nullptr);

    print_key (pos.posi_key ());
    
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
    */

    //auto book_move = book.probe_move (pos, false);
    //cout << book_move << endl;

    cout << book.read_entries (pos);

}
