#include "Parser.h"

#include "Position.h"
#include "PGN.h"

using namespace std;

void Parser::parse ()
{
    BitBoard::initialize ();
    //Position::initialize ();

    PGN pgn ("test.pgn", std::ios::in);
    string text;

    text = pgn.read_text (1);
    cout << text << endl;
    //text = pgn.read_text (2);
    //cout << text << endl;
    //text = pgn.read_text (3);
    //cout << text << endl;

    Game game;
    Game::parse (game, text);


    cout << game;

}
