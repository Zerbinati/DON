#include "Parser.h"

#include <iostream>
#include <string>
#include <regex>

#include "Position.h"
#include "MoveGenerator.h"
#include "Notation.h"
#include "Polyglot.h"
#include "PGNFile.h"
#include "PGN.h"

using namespace std;
using namespace MoveGen;
using namespace Notation;
using namespace Polyglot;

void print_key (Key key)
{
    cout
        << hex << uppercase << setfill ('0')
        << "Posi key: " << setw (16) << key << "\n"
        << setfill (' ') << nouppercase << dec
        << endl;
}

namespace Parser {

    void parse ()
    {
        BitBoard::initialize ();
        Position::initialize ();

        /*
        //PGNFile pgn ("test.pgn", ios_base::in|ios_base::out);
        PGNFile pgn ("book.pgn", ios_base::in);
        //PGNFile pgn ("5lac.pgn", ios_base::in);
        u32 beg_game;
        u32 end_game;
        cout << pgn.game_count () << endl;
        system ("pause");
        ofstream of ("text.pgn");
        cout << "Begin Game ";
        cin >> beg_game;
        cout << "End Game";
        cin >> end_game;
        of << pgn.read_text (beg_game, end_game);
        //cout << pgn.read_text (beg_game, end_game) << endl;
        of.close ();
        */
        /*
        string text = "";
        
    "[Event \"approx\"]\n\
    [Site \"Europe\"]\n\
    [Date \"1625.??.??\"]\n\
    [Round \"?\"]\n\
    [White \"Greco, Gioacchino\"]\n\
    [Black \"Analysis\ Analyse\"]\n\
    [Result \"1-0\"]\n\
    [WhiteEngine \"komodo-9.2-64bit\"]\n\
    [BlackEngine \"stockfish_15091918_x64_modern\"]\n\
    [TimeControl \"0:0/0:0\"]\n\
    [ECO \"C54\"]\n\
    [WhiteElo \"2596\"]\n\
    [BlackElo \"2488\"]\n\
    [PlyCount \"37\"]\n\
    [EventDate \"1625.??.??\"]\n\
    [EventType \"game\"]\n\
    [EventRounds \"1\"]\n\
    [EventCountry \"ITA\"]\n\
    [Source \"ChessBase\"]\n\
    \n\
    1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. c3 Nf6 5. d4 exd4 6. cxd4 Bb4+ 7. Nc3 Nxe4 8. \n\
    O-O Nxc3 9. bxc3 Bxc3 10. Qb3 Bxa1 11. Bxf7+ Kf8 12. Bg5 Ne7 13. Ne5 Bxd4 14. \n\
    Bg6 d5 15. Qf3+ Bf5 16. Bxf5 Bxe5 17. Be6+ Bf6 18. Bxf6 Ke8 19. Bxg7 1-0 ffff";
    */
        //pgn.write_text (text);
        //text = pgn.read_text (1);
        //cout << text << "------------------------------------" << endl;
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
        Table table;
        table.load ("book.bin");
        table.filter ();
        table.sort ();
        table.save ("book1.bin");

        Book book;
        book.open ("book1.bin", ios_base::in);
        
        string move;
        StateStack states;
        Position pos(STARTUP_FEN);

        print_key (pos.posi_key ());

        states.push (StateInfo ());
        move = "e2e4";
        pos.do_move (move, states.top ());
        print_key (pos.posi_key ());

        states.push (StateInfo ());
        move = "e7e5";
        pos.do_move (move, states.top ());
        print_key (pos.posi_key ());
        
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
        
        auto book_move = book.probe_move (pos, false);
        cout << book_move << endl;
        cout << book.read_entries (pos);
        */


        PGN pgn;
        string pgn_fn = "book.pgn";
        pgn.open (pgn_fn);
        while (pgn.next_game ())
        {
            Position pos (pgn.fen.empty () ? STARTUP_FEN : pgn.fen);

            string move;
            StateStack states;
            while (pgn.next_move (move))
            {
                auto m = move_from_san (move, pos);
                if (m == MOVE_NONE)
                {
                    for (const auto &mm : MoveList<LEGAL> (pos))
                    {
                        cout << move_to_san (mm, pos) << " ";
                    }
                    break;
                }
                cout << m << " ";

                states.push (StateInfo ());
                pos.do_move (m, states.top (), pos.gives_check (m, CheckInfo (pos)));
            
            }

            cout << pgn.moves << endl;
        }
        pgn.close ();

        cout << pgn.games << endl;


    }

}