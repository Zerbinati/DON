#ifndef _NOTATION_H_INC_
#define _NOTATION_H_INC_

#include <cstring>
#include <iostream>

#include "Type.h"
#include "Thread.h"

class Position;

const std::string PieceChar ("PNBRQK  pnbrqk");
const std::string ColorChar ("wb-");

namespace Notation {

    inline char to_char (File f, bool lower = true)
    {
        return char((lower ? 'a' : 'A') + i08(f) - i08(F_A));
    }

    inline char to_char (Rank r)
    {
        return char('1' + i08(r) - i08(R_1));
    }

    inline std::string to_string (Square s)
    {
        return std::string{ to_char (_file (s)), to_char (_rank (s)) };
    }
    // to_string() converts a value to a string suitable
    // for use with the UCI protocol specifications:
    //
    // cp   <x>   The score x from the engine's point of view in centipawns.
    // mate <y>   Mate in y moves, not plies.
    //            If the engine is getting mated use negative values for y.
    inline std::string to_string (Value v)
    {
        return abs (v) < +VALUE_MATE - i32(MaxPlies) ?
            "cp " + std::to_string (i32(value_to_cp (v)*100)) :
            "mate " + std::to_string (i32(v > VALUE_ZERO ? +(VALUE_MATE - v + 1) : -(VALUE_MATE + v))/2);
    }

    extern std::string move_to_can (Move m, bool c960 = false);
    extern std::string move_to_san (Move m, Position &pos);
    //extern std::string move_to_lan (Move m, Position &pos);

    extern Move move_from_can (const std::string &can, const Position &pos);
    extern Move move_from_san (const std::string &san,       Position &pos);
    //extern Move move_from_lan (const std::string &lan,       Position &pos);

    extern std::string pretty_pv_info ();
}

template<class CharT, class Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, Move m)
{
    os << Notation::move_to_can (m);
    return os;
}

template<class CharT, class Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, Color c)
{
    os << ColorChar[c];
    return os;
}

template<class CharT, class Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, File f)
{
    os << Notation::to_char (f);
    return os;
}

template<class CharT, class Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, Rank r)
{
    os << Notation::to_char (r);
    return os;
}

template<class CharT, class Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, Square s)
{
    os << Notation::to_string (s);
    return os;
}

template<class CharT, class Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, Piece p)
{
    os << PieceChar[p];
    return os;
}

//inline std::string to_string (CastleRight cr)
//{
//    std::string scr;
//    if ((cr & CR_ANY) != CR_NONE)
//    {
//        if ((cr & CR_WHITE) != CR_NONE)
//        {
//            scr += "W:";
//            if ((cr & CR_WKING) != CR_NONE) scr += " OO";
//            if ((cr & CR_WQUEN) != CR_NONE) scr += " OOO";
//            scr += " - ";
//        }
//        if ((cr & CR_BLACK) != CR_NONE)
//        {
//            scr += "B:";
//            if ((cr & CR_BKING) != CR_NONE) scr += " OO";
//            if ((cr & CR_BQUEN) != CR_NONE) scr += " OOO";
//        }
//    }
//    else
//    {
//        scr = "-";
//    }
//    return scr;
//}
//
//template<class CharT, class Traits>
//inline std::basic_ostream<CharT, Traits>&
//operator<< (std::basic_ostream<CharT, Traits> &os, CastleRight cr)
//{
//    os << to_string (cr);
//    return os;
//}

//template<class CharT, class Traits>
//inline std::basic_ostream<CharT, Traits>&
//    operator<< (std::basic_ostream<CharT, Traits> &os, const SquareVector &squares)
//{
//    std::for_each (squares.begin (), squares.end (), [&os] (Square s) { os << s << std::endl; });
//    return os;
//}

//template<class CharT, class Traits>
//inline std::basic_ostream<CharT, Traits>&
//operator<< (std::basic_ostream<CharT, Traits> &os, const MoveVector &moves)
//{
//    std::for_each (moves.begin (), moves.end (), [&os](Move m) { os << m << std::endl; });
//    return os;
//}

#endif // _NOTATION_H_INC_
