#include "PGN.h"

#include <iostream>

using namespace std;

PGN::PGN ()
    : fstream()
    , _fn_pgn ("")
    , _mode (ios_base::openmode(0))
    , _size_pgn (0)
{}

PGN::PGN (const string &fn_pgn, ios_base::openmode mode)
    : fstream (fn_pgn, mode | ios_base::binary)
    , _fn_pgn (fn_pgn)
    , _mode (mode)
    , _size_pgn (0)
{
    clear (); // Reset any error flag to allow retry open()
    _build_indexes ();
}

PGN::~PGN ()
{
    close ();
}

bool PGN::open (const string &fn_pgn, ios_base::openmode mode)
{
    close ();
    fstream::open (fn_pgn, mode | ios_base::binary);
    clear (); // Reset any error flag to allow retry open()
    _fn_pgn = fn_pgn;
    _mode   = mode;
    _build_indexes ();
    return is_open ();
}

void PGN::close () { if (is_open ()) { fstream::close (); _reset (); } }

void PGN::_reset ()
{
    //_fn_pgn.clear ();
    //_mode       = 0;
    _size_pgn   = 0;
    _indexes_game.clear ();
}

void PGN::_build_indexes ()
{
    if (is_open () && (_mode & ios_base::in) && good ())
    {
        if (0 < game_count ())
        {
            _reset ();
        }

        size ();


        u64 pos = 0;
        State state = S_NEW;

        // Clear the char stack
        while (!_stk_char.empty ()) _stk_char.pop ();

        seekg (0L);
        do
        {
            const i32 MaxBuff = 32*1024;
            //std::string str;
            //std::getline (*this, str);

            std::string str (MaxBuff, '\0');
            read (&str[0], MaxBuff);
                
            _scan_index (str, pos, state);
        }
        while (!eof () && good () && S_ERR != state);
        clear ();

        if (S_ERR == state)
        {
            // error at offset
        }
        if (   S_MOV_NEW == state
            || S_MOV_LST == state
            )
        {
            _add_index (pos);
        }

#undef MAX_SIZE

        
    }
}

#undef SKIP_WHITESPACE
#undef CHECK_INCOMPLETE

#define SKIP_WHITESPACE() do { if (length == offset) goto finish; c = str[offset++]; } while (isspace (c) && c != '\n')
#define CHECK_INCOMPLETE() do { if ('\0' == c) { cerr << "ERROR: incomplete game"; state = S_ERR; goto finish; } } while (false)

void PGN::_scan_index (const string &str, u64 &pos, State &state)
{
    size_t offset = 0;
    auto length = str.length ();

    while (offset < length)
    {
        u08 c;

        SKIP_WHITESPACE ();

        if ('\0' == c)
        {
            if (!_stk_char.empty ())
            {
                cerr << "ERROR: missing closing character of: " << _stk_char.top () << "at location: " << (pos + offset);
                state = S_ERR;
                goto finish;
            }
            else
            {
                cerr << ("****SUCCESS****");
            }
            break;
        }

        switch (state)
        {
        case S_NEW:
            switch (c)
            {
            case '\n':
                break;
            case  '[':
                state = S_TAG_BEG;
                break;
            case  '0': case  '1': case  '2': case  '3': case  '4':
            case  '5': case  '6': case  '7': case  '8': case  '9':
            case  '*':
                state = S_MOV_LST;
                break;
            case  '{':
                state = S_MOV_COM;
                break;
            case  ';':
                while ('\n' != c)
                {
                    SKIP_WHITESPACE ();
                    CHECK_INCOMPLETE ();
                }
                state = S_MOV_LST;
                break;
            case  '%':
                while ('\n' != c)
                {
                    SKIP_WHITESPACE ();
                    CHECK_INCOMPLETE ();
                }
                state = S_MOV_LST;
                break;
            default:
                cerr << ("ERROR: invalid character");
                state = S_ERR;
                goto finish;
                break;
            }
            break;

        case S_TAG_NEW:
            switch (c)
            {
            case '\n':
                state = S_NEW;
                break;
            case  '[':
                state = S_TAG_BEG;
                break;
            }
            break;

        case S_TAG_BEG:
            while (']' != c)
            {
                SKIP_WHITESPACE ();
                CHECK_INCOMPLETE ();
            }
            state = S_TAG_END;
            break;

        case S_TAG_END:
            switch (c)
            {
            case '\n':
                state = S_TAG_NEW;
                break;
            case  '[':
                state = S_TAG_BEG;
                break;
            }
            break;

        case S_MOV_NEW:
            switch (c)
            {
            case '\n':
                state = S_NEW;
                // New game
                _add_index (pos + offset);
                break;
            case  '(':
                state = S_VAR_LST;
                _stk_char.push (c);
                break;
            case  '{':
                state = S_MOV_COM;
                break;
            case  ';':
                while ('\n' != c)
                {
                    SKIP_WHITESPACE ();
                    CHECK_INCOMPLETE ();
                }
                break;
            case  '%':
                while ('\n' != c)
                {
                    SKIP_WHITESPACE ();
                    CHECK_INCOMPLETE ();
                }
                break;
            default:
                state = S_MOV_LST;
                break;
            }
            break;

        case S_MOV_LST:
            switch (c)
            {
            case '\n':
                state = S_MOV_NEW;
                break;
            case  '(':
                state = S_VAR_LST;
                _stk_char.push (c);
                break;
            case  '{':
                state = S_MOV_COM;
                break;
            case  ';':
                while ('\n' != c)
                {
                    SKIP_WHITESPACE ();
                    CHECK_INCOMPLETE ();
                }
                state = S_MOV_NEW;
                break;
            default: break;
            }
            break;

        case S_MOV_COM:
            while ('}' != c)
            {
                SKIP_WHITESPACE ();
                CHECK_INCOMPLETE ();
            }
            state = S_MOV_LST;
            break;

        case S_VAR_LST:
            switch (c)
            {
            case  '(':
                _stk_char.push (c);
                break;
            case  ')':
                if (_stk_char.empty () || '(' != _stk_char.top ())
                {
                    cerr << ("ERROR: missing opening of variation");
                    state = S_ERR;
                    goto finish;
                }
                else
                {
                    _stk_char.pop ();
                }
                break;
            case  '{':
                state = S_VAR_COM;
                _stk_char.push (c);
                break;
            default:
                if (_stk_char.empty ())
                {
                    state = S_MOV_LST;
                }
                break;
            }
            break;

        case S_VAR_COM:
            switch (c)
            {
            case  '}':
                if (_stk_char.empty () || '{' != _stk_char.top ())
                {
                    cerr << ("ERROR:: missing opening of variation comment");
                    state = S_ERR;
                    goto finish;
                }
                else
                {
                    state = S_VAR_LST;
                    _stk_char.pop ();
                }
                break;
            }
            break;

        case S_ERR:
            goto finish;
            break;

        default:
            break;
        }
    }

finish:
    pos += offset;
}
#undef SKIP_WHITESPACE
#undef CHECK_INCOMPLETE

void PGN::_add_index (u64 pos)
{
    //u64 g_count = game_count ();
    //if (0 < g_count)
    //{
    //    if (pos <= _indexes_game[g_count - 1]) return;
    //}

    _indexes_game.push_back (pos);
}

/*
// Remove (first occurence of) sub
char* remove_substrs (  char *str, const   char *sub)
{
    const size_t length = strlen (sub);
    char *p = strstr (str, sub);
    while (p != NULL)
    {
        strcpy (p, p + length);
        //memmove (p, p + length, strlen (p + length) + 1);
        p = strstr (p, sub);
    }
    return str;
}
*/
void  remove_substrs (string &str, const string &sub)
{
    auto len = sub.length ();
    for (auto i = str.find (sub);
              i != string::npos;
              i = str.find (sub, i)
        )
    {
        str.erase (i, len);
    }
}

// Read the text index (1...n)
string PGN::read_text (u64 index)
{
    if (1 <= index && index <= game_count ())
    {
        if (is_open () && good ())
        {
            auto beg_pos = 1 == index ? 0 : _indexes_game[index - 2];
            auto end_pos = _indexes_game[index - 1];

            auto size = end_pos - beg_pos;
            
            /*
            // using char *
            auto buf = new char[(size + 1)];
            if (buf != NULL)
            {
                seekg (beg_pos);
                read (buf, size);
                buf[size] = '\0';
            
                remove_substrs (buf, "\r");
            
                string text = buf;
                delete[] buf; buf = NULL;
            
                //remove_substrs (text, "\r");
                return text;
            }
            */

            // using string
            string text (size, '\0');
            seekg (beg_pos, ios_base::beg);
            read (&text[0], size);
            remove_substrs (text, "\r");

            return text;
        }
    }
    return "";
}
// Read the text beg_index (1...n), end_index (1...n)
string PGN::read_text (u64 beg_index, u64 end_index)
{
    if (   beg_index <= end_index
        && game_count () >= beg_index && end_index <= game_count ()
       )
    {
        if (is_open () && good ())
        {
            auto beg_pos = 1 == beg_index ? 0 : _indexes_game[beg_index - 2];
            auto end_pos = _indexes_game[end_index - 1];

            auto size = end_pos - beg_pos;

            /*
            // using char *
            auto *buf = new char[(size + 1)];
            if (buf)
            {
                seekg (beg_pos);
                read (buf, size);
                buf[size] = '\0';
            
                remove_substrs (buf, "\r");
            
                string text = buf;
                delete[] buf; buf = NULL;
            
                //remove_substrs (text, "\r");
                return text;
            }
            */

            // using string
            string text (size, '\0');
            seekg (beg_pos, ios_base::beg);
            read (&text[0], size);
            remove_substrs (text, "\r");

            return text;
        }
    }
    return "";
}
// Write the text and return index of the text
u64    PGN::write_text (const string &text)
{
    if (is_open () && good ())
    {
        *this << text;
    }
    return 0;
}

// Read the game from index (1...n)
Game   PGN::read_game (u64 index)
{
    return Game (read_text (index));
}
// Write the game and return index of the game
u64    PGN::write_game (const Game &game)
{
    // TODO::
    string pgn = game.pgn ();
    *this << pgn;

    return 0;
}
