#include "PGN.h"

#include <iostream>

using namespace std;

PGN::PGN (const string &pgn_fn, ios_base::openmode mode)
    : fstream (pgn_fn, mode | ios_base::binary)
    , _pgn_fn (pgn_fn)
    , _mode (mode)
    , _size (0)
{
    clear (); // Reset any error flag to allow retry open()
    _build_indexes ();
}

PGN::~PGN ()
{
    close ();
}

bool PGN::open (const string &pgn_fn, ios_base::openmode mode)
{
    close ();
    fstream::open (pgn_fn, mode | ios_base::binary);
    clear (); // Reset any error flag to allow retry open()
    _pgn_fn = pgn_fn;
    _mode   = mode;
    _build_indexes ();
    return is_open ();
}

void PGN::close () { if (is_open ()) { fstream::close (); _reset (); } }

void PGN::_reset ()
{
    //_pgn_fn.clear ();
    //_mode       = 0;
    _size   = 0;
    _game_indexes.clear ();
}

void PGN::_build_indexes ()
{
    if (is_open () && good ())
    {
        if (0 < game_count ())
        {
            _reset ();
        }

        size ();

        u64 pos = 0;
        State state = S_NEW;
        
        // Clear the char stack
        while (!_char_stack.empty ())
        {
            _char_stack.pop ();
        }
        //std::stack<char> empty;
        //_char_stack.swap (empty);

        seekg (0L);
        do
        {
            const i32 MAX_BUFF = 32*1024;
            //std::string str;
            //std::getline (*this, str);

            std::string str (MAX_BUFF, '\0');
            read (&str[0], MAX_BUFF);
                
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
            if (!_char_stack.empty ())
            {
                cerr << "ERROR: missing closing character of: " << _char_stack.top () << "at location: " << (pos + offset);
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
                _char_stack.push (c);
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
                _char_stack.push (c);
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
                _char_stack.push (c);
                break;
            case  ')':
                if (_char_stack.empty () || '(' != _char_stack.top ())
                {
                    cerr << ("ERROR: missing opening of variation");
                    state = S_ERR;
                    goto finish;
                }
                else
                {
                    _char_stack.pop ();
                }
                break;
            case  '{':
                state = S_VAR_COM;
                _char_stack.push (c);
                break;
            default:
                if (_char_stack.empty ())
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
                if (_char_stack.empty () || '{' != _char_stack.top ())
                {
                    cerr << ("ERROR:: missing opening of variation comment");
                    state = S_ERR;
                    goto finish;
                }
                else
                {
                    state = S_VAR_LST;
                    _char_stack.pop ();
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
    //    if (pos <= _game_indexes[g_count - 1]) return;
    //}

    _game_indexes.push_back (pos);
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
            auto beg_pos = 1 == index ? 0 : _game_indexes[index - 2];
            auto end_pos = _game_indexes[index - 1];

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
            auto beg_pos = 1 == beg_index ? 0 : _game_indexes[beg_index - 2];
            auto end_pos = _game_indexes[end_index - 1];

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
