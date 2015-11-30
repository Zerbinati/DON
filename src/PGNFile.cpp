#include "PGNFile.h"


#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/stat.h>

using namespace std;

namespace {
    
    bool Error;
    FILE *LogFile = nullptr;

    void log_fatal (const char format[], ...);

#define FormatBufferSize 4096

#define CONSTRUCT_ARG_STRING(format, buf)                            \
{                                                                    \
    va_list arg_list;                                                \
    int done;                                                        \
    va_start(arg_list, format);                                      \
    done = vsnprintf(buf, sizeof (buf), format, arg_list);           \
    va_end(arg_list);                                                \
    buf[sizeof (buf)-1]='\0';                                        \
    if (done >= sizeof (buf) || done < 0) {                          \
    log_fatal("write_buffer overflow: file \"%s\", line %d\n",       \
    __FILE__, __LINE__);                                             \
    }                                                                \
} 

    const bool DispMove     = false;
    const bool DispToken    = false;
    const bool DispChar     = false;

    const int TAB_SIZE = 8;

    const int CHAR_EOF = 256;

    enum token_t
    {
        TOKEN_ERROR   = -1,
        TOKEN_EOF     = 256,
        TOKEN_SYMBOL  = 257,
        TOKEN_STRING  = 258,
        TOKEN_INTEGER = 259,
        TOKEN_NAG     = 260,
        TOKEN_RESULT  = 261
    };

    double now_time ()
    {
#ifndef _WIN32
        struct timeval tv[1];
        struct timezone tz[1];

        tz->tz_minuteswest = 0;
        tz->tz_dsttime = 0; // DST_NONE not declared in linux

        if (gettimeofday (tv, tz) == -1)
        {
            log_fatal ("now_time(): gettimeofday(): %s\n", strerror (errno));
        }

        return tv->tv_sec + tv->tv_usec * 1E-6;
#else
        struct _timeb timeptr;
        _ftime (&timeptr);
        return (timeptr.time + ((double) timeptr.millitm)/1000.0);
        //   return (double) GetTickCount() / 1000.0;  // we can do better here:-)
#endif
    }

    void log_msg (const char format[], ...)
    {
        char string[FormatBufferSize];

        assert(format != NULL);
        //  format
        CONSTRUCT_ARG_STRING (format, string);
        if (LogFile != NULL)
        {
            fprintf (LogFile, "%.3f %s", now_time (), string);
#ifdef _WIN32
            fflush (LogFile);
#endif
        }
    }

    void quit ()
    {
        log_msg ("POLYGLOT *** QUIT ***\n");
        //if (Init && !Engine->pipex->quit_pending)
        //{
        //    stop_search ();
        //    Engine->pipex->quit_pending = true;
        //    send_engine (Engine, "quit");
        //    close_engine (Engine);
        //}
        //my_sleep (200);
        log_msg ("POLYGLOT Calling exit\n");
        exit (EXIT_SUCCESS);
    }

    void log_fatal (const char format[], ...)
    {
        char string[FormatBufferSize];
        assert(format != NULL);
        // format
        CONSTRUCT_ARG_STRING (format, string);

        log_msg ("POLYGLOT %s", string);
        // This should be send_gui but this does not work.
        // Why?

        printf ("tellusererror POLYGLOT: %s", string);

        if (Error)
        { // recursive error
            log_msg ("POLYGLOT *** RECURSIVE ERROR ***\n");
            exit (EXIT_FAILURE);
            //abort();
        }
        else
        {
            Error = true;
            quit ();
        }
    }

    bool symbol_start (int c)
    {
        return strchr ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", c) != nullptr;
    }
    bool symbol_next (int c)
    {
        return strchr ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_+#=:-/", c) != nullptr;
    }
    
}

void pgn_t::read_char ()
{
    // char "stack"
    if (char_unread)
    {
        char_unread = false;
        return;
    }

    // consume the current character
    if (char_first)
    {
        char_first = false;
    }
    else
    {
        // update counters
        assert (char_hack != CHAR_EOF);
        if (false)
        {}
        else if (char_hack == '\n')
        {
            char_line++;
            char_column = 0;
        }
        else if (char_hack == '\t')
        {
            char_column += TAB_SIZE - (char_column % TAB_SIZE);
        }
        else
        {
            char_column++;
        }
    }

    // read a new character
    char_hack = fgetc (file);

    if (char_hack == EOF)
    {
        if (ferror (file))
        {
            log_fatal ("read_char(): fgetc(): %s\n", strerror (errno));
        }
        char_hack = CHAR_EOF;
    }

    if (DispChar)
    {
        printf ("< L%d C%d '%c' (%02X)\n", char_line, char_column, char_hack, char_hack);
    }
}
void pgn_t::unread_char ()
{
    assert (!char_unread);
    assert (!char_first);
    char_unread = true;
}

void pgn_t::read_skip_blanks ()
{
    while (true)
    {
        read_char ();
        if (false)
        {
        }
        else if (char_hack == CHAR_EOF)
        {
            break;
        }
        else if (isspace (char_hack))
        {
            // skip white space
        }
        else if (char_hack == ';')
        {
            // skip comment to EOL
            do
            {
                read_char ();
                if (char_hack == CHAR_EOF)
                {
                    log_fatal ("read_skip_blanks(): EOF in comment at line %d, column %d, game %d\n", char_line, char_column, games);
                }
            } while (char_hack != '\n');
        }
        else if (char_hack == '%' && char_column == 0)
        {
            // skip comment to EOL
            do
            {
                read_char ();
                if (char_hack == CHAR_EOF)
                {
                    log_fatal ("read_skip_blanks(): EOF in comment at line %d, column %d, game %d\n", char_line, char_column, games);
                }
            } while (char_hack != '\n');
        }
        else if (char_hack == '{')
        {
            // skip comment to next '}'
            do
            {
                read_char ();

                if (char_hack == CHAR_EOF)
                {
                    log_fatal ("read_skip_blanks(): EOF in comment at line %d, column %d, game %d\n", char_line, char_column, games);
                }
            } while (char_hack != '}');
        }
        else
        { // not a white space
            break;
        }
    }
}

void pgn_t::read_token ()
{

    // token "stack"
    if (token_unread)
    {
        token_unread = false;
        return;
    }

    // consume the current token
    if (token_first)
    {
        token_first = false;
    }
    else
    {
        assert (token_type != TOKEN_ERROR);
        assert (token_type != TOKEN_EOF);
    }

    // read a new token
    read_tok ();

    if (token_type == TOKEN_ERROR)
    {
        log_fatal ("read_token(): lexical error at line %d, column %d, game %d\n", char_line, char_column, games);
    }
    if (DispToken)
    {
        printf ("< L%d C%d \"%s\" (%03X)\n", token_line, token_column, token.c_str (), token_type);
    }
}
void pgn_t::unread_token ()
{
    assert (!token_unread);
    assert (!token_first);
    token_unread = true;
}

void pgn_t::read_tok ()
{
    // skip white-space characters
    read_skip_blanks ();

    // init
    token = "";
    token_type = TOKEN_ERROR;
    token_line = char_line;
    token_column = char_column;

    // determine token type
    if (false)
    {
    }
    else if (char_hack == CHAR_EOF)
    {
        token_type = TOKEN_EOF;
    }
    else if (strchr (".[]()<>", char_hack) != nullptr)
    {
        // single-character token
        token_type = char_hack;
        token = (char)char_hack;
    }
    else if (char_hack == '*')
    {
        token_type = TOKEN_RESULT;
        token = (char)char_hack;
    }
    else if (char_hack == '!')
    {
        read_char ();
        if (false)
        {
        }
        else if (char_hack == '!')
        { // "!!"
            token_type = TOKEN_NAG;
            token = "3";
        }
        else if (char_hack == '?')
        { // "!?"
            token_type = TOKEN_NAG;
            token = "5";
        }
        else
        { // "!"
            unread_char ();
            token_type = TOKEN_NAG;
            token = "1";
        }
    }
    else if (char_hack == '?')
    {
        read_char ();
        if (false)
        {
        }
        else if (char_hack == '?')
        { // "??"
            token_type = TOKEN_NAG;
            token = "4";
        }
        else if (char_hack == '!')
        { // "?!"
            token_type = TOKEN_NAG;
            token = "6";
        }
        else
        { // "?"
            unread_char ();
            token_type = TOKEN_NAG;
            token = "2";
        }
    }
    else if (symbol_start (char_hack))
    {
        // symbol, integer, or result
        token_type = TOKEN_INTEGER;
        token = "";
        do
        {
            //if (token.length () >= STRING_SIZE-1)
            //{
            //    log_fatal ("read_tok(): symbol too long at line %d, column %d, game %d\n", char_line, char_column, games);
            //}

            if (!isdigit (char_hack)) token_type = TOKEN_SYMBOL;

            token += (char)char_hack;

            read_char ();
            if (char_hack == CHAR_EOF) break;
        } while (symbol_next (char_hack));

        unread_char ();

        assert (0 < token.length ()
            //&& token.length () < STRING_SIZE
            );
        //token += '\0';

        if (   token == "1-0"
            || token == "0-1"
            || token == "1/2-1/2"
            )
        {
            token_type = TOKEN_RESULT;
        }
    }
    else if (char_hack == '"')
    {
        // string
        token_type = TOKEN_STRING;
        token = "";

        while (true)
        {
            read_char ();
            if (char_hack == CHAR_EOF)
            {
                log_fatal ("read_tok(): EOF in string at line %d, column %d, game %d\n", char_line, char_column, games);
            }

            if (char_hack == '"') break;

            if (char_hack == '\\')
            {
                read_char ();
                if (char_hack == CHAR_EOF)
                {
                    log_fatal ("read_tok(): EOF in string at line %d, column %d, game %d\n", char_line, char_column, games);
                }

                if (char_hack != '"' && char_hack != '\\')
                {
                    // bad escape, ignore
                    //if (token.length () >= STRING_SIZE-1)
                    //{
                    //    log_fatal ("read_tok(): string too long at line %d, column %d, game %d\n", char_line, char_column, games);
                    //}
                    token += '\\';
                }
            }

            //if (token.length () >= STRING_SIZE-1)
            //{
            //    log_fatal ("read_tok(): string too long at line %d, column %d, game %d\n", char_line, char_column, games);
            //}

            token += (char)char_hack;
        }

        assert (0 <= token.length ()
            //&& token.length () < STRING_SIZE
            );
        //token += '\0';
    }
    else if (char_hack == '$')
    {
        // NAG
        token_type = TOKEN_NAG;
        token = "";

        while (true)
        {
            read_char ();

            if (!isdigit (char_hack)) break;

            if (token.length () >= 3)
            {
                log_fatal ("read_tok(): NAG too long at line %d, column %d, game %d\n", char_line, char_column, games);
            }
            token += (char)char_hack;
        }

        unread_char ();

        if (token.length () == 0)
        {
            log_fatal ("read_tok(): malformed NAG at line %d, column %d, game %d\n", char_line, char_column, games);
        }

        assert (0 < token.length () && token.length () <= 3);
        //token += '\0';
    }
    else
    {
        // unknown token
        log_fatal ("lexical error at line %d, column %d, game %d\n", char_line, char_column, games);
    }
}

void pgn_t::open (const string &pgn_fn)
{
    file = fopen (pgn_fn.c_str (), "r");
    if (file == nullptr)
    {
        log_fatal ("open_pgn(): can't open file \"%s\": %s\n", pgn_fn.c_str (), strerror (errno));
    }

    char_hack      = CHAR_EOF; // DEBUG
    char_line      = 1;
    char_column    = 0;
    char_unread    = false;
    char_first     = true;

    token          = "?";
    token_type     = TOKEN_ERROR; // DEBUG
    token_line     = -1; // DEBUG
    token_column   = -1; // DEBUG
    token_unread   = false;
    token_first    = true;

    games          = 0;
    result         = "?"; // DEBUG
    fen            = "?"; // DEBUG
    white_elo      = "?"; // DEBUG
    black_elo      = "?"; // DEBUG

    moves          = 0;
    move_line      = -1; // DEBUG
    move_column    = -1; // DEBUG
}
void pgn_t::close ()
{
    fclose (file);
    file = nullptr;
}

bool pgn_t::next_game ()
{
    string name;
    string value;

    // init
    result    = "*";
    fen       = "";
    white_elo = "0";
    black_elo = "0";

    while (true)
    {
        read_token ();

        if (token_type != '[') break;

        // tag
        read_token ();
        if (token_type != TOKEN_SYMBOL)
        {
            log_fatal ("next_game_pgn(): malformed tag at line %d, column %d, game %d\n", token_line, token_column, games);
        }
        name = token;

        read_token ();
        if (token_type != TOKEN_STRING)
        {
            log_fatal ("next_game_pgn(): malformed tag at line %d, column %d, game %d\n", token_line, token_column, games);
        }
        value = token;

        read_token ();
        if (token_type != ']')
        {
            log_fatal ("next_game_pgn(): malformed tag at line %d, column %d, game %d\n", token_line, token_column, games);
        }

        // special tag?
        if (false)
        {}
        else if (name == "Result")
        {
            result = value;
        }
        else if (name == "FEN")
        {
            fen = value;
        }
        else if (name == "WhiteElo")
        {
            white_elo = value;
        }
        else if (name == "BlackElo")
        {
            black_elo = value;
        }
    }

    if (token_type == TOKEN_EOF) return false;
    
    unread_token ();
    ++games;
    moves = 0;

    return true;
}
bool pgn_t::next_move (string &move)
{
    // init
    move_line      = -1;  // DEBUG
    move_column    = -1;  // DEBUG
    
    move = "";
    int depth = 0;

    while (true)
    {
        read_token ();
        if (false)
        {}
        else if (token_type == '(')
        {
            // open RAV
            ++depth;
        }
        else if (token_type == ')')
        {
            // close RAV
            if (depth == 0)
            {
                log_fatal ("next_move_pgn(): malformed variation at line %d, column %d, game %d\n", token_line, token_column, games);
            }
            --depth;
            assert(depth >= 0);
        }
        else if (token_type == TOKEN_RESULT)
        {
            // game finished
            if (depth > 0)
            {
                log_fatal ("next_move_pgn(): malformed variation at line %d, column %d, game %d\n", token_line, token_column, games);
            }
            return false;
        }
        else
        {
            // skip optional move number
            if (token_type == TOKEN_INTEGER)
            {
                do
                {
                    read_token ();
                }
                while (token_type == '.');
            }

            // move must be a symbol
            if (token_type != TOKEN_SYMBOL)
            {
                log_fatal ("next_move_pgn(): malformed move at line %d, column %d, game %d\n", token_line, token_column, games);
            }

            // store move for later use
            if (depth == 0)
            {
                move = token;
                ++moves;
                move_line = token_line;
                move_column = token_column;
            }

            // skip optional NAGs
            do
            {
                read_token ();
            }
            while (token_type == TOKEN_NAG);
            unread_token ();

            // return move
            if (depth == 0)
            {
                if (DispMove) printf ("move=\"%s\"\n", move.c_str ());
                return true;
            }
        }
    }
    assert(false);
    return false;
}
