#include "PGNFile.h"


#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/stat.h>

namespace {
    
    bool Error;
    FILE *LogFile = NULL;

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

        assert (format != NULL);
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
        assert (format != NULL);
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

    // ------------------------------------
    void pgn_token_read (pgn_t *pgn);
    void pgn_token_unread (pgn_t *pgn);

    void pgn_read_token (pgn_t *pgn);

    bool is_symbol_start (int c);
    bool is_symbol_next (int c);

    void pgn_skip_blanks (pgn_t *pgn);

    void pgn_char_read (pgn_t *pgn);
    void pgn_char_unread (pgn_t *pgn);
    // ------------------------------------

    bool is_equal (const char string_1[], const char string_2[])
    {
        assert (string_1 != NULL);
        assert (string_2 != NULL);
        return strcmp (string_1, string_2) == 0;
    }

    void pgn_token_read (pgn_t *pgn)
    {
        assert (pgn != NULL);

        // token "stack"
        if (pgn->token_unread)
        {
            pgn->token_unread = false;
            return;
        }

        // consume the current token
        if (pgn->token_first)
        {
            pgn->token_first = false;
        }
        else
        {
            assert (pgn->token_type != TOKEN_ERROR);
            assert (pgn->token_type != TOKEN_EOF);
        }

        // read a new token
        pgn_read_token (pgn);
        if (pgn->token_type == TOKEN_ERROR)
        {
            log_fatal ("pgn_token_read(): lexical error at line %d, column %d, game %d\n", pgn->char_line, pgn->char_column, pgn->game_nb);
        }
        if (DispToken)
        {
            printf ("< L%d C%d \"%s\" (%03X)\n", pgn->token_line, pgn->token_column, pgn->token_string, pgn->token_type);
        }
    }

    void pgn_token_unread (pgn_t *pgn)
    {
        assert (pgn != NULL);
        assert (!pgn->token_unread);
        assert (!pgn->token_first);
        pgn->token_unread = true;
    }

    void pgn_read_token (pgn_t *pgn)
    {
        assert (pgn != NULL);

        // skip white-space characters
        pgn_skip_blanks (pgn);

        // init
        pgn->token_type = TOKEN_ERROR;
        strcpy (pgn->token_string, "");
        pgn->token_length = 0;
        pgn->token_line = pgn->char_line;
        pgn->token_column = pgn->char_column;

        // determine token type
        if (false)
        {
        }
        else if (pgn->char_hack == CHAR_EOF)
        {
            pgn->token_type = TOKEN_EOF;
        }
        else if (strchr (".[]()<>", pgn->char_hack) != NULL)
        {
            // single-character token
            pgn->token_type = pgn->char_hack;
            sprintf (pgn->token_string, "%c", pgn->char_hack);
            pgn->token_length = 1;
        }
        else if (pgn->char_hack == '*')
        {
            pgn->token_type = TOKEN_RESULT;
            sprintf (pgn->token_string, "%c", pgn->char_hack);
            pgn->token_length = 1;
        }
        else if (pgn->char_hack == '!')
        {
            pgn_char_read (pgn);
            if (false)
            {
            }
            else if (pgn->char_hack == '!')
            { // "!!"
                pgn->token_type = TOKEN_NAG;
                strcpy (pgn->token_string, "3");
                pgn->token_length = 1;
            }
            else if (pgn->char_hack == '?')
            { // "!?"
                pgn->token_type = TOKEN_NAG;
                strcpy (pgn->token_string, "5");
                pgn->token_length = 1;
            }
            else
            { // "!"
                pgn_char_unread (pgn);
                pgn->token_type = TOKEN_NAG;
                strcpy (pgn->token_string, "1");
                pgn->token_length = 1;
            }
        }
        else if (pgn->char_hack == '?')
        {
            pgn_char_read (pgn);

            if (false)
            {
            }
            else if (pgn->char_hack == '?')
            { // "??"
                pgn->token_type = TOKEN_NAG;
                strcpy (pgn->token_string, "4");
                pgn->token_length = 1;
            }
            else if (pgn->char_hack == '!')
            { // "?!"
                pgn->token_type = TOKEN_NAG;
                strcpy (pgn->token_string, "6");
                pgn->token_length = 1;
            }
            else
            { // "?"
                pgn_char_unread (pgn);
                pgn->token_type = TOKEN_NAG;
                strcpy (pgn->token_string, "2");
                pgn->token_length = 1;
            }
        }
        else if (is_symbol_start (pgn->char_hack))
        {
            // symbol, integer, or result
            pgn->token_type = TOKEN_INTEGER;
            pgn->token_length = 0;
            do
            {
                if (pgn->token_length >= STRING_SIZE-1)
                {
                    log_fatal ("pgn_read_token(): symbol too long at line %d, column %d, game %d\n",
                               pgn->char_line, pgn->char_column, pgn->game_nb);
                }

                if (!isdigit (pgn->char_hack)) pgn->token_type = TOKEN_SYMBOL;

                pgn->token_string[pgn->token_length++] = pgn->char_hack;

                pgn_char_read (pgn);
                if (pgn->char_hack == CHAR_EOF) break;
            }
            while (is_symbol_next (pgn->char_hack));

            pgn_char_unread (pgn);

            assert (pgn->token_length > 0 && pgn->token_length < STRING_SIZE);
            pgn->token_string[pgn->token_length] = '\0';

            if (   is_equal (pgn->token_string, "1-0")
                || is_equal (pgn->token_string, "0-1")
                || is_equal (pgn->token_string, "1/2-1/2")
               )
            {
                pgn->token_type = TOKEN_RESULT;
            }
        }
        else if (pgn->char_hack == '"')
        {
            // string
            pgn->token_type = TOKEN_STRING;
            pgn->token_length = 0;

            while (true)
            {
                pgn_char_read (pgn);
                if (pgn->char_hack == CHAR_EOF)
                {
                    log_fatal ("pgn_read_token(): EOF in string at line %d, column %d, game %d\n",
                               pgn->char_line, pgn->char_column, pgn->game_nb);
                }

                if (pgn->char_hack == '"') break;

                if (pgn->char_hack == '\\')
                {
                    pgn_char_read (pgn);
                    if (pgn->char_hack == CHAR_EOF)
                    {
                        log_fatal ("pgn_read_token(): EOF in string at line %d, column %d, game %d\n",
                                   pgn->char_line, pgn->char_column, pgn->game_nb);
                    }

                    if (pgn->char_hack != '"' && pgn->char_hack != '\\')
                    {
                        // bad escape, ignore
                        if (pgn->token_length >= STRING_SIZE-1)
                        {
                            log_fatal ("pgn_read_token(): string too long at line %d, column %d, game %d\n",
                                       pgn->char_line, pgn->char_column, pgn->game_nb);
                        }
                        pgn->token_string[pgn->token_length++] = '\\';
                    }
                }

                if (pgn->token_length >= STRING_SIZE-1)
                {
                    log_fatal ("pgn_read_token(): string too long at line %d, column %d, game %d\n",
                               pgn->char_line, pgn->char_column, pgn->game_nb);
                }

                pgn->token_string[pgn->token_length++] = pgn->char_hack;
            }

            assert (pgn->token_length >= 0 && pgn->token_length < STRING_SIZE);
            pgn->token_string[pgn->token_length] = '\0';

        }
        else if (pgn->char_hack == '$')
        {
            // NAG
            pgn->token_type = TOKEN_NAG;
            pgn->token_length = 0;

            while (true)
            {
                pgn_char_read (pgn);

                if (!isdigit (pgn->char_hack)) break;

                if (pgn->token_length >= 3)
                {
                    log_fatal ("pgn_read_token(): NAG too long at line %d, column %d, game %d\n",
                               pgn->char_line, pgn->char_column, pgn->game_nb);
                }
                pgn->token_string[pgn->token_length++] = pgn->char_hack;
            }

            pgn_char_unread (pgn);

            if (pgn->token_length == 0)
            {
                log_fatal ("pgn_read_token(): malformed NAG at line %d, column %d, game %d\n",
                           pgn->char_line, pgn->char_column, pgn->game_nb);
            }
            assert (pgn->token_length>0&&pgn->token_length<=3);
            pgn->token_string[pgn->token_length] = '\0';
        }
        else
        {
            // unknown token
            log_fatal ("lexical error at line %d, column %d, game %d\n", pgn->char_line, pgn->char_column, pgn->game_nb);
        }
    }

    void pgn_skip_blanks (pgn_t *pgn)
    {
        assert (pgn != NULL);
        while (true)
        {
            pgn_char_read (pgn);
            if (false)
            {
            }
            else if (pgn->char_hack == CHAR_EOF)
            {
                break;
            }
            else if (isspace (pgn->char_hack))
            {
                // skip white space
            }
            else if (pgn->char_hack == ';')
            {
                // skip comment to EOL
                do
                {
                    pgn_char_read (pgn);
                    if (pgn->char_hack == CHAR_EOF)
                    {
                        log_fatal ("pgn_skip_blanks(): EOF in comment at line %d, column %d, game %d\n",
                                   pgn->char_line, pgn->char_column, pgn->game_nb);
                    }
                }
                while (pgn->char_hack != '\n');
            }
            else if (pgn->char_hack == '%' && pgn->char_column == 0)
            {
                // skip comment to EOL
                do
                {
                    pgn_char_read (pgn);
                    if (pgn->char_hack == CHAR_EOF)
                    {
                        log_fatal ("pgn_skip_blanks(): EOF in comment at line %d, column %d, game %d\n",
                                   pgn->char_line, pgn->char_column, pgn->game_nb);
                    }
                }
                while (pgn->char_hack != '\n');
            }
            else if (pgn->char_hack == '{')
            {
                // skip comment to next '}'
                do
                {
                    pgn_char_read (pgn);

                    if (pgn->char_hack == CHAR_EOF)
                    {
                        log_fatal ("pgn_skip_blanks(): EOF in comment at line %d, column %d, game %d\n",
                                   pgn->char_line, pgn->char_column, pgn->game_nb);
                    }
                }
                while (pgn->char_hack != '}');
            }
            else
            { // not a white space
                break;
            }
        }
    }

    bool is_symbol_start (int c)
    {
        return strchr ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", c) != NULL;
    }

    bool is_symbol_next (int c)
    {
        return strchr ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_+#=:-/", c) != NULL;
    }

    void pgn_char_read (pgn_t *pgn)
    {
        assert (pgn != NULL);
        // char "stack"
        if (pgn->char_unread)
        {
            pgn->char_unread = false;
            return;
        }

        // consume the current character
        if (pgn->char_first)
        {
            pgn->char_first = false;
        }
        else
        {
            // update counters
            assert (pgn->char_hack != CHAR_EOF);

            if (false)
            {
            }
            else if (pgn->char_hack == '\n')
            {
                pgn->char_line++;
                pgn->char_column = 0;
            }
            else if (pgn->char_hack == '\t')
            {
                pgn->char_column += TAB_SIZE - (pgn->char_column % TAB_SIZE);
            }
            else
            {
                pgn->char_column++;
            }
        }

        // read a new character
        pgn->char_hack = fgetc (pgn->file);

        if (pgn->char_hack == EOF)
        {
            if (ferror (pgn->file)) log_fatal ("pgn_char_read(): fgetc(): %s\n", strerror (errno));
            pgn->char_hack = CHAR_EOF;
        }

        if (DispChar)
        {
            printf ("< L%d C%d '%c' (%02X)\n", pgn->char_line, pgn->char_column, pgn->char_hack, pgn->char_hack);
        }
    }

    void pgn_char_unread (pgn_t *pgn)
    {
        assert (pgn != NULL);
        assert (!pgn->char_unread);
        assert (!pgn->char_first);

        pgn->char_unread = true;
    }

}


void open_pgn (pgn_t *pgn, const char *fn_pgn)
{
    assert (pgn != NULL);
    assert (fn_pgn != NULL);

    pgn->file = fopen (fn_pgn, "r");
    if (pgn->file == NULL) log_fatal ("open_pgn(): can't open file \"%s\": %s\n", fn_pgn, strerror (errno));

    pgn->char_hack      = CHAR_EOF; // DEBUG
    pgn->char_line      = 1;
    pgn->char_column    = 0;
    pgn->char_unread    = false;
    pgn->char_first     = true;

    pgn->token_type     = TOKEN_ERROR; // DEBUG
    strcpy (pgn->token_string, "?"); // DEBUG
    pgn->token_length   = -1; // DEBUG
    pgn->token_line     = -1; // DEBUG
    pgn->token_column   = -1; // DEBUG
    pgn->token_unread   = false;
    pgn->token_first    = true;

    pgn->result   = "?"; // DEBUG
    pgn->fen      = "?"; // DEBUG
    pgn->WhiteELO = "?"; // DEBUG
    pgn->BlackELO = "?"; // DEBUG

    pgn->move_line      = -1; // DEBUG
    pgn->move_column    = -1; // DEBUG
}

void close_pgn (pgn_t *pgn)
{
    assert (pgn != NULL);
    fclose (pgn->file);
}

bool next_game_pgn (pgn_t *pgn)
{
    char name[STRING_SIZE];
    char value[STRING_SIZE];

    assert (pgn != NULL);

    // init
    pgn->result   = "*";
    pgn->fen      = "";
    pgn->WhiteELO = "0";
    pgn->BlackELO = "0";

    while (true)
    {
        pgn_token_read (pgn);

        if (pgn->token_type != '[') break;

        // tag
        pgn_token_read (pgn);
        if (pgn->token_type != TOKEN_SYMBOL)
        {
            log_fatal ("next_game_pgn(): malformed tag at line %d, column %d, game %d\n", pgn->token_line, pgn->token_column, pgn->game_nb);
        }
        strcpy (name, pgn->token_string);

        pgn_token_read (pgn);
        if (pgn->token_type != TOKEN_STRING)
        {
            log_fatal ("next_game_pgn(): malformed tag at line %d, column %d, game %d\n",
                       pgn->token_line, pgn->token_column, pgn->game_nb);
        }
        strcpy (value, pgn->token_string);

        pgn_token_read (pgn);
        if (pgn->token_type != ']')
        {
            log_fatal ("next_game_pgn(): malformed tag at line %d, column %d, game %d\n",
                       pgn->token_line, pgn->token_column, pgn->game_nb);
        }

        // special tag?
        if (false)
        {
        }
        else if (is_equal (name, "Result"))
        {
            pgn->result = value;
        }
        else if (is_equal (name, "FEN"))
        {
            pgn->fen = value;
        }
        else if (is_equal (name, "WhiteElo"))
        {
            pgn->WhiteELO =  value;
        }
        else if (is_equal (name, "BlackElo"))
        {
            pgn->BlackELO = value;
        }
    }
    if (pgn->token_type == TOKEN_EOF) return false;

    pgn_token_unread (pgn);
    
    pgn->game_nb++;
    return true;
}

bool next_move_pgn (pgn_t *pgn, char *move_s, int size)
{
    int depth;

    assert (pgn != NULL);
    assert (move_s != NULL);
    assert (size >= STRING_SIZE);

    // init
    pgn->move_line      = -1;  // DEBUG
    pgn->move_column    = -1;  // DEBUG

    depth = 0;

    while (true)
    {
        pgn_token_read (pgn);
        if (false)
        {
        }
        else if (pgn->token_type == '(')
        {
            // open RAV
            ++depth;
        }
        else if (pgn->token_type == ')')
        {
            // close RAV
            if (depth == 0)
            {
                log_fatal ("next_move_pgn(): malformed variation at line %d, column %d, game %d\n",
                           pgn->token_line, pgn->token_column, pgn->game_nb);
            }
            --depth;
            assert (depth >= 0);
        }
        else if (pgn->token_type == TOKEN_RESULT)
        {
            // game finished
            if (depth > 0)
            {
                log_fatal ("next_move_pgn(): malformed variation at line %d, column %d, game %d\n",
                           pgn->token_line, pgn->token_column, pgn->game_nb);
            }
            return false;
        }
        else
        {
            // skip optional move number
            if (pgn->token_type == TOKEN_INTEGER)
            {
                do
                {
                    pgn_token_read (pgn);
                }
                while (pgn->token_type == '.');
            }

            // move must be a symbol
            if (pgn->token_type != TOKEN_SYMBOL)
            {
                log_fatal ("next_move_pgn(): malformed move at line %d, column %d, game %d\n",
                           pgn->token_line, pgn->token_column, pgn->game_nb);
            }

            // store move for later use
            if (depth == 0)
            {
                if (pgn->token_length >= size)
                {
                    log_fatal ("next_move_pgn(): move too long at line %d, column %d, game %d\n",
                               pgn->token_line, pgn->token_column, pgn->game_nb);
                }

                strcpy (move_s, pgn->token_string);
                pgn->move_line = pgn->token_line;
                pgn->move_column = pgn->token_column;
            }

            // skip optional NAGs
            do
            {
                pgn_token_read (pgn);
            }
            while (pgn->token_type == TOKEN_NAG);
            pgn_token_unread (pgn);

            // return move
            if (depth == 0)
            {
                if (DispMove) printf ("move=\"%s\"\n", move_s);
                return true;
            }
        }
    }
    assert (false);
    return false;
}

