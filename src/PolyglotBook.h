#ifndef _POLYGLOT_BOOK_H_INC_
#define _POLYGLOT_BOOK_H_INC_

#include <fstream>

#include "Type.h"

class Position;

namespace OpeningBook {

    // A Polyglot book is a series of entries of 16 bytes.
    // All integers are stored in big-endian format,
    // with the highest byte first (regardless of size).
    // The entries are ordered according to the key in ascending order.
    // Polyglot book file has *.bin extension
    class PolyglotBook
        : public std::fstream
    {

    public:

        // Polyglot Book entry needs 16 bytes to be stored.
        //  - Key       8 bytes
        //  - Move      2 bytes
        //  - Weight    2 bytes
        //  - Learn     4 bytes
        struct PBEntry
        {
            static const PBEntry NullEntry;

            u64 key     = U64(0);
            u16 move    = MOVE_NONE;
            u16 weight  = 0;

            union
            {
                u32 learn   = 0;
                struct
                {
                    u16 n;
                    u16 sum;
                };
            } _;
            

            PBEntry () = default;
            PBEntry (u64 k, u16 m, u16 w, u32 l)
                : key (k)
                , move (m)
                , weight (w)
            {
                _.learn = l;
            }

            operator Move () const { return Move(move); }

            bool operator== (const PolyglotBook::PBEntry &pe)
            {
                return key == pe.key
                    && move == pe.move
                    && weight == pe.weight;
            }
            bool operator!= (const PolyglotBook::PBEntry &pe)
            {
                //return !(*this == pe);
                return key != pe.key
                    || move != pe.move
                    || weight != pe.weight;
            }
            bool operator>  (const PolyglotBook::PBEntry &pe)
            {
                return key != pe.key ?
                        key > pe.key :
                        //move > pe.move;      // order by move
                        weight > pe.weight;  // order by weight
            }
            bool operator<  (const PolyglotBook::PBEntry &pe)
            {
                return key != pe.key ?
                        key < pe.key :
                        //move < pe.move;      // order by move
                        weight < pe.weight;  // order by weight
            }
            bool operator>= (const PolyglotBook::PBEntry &pe)
            {
                return key != pe.key ?
                        key >= pe.key :
                        //move >= pe.move;      // order by move
                        weight >= pe.weight;  // order by weight
            }
            bool operator<= (const PolyglotBook::PBEntry &pe)
            {
                return key != pe.key ?
                        key <= pe.key :
                        //move <= pe.move;      // order by move
                        weight <= pe.weight;  // order by weight
            }

            explicit operator std::string () const;

        };

        struct Book
        {
            size_t   alloc;
            size_t   mask;
            size_t   size;
            PBEntry  *entries;
            intptr_t *hash;

            static const int NullHash = -1;

            void clear ();
            void free ();

            void clean ();

            void rebuild_hash_table ();
            void resize ();

        };

        static const u08 HeaderSize = 96;
        static_assert (HeaderSize == 96, "Header size incorrect");

        static const u08 EntrySize  = sizeof (PBEntry);
        static_assert (EntrySize == 16, "Entry size incorrect");

    private:

        std::string _filename;
        openmode    _mode;
        size_t      _size;

        Book        _book;

        template<class T>
        PolyglotBook& operator>> (      T &t);
        template<class T>
        PolyglotBook& operator<< (const T &t);

    public:
        // find_index() takes a hash-key as input, and search through the book file for the given key.
        // Returns the index of the 1st book entry with the same key as the input.
        size_t find_index (const Key key);
        size_t find_index (const Position &pos);
        size_t find_index (const std::string &fen, bool c960 = false);

        PolyglotBook ();
        PolyglotBook (const std::string &book_fn, openmode mode);

        PolyglotBook (const PolyglotBook&) = delete;
        PolyglotBook& operator= (const PolyglotBook&) = delete;

        ~PolyglotBook ();

        std::string filename () const { return _filename; }

        size_t size ()
        {
            if (_size != 0) return _size;

            size_t cur_pos = tellg ();
            seekg (0L, ios_base::end);
            _size = tellg ();
            seekg (cur_pos, ios_base::beg);
            clear ();
            return _size;
        }

        bool open (const std::string &book_fn, openmode mode);

        void close ()
        {
            if (is_open ())
            {
                std::fstream::close ();
            }
        }

        // probe_move() tries to find a book move for the given position.
        // If no move is found returns MOVE_NONE.
        // If pick_best is true returns always the highest rated move,
        // otherwise randomly chooses one, based on the move score.
        Move probe_move (const Position &pos, bool pick_best = true);

        std::string read_entries (const Position &pos);

        bool keep_entry (PBEntry &pbe) const;
        void load ();
        void save ();
    };

    template<class CharT, class Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, const PolyglotBook::PBEntry &pbe)
    {
        os << std::string(pbe);
        return os;
    }

}

#endif // _POLYGLOT_BOOK_H_INC_
