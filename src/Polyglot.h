#ifndef _POLYGLOT_H_INC_
#define _POLYGLOT_H_INC_

#include <fstream>

#include "Type.h"

class Position;

namespace Polyglot {

    // Polyglot::Entry needs 16 bytes to be stored.
    //  - Key       8 bytes
    //  - Move      2 bytes
    //  - Weight    2 bytes
    //  - Learn     4 bytes
    struct Entry
    {
        static const u08 Size;
        static const Entry NullEntry;

        u64 key     = U64(0);
        u16 move    = 0;
        u16 weight  = 0;
        u32 learn   = 0;

        Entry () = default;
        Entry (u64 k, u16 m, u16 w, u32 l)
            : key (k)
            , move (m)
            , weight (w)
            , learn (l)
        {}

        Entry& operator= (const Entry&) = default;

        explicit operator Move () const { return Move (move); }

        bool operator== (const Entry &pe)
        {
            return key == pe.key
                && move == pe.move
                && weight == pe.weight;
        }
        bool operator!= (const Entry &pe)
        {
            //return !(*this == pe);
            return key != pe.key
                || move != pe.move
                || weight != pe.weight;
        }
        bool operator>  (const Entry &pe)
        {
            return
                key != pe.key ?
                    key > pe.key :
                    weight != pe.weight ?
                        weight > pe.weight :
                        move > pe.move;
        }
        bool operator<  (const Entry &pe)
        {
            return
                key != pe.key ?
                    key < pe.key :
                    weight != pe.weight ?
                        weight < pe.weight :
                        move < pe.move;
        }
        bool operator>= (const Entry &pe)
        {
            return
                key != pe.key ?
                    key >= pe.key :
                    weight != pe.weight ?
                        weight >= pe.weight :
                        move >= pe.move;
        }
        bool operator<= (const Entry &pe)
        {
            return
                key != pe.key ?
                    key <= pe.key :
                    weight != pe.weight ?
                        weight <= pe.weight :
                        move <= pe.move;
        }

        bool operator== (Move m) { return move == m; }
        bool operator!= (Move m) { return move != m; }

        explicit operator std::string () const;

    };

    template<class CharT, class Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, const Entry &pe)
    {
        os << std::string(pe);
        return os;
    }

    // Polyglot::Book is a file containing series of Polyglot::Entry.
    // All integers are stored in big-endian format,
    // with the highest byte first (regardless of size).
    // The entries are ordered according to the key in ascending order.
    // Polyglot::Book file has *.bin extension.
    class Book
        : public std::fstream
    {
    private:

        std::string _book_fn = "";
        openmode    _mode    = openmode(0);
        size_t      _size    = 0U;

    public:
        static const u08 HeaderSize;

        Book () = default;
        Book (const std::string &book_fn, openmode mode);

        Book (const Book&) = delete;
        Book& operator= (const Book&) = delete;

        ~Book ();

        std::string book_fn () const { return _book_fn; }

        size_t size ()
        {
            if (_size != 0U) return _size;

            size_t cur_pos = tellg ();
            seekg (0L, ios_base::end);
            _size = tellg ();
            seekg (cur_pos, ios_base::beg);
            clear ();
            return _size;
        }

        bool open (const std::string &book_fn, openmode mode);
        void close ();

        template<class T>
        Book& operator>> (      T &t);
        template<class T>
        Book& operator<< (const T &t);

        size_t find_index (const Key key);
        size_t find_index (const Position &pos);
        size_t find_index (const std::string &fen, bool c960 = false);

        Move probe_move (const Position &pos, bool pick_best = true);

        std::string read_entries (const Position &pos);

    };

    class Table
    {
    private:
        size_t   _entry_alloc   = 1;
        size_t   _hash_mask     = 1;
        size_t   _entry_count   = 0;
        Entry    *_entries      = nullptr;
        intptr_t *_hashes       = nullptr;

        static const intptr_t NullHash = -1;

        void empty_hash_table ();
        void rebuild_hash_table ();

    public:

        explicit Table (size_t entry_alloc = 1);

        size_t size () const { return _entry_count; }

        void init (size_t entry_alloc = 1);
        void free ();

        void resize ();

        size_t   find_null_index (Key key) const;

        intptr_t find_entry_pos (Key key, Move move) const;
        intptr_t find_entry_pos (const Entry &pe) const;

        intptr_t new_entry_pos (const Entry &pe);

        void clean ();
        
        bool keep_entry (const Entry &pe) const;

        void filter ();
        void sort ();

        void save (const std::string &book_fn) const;
        void load (const std::string &book_fn);

        /*
        template<class CharT, class Traits>
        friend std::basic_ostream<CharT, Traits>&
            operator<< (std::basic_ostream<CharT, Traits> &os, const Table &pt)
        {
            // Save header
            for (size_t i = 0; i < 6; ++i)
            {
                os << Entry::NullEntry;
            }
            // Loop Entry
            for (size_t pos = 0; pos < size (); ++pos)
            {
                const auto &pe = entries[pos];
                //assert(entry_keep (pe));

                // Null keys are reserved for the header
                if (pe.key != U64 (0))
                {
                    os << pe;
                }
            }
            return os;
        }

        template<class CharT, class Traits>
        friend std::basic_istream<CharT, Traits>&
            operator>> (std::basic_istream<CharT, Traits> &is, Table &pt)
        {
            //is.seekg (OFFSET(0), ios_base::beg);
            size_t entry_count = (is.size () - Book::HeaderSize) / Entry::Size;
            for (size_t i = 0; i < entry_count; ++i)
            {
                Entry pe;
                is >> pe;
                assert (pe.key != U64 (0));
                new_entry_pos (pe);
            }
            return is;
        }
        */
    };

}

#endif // _POLYGLOT_H_INC_
