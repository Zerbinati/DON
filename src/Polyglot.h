#ifndef _POLYGLOT_H_INC_
#define _POLYGLOT_H_INC_

#include <fstream>
#include "Position.h"
#include "Type.h"

namespace Polyglot {

    /// Polyglot::Entry needs 16 bytes to be stored.
    ///  - Key       8 bytes
    ///  - Move      2 bytes
    ///  - Weight    2 bytes
    ///  - Learn     4 bytes
    struct Entry
    {
        static const Entry NullEntry;

        u64 key;
        u16 move;
        u16 weight;
        u32 learn;

        Entry () = default;
        Entry (u64 k, u16 m, u16 w, u32 l)
            : key (k)
            , move (m)
            , weight (w)
            , learn (l)
        {}

        Entry& operator= (const Entry&) = default;

        explicit operator Move () const { return Move(move); }

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

    template<typename CharT, typename Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, const Entry &pe)
    {
        os << std::string(pe);
        return os;
    }

    /// Polyglot::Book is a file containing series of Polyglot::Entry.
    /// All integers are stored in big-endian format,
    /// with the highest byte first (regardless of size).
    /// The entries are ordered according to the key in ascending order.
    /// Polyglot::Book file has *.bin extension.
    class Book
        : public std::fstream
    {
    public:

        std::string book_fn = "";
        std::ios_base::openmode mode;
        size_t size;

        static const u08 HeaderSize;

        Book ();
        Book (const std::string&, std::ios_base::openmode);
        Book (const Book&) = delete;
        Book& operator= (const Book&) = delete;

        ~Book ();

        size_t get_size ()
        {
            if (size == size_t(0))
            {
                auto cur_pos = tellg ();
                seekg (0L, ios_base::end);
                size = size_t(tellg ());
                seekg (cur_pos, ios_base::beg);
                clear ();
            }
            return size;
        }

        bool open (const std::string&, std::ios_base::openmode);
        void close ();

        template<typename T>
        Book& operator>> (      T&);
        template<typename T>
        Book& operator<< (const T&);

        size_t find_index (const Key);
        size_t find_index (const Position&);
        size_t find_index (const std::string&, bool = false);

        Move probe_move (const Position&, bool = true);

        std::string read_entries (const Position&);

    };

}

#endif // _POLYGLOT_H_INC_
