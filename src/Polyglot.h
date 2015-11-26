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
            return key != pe.key ?
                key > pe.key :
            //move > pe.move;      // order by move
            weight > pe.weight;  // order by weight
        }
        bool operator<  (const Entry &pe)
        {
            return key != pe.key ?
                key < pe.key :
                //move < pe.move;      // order by move
                weight < pe.weight;  // order by weight
        }
        bool operator>= (const Entry &pe)
        {
            return key != pe.key ?
                key >= pe.key :
                //move >= pe.move;      // order by move
                weight >= pe.weight;  // order by weight
        }
        bool operator<= (const Entry &pe)
        {
            return key != pe.key ?
                key <= pe.key :
                //move <= pe.move;      // order by move
                weight <= pe.weight;  // order by weight
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

        std::string _filename = "";
        openmode    _mode     = openmode(0);
        size_t      _size     = 0UL;

    public:
        static const u08 HeaderSize = 96;

        Book () = default;
        Book (const std::string &filename, openmode mode);

        Book (const Book&) = delete;
        Book& operator= (const Book&) = delete;

        ~Book ();

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

        bool open (const std::string &filename, openmode mode);
        void close ();

        template<class T>
        Book& operator>> (      T &t);
        template<class T>
        Book& operator<< (const T &t);

        size_t find_index (      Key key);
        size_t find_index (const Position &pos);
        size_t find_index (const std::string &fen, bool c960 = false);

        Move probe_move (const Position &pos, bool pick_best = true);

        std::string read_entries (const Position &pos);

    };

    class Table
    {
    private:
        size_t   _entry_alloc = 1;
        size_t   _hash_mask  = 1;
        size_t   _entry_count  = 0;

        void empty_hash_table ();
        void rebuild_hash_table ();

    public:
        Entry    *entries = nullptr;
        intptr_t *hash    = nullptr;

        static const intptr_t NullHash = -1;

        explicit Table (size_t entry_alloc = 1);

        size_t size () const { return _entry_count; }

        void init (size_t entry_alloc = 1);
        void free ();

        void clean ();
        
        void resize ();

        size_t   find_null_index (Key key) const;

        intptr_t find_entry_pos (Key key, Move move) const;
        intptr_t find_entry_pos (const Entry &pe) const;

        intptr_t new_entry_pos (const Entry &pe);

        void save (const std::string &book_fn) const;
        void load (const std::string &book_fn);

        /*
        template<class CharT, class Traits>
        friend std::basic_ostream<CharT, Traits>&
            operator<< (std::basic_ostream<CharT, Traits> &os, const Table &pt)
        {
            //u32 mem_size_mb = tt.size ();
            //u08 dummy = 0;
            //os.write (reinterpret_cast<const CharT*> (&mem_size_mb), sizeof (mem_size_mb));
            //os.write (reinterpret_cast<const CharT*> (&dummy), sizeof (dummy));
            //os.write (reinterpret_cast<const CharT*> (&dummy), sizeof (dummy));
            //os.write (reinterpret_cast<const CharT*> (&dummy), sizeof (dummy));
            //os.write (reinterpret_cast<const CharT*> (&tt._generation), sizeof (tt._generation));
            //os.write (reinterpret_cast<const CharT*> (&tt._cluster_count), sizeof (tt._cluster_count));
            //u32 cluster_bulk = u32 (tt._cluster_count / BufferSize);
            //for (u32 i = 0; i < cluster_bulk; ++i)
            //{
            //    os.write (reinterpret_cast<const CharT*> (tt._clusters+i*BufferSize), Cluster::Size*BufferSize);
            //}
            return os;
        }

        template<class CharT, class Traits>
        friend std::basic_istream<CharT, Traits>&
            operator>> (std::basic_istream<CharT, Traits> &is, Table &pt)
        {
            //u32 mem_size_mb;
            //u08 generation;
            //u08 dummy;
            //is.read (reinterpret_cast<CharT*> (&mem_size_mb), sizeof (mem_size_mb));
            //is.read (reinterpret_cast<CharT*> (&dummy), sizeof (dummy));
            //is.read (reinterpret_cast<CharT*> (&dummy), sizeof (dummy));
            //is.read (reinterpret_cast<CharT*> (&dummy), sizeof (dummy));
            //is.read (reinterpret_cast<CharT*> (&generation), sizeof (generation));
            //is.read (reinterpret_cast<CharT*> (&tt._cluster_count), sizeof (tt._cluster_count));
            //tt.resize (mem_size_mb);
            //tt._generation = generation;
            //u32 cluster_bulk = u32 (tt._cluster_count / BufferSize);
            //for (u32 i = 0; i < cluster_bulk; ++i)
            //{
            //    is.read (reinterpret_cast<CharT*> (tt._clusters+i*BufferSize), Cluster::Size*BufferSize);
            //}
            return is;
        }
        */
    };

}

#endif // _POLYGLOT_H_INC_
