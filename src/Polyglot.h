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
        static const u08 Size;

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

    class PolyBook
    {
    private:

        Entry *entries;
        size_t entry_count;

        u08 fail_counter;
        bool do_probe;
        Bitboard last_pieces;
        i32 last_piece_count;

        void clear ();

        i64 find_index (const Key);
        //i64 find_index (const Position&);
        //i64 find_index (const std::string&, bool = false);

        bool can_probe (const Position&);

    public:

        static const size_t HeaderSize;

        bool use;
        bool enabled;
        std::string filename;
        bool pick_best;
        i16 move_count;

        PolyBook ();
       ~PolyBook ();

        void initialize (const std::string&);

        Move probe (Position&);

        std::string show (const Position&);
    };

}

// Global Book
extern Polyglot::PolyBook Book;

#endif // _POLYGLOT_H_INC_
