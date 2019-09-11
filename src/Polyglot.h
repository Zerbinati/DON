#pragma once

#include "Position.h"
#include "PRNG.h"
#include "Type.h"

/// Polyglot::Entry needs 16 bytes to be stored.
///  - Key       8 bytes
///  - Move      2 bytes
///  - Weight    2 bytes
///  - Learn     4 bytes
struct PolyEntry
{
    u64 key;
    u16 move;
    u16 weight;
    u32 learn;


    PolyEntry(u64 k, u16 m, u16 w, u32 l)
        : key{k}
        , move{m}
        , weight{w}
        , learn{l}
    {}
    PolyEntry()
        : PolyEntry{0, 0, 0, 0}
    {}

    PolyEntry& operator=(PolyEntry const&) = default;

    explicit operator Move() const { return Move(move); }

    bool operator==(PolyEntry const &entry) const
    {
        return key == entry.key
            && move == entry.move
            && weight == entry.weight;
    }
    bool operator!=(PolyEntry const &entry) const
    {
        return key != entry.key
            || move != entry.move
            || weight != entry.weight;
    }
    bool operator>(PolyEntry const &entry) const
    {
        return key != entry.key ?
                    key > entry.key :
                    weight != entry.weight ?
                        weight > entry.weight :
                        move > entry.move;
    }
    bool operator<(PolyEntry const &entry) const
    {
        return key != entry.key ?
                    key < entry.key :
                    weight != entry.weight ?
                        weight < entry.weight :
                        move < entry.move;
    }
    bool operator>=(PolyEntry const &entry) const
    {
        return key != entry.key ?
                    key >= entry.key :
                    weight != entry.weight ?
                        weight >= entry.weight :
                        move >= entry.move;
    }
    bool operator<=(PolyEntry const &entry) const
    {
        return key != entry.key ?
                    key <= entry.key :
                    weight != entry.weight ?
                        weight <= entry.weight :
                        move <= entry.move;
    }

    bool operator==(Move m) const { return move == m; }
    bool operator!=(Move m) const { return move != m; }

    explicit operator std::string() const;

};

static_assert (sizeof (PolyEntry) == 16, "Entry size incorrect");

template<typename CharT, typename Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<<(std::basic_ostream<CharT, Traits> &os, PolyEntry const &entry)
{
    os << std::string(entry);
    return os;
}

class PolyBook
{
private:
    static PRNG prng;

    PolyEntry   *entries;
    size_t      entry_count;

    u08         fail_counter;
    bool        do_probe;
    Bitboard    last_pieces;
    i32         last_piece_count;

    void clear();

    i64 find_index(Key) const;
    //i64 find_index(Position const&) const;
    //i64 find_index(std::string const&, bool = false) const;

    bool can_probe(Position const&);

public:

    size_t const HeaderSize = 0;

    bool enabled;
    std::string book_fn;

    PolyBook();
    ~PolyBook();

    void initialize(std::string const&);

    Move probe(Position&, i16, bool);

    std::string show(Position const&) const;
};

// Global Polyglot Book
extern PolyBook Book;
