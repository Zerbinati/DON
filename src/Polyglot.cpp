#include "Polyglot.h"

#include <iomanip>

#include "Position.h"
#include "PRNG.h"
#include "Zobrist.h"
#include "MoveGenerator.h"
#include "manipulator.h"
#include "Notation.h"

namespace Polyglot {

    using namespace std;
    using namespace MoveGen;
    using namespace Notation;

    #define OFFSET(x)  (Book::HeaderSize + (x)*Entry::Size)

    namespace {

        u32 BegGame = 1;
        u32 EndGame = 100;
        u32 MinGame = 3;

        void* realloc_ex (void *old_address, size_t size)
        {
            assert(old_address != nullptr);
            assert(size > 0);

            void *newaddress = realloc (old_address, size);

            if (newaddress == nullptr)
            {
                free (old_address);
            }
            old_address = newaddress;
            return old_address;
        }

    }

    // -------------------------

    const u08 Entry::Size = sizeof (Entry);
    static_assert (Entry::Size == 16, "Incorrect Entry::size");

    const Entry Entry::NullEntry = { 0 , 0 , 0 , 0 };

    Entry::operator string () const
    {
        ostringstream oss;

        auto m = Move(move);
        // Set new type for promotion piece
        auto pt = PieceT((m >> 12) & TOTL);
        if (pt != PAWN) promote (m, pt);
        // TODO::Add special move flags and verify it is legal
        oss << " key: "    << std::setw (16) << std::setfill ('0') << std::hex << std::uppercase << key << std::nouppercase
            << " move: "   << std::setw ( 5) << std::setfill (' ') << std::left << move_to_can (m) << std::right
            << " weight: " << std::setw ( 4) << std::setfill ('0') << std::dec << weight
            << " learn: "  << std::setw ( 2) << std::setfill ('0') << std::dec << learn
            << std::setfill (' ');

        return oss.str ();
    }

    // -------------------------

    Book::Book (const string &filename, openmode mode)
        : fstream (filename, mode|ios_base::binary)
        , _filename (filename)
        , _mode (mode)
        , _size (0)
    {}

    Book::~Book ()
    {
        close ();
    }

    // open() tries to open a book file with the given name after closing any existing one.
    // mode:
    // Read -> ios_base::in
    // Write-> ios_base::out
    bool Book::open (const string &filename, openmode mode)
    {
        _filename = filename;
        _mode     = mode;
        _size     = 0;
        fstream::open (filename, mode|ios_base::binary);
        clear (); // Reset any error flag to allow retry open()
        return is_open ();
    }
    void Book::close ()
    {
        if (is_open ())
        {
            std::fstream::close ();
        }
    }

    template<class T>
    Book& Book::operator>> (      T &t)
    {
        t = T ();
        for (u08 i = 0; i < sizeof (t) && good (); ++i)
        {
            u08 byte = u08 (get ());
            t = T ((t << 8) + byte);
        }
        return *this;
    }
    template<class T>
    Book& Book::operator<< (const T &t)
    {
        for (u08 i = 0; i < sizeof (t) && good (); ++i)
        {
            u08 byte = u08 (t >> (8*(sizeof (t) - 1 - i)));
            put (byte);
        }
        return *this;
    }

    template<>
    Book& Book::operator>> (      Entry &pe)
    {
        *this
            >> pe.key
            >> pe.move
            >> pe.weight
            >> pe.learn;
        return *this;
    }
    template<>
    Book& Book::operator<< (const Entry &pe)
    {
        *this
            << pe.key
            << pe.move
            << pe.weight
            << pe.learn;
        return *this;
    }

    // find_index() takes a hash-key as input, and search through the book file for the given key.
    // Returns the index of the 1st book entry with the same key as the input.
    size_t Book::find_index (      Key key)
    {
        if (!is_open ()) return streampos(-1);

        auto beg_index = size_t(0);
        auto end_index = size_t((size () - HeaderSize) / Entry::Size - 1);

        Entry pe;

        assert(beg_index <= end_index);
        while (beg_index < end_index && good ())
        {
            auto mid_index = size_t((beg_index + end_index) / 2);
            assert(mid_index >= beg_index && mid_index < end_index);

            seekg (OFFSET(mid_index), ios_base::beg);
            *this >> pe;

            if (key <= pe.key)
            {
                end_index = mid_index;
            }
            else
            {
                beg_index = mid_index + 1;
            }
        }
        assert(beg_index == end_index);

        return beg_index;
    }
    size_t Book::find_index (const Position &pos)
    {
        return find_index (pos.posi_key ());
    }
    size_t Book::find_index (const string &fen, bool c960)
    {
        return find_index (Position (fen, c960).posi_key ());
    }

    // probe_move() tries to find a book move for the given position.
    // If no move is found returns MOVE_NONE.
    // If pick_best is true returns always the highest rated move,
    // otherwise randomly chooses one, based on the move score.
    Move Book::probe_move (const Position &pos, bool pick_best)
    {
        static PRNG pr (now ());

        Key key = pos.posi_key ();

        auto index = find_index (key);

        seekg (OFFSET(index));

        auto move = MOVE_NONE;

        Entry pe;

        u16 max_weight = 0;
        u32 weight_sum = 0;

        //vector<Entry> pes;
        //while ((*this >> pe), (pe.key == key))
        //{
        //    pes.push_back (pe);
        //    max_weight = max (max_weight, pe.weight);
        //    weight_sum += pe.weight;
        //}
        //if (!pes.size ()) return MOVE_NONE;
        //
        //if (pick_best)
        //{
        //    auto itr = pes.begin ();
        //    while (itr != pes.end ())
        //    {
        //        pe = *itr;
        //        if (pe.weight == max_weight)
        //        {
        //            move = Move(pe.move);
        //            break;
        //        }
        //        ++itr;
        //    }
        //}
        //else
        //{
        //    //There is a straightforward algorithm for picking an item at random, where items have individual weights:
        //    //1) calculate the sum of all the weights
        //    //2) pick a random number that is 0 or greater and is less than the sum of the weights
        //    //3) go through the items one at a time, subtracting their weight from your random number, until you get the item where the random number is less than that item's weight
        //
        //    u32 rand = (pr.rand<u32> () % weight_sum);
        //    auto itr = pes.begin ();
        //    while (itr != pes.end ())
        //    {
        //        pe = *itr;
        //        if (pe.weight > rand)
        //        {
        //            move = Move(pe.move);
        //            break;
        //        }
        //        rand -= pe.weight;
        //        ++itr;
        //    }
        //}

        while ((*this >> pe), (pe.key == key))
        {
            if (pe == MOVE_NONE) continue;

            max_weight = max (max_weight, pe.weight);
            weight_sum += pe.weight;

            if (pick_best)
            {
                if (pe.weight == max_weight) move = Move(pe);
            }
            // Choose book move according to its score.
            // If a move has a very high score it has a higher probability
            // of being choosen than a move with a lower score.
            else
            if (weight_sum != 0)
            {
                u16 rand = pr.rand<u16> () % weight_sum;
                if (pe.weight > rand) move = Move(pe);
            }
            // Note that first entry is always chosen if not pick best and sum of weight = 0
            else
            if (move == MOVE_NONE)
            {
                move = Move(pe);
            }
        }

        if (move == MOVE_NONE) return MOVE_NONE;

        // Polyglot book move is encoded as follows:
        //
        // bit 00-05: destiny square    (0...63)
        // bit 06-11: origin square     (0...63)
        // bit 12-14: promotion piece   (NONE = 0, KNIGHT = 1 ... QUEEN = 4)
        // bit    15: empty
        // Move is "0" (a1a1) then it should simply be ignored.
        // It seems to me that in that case one might as well delete the entry from the book.

        // Castling moves follow "king captures rook" representation.
        // Promotion moves have promotion piece different then our structure of move
        // So in case book move is a promotion have to convert to our representation,
        // in all the other cases can directly compare with a Move after having masked out
        // the special Move's flags (bit 14-15) that are not supported by Polyglot.
        // Polyglot use 3 bits while engine use 2 bits.
        auto pt = PieceT((move >> 12) & TOTL);
        // Set new type for promotion piece
        if (pt != PAWN) promote (move, pt);

        // Add special move flags and verify it is legal
        for (const auto &m : MoveList<LEGAL> (pos))
        {
            if ((m.move & ~PROMOTE) == move)
            {
                return m;
            }
        }

        return MOVE_NONE;
    }

    string Book::read_entries (const Position &pos)
    {
        ostringstream oss;

        if (!is_open () || !(_mode & ios_base::in)) return oss.str ();

        Key key = pos.posi_key ();

        auto index = find_index (key);

        seekg (OFFSET(index));

        vector<Entry> pes;
        Entry pe;
        u32 weight_sum = 0;
        while ((*this >> pe), (pe.key == key))
        {
            pes.push_back (pe);
            weight_sum += pe.weight;
        }
        
        if (pes.empty ())
        {
            std::cerr << "ERROR: no such key... "
                        << std::hex << std::uppercase << key << std::nouppercase << std::dec
                        << std::endl;
        }
        else
        {
            for_each (pes.begin (), pes.end (), [&oss, &weight_sum] (Entry e)
            {
                oss << e << " prob: " << std::setfill ('0') << std::fixed << std::width_prec (6, 2) << (weight_sum != 0 ? 100.0 * e.weight / weight_sum : 0.0) << std::setfill (' ')
                    << endl;
            });
        }
        return oss.str ();
    }

    // -------------------------

    Table::Table (size_t entry_alloc)
    {
        init (entry_alloc);
    }

    void Table::init (size_t entry_alloc)
    {
        _entry_alloc = entry_alloc;
        _entry_count = 0;
        _hash_mask   = _entry_alloc * 2 - 1;

        entries = (Entry  *) malloc (_entry_alloc * Entry::Size);
        hash    = (intptr_t *) malloc (_entry_alloc * 2 * sizeof (intptr_t));

        empty_hash_table ();
    }

    void Table::free ()
    {
        if (entries != nullptr)
        {
            ::free (entries);
            entries = nullptr;
        }
        if (hash != nullptr)
        {
            ::free (hash);
            hash = nullptr;
        }

        _entry_alloc = 1;
        _entry_count = 0;
        _hash_mask   = _entry_alloc * 2 - 1;
    }

    void Table::clean ()
    {
        size_t read_entry_count  = 0;
        size_t write_entry_count = 0;
        while (read_entry_count < _entry_count)
        {
            if (entries[read_entry_count].move != MOVE_NONE)
            {
                entries[write_entry_count] = entries[read_entry_count];
                ++write_entry_count;
            }
            ++read_entry_count;
        }
        _entry_count = write_entry_count;
    }

    void Table::empty_hash_table ()
    {
        for (size_t index = 0; index < _entry_alloc * 2; ++index)
        {
            hash[index] = NullHash;
        }
    }

    void Table::rebuild_hash_table ()
    {
        empty_hash_table ();

        for (intptr_t pos = 0; (size_t)pos < _entry_count; ++pos)
        {
            hash[find_null_index (entries[pos].key)] = pos;
        }
    }

    void Table::resize ()
    {
        assert(_entry_count == _entry_alloc);

        _entry_alloc = _entry_alloc * 2;
        _hash_mask   = _entry_alloc * 2 - 1;

        //int alloc_size = 0;
        //alloc_size += _entry_alloc * EntrySize;
        //alloc_size += _entry_alloc * 2 * sizeof (intptr_t);
        //if (size >= 0x100000)
        //{
        //    if (!DebugInfo)
        //    {
        //        printf ("Allocating %gMB ...\n", (double)alloc_size / 0x100000);
        //    }
        //}

        entries = (Entry  *) realloc_ex (entries, _entry_alloc * Entry::Size);
        hash    = (intptr_t *) realloc_ex (hash   , _entry_alloc * 2 * sizeof (intptr_t));

        rebuild_hash_table ();
    }

    size_t Table::find_null_index (Key key) const
    {
        // Find free hash table spot
        size_t index = (u32)key & _hash_mask;
        while (hash[index] != NullHash)
        {
            index = (index + 1) & _hash_mask;
        }
        assert(0 <= index && index < _entry_alloc * 2);
        assert(hash[index] == NullHash);
        return index;
    }

    intptr_t Table::find_entry_pos (Key key, Move move) const
    {
        size_t index = (u32)key & _hash_mask;
        while (hash[index] != NullHash)
        {
            intptr_t pos = hash[index];
            assert(0 <= pos && (size_t)pos < _entry_count);
            const auto &e = entries[pos];
            if (   e.key == key
                && e.move == move
               )
            {
                return pos;
            }

            index = (index + 1) & _hash_mask;
        }
        return NullHash;
    }
    intptr_t Table::find_entry_pos (const Entry &pe) const
    {
        return find_entry_pos (pe.key, Move(pe));
    }

    intptr_t Table::new_entry_pos (const Entry &pe)
    {
        assert(_entry_count <= _entry_alloc);
        if (_entry_count == _entry_alloc)
        {
            // Allocate more memory
            resize ();
        }
        assert(_entry_count < _entry_alloc);
        
        // Insert new entry
        intptr_t pos = _entry_count++;
        assert(0 <= pos && (size_t)pos < _entry_count);

        entries[pos] = pe;

        // Insert pos into the hash table
        hash[find_null_index (entries[pos].key)] = pos;
        return pos;
    }

    void Table::save (const string &book_fn) const
    {
        Book book (book_fn, ios_base::out);

        if (book.is_open () && book.good ())
        {
            book.seekg (0L, ios_base::beg);

            // Save header
            for (size_t i = 0; i < 6; ++i)
            {
                book << Entry::NullEntry;
            }
            // Loop Entry
            for (size_t pos = 0; pos < size (); ++pos)
            {
                const auto &pe = entries[pos];
                //assert(entry_keep (pe));

                // Null keys are reserved for the header
                if (pe.key != U64(0))
                {
                    book << pe;
                }
            }
            book.close ();
        }
    }
    void Table::load (const string &book_fn)
    {
        Book book (book_fn, ios_base::in);

        if (book.is_open () && book.good ())
        {
            book.seekg (OFFSET(0), ios_base::beg);
            size_t entry_count = (book.size () - Book::HeaderSize) / Entry::Size;
            for (size_t i = 0; i < entry_count; ++i)
            {
                Entry pe;
                book >> pe;
                assert(pe.key != U64(0));
                new_entry_pos (pe);
            }
            book.close ();
        }
    }

}
