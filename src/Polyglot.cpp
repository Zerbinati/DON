#include "Polyglot.h"

#include "MoveGenerator.h"
#include "Notation.h"
#include "PRNG.h"
#include "Zobrist.h"

namespace Polyglot {

    using namespace std;
    using namespace MoveGen;
    using namespace Notation;

    #define OFFSET(x)  (Book::HeaderSize + (x)*sizeof (Entry))

    // Size of Book entry (16 bytes)
    static_assert (sizeof (Entry) == 16, "Entry size incorrect");

    Entry::operator string () const
    {
        ostringstream oss;

        auto m = Move(move);
        // Set new type for promotion piece
        auto pt = PieceType((m >> 12) & MAX_PTYPE);
        if (pt != PAWN)
        {
            promote (m, pt);
        }
        // TODO:: Add special move flags and verify it is legal
        oss << " key: "    << std::setw (16) << std::setfill ('0') << std::hex << std::uppercase << key << std::nouppercase
            << " move: "   << std::setw ( 5) << std::setfill (' ') << std::left << move_to_can (m) << std::right
            << " weight: " << std::setw ( 4) << std::setfill ('0') << std::dec << weight
            << " learn: "  << std::setw ( 2) << std::setfill ('0') << std::dec << learn
            << std::setfill (' ');

        return oss.str ();
    }

    const u08 Book::HeaderSize = 96;
    static_assert (Book::HeaderSize == 96, "Book header size incorrect");

    namespace {

        Move move_convert (const Position &pos, Move pg_move)
        {
            // Polyglot book move is encoded as follows:
            //
            // bit 00-05: destiny square    (0...63)
            // bit 06-11: origin square     (0...63)
            // bit 12-14: promotion piece   (None = 0, Knight = 1 ... Queen = 4)
            // bit    15: empty
            // Move is "0" (a1a1) then it should simply be ignored.
            // It seems to me that in that case one might as well delete the entry from the book.

            // Castling moves follow "king captures rook" representation.
            // Promotion moves have promotion piece different then our structure of move
            // So in case book move is a promotion have to convert to our representation,
            // in all the other cases can directly compare with a Move after having masked out
            // the special Move's flags (bit 14-15) that are not supported by Polyglot.
            // Polyglot use 3 bits while engine use 2 bits.
            i08 pt = (pg_move >> 12) & MAX_PTYPE;
            // Set new type for promotion piece
            if (0 != pt)
            {
                assert (NIHT <= pt && pt <= QUEN);
                promote (pg_move, PieceType (pt));
            }
            // Add special move flags and verify it is legal
            for (const auto &vm : MoveList<GenType::LEGAL> (pos))
            {
                if ((vm.move & ~PROMOTE) == pg_move)
                {
                    return vm.move;
                }
            }

            return MOVE_NONE;
        }

    }

    Book::Book ()
        : fstream ()
        , book_fn ("")
        , mode (ios_base::openmode(0))
        , size (size_t(0))
    {}
    Book::Book (const string &bk_fn, ios_base::openmode m)
        : fstream (bk_fn, m|ios_base::binary)
        , book_fn (bk_fn)
        , mode (m)
        , size (0U)
    {}

    Book::~Book ()
    {
        book_fn = "";
        mode = ios_base::openmode(0);
        size = size_t(0);
        close ();
    }

    /// open() tries to open a book file with the given name after closing any existing one.
    bool Book::open (const string &bk_fn, ios_base::openmode m)
    {
        book_fn = bk_fn;
        mode = m;
        size = size_t(0);
        fstream::open (bk_fn, m|ios_base::binary);
        clear (); // Reset any error flag to allow retry open()
        return is_open ();
    }
    /// close()
    void Book::close ()
    {
        if (is_open ())
        {
            std::fstream::close ();
        }
    }

    template<typename T> Book& Book::operator>> (      T &t)
    {
        t = T ();
        for (u08 i = 0; i < sizeof (t) && good (); ++i)
        {
            u08 byte = u08(get ());
            t = T ((t << 8) + byte);
        }
        return *this;
    }
    template<typename T> Book& Book::operator<< (const T &t)
    {
        for (u08 i = 0; i < sizeof (t) && good (); ++i)
        {
            u08 byte = u08(t >> (8*(sizeof (t) - 1 - i)));
            put (byte);
        }
        return *this;
    }

    template<> Book& Book::operator>> (      Entry &pe)
    {
        *this >> pe.key
              >> pe.move
              >> pe.weight
              >> pe.learn;
        return *this;
    }
    template<> Book& Book::operator<< (const Entry &pe)
    {
        *this << pe.key
              << pe.move
              << pe.weight
              << pe.learn;
        return *this;
    }

    /// find_index() returns the index of the 1st book entry with the same key as the input.
    size_t Book::find_index (const Key key)
    {
        if (!is_open ())
        {
            return size_t(-1);
        }

        auto beg_index = size_t(0);
        auto end_index = size_t((get_size () - HeaderSize) / sizeof (Entry) - 1);
        assert(beg_index <= end_index);

        while (   good ()
               && beg_index < end_index)
        {
            auto mid_index = (beg_index + end_index) / 2;
            assert(beg_index <= mid_index && mid_index < end_index);

            seekg (OFFSET(mid_index), ios_base::beg);
            Entry pe;
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
        return find_index (pos.pg_key ());
    }
    size_t Book::find_index (const string &fen, bool c960)
    {
        StateInfo si;
        return find_index (Position ().setup (fen, si, nullptr, c960).pg_key ());
    }

    /// probe_move() Tries to find a book move for the given position.
    /// If no move is found returns MOVE_NONE.
    /// If pick_best is true returns always the highest rated move,
    /// otherwise randomly chooses one, based on the move score.
    Move Book::probe_move (const Position &pos, bool pick_best)
    {
        static PRNG prng (now ());
        if (!is_open ())
        {
            return MOVE_NONE;
        }

        Key key = pos.pg_key ();

        auto index = find_index (key);

        seekg (OFFSET(index));

        auto move = MOVE_NONE;

        Entry pe;

        u16 max_weight = 0;
        u32 weight_sum = 0;

        while (   *this >> pe
               && pe.key == key)
        {
            if (MOVE_NONE == pe.move)
            {
                continue;
            }

            max_weight = std::max (pe.weight, max_weight);
            weight_sum += pe.weight;

            if (pick_best)
            {
                if (pe.weight == max_weight)
                {
                    move = Move(pe.move);
                }
            }
            // Choose book move according to its score.
            // If a move has a very high score it has a higher probability
            // of being choosen than a move with a lower score.
            else
            {
                if (0 != weight_sum)
                {
                    u16 rand = prng.rand<u16> () % weight_sum;
                    if (pe.weight > rand)
                    {
                        move = Move(pe.move);
                    }
                }
                else
                // Note that first entry is always chosen if sum of weight = 0
                if (MOVE_NONE == move)
                {
                    move = Move(pe.move);
                }
            }
        }

        if (MOVE_NONE == move)
        {
            return MOVE_NONE;
        }

        return move_convert (pos, move);
    }

    string Book::read_entries (const Position &pos)
    {
        if (!is_open ())
        {
            return "";
        }

        ostringstream oss;

        Key key = pos.pg_key ();

        auto index = find_index (key);

        seekg (OFFSET(index));

        Entry pe;
        vector<Entry> pes;
        u32 weight_sum = 0;
        while (   *this >> pe
               && pe.key == key)
        {
            if (MOVE_NONE == pe.move)
            {
                continue;
            }
            pes.push_back (pe);
            weight_sum += pe.weight;
        }

        if (pes.empty ())
        {
            std::cerr << "ERROR: Position not found... "
                      << std::hex << std::uppercase << key << std::nouppercase << std::dec << std::endl;
        }
        else
        {
            for_each (pes.begin (), pes.end (), [&oss, &weight_sum] (Entry e)
            {
                oss << e << " prob: " << std::setfill ('0') << std::setw (6) << std::setprecision (2) << (weight_sum != 0 ? 100.0 * e.weight / weight_sum : 0.0) << std::setfill (' ') << std::endl;
            });
        }

        return oss.str ();
    }
}
