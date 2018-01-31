#include "Polyglot.h"

#include "MoveGenerator.h"
#include "Notation.h"
#include "PRNG.h"
#include "Zobrist.h"

Polyglot::PolyBook Book;

namespace Polyglot {

    using namespace std;
    using namespace MoveGen;
    using namespace Notation;

    namespace {

        PRNG prng (now ());

        template<typename T>
        ifstream& operator>> (ifstream &ifs, T &t)
        {
            t = T();
            for (u08 i = 0; i < sizeof (t) && ifs.good (); ++i)
            {
                u08 byte = u08(ifs.get ());
                t = T((t << 8) + byte);
            }
            return ifs;
        }
        //template<typename T>
        //ofstream& operator<< (ofstream &ofs, const T &t)
        //{
        //    for (u08 i = 0; i < sizeof (t) && ofs.good (); ++i)
        //    {
        //        u08 byte = u08(t >> (8*(sizeof (t) - 1 - i)));
        //        ofs.put (byte);
        //    }
        //    return ofs;
        //}

        ifstream& operator>> (ifstream &ifs, Entry &pe)
        {
            ifs >> pe.key
                >> pe.move
                >> pe.weight
                >> pe.learn;
            return ifs;
        }
        //ofstream& operator<< (ofstream &ofs, const Entry &pe)
        //{
        //    ofs << pe.key
        //        << pe.move
        //        << pe.weight
        //        << pe.learn;
        //    return ofs;
        //}


        Move convert_move (const Position &pos, Move move)
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
            i08 pt = (move >> 12) & MAX_PTYPE;
            // Set new type for promotion piece
            if (0 != pt)
            {
                assert (NIHT <= pt && pt <= QUEN);
                promote (move, PieceType(pt));
            }
            // Add special move flags and verify it is legal
            for (const auto &vm : MoveList<GenType::LEGAL> (pos))
            {
                if ((vm.move & ~PROMOTE) == move)
                {
                    return vm.move;
                }
            }

            return MOVE_NONE;
        }

        bool is_draw (Position &pos, Move m)
        {
            StateInfo si;
            pos.do_move (m, si);
            bool dr = pos.draw (64);
            pos.undo_move (m);
            return dr;
        }

    }

    const u08 Entry::Size = sizeof (Entry);
    static_assert (Entry::Size == 16, "Entry size incorrect");

    Entry::operator string () const
    {
        ostringstream oss;

        auto m = Move(move);
        // Set new type for promotion piece
        auto pt = PieceType((m >> 12) & MAX_PTYPE);
        if (pt != PAWN)
        {
            promote (m, pt);
            m = Move(m + PROMOTE);
        }
        // TODO:: Add special move flags and verify it is legal
        oss << " key: "    << std::setw (16) << std::setfill ('0') << std::hex << std::uppercase << key << std::nouppercase
            << " move: "   << std::setw ( 5) << std::setfill (' ') << std::left << move_to_can (m) << std::right
            << " weight: " << std::setw ( 5) << std::setfill ('0') << std::dec << weight
            << " learn: "  << std::setw ( 2) << std::setfill ('0') << std::dec << learn
            << std::setfill (' ');

        return oss.str ();
    }


    const size_t PolyBook::HeaderSize = 0;
    static_assert (PolyBook::HeaderSize == 0, "Book header size incorrect");

    PolyBook::PolyBook ()
        : entries (nullptr)
        , entry_count (0)
        , fail_counter (0)
        , do_probe (true)
        , last_pieces (0)
        , last_piece_count (0)
        , use (false)
        , enabled (false)
        , filename ("Book.bin")
        , pick_best (true)
        , move_count (20)
    {
    }

    PolyBook::~PolyBook ()
    {
        clear ();
    }

    void PolyBook::clear ()
    {
        if (nullptr != entries)
        {
            delete[] entries;
            entries = nullptr;
        }
    }

    i64 PolyBook::find_index (const Key key) const
    {
        i64 beg = i64(0);
        i64 end = i64(entry_count);

        while (beg + 8 < end)
        {
            i64 mid = (beg + end) / 2;

            if (key > entries[mid].key)
            {
                beg = mid;
            }
            else
            if (key < entries[mid].key)
            {
                end = mid;
            }
            else
            {
                beg = std::max (mid - 4, i64(0));
                end = std::min (mid + 4, i64(entry_count));
            }
        }

        while (beg < end)
        {
            if (key == entries[beg].key)
            {
                while (   beg > 0
                       && key == entries[beg - 1].key)
                {
                    --beg;
                }
                return beg;
            }
            ++beg;
        }

        return -1;
    }
    //i64 PolyBook::find_index (const Position &pos) const
    //{
    //    return find_index (pos.pg_key ());
    //}
    //i64 PolyBook::find_index (const string &fen, bool c960) const
    //{
    //    StateInfo si;
    //    return find_index (Position ().setup (fen, si, nullptr, c960).pg_key ());
    //}

    bool PolyBook::can_probe (const Position &pos)
    {
        Bitboard pieces = pos.pieces ();
        i32 piece_count = pop_count (pieces);
        
        if (   pieces != last_pieces
            //|| pop_count (pieces ^ last_pieces) > 6
            || piece_count > last_piece_count
            || piece_count < last_piece_count - 2
            || U64(0x3DE128A923B62420) == pos.si->posi_key
            || U64(0x463B96181691FC9C) == pos.pg_key ())
        {
            do_probe = true;
        }

        last_pieces = pieces;
        last_piece_count = piece_count;

        return do_probe;
    }

    void PolyBook::initialize (const string &book_fn)
    {
        clear ();

        if (!use)
        {
            enabled = false;
            return;
        }

        filename = book_fn;

        trim (filename);
        convert_path (filename);

        if (white_spaces (filename))
        {
            enabled = false;
            return;
        }

        ifstream polyglot (filename, ios_base::binary);
        if (!polyglot.is_open ())
        {
            enabled = false;
            return;
        }

        polyglot.seekg (0LL, ios_base::end);
        size_t filesize = polyglot.tellg ();
        polyglot.seekg (0LL, ios_base::beg);

        entry_count = (filesize - HeaderSize) / Entry::Size;
        entries = new Entry[entry_count];

        Entry dummy;
        for (size_t i = 0; i < HeaderSize / Entry::Size; ++i)
        {
            polyglot >> dummy;
        }
        for (size_t i = 0; i < entry_count; ++i)
        {
            polyglot >> entries[i];
        }
        polyglot.close ();

        sync_cout << "info string Book entries found " << entry_count << " from file \'" << book_fn << "\'" << sync_endl;
        enabled = true;
    }
    /// PolyBook::probe() tries to find a book move for the given position.
    /// If no move is found returns MOVE_NONE.
    /// If pick_best is true returns always the highest rated move,
    /// otherwise randomly chooses one, based on the move score.
    Move PolyBook::probe (Position &pos)
    {
        auto move = MOVE_NONE;

        if (   !enabled
            || nullptr == entries
            || (   0 != move_count
                && pos.move_num () > move_count)
            || !can_probe (pos))
        {
            return move;
        }

        auto key = pos.pg_key ();

        auto index = find_index (key);
        if (index < 1)
        {
            if (++fail_counter > 4)
            {
                // Stop probe after 4 times not in the book till position changes according to can_probe()
                do_probe = false;
                fail_counter = 0;
            }

            return move;
        }

        u08 count = 0;
        u16 max_weight = 0;
        u32 weight_sum = 0;

        size_t pick_index1 = index;
        for (size_t i = index; i < entry_count && key == entries[i].key; ++i)
        {
            if (MOVE_NONE == entries[i].move) continue;

            ++count;
            max_weight = std::max (entries[i].weight, max_weight);
            weight_sum += entries[i].weight;

            // Choose book move

            if (pick_best)
            {
                if (entries[i].weight == max_weight)
                {
                    pick_index1 = i;
                }
            }
            // If a move has a very high score it has a higher probability
            // of being choosen than a move with a lower score.
            else
            if (0 != weight_sum)
            {
                u16 rand = prng.rand<u16> () % weight_sum;
                if (entries[i].weight > rand)
                {
                    pick_index1 = i;
                }
            }
        }

        move = Move(entries[pick_index1].move);
        if (MOVE_NONE == move) return move;

        move = convert_move (pos, move);

        if (   !pos.draw (64)
            || 1 >= count)
        {
            return move;
        }

        if (!is_draw (pos, move))
        {
            return move;
        }

        // Special case draw position and more than one moves available

        size_t pick_index2 = index;
        if (pick_index2 == pick_index1)
        {
            ++pick_index2;
        }

        move = Move(entries[pick_index2].move);
        if (MOVE_NONE == move) return move;

        move = convert_move (pos, move);

        if (!is_draw (pos, move))
        {
            return move;
        }

        return MOVE_NONE;
    }

    string PolyBook::show (const Position &pos) const
    {
        ostringstream oss;

        if (   nullptr == entries
            || !enabled)
        {
            return oss.str ();
        }

        auto key = pos.pg_key ();

        auto index = find_index (key);
        if (index < 1)
        {
            return oss.str ();
        }

        list<Entry> pes;
        u32 weight_sum = 0;
        for (size_t i = index; i < entry_count && key == entries[i].key; ++i)
        {
            pes.push_back (entries[i]);
            weight_sum += entries[i].weight;
        }
        if (!pes.empty ())
        {
            pes.sort ();
            pes.reverse ();
            for (auto pe : pes)
            {
                oss << "\n"
                    << pe
                    << " prob: "
                    << std::setw (7)
                    << std::setfill ('0')
                    << std::fixed << std::setprecision (4) << (0 != weight_sum ? 100.0 * pe.weight / weight_sum : 0.0)
                    << std::setfill (' ');
            }
        }
        return oss.str ();
    }

}
