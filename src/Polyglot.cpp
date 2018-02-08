#include "Polyglot.h"

#include "MoveGenerator.h"
#include "Notation.h"
#include "PRNG.h"
#include "Zobrist.h"

PolyBook Book;

using namespace std;

namespace {

    template<typename T>
    ifstream& operator>> (ifstream &ifs, T &t)
    {
        t = T();
        for (u08 i = 0; i < sizeof (t) && ifs.good (); ++i)
        {
            t = T((t << 8) + u08(ifs.get ()));
        }
        return ifs;
    }
    //template<typename T>
    //ofstream& operator<< (ofstream &ofs, const T &t)
    //{
    //    for (u08 i = 0; i < sizeof (t) && ofs.good (); ++i)
    //    {
    //        ofs.put (u08(t >> (8*(sizeof (t) - 1 - i))));
    //    }
    //    return ofs;
    //}

    ifstream& operator>> (ifstream &ifs, PolyEntry &entry)
    {
        ifs >> entry.key
            >> entry.move
            >> entry.weight
            >> entry.learn;
        return ifs;
    }
    //ofstream& operator<< (ofstream &ofs, const PolyEntry &entry)
    //{
    //    ofs << entry.key
    //        << entry.move
    //        << entry.weight
    //        << entry.learn;
    //    return ofs;
    //}


    Move convert_move (const Position &pos, Move m)
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
        u08 pt = (m >> 12) & MAX_PTYPE;
        // Set new type for promotion piece
        if (0 != pt)
        {
            assert(NIHT <= pt && pt <= QUEN);
            promote (m, PieceType(pt));
        }
        // Add special move flags and verify it is legal
        for (const auto &vm : MoveList<GenType::LEGAL> (pos))
        {
            if ((vm.move & ~PROMOTE) == m)
            {
                return vm.move;
            }
        }

        return MOVE_NONE;
    }

    bool move_draw (Position &pos, Move m)
    {
        StateInfo si;
        pos.do_move (m, si);
        bool dr = pos.draw (64);
        pos.undo_move (m);
        return dr;
    }

}

const u08 PolyEntry::Size = sizeof (PolyEntry);
static_assert (PolyEntry::Size == 16, "Entry size incorrect");

PolyEntry::operator string () const
{
    ostringstream oss;
    oss << " key: " << std::setw (16) << std::setfill ('0') << std::hex << std::uppercase << key << std::nouppercase << std::dec
        << " move: " << std::setw ( 5) << std::setfill (' ') << std::left << move_to_can (Move(move)) << std::right
        << " weight: " << std::setw ( 5) << std::setfill ('0') << weight
        << " learn: " << std::setw ( 2) << std::setfill ('0') << learn
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
    enabled = false;
    filename = Empty;
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
        else //if (key == entries[beg].key)
        {
            beg = std::max (mid - 4, i64(0));
            end = std::min (mid + 4, i64(entry_count));
        }
    }

    while (beg < end)
    {
        if (key == entries[beg].key)
        {
            while (   0 < beg
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
        return;
    }

    filename = book_fn;

    trim (filename);
    convert_path (filename);

    if (white_spaces (filename))
    {
        return;
    }

    ifstream polyglot (filename, ios_base::binary);
    if (!polyglot.is_open ())
    {
        return;
    }

    polyglot.seekg (size_t(0), ios_base::end);
    size_t filesize = polyglot.tellg ();
    polyglot.seekg (size_t(0), ios_base::beg);

    entry_count = (filesize - HeaderSize) / PolyEntry::Size;
    entries = new PolyEntry[entry_count];

    if (0 != HeaderSize)
    {
        PolyEntry dummy;
        for (size_t i = 0; i < HeaderSize / PolyEntry::Size; ++i)
        {
            polyglot >> dummy;
        }
    }
    for (size_t i = 0; i < entry_count; ++i)
    {
        polyglot >> entries[i];
    }
    polyglot.close ();

    sync_cout << "info string Book entries found " << entry_count << " from file \'" << filename << "\'" << sync_endl;
    enabled = true;
}
/// PolyBook::probe() tries to find a book move for the given position.
/// If no move is found returns MOVE_NONE.
/// If pick_best is true returns always the highest rated move,
/// otherwise randomly chooses one, based on the move score.
Move PolyBook::probe (Position &pos)
{
    static PRNG prng (now ());

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
    if (1 > index)
    {
        if (4 < ++fail_counter)
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

    size_t pick1_index = index;
    size_t i = index;
    while (   i < entry_count
           && key == entries[i].key)
    {
        if (MOVE_NONE == entries[i].move) continue;

        ++count;
        max_weight = std::max (entries[i].weight, max_weight);
        weight_sum += entries[i].weight;

        // Choose the move

        if (pick_best)
        {
            if (max_weight == entries[i].weight)
            {
                pick1_index = i;
            }
        }
        else
        {
            // Move with a very high score, has a higher probability of being choosen.
            if (   0 != weight_sum
                && (prng.rand<u32> () % weight_sum) < entries[i].weight)
            {
                pick1_index = i;
            }
        }
        ++i;
    }

    move = Move(entries[pick1_index].move);
    if (MOVE_NONE == move) return move;

    move = convert_move (pos, move);

    if (   !pos.draw (64)
        || 1 >= count)
    {
        return move;
    }

    if (!move_draw (pos, move))
    {
        return move;
    }

    // Special case draw position and more than one moves available

    size_t pick2_index = index;
    if (pick2_index == pick1_index)
    {
        ++pick2_index;
    }

    move = Move(entries[pick2_index].move);
    if (MOVE_NONE == move) return move;

    move = convert_move (pos, move);

    if (!move_draw (pos, move))
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
    if (1 > index)
    {
        return oss.str ();
    }

    list<PolyEntry> list_entries;
    u32 weight_sum = 0;
    while (   size_t(index) < entry_count
           && key == entries[index].key)
    {
        list_entries.push_back (entries[index]);
        weight_sum += entries[index].weight;
        ++index;
    }
    if (!list_entries.empty ())
    {
        list_entries.sort ();
        list_entries.reverse ();
        oss << "\nBook entries: " << list_entries.size ();
        for (auto entry : list_entries)
        {
            entry.move = convert_move (pos, Move(entry.move));
            oss << "\n"
                << entry
                << " prob: "
                << std::setw (7)
                << std::setfill ('0')
                << std::fixed << std::setprecision (4) << (0 != weight_sum ? 100.0 * entry.weight / weight_sum : 0.0)
                << std::setfill (' ');
        }
    }
    return oss.str ();
}
