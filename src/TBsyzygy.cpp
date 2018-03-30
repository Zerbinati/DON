#include "TBsyzygy.h"

#include <algorithm>
#include <atomic>
#include <cstdint>
#include <cstring>   // For std::memset and std::memcpy
#include <deque>
#include <fstream>
#include <iostream>
#include <vector>
#include "BitBoard.h"
#include "Engine.h"
#include "MoveGenerator.h"
#include "Notation.h"
#include "Position.h"
#include "Thread.h"

#if defined(_WIN32)
#   if !defined(NOMINMAX)
#       define NOMINMAX // Disable macros min() and max()
#   endif
#   if !defined(WIN32_LEAN_AND_MEAN)
#       define WIN32_LEAN_AND_MEAN
#   endif
#   include <windows.h>
#else
#   include <fcntl.h>
#   include <unistd.h>
#   include <sys/mman.h>
#   include <sys/stat.h>
#endif

namespace TBSyzygy {

    using namespace std;
    using namespace BitBoard;
    using namespace Searcher;

    string PathString = Empty;
    i32    MaxLimitPiece = 0;

    namespace {

        constexpr i32 TBPIECES = 7;

        enum TBType : u08 { WDL, DTZ }; // Used as template parameter

        // Each table has a set of flags: all of them refer to DTZ tables, the last one to WDL tables
        enum TBFlag : u08
        {
            STM = 1,
            MAPPED = 2,
            WIN_PLIES = 4,
            LOSS_PLIES = 8,
            SINGLE_VALUE = 128
        };

        Piece tb_piece (i32 pc) { return 0 != pc ? Piece(pc - 1) : NO_PIECE; }

        // DTZ tables don't store valid scores for moves that reset the rule50 counter
        // like captures and pawn moves but we can easily recover the correct dtz of the
        // previous move if we know the position's WDL score.
        i32 dtz_before_zeroing (WDLScore wdl)
        {
            switch (wdl)
            {
            case WDLScore::LOSS:         return -1;
            case WDLScore::BLESSED_LOSS: return -101;
            case WDLScore::CURSED_WIN:   return +101;
            case WDLScore::WIN:          return +1;
            case WDLScore::DRAW:
            default:                     return 0;
            }
        }

        // Numbers in little endian used by sparse_index[] to point into block_length[]
        struct SparseEntry
        {
            char block[4];   // Number of block
            char offset[2];  // Offset within the block
        };

        static_assert (sizeof (SparseEntry) == 6, "SparseEntry size incorrect");

        typedef u16 Sym; // Huffman symbol

        struct LR
        {
        public:
            enum Side : u08
            {
                Left,
                Right,
                Center
            };

            // The 1st 12 bits is the left-hand symbol,
            // the 2nd 12 bits is the right-hand symbol.
            // If symbol has length 1, then the first byte is the stored value.
            u08 lr[3];
            template<Side S>
            Sym get ()
            {
                return S == Side::Left  ? ((lr[1] & 0xF) << 8) | lr[0] :
                       S == Side::Right ?  (lr[2] << 4) | (lr[1] >> 4) :
                       S == Side::Center ?  lr[0] : (assert(false), -1);
            }
        };

        static_assert (sizeof (LR) == 3, "LR size incorrect");

        struct PairsData
        {
        public:
            i32 flags;
            size_t block_size;              // Block size in bytes
            size_t span;                    // About every span values there is a SparseIndex[] entry
            i32 num_blocks;                 // Number of blocks in the TB file
            i32 max_sym_len;                // Maximum length in bits of the Huffman symbols
            i32 min_sym_len;                // Minimum length in bits of the Huffman symbols
            Sym *lowest_sym;                // lowest_sym[l] is the symbol of length l with the lowest value
            LR *btree;                      // btree[sym] stores the left and right symbols that expand sym
            u16 *block_length;              // Number of stored positions (minus one) for each block: 1..65536
            i32 block_length_size;          // Size of block_length[] table: padded so it's bigger than num_blocks
            SparseEntry *sparse_index;      // Partial indices into block_length[]
            size_t sparse_index_size;       // Size of sparse_index[] table
            u08 *data;                      // Start of Huffman compressed data
            vector<u64> base64;             // base64[l - min_sym_len] is the 64bit-padded lowest symbol of length l
            vector<u08> sym_len;            // Number of values (-1) represented by a given Huffman symbol: 1..256
            Piece pieces[TBPIECES];         // Position pieces: the order of pieces defines the groups
            u64 group_idx[TBPIECES+1];      // Start index used for the encoding of the group's pieces
            i32 group_len[TBPIECES+1];      // Number of pieces in a given group: KRKN -> (3, 1)
            u16 map_idx[4];                 // WDLWin, WDLLoss, WDLCursedWin, WDLBlessedLoss (used in DTZ)
        };

        template<TBType Type>
        struct TBEntry
        {
            typedef typename std::conditional<WDL == Type, WDLScore, i32>::type Result;

            static constexpr i32 Sides = WDL == Type ? 2 : 1;

            std::atomic<bool> ready;
            void* base_address;
            uint8_t* map;
            uint64_t mapping;
            Key key1;
            Key key2;
            i32 piece_count;
            bool has_pawns;
            bool has_unique_pieces;
            uint8_t pawn_count[CLR_NO]; // [Lead color / other color]
            PairsData items[Sides][4]; // [wtm / btm][FILE_A..FILE_D or 0]

            PairsData* get (i32 stm, i32 f)
            {
                return &items[stm % Sides][has_pawns ? f : 0];
            }

            TBEntry () = delete;

            explicit TBEntry (const std::string&);
            explicit TBEntry (const TBEntry<WDL>&);
            ~TBEntry ();
        };

        template<>
        TBEntry<WDL>::TBEntry (const std::string &code)
            : ready (false)
            , base_address (nullptr)
            , map (nullptr)
            , mapping (0)
        {
            StateInfo si;
            Position pos;
            key1 = pos.setup (code, si, WHITE).si->matl_key;
            piece_count = pos.count ();
            has_pawns = 0 != pos.count (PAWN);
            for (auto c : { WHITE, BLACK })
            {    
                for (auto pt : { PAWN, NIHT, BSHP, ROOK, QUEN })
                {
                    if (1 == pos.count (c, pt))
                    {
                        has_unique_pieces = true;
                        goto after_unique_pieces;
                    }
                }
            }
            after_unique_pieces:
            if (has_pawns)
            {
                // Set the leading color. In case both sides have pawns the leading color
                // is the side with less pawns because this leads to better compression.
                auto lead_color = pos.count (BLACK, PAWN) == 0
                               || (   pos.count (WHITE, PAWN) != 0
                                   && pos.count (BLACK, PAWN) >= pos.count (WHITE, PAWN)) ? WHITE : BLACK;

                pawn_count[0] = u08(pos.count ( lead_color, PAWN));
                pawn_count[1] = u08(pos.count (~lead_color, PAWN));
            }
            key2 = pos.setup (code, si, BLACK).si->matl_key;
        }

        template<>
        TBEntry<DTZ>::TBEntry (const TBEntry<WDL> &wdl)
            : ready (false)
            , base_address (nullptr)
            , map (nullptr)
            , mapping (0)
        {
            key1 = wdl.key1;
            key2 = wdl.key2;
            piece_count = wdl.piece_count;
            has_pawns = wdl.has_pawns;
            has_unique_pieces = wdl.has_unique_pieces;

            if (has_pawns)
            {
                pawn_count[0] = wdl.pawn_count[0];
                pawn_count[1] = wdl.pawn_count[1];
            }
        }

        i32 MapPawns[SQ_NO];
        i32 MapB1H1H7[SQ_NO];
        i32 MapA1D1D4[SQ_NO];
        i32 MapKK[10][SQ_NO]; // [MapA1D1D4][SQ_NO]

        /// Comparison function to sort leading pawns in ascending MapPawns[] order
        bool pawns_comp (Square i, Square j)
        {
            return MapPawns[i] < MapPawns[j];
        }
        i32 off_A1H8 (Square sq)
        {
            return i32(_rank (sq)) - i32(_file (sq));
        }

        constexpr Value WDL_To_Value[] =
        {
            -VALUE_MATE + i32(MaxPlies) + 1,
             VALUE_DRAW - 2,
             VALUE_DRAW,
             VALUE_DRAW + 2,
            +VALUE_MATE - i32(MaxPlies) - 1
        };

        i32 Binomial[6][SQ_NO];    // [k][n] k elements from a set of n elements
        i32 LeadPawnIdx[5][SQ_NO]; // [lead_pawn_count][SQ_NO]
        i32 LeadPawnsSize[5][4];   // [lead_pawn_count][F_A..F_D]

        enum Endian
        { 
            BIG,
            LITTLE
        };

        template<typename T, i32 Half = sizeof (T) / 2, i32 End = sizeof (T) - 1>
        inline void swap_byte (T &x)
        {
            auto *c = (char*)(&x);
            for (i32 i = 0; i < Half; ++i)
            {
                auto tmp = c[i];
                c[i] = c[End - i];
                c[End - i] = tmp;
            }
        }
        template<>
        inline void swap_byte<u08, 0, 0> (u08 &)
        {}

        template<typename T, Endian E>
        T number (void *addr)
        {
            constexpr union { u32 i; char c[4]; } e = { 0x01020304 };
            T v;
            if (0 != (uintptr_t(addr) & (alignof(T) -1))) // Unaligned pointer (very rare)
            {
                std::memcpy (&v, addr, sizeof (v));
            }
            else
            {
                v = *((T*)addr);
            }
            if (E != (e.c[0] == 4 ? Endian::LITTLE : Endian::BIG))
            {
                swap_byte (v);
            }
            return v;
        }

        class HashTable
        {
        private:
            static constexpr i32 TBHASHBITS = 10;
            static constexpr i32 HSHMAX = 6;

            typedef pair<Key, pair<TBEntry<WDL>*, TBEntry<DTZ>*>> Entry;

            Entry table[1 << TBHASHBITS][HSHMAX];

            deque<TBEntry<WDL>> wdl_table;
            deque<TBEntry<DTZ>> dtz_table;

            void insert (Key key, TBEntry<WDL> *wdl, TBEntry<DTZ> *dtz)
            {
                for (auto &entry : table[key >> (64 - TBHASHBITS)])
                {
                    if (   nullptr == entry.second.first
                        || key == entry.first)
                    {
                        entry = std::make_pair (key, std::make_pair (wdl, dtz));
                        return;
                    }
                }
                std::cerr << "HSHMAX too low!" << std::endl;
                stop (EXIT_FAILURE);
            }

        public:

            template<TBType Type>
            TBEntry<Type>* get (Key key)
            {
                for (auto &entry : table[key >> (64 - TBHASHBITS)])
                {
                    if (entry.first == key)
                    {
                        return std::get<Type> (entry.second);
                    }
                }
                return nullptr;
            }

            void clear ()
            {
                std::memset (table, 0, sizeof (table));
                wdl_table.clear ();
                dtz_table.clear ();
            }

            size_t size () const { return wdl_table.size (); }
            void insert (const vector<PieceType>&);
        };

        HashTable EntryTable;

        class TBFile
            : public std::ifstream
        {
        public:
            // Look for and open the file among the Paths directories where the .rtbw and .rtbz files can be found.
            static vector<string> Paths;

            string filename;

            TBFile (const string &code, const string &ext)
            {
                auto file = code;
                file.insert (file.find ('K', 1), "v");
                file += ext;
                for (const auto &path : Paths)
                {
                    const auto fname = append_path (path, file);
                    std::ifstream::open (fname);
                    if (std::ifstream::is_open ())
                    {
                        filename = fname;
                        break;
                    }
                }
            }

            // Memory map the file and check it. File should be already open and will be
            // closed after mapping.
            u08* map (void **base_address, u64 *mapping, const u08 *TB_MAGIC)
            {
                assert(!white_spaces (filename) && std::ifstream::is_open ());
                
                std::ifstream::close (); // Need to re-open to get native file descriptor

#           ifndef _WIN32
                i32 fd = ::open (filename.c_str (), O_RDONLY);
                if (-1 == fd)
                {
                    *base_address = nullptr;
                    return nullptr;
                }

                stat statbuf;
                fstat (fd, &statbuf);
                if (0 == statbuf.st_size)
                {
                    std::cerr << "fstat() failed, name = " << filename << std::endl;
                    ::close (fd);
                    stop (EXIT_FAILURE);
                    return nullptr;
                }

                *mapping = statbuf.st_size;
                *base_address = mmap (nullptr, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0);
                ::close (fd);
                if (MAP_FAILED == *base_address)
                {
                    std::cerr << "Could not mmap() " << filename << std::endl;
                    Engine::stop (EXIT_FAILURE);
                }
#           else
                HANDLE fd = CreateFile (filename.c_str (), GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);
                if (INVALID_HANDLE_VALUE == fd)
                {
                    *base_address = nullptr;
                    return nullptr;
                }

                DWORD size_high;
                DWORD size_low = GetFileSize (fd, &size_high);
                HANDLE mmap = CreateFileMapping (fd, nullptr, PAGE_READONLY, size_high, size_low, nullptr);
                CloseHandle (fd);
                if (0 == mmap)
                {
                    std::cerr << "CreateFileMapping() failed, name = " << filename << ", error = " << GetLastError () << std::endl;
                    stop (EXIT_FAILURE);
                    return nullptr;
                }

                *mapping = u64(mmap);
                *base_address = MapViewOfFile (mmap, FILE_MAP_READ, 0, 0, 0);
                if (nullptr == *base_address)
                {
                    std::cerr << "MapViewOfFile() failed, name = " << filename << ", error = " << GetLastError () << std::endl;
                    return nullptr;
                }
#           endif

                auto *data = (u08*)(*base_address);

                if (   *data++ != *TB_MAGIC++
                    || *data++ != *TB_MAGIC++
                    || *data++ != *TB_MAGIC++
                    || *data++ != *TB_MAGIC)
                {
                    std::cerr << "Corrupted table in file " << filename << std::endl;
                    unmap (*base_address, *mapping);
                    *base_address = nullptr;
                    return nullptr;
                }

                return data;
            }

            static void unmap (void *base_address, u64 mapping)
            {
#ifndef _WIN32
                munmap (base_address, mapping);
#else
                UnmapViewOfFile (base_address);
                CloseHandle (HANDLE(mapping));
#endif
            }
        };

        vector<string> TBFile::Paths;

        template<TBType Type>
        TBEntry<Type>::~TBEntry ()
        {
            if (nullptr != base_address)
            {
                TBFile::unmap (base_address, mapping);
            }
        }

        void HashTable::insert (const vector<PieceType> &pieces)
        {
            string code;
            for (auto pt : pieces)
            {
                code += PieceChar[pt];
            }

            TBFile file (code, ".rtbw");
            if (file.is_open ()) // Only WDL file is checked
            {
                file.close ();

                MaxLimitPiece = std::max (i32(pieces.size ()), MaxLimitPiece);

                wdl_table.emplace_back (code);
                dtz_table.emplace_back (wdl_table.back ());

                insert (wdl_table.back ().key1, &wdl_table.back (), &dtz_table.back ());
                insert (wdl_table.back ().key2, &wdl_table.back (), &dtz_table.back ());
            }
        }

        /// TB tables are compressed with canonical Huffman code. The compressed data is divided into
        /// blocks of size d->block_size, and each block stores a variable number of symbols.
        /// Each symbol represents either a WDL or a (remapped) DTZ value, or a pair of other symbols
        /// (recursively). If you keep expanding the symbols in a block, you end up with up to 65536
        /// WDL or DTZ values. Each symbol represents up to 256 values and will correspond after
        /// Huffman coding to at least 1 bit. So a block of 32 bytes corresponds to at most
        /// 32 x 8 x 256 = 65536 values. This maximum is only reached for tables that consist mostly
        /// of draws or mostly of wins, but such tables are actually quite common. In principle, the
        /// blocks in WDL tables are 64 bytes long (and will be aligned on cache lines). But for
        /// mostly-draw or mostly-win tables this can leave many 64-byte blocks only half-filled, so
        /// in such cases blocks are 32 bytes long. The blocks of DTZ tables are up to 1024 bytes long.
        /// The generator picks the size that leads to the smallest table. The "book" of symbols and
        /// Huffman codes is the same for all blocks in the table. A non-symmetric pawnless TB file
        /// will have one table for wtm and one for btm, a TB file with pawns will have tables per
        /// file a,b,c,d also in this case one set for wtm and one for btm.
        i32 decompress_pairs (PairsData *d, u64 idx)
        {
            // Special case where all table positions store the same value
            if (0 != (d->flags & TBFlag::SINGLE_VALUE))
            {
                return d->min_sym_len;
            }

            // First we need to locate the right block that stores the value at index "idx".
            // Because each block n stores block_length[n] + 1 values, the index i of the block
            // that contains the value at position idx is:
            //
            //     for (i = -1, sum = 0; sum <= idx; ++i)
            //         sum += block_length[i + 1] + 1;
            //
            // This can be slow, so we use SparseIndex[] populated with a set of SparseEntry that
            // point to known indices into block_length[]. Namely SparseIndex[k] is a SparseEntry
            // that stores the block_length[] index and the offset within that block of the value
            // with index I(k), where:
            //
            //     I(k) = k * d->span + d->span / 2      (1)

            // First step is to get the 'k' of the I(k) nearest to our idx, using definition (1)
            u32 k = u32(idx / d->span);

            // Then we read the corresponding SparseIndex[] entry
            u32 block  = number<u32, Endian::LITTLE> (&d->sparse_index[k].block);
            i32 offset = number<u16, Endian::LITTLE> (&d->sparse_index[k].offset);

            // Now compute the difference idx - I(k). From definition of k we know that
            //
            //     idx = k * d->span + idx % d->span    (2)
            //
            // So from (1) and (2) we can compute idx - I(K):
            i32 diff = idx % d->span - d->span / 2;

            // Sum the above to offset to find the offset corresponding to our idx
            offset += diff;

            // Move to previous/next block, until we reach the correct block that contains idx,
            // that is when 0 <= offset <= d->block_length[block]
            while (offset < 0)
            {
                offset += d->block_length[--block] + 1;
            }

            while (offset > d->block_length[block])
            {
                offset -= d->block_length[block++] + 1;
            }

            // Finally, we find the start address of our block of canonical Huffman symbols
            u32* ptr = (u32*)(d->data + block * d->block_size);

            // Read the first 64 bits in our block, this is a (truncated) sequence of
            // unknown number of symbols of unknown length but we know the first one
            // is at the beginning of this 64 bits sequence.
            u64 buf64 = number<u64, Endian::BIG> (ptr);
            ptr += 2;
            i32 buf64Size = 64;
            Sym sym;

            while (true)
            {
                i32 len = 0; // This is the symbol length - d->min_sym_len

                // Now get the symbol length. For any symbol s64 of length l right-padded
                // to 64 bits we know that d->base64[l-1] >= s64 >= d->base64[l] so we
                // can find the symbol length iterating through base64[].
                while (buf64 < d->base64[len])
                {
                    ++len;
                }

                // All the symbols of a given length are consecutive integers (numerical
                // sequence property), so we can compute the offset of our symbol of
                // length len, stored at the beginning of buf64.
                sym = Sym((buf64 - d->base64[len]) >> (64 - len - d->min_sym_len));

                // Now add the value of the lowest symbol of length len to get our symbol
                sym += number<Sym, Endian::LITTLE> (&d->lowest_sym[len]);

                // If our offset is within the number of values represented by symbol sym
                if (offset < d->sym_len[sym] + 1)
                {
                    break;
                }

                // ...otherwise update the offset and continue to iterate
                offset -= d->sym_len[sym] + 1;
                len += d->min_sym_len;  // Get the real length
                buf64 <<= len;          // Consume the just processed symbol
                buf64Size -= len;

                if (buf64Size <= 32)
                { 
                    // Refill the buffer
                    buf64Size += 32;
                    buf64 |= u64(number<u32, Endian::BIG> (ptr++)) << (64 - buf64Size);
                }
            }

            // Ok, now we have our symbol that expands into d->sym_len[sym] + 1 symbols.
            // We binary-search for our value recursively expanding into the left and
            // right child symbols until we reach a leaf node where sym_len[sym] + 1 == 1
            // that will store the value we need.
            while (0 != d->sym_len[sym])
            {
                Sym left = d->btree[sym].get<LR::Side::Left> ();

                // If a symbol contains 36 sub-symbols (d->sym_len[sym] + 1 = 36) and
                // expands in a pair (d->sym_len[left] = 23, d->sym_len[right] = 11), then
                // we know that, for instance the ten-th value (offset = 10) will be on
                // the left side because in Recursive Pairing child symbols are adjacent.
                if (offset < d->sym_len[left] + 1)
                {
                    sym = left;
                }
                else
                {
                    offset -= d->sym_len[left] + 1;
                    sym = d->btree[sym].get<LR::Right> ();
                }
            }

            return d->btree[sym].get<LR::Side::Center> ();
        }

        bool check_dtz_stm (TBEntry<WDL>*, i32, File)
        {
            return true;
        }

        bool check_dtz_stm (TBEntry<DTZ> *entry, i32 stm, File f)
        {
            return (entry->get (stm, f)->flags & TBFlag::STM) == stm
                || (   entry->key1 == entry->key2
                    && !entry->has_pawns);
        }

        /// DTZ scores are sorted by frequency of occurrence and then assigned the
        /// values 0, 1, 2, ... in order of decreasing frequency. This is done for each
        //// of the four WDLScore values. The mapping information necessary to reconstruct
        /// the original values is stored in the TB file and read during map[] init.
        WDLScore map_score (TBEntry<WDL>*, File, i32 value, WDLScore)
        {
            return WDLScore(value - 2);
        }

        i32 map_score (TBEntry<DTZ> *entry, File f, i32 value, WDLScore wdl)
        {
            constexpr i32 WDLMap[] = { 1, 3, 0, 2, 0 };

            i32 flags = entry->get (0, f)->flags;
            u08 *map = entry->map;
            u16* idx = entry->get (0, f)->map_idx;
            if (0 != (flags & TBFlag::MAPPED))
            {
                value = map[idx[WDLMap[wdl + 2]] + value];
            }

            // DTZ tables store distance to zero in number of moves or plies. We
            // want to return plies, so we have convert to plies when needed.
            if (   (WDLScore::WIN  == wdl && 0 == (flags & TBFlag::WIN_PLIES))
                || (WDLScore::LOSS == wdl && 0 == (flags & TBFlag::LOSS_PLIES))
                ||  WDLScore::CURSED_WIN == wdl
                ||  WDLScore::BLESSED_LOSS == wdl)
            {
                value *= 2;
            }

            return value + 1;
        }

        /// Compute a unique index out of a position and use it to probe the TB file. To
        /// encode k pieces of same type and color, first sort the pieces by square in
        /// ascending order s1 <= s2 <= ... <= sk then compute the unique index as:
        ///
        ///      idx = Binomial[1][s1] + Binomial[2][s2] + ... + Binomial[k][sk]
        ///
        template<TBType Type, typename T = typename TBEntry<Type>::Result>
        T do_probe_table (const Position &pos, TBEntry<Type> *entry, WDLScore wdl, ProbeState &state)
        {
            Square squares[TBPIECES];
            Piece pieces[TBPIECES];
            u64 idx;
            i32 next = 0, size = 0, lead_pawn_count = 0;
            PairsData *d;
            Bitboard b, lead_pawns = 0;
            File tb_file = F_A;

            bool flip =
                // Black Symmetric
                // A given TB entry like KRK has associated two material keys: KRvK and KvKR.
                // If both sides have the same pieces keys are equal. In this case TB tables
                // only store the 'white to move' case, so if the position to lookup has black
                // to move, we need to switch the color and flip the squares before to lookup.
                        (   pos.active == BLACK
                         && entry->key1 == entry->key2)
                // Black Stronger
                // TB files are calculated for white as stronger side. For instance we have
                // KRvK, not KvKR. A position where stronger side is white will have its
                // material key == entry->key1, otherwise we have to switch the color and
                // flip the squares before to lookup.
                    || (pos.si->matl_key != entry->key1);

            // For pawns, TB files store 4 separate tables according if leading pawn is on
            // file a, b, c or d after reordering. The leading pawn is the one with maximum
            // MapPawns[] value, that is the one most toward the edges and with lowest rank.
            if (entry->has_pawns)
            {
                // In all the 4 tables, pawns are at the beginning of the piece sequence and
                // their color is the reference one. So we just pick the first one.
                Piece pc = flip ?
                            ~Piece(entry->get (0, 0)->pieces[0]) :
                             Piece(entry->get (0, 0)->pieces[0]);

                assert(PAWN == ptype (pc));

                lead_pawns = b = pos.pieces (color (pc), PAWN);
                do
                {
                    squares[size++] = flip ?
                                       ~pop_lsq (b) :
                                        pop_lsq (b);
                }
                while (0 != b);
                lead_pawn_count = size;

                std::swap (squares[0], *std::max_element (squares, squares + lead_pawn_count, pawns_comp));

                tb_file = _file (squares[0]);
                if (tb_file > F_D)
                {
                    tb_file = _file (!squares[0]); // Horizontal flip: SQ_H1 -> SQ_A1
                }
            }

            // DTZ tables are one-sided, i.e. they store positions only for white to
            // move or only for black to move, so check for side to move to be color,
            // early exit otherwise.
            if (   DTZ == Type
                && !check_dtz_stm (entry, flip ? ~pos.active : pos.active, tb_file))
            {
                state = ProbeState::CHANGE_STM;
                return T();
            }

            // Now we are ready to get all the position pieces (but the lead pawns) and
            // directly map them to the correct color and square.
            b = pos.pieces () ^ lead_pawns;
            do
            {
                auto s = pop_lsq (b);
                squares[size] = flip ?
                                 ~s :
                                  s;
                pieces[size] =  flip ?
                                 ~pos[s] :
                                  pos[s];
                ++size;
            }
            while (0 != b);

            assert(size >= 2);

            d = entry->get (pos.active, tb_file);

            // Then we reorder the pieces to have the same sequence as the one stored
            // in pieces[i]: the sequence that ensures the best compression.
            for (i32 i = lead_pawn_count; i < size; ++i)
            {
                for (i32 j = i; j < size; ++j)
                {
                    if (d->pieces[i] == pieces[j])
                    {
                        std::swap (pieces[i], pieces[j]);
                        std::swap (squares[i], squares[j]);
                        break;
                    }
                }
            }

            // Now we map again the squares so that the square of the lead piece is in
            // the triangle A1-D1-D4.
            if (_file (squares[0]) > F_D)
            {
                for (i32 i = 0; i < size; ++i)
                {
                    squares[i] = !squares[i];
                }
            }

            // Encode leading pawns starting with the one with minimum MapPawns[] and
            // proceeding in ascending order.
            if (entry->has_pawns)
            {
                idx = LeadPawnIdx[lead_pawn_count][squares[0]];

                std::sort (squares + 1, squares + lead_pawn_count, pawns_comp);

                for (i32 i = 1; i < lead_pawn_count; ++i)
                {
                    idx += Binomial[i][MapPawns[squares[i]]];
                }
            }
            // In positions without pawns:
            else
            {
                // Flip the squares to ensure leading piece is below R_5.
                if (_rank (squares[0]) > R_4)
                {
                    for (i32 i = 0; i < size; ++i)
                    {
                        squares[i] = ~squares[i];
                    }
                }
                // Look for the first piece of the leading group not on the A1-D4 diagonal
                // and ensure it is mapped below the diagonal.
                for (i32 i = 0; i < d->group_len[0]; ++i)
                {
                    if (!off_A1H8 (squares[i]))
                    {
                        continue;
                    }

                    if (off_A1H8 (squares[i]) > 0) // A1-H8 diagonal flip: SQ_A3 -> SQ_C3
                    {
                        for (i32 j = i; j < size; ++j)
                        {
                            squares[j] = Square(((squares[j] >> 3) | (squares[j] << 3)) & 63);
                        }
                    }
                    break;
                }

                // Encode the leading group.
                //
                // Suppose we have KRvK. Let's say the pieces are on square numbers wK, wR
                // and bK (each 0...63). The simplest way to map this position to an index
                // is like this:
                //
                //   index = wK * 64 * 64 + wR * 64 + bK;
                //
                // But this way the TB is going to have 64*64*64 = 262144 positions, with
                // lots of positions being equivalent (because they are mirrors of each
                // other) and lots of positions being invalid (two pieces on one square,
                // adjacent kings, etc.).
                // Usually the first step is to take the wK and bK together. There are just
                // 462 ways legal and not-mirrored ways to place the wK and bK on the board.
                // Once we have placed the wK and bK, there are 62 squares left for the wR
                // Mapping its square from 0..63 to available squares 0..61 can be done like:
                //
                //   wR -= (wR > wK) + (wR > bK);
                //
                // In words: if wR "comes later" than wK, we deduct 1, and the same if wR
                // "comes later" than bK. In case of two same pieces like KRRvK we want to
                // place the two Rs "together". If we have 62 squares left, we can place two
                // Rs "together" in 62 * 61 / 2 ways (we divide by 2 because rooks can be
                // swapped and still get the same position.)
                //
                // In case we have at least 3 unique pieces (included kings) we encode them together.
                if (entry->has_unique_pieces)
                {
                    i32 adjust1 =  squares[1] > squares[0];
                    i32 adjust2 = (squares[2] > squares[0]) + (squares[2] > squares[1]);

                    // First piece is below a1-h8 diagonal. MapA1D1D4[] maps the b1-d1-d3
                    // triangle to 0...5. There are 63 squares for second piece and and 62
                    // (mapped to 0...61) for the third.
                    if (off_A1H8 (squares[0]))
                    {
                        idx = MapA1D1D4[squares[0]]  * 63 * 62
                            + (squares[1] - adjust1) * 62
                            +  squares[2] - adjust2;
                    }
                    // First piece is on a1-h8 diagonal, second below: map this occurrence to
                    // 6 to differentiate from the above case, rank() maps a1-d4 diagonal
                    // to 0...3 and finally MapB1H1H7[] maps the b1-h1-h7 triangle to 0..27.
                    else
                    if (off_A1H8 (squares[1]))
                    {
                        idx = 6 * 63 * 62
                            + _rank (squares[0]) * 28 * 62
                            + MapB1H1H7[squares[1]] * 62
                            + squares[2] - adjust2;
                    }
                    // First two pieces are on a1-h8 diagonal, third below
                    else
                    if (off_A1H8 (squares[2]))
                    {
                        idx =  6 * 63 * 62 + 4 * 28 * 62
                            +  _rank (squares[0]) * 7 * 28
                            + (_rank (squares[1]) - adjust1) * 28
                            +  MapB1H1H7[squares[2]];
                    }
                    // All 3 pieces on the diagonal a1-h8
                    else
                    {
                        idx = 6 * 63 * 62 + 4 * 28 * 62 + 4 * 7 * 28
                            +  _rank (squares[0]) * 7 * 6
                            + (_rank (squares[1]) - adjust1) * 6
                            + (_rank (squares[2]) - adjust2);
                    }
                }
                else
                {
                    // We don't have at least 3 unique pieces, like in KRRvKBB, just map the kings.
                    idx = MapKK[MapA1D1D4[squares[0]]][squares[1]];
                }
            }

            idx *= d->group_idx[0];
            auto *group_sq = squares + d->group_len[0];

            // Encode remaining pawns then pieces according to square, in ascending order
            bool pawn_remain = entry->has_pawns && 0 != entry->pawn_count[1];

            while (0 < d->group_len[++next])
            {
                assert(0 <= d->group_len[next] && d->group_len[next] <= 6);
                std::sort (group_sq, group_sq + d->group_len[next]);
                u64 n = 0;

                // Map down a square if "comes later" than a square in the previous
                // groups (similar to what done earlier for leading group pieces).
                for (i32 i = 0; i < d->group_len[next]; ++i)
                {
                    auto adjust = std::count_if (squares, group_sq, [&](Square s) { return group_sq[i] > s; });
                    n += Binomial[i + 1][group_sq[i] - adjust - 8 * (pawn_remain ? 1 : 0)];
                }

                pawn_remain = false;
                idx += n * d->group_idx[next];
                group_sq += d->group_len[next];
            }

            // Now that we have the index, decompress the pair and get the score
            return map_score (entry, tb_file, decompress_pairs (d, idx), wdl);
        }

        /// Group together pieces that will be encoded together. The general rule is that
        /// a group contains pieces of same type and color. The exception is the leading
        /// group that, in case of positions without pawns, can be formed by 3 different
        /// pieces (default) or by the king pair when there is not a unique piece apart
        /// from the kings. When there are pawns, pawns are always first in pieces[].
        ///
        /// As example KRKN -> KRK + N, KNNK -> KK + NN, KPPKP -> P + PP + K + K
        ///
        /// The actual grouping depends on the TB generator and can be inferred from the
        /// sequence of pieces in piece[] array.
        template<TBType Type>
        void set_groups (TBEntry<Type> &e, PairsData *d, i32 *order, File f)
        {
            i32 n = 0;
            i32 firstLen = e.has_pawns ? 0 : e.has_unique_pieces ? 3 : 2;
            d->group_len[n] = 1;

            // Number of pieces per group is stored in group_len[], for instance in KRKN
            // the encoder will default on '111', so group_len[] will be (3, 1).
            for (i32 i = 1; i < e.piece_count; ++i)
            {
                if (   --firstLen > 0
                    || d->pieces[i] == d->pieces[i - 1])
                {
                    d->group_len[n]++;
                }
                else
                {
                    d->group_len[++n] = 1;
                }
            }
            d->group_len[++n] = 0; // Zero-terminated

            // The sequence in pieces[] defines the groups, but not the order in which
            // they are encoded. If the pieces in a group g can be combined on the board
            // in N(g) different ways, then the position encoding will be of the form:
            //
            //           g1 * N(g2) * N(g3) + g2 * N(g3) + g3
            //
            // This ensures unique encoding for the whole position. The order of the
            // groups is a per-table parameter and could not follow the canonical leading
            // pawns/pieces -> remainig pawns -> remaining pieces. In particular the
            // first group is at order[0] position and the remaining pawns, when present,
            // are at order[1] position.
            bool pp = e.has_pawns && e.pawn_count[1]; // Pawns on both sides
            i32 next = pp ? 2 : 1;
            i32 free_squares = 64 - d->group_len[0] - (pp ? d->group_len[1] : 0);
            u64 idx = 1;

            for (i32 k = 0; next < n || k == order[0] || k == order[1]; ++k)
            {
                if (k == order[0]) // Leading pawns or pieces
                {
                    d->group_idx[0] = idx;
                    idx *= e.has_pawns ?
                            LeadPawnsSize[d->group_len[0]][f] :
                            e.has_unique_pieces ?
                                31332 :
                                462;
                }
                else
                if (k == order[1]) // Remaining pawns
                {
                    d->group_idx[1] = idx;
                    idx *= Binomial[d->group_len[1]][48 - d->group_len[0]];
                }
                else // Remaining pieces
                {
                    d->group_idx[next] = idx;
                    idx *= Binomial[d->group_len[next]][free_squares];
                    free_squares -= d->group_len[next++];
                }
            }
            d->group_idx[n] = idx;
        }

        /// In Recursive Pairing each symbol represents a pair of children symbols. So
        /// read d->btree[] symbols data and expand each one in his left and right child
        /// symbol until reaching the leafs that represent the symbol value.
        u08 set_symlen (PairsData *d, Sym s, vector<bool> &visited)
        {
            visited[s] = true; // We can set it now because tree is acyclic
            Sym sr = d->btree[s].get<LR::Side::Right> ();

            if (sr == 0xFFF)
            {
                return 0;
            }

            Sym sl = d->btree[s].get<LR::Side::Left> ();

            if (!visited[sl])
            {
                d->sym_len[sl] = set_symlen (d, sl, visited);
            }
            if (!visited[sr])
            {
                d->sym_len[sr] = set_symlen (d, sr, visited);
            }

            return d->sym_len[sl] + d->sym_len[sr] + 1;
        }

        u08* set_sizes (PairsData *d, u08 *data)
        {
            d->flags = *data++;

            if (0 != (d->flags & TBFlag::SINGLE_VALUE))
            {
                d->num_blocks = 0;
                d->span = 0;
                d->block_length_size = 0;
                d->sparse_index_size = 0;
                d->min_sym_len = *data++; // Here we store the single value
                return data;
            }

            // group_len[] is a zero-terminated list of group lengths, the last group_idx[]
            // element stores the biggest index that is the tb size.
            u64 tb_size = d->group_idx[std::find (d->group_len, d->group_len + 7, 0) - d->group_len];

            d->block_size = 1ULL << *data++;
            d->span = 1ULL << *data++;
            d->sparse_index_size = (tb_size + d->span - 1) / d->span; // Round up
            i32 padding = number<u08, Endian::LITTLE> (data++);
            d->num_blocks = number<u32, Endian::LITTLE> (data); data += sizeof (u32);
            d->block_length_size = d->num_blocks + padding; // Padded to ensure SparseIndex[] does not point out of range.
            d->max_sym_len = *data++;
            d->min_sym_len = *data++;
            d->lowest_sym = (Sym*)(data);
            d->base64.resize (d->max_sym_len - d->min_sym_len + 1);

            // The canonical code is ordered such that longer symbols (in terms of
            // the number of bits of their Huffman code) have lower numeric value,
            // so that d->lowest_sym[i] >= d->lowest_sym[i+1] (when read as LittleEndian).
            // Starting from this we compute a base64[] table indexed by symbol length
            // and containing 64 bit values so that d->base64[i] >= d->base64[i+1].
            // See http://www.eecs.harvard.edu/~michaelm/E210/huffman.pdf
            for (i32 i = i32(d->base64.size () - 2); i >= 0; --i)
            {
                d->base64[i] = (  d->base64[i + 1]
                                + number<Sym, Endian::LITTLE> (&d->lowest_sym[i])
                                - number<Sym, Endian::LITTLE> (&d->lowest_sym[i + 1])) / 2;

                assert(d->base64[i] * 2 >= d->base64[i+1]);
            }

            // Now left-shift by an amount so that d->base64[i] gets shifted 1 bit more
            // than d->base64[i+1] and given the above assert condition, we ensure that
            // d->base64[i] >= d->base64[i+1]. Moreover for any symbol s64 of length i
            // and right-padded to 64 bits holds d->base64[i-1] >= s64 >= d->base64[i].
            for (size_t i = 0; i < d->base64.size (); ++i)
            {
                d->base64[i] <<= 64 - i - d->min_sym_len; // Right-padding to 64 bits
            }
            data += d->base64.size () * sizeof (Sym);
            d->sym_len.resize (number<u16, Endian::LITTLE> (data)); data += sizeof (u16);
            d->btree = (LR*) data;

            // The compression scheme used is "Recursive Pairing", that replaces the most
            // frequent adjacent pair of symbols in the source message by a new symbol,
            // reevaluating the frequencies of all of the symbol pairs with respect to
            // the extended alphabet, and then repeating the process.
            // See http://www.larsson.dogma.net/dcc99.pdf
            vector<bool> visited (d->sym_len.size ());

            for (Sym sym = 0; sym < d->sym_len.size (); ++sym)
            {
                if (!visited[sym])
                {
                    d->sym_len[sym] = set_symlen (d, sym, visited);
                }
            }
            return data + d->sym_len.size () * sizeof (LR) + (d->sym_len.size () & 1);
        }

        u08* set_dtz_map (TBEntry<WDL> &, u08 *, File)
        {
            return nullptr;
        }

        u08* set_dtz_map (TBEntry<DTZ> &e, u08 *data, File max_file)
        {
            e.map = data;
            for (auto f = F_A; f <= max_file; ++f)
            {
                if (0 != (e.get (0, f)->flags & TBFlag::MAPPED))
                {
                    for (i32 i = 0; i < 4; ++i)
                    { 
                        // Sequence like 3,x,x,x,1,x,0,2,x,x
                        e.get (0, f)->map_idx[i] = u16(data - e.map + 1);
                        data += *data + 1;
                    }
                }
            }
            return data += uintptr_t(data) & 1; // Word alignment
        }

        template<TBType Type>
        void do_init (TBEntry<Type> &e,  u08 *data)
        {
            assert(e.has_pawns        == !!(*data & 2)); // HasPawns
            assert((e.key1 != e.key2) == !!(*data & 1)); // Split

            data++; // First byte stores flags

            const i32  Sides = WDL == Type && (e.key1 != e.key2) ? 2 : 1;
            const File MaxFile = e.has_pawns ? F_D : F_A;

            bool pp = e.has_pawns && 0 != e.pawn_count[1]; // Pawns on both sides

            assert(!pp || 0 != e.pawn_count[0]);

            for (auto f = F_A; f <= MaxFile; ++f)
            {
                for (i32 i = 0; i < Sides; ++i)
                {
                    *e.get (i, f) = PairsData ();
                }

                i32 order[][2] = 
                { 
                    { *data & 0xF, pp ? *(data + 1) & 0xF : 0xF },
                    { *data >>  4, pp ? *(data + 1) >>  4 : 0xF }
                };
                data += 1 + (pp ? 1 : 0);

                for (i32 k = 0; k < e.piece_count; ++k, ++data)
                {
                    for (i32 i = 0; i < Sides; ++i)
                    {
                        e.get (i, f)->pieces[k] = tb_piece (i ? *data >>  4 : *data & 0xF);
                    }
                }

                for (i32 i = 0; i < Sides; ++i)
                {
                    set_groups (e, e.get (i, f), order[i], f);
                }
            }

            data += uintptr_t(data) & 1; // Word alignment

            for (auto f = F_A; f <= MaxFile; ++f)
            {
                for (i32 i = 0; i < Sides; ++i)
                {
                    data = set_sizes (e.get (i, f), data);
                }
            }
            if (DTZ == Type)
            {
                data = set_dtz_map (e, data, MaxFile);
            }

            PairsData *d;
            for (auto f = F_A; f <= MaxFile; ++f)
            {
                for (i32 i = 0; i < Sides; ++i)
                {
                    (d = e.get (i, f))->sparse_index = (SparseEntry*)(data);
                    data += d->sparse_index_size * sizeof (SparseEntry);
                }
            }
            for (auto f = F_A; f <= MaxFile; ++f)
            {
                for (i32 i = 0; i < Sides; ++i)
                {
                    (d = e.get (i, f))->block_length = (u16*)(data);
                    data += d->block_length_size * sizeof (u16);
                }
            }
            for (auto f = F_A; f <= MaxFile; ++f)
            {
                for (i32 i = 0; i < Sides; ++i)
                {
                    data = (u08*)((uintptr_t(data) + 0x3F) & ~0x3F); // 64 byte alignment
                    (d = e.get (i, f))->data = data;
                    data += d->num_blocks * d->block_size;
                }
            }
        }

        template<TBType Type>
        void* init (TBEntry<Type> &e, const Position &pos)
        {
            static Mutex mutex;

            // Avoid a thread reads 'ready' == true while another is still in do_init(),
            // this could happen due to compiler reordering.
            if (e.ready.load (std::memory_order::memory_order_acquire))
            {
                return e.base_address;
            }

            std::unique_lock<Mutex> lk (mutex);

            if (e.ready.load (std::memory_order::memory_order_relaxed)) // Recheck under lock
            {
                return e.base_address;
            }

            // Pieces strings in decreasing order for each color, like ("KPP","KR")
            string w, b;
            for (auto pt : { KING, QUEN, ROOK, BSHP, NIHT, PAWN })
            {
                w += string(pos.count (WHITE, pt), PieceChar[pt]);
                b += string(pos.count (BLACK, pt), PieceChar[pt]);
            }

            constexpr u08 TB_MAGIC[][4] =
            {
                { 0xD7, 0x66, 0x0C, 0xA5 },
                { 0x71, 0xE8, 0x23, 0x5D }
            };

            TBFile file ((e.key1 == pos.si->matl_key ? w + b : b + w), WDL == Type ? ".rtbw" : ".rtbz");
            u08 *data = file.map (&e.base_address, &e.mapping, TB_MAGIC[WDL == Type ? 1 : 0]);
            if (nullptr != data)
            {
                do_init (e, data);
            }
            e.ready.store (true, std::memory_order::memory_order_release);
            return e.base_address;
        }

        template<TBType Type, typename T = typename TBEntry<Type>::Result>
        T probe_table (const Position &pos, ProbeState &state, WDLScore wdl = WDLScore::DRAW)
        {
            if (0 == (pos.pieces () ^ pos.pieces (KING)))
            {
                return T(WDLScore::DRAW); // KvK
            }

            TBEntry<Type> *entry = EntryTable.get<Type> (pos.si->matl_key);

            if (   nullptr == entry
                || nullptr == init (*entry, pos))
            {
                state = ProbeState::FAILURE;
                return T();
            }

            return do_probe_table (pos, entry, wdl, state);
        }

        /// For a position where the side to move has a winning capture it is not necessary
        /// to store a winning value so the generator treats such positions as "don't cares"
        /// and tries to assign to it a value that improves the compression ratio. Similarly,
        /// if the side to move has a drawing capture, then the position is at least drawn.
        /// If the position is won, then the TB needs to store a win value. But if the
        /// position is drawn, the TB may store a loss value if that is better for compression.
        /// All of this means that during probing, the engine must look at captures and probe
        /// their results and must probe the position itself. The "best" state of these
        /// probes is the correct state for the position.
        /// DTZ table don't store values when a following move is a zeroing winning move
        /// (winning capture or winning pawn move). Also DTZ store wrong values for positions
        /// where the best move is an ep-move (even if losing). So in all these cases set
        /// the state to ZEROING_BEST_MOVE.
        WDLScore search (Position &pos, ProbeState &state, bool chech_zeroing)
        {
            auto move_list = MoveList<GenType::LEGAL> (pos);
            size_t move_count = 0;
            auto best_wdl = WDLScore::LOSS;
            StateInfo si;
            for (const auto &move : move_list)
            {
                if (   !pos.capture (move)
                    && (   !chech_zeroing
                        || PAWN != ptype (pos[org_sq (move)])))
                {
                    continue;
                }

                ++move_count;

                pos.do_move (move, si);
                auto wdl = -search (pos, state, false);
                pos.undo_move (move);

                if (ProbeState::FAILURE == state)
                {
                    return WDLScore::DRAW;
                }

                if (best_wdl < wdl)
                {
                    best_wdl = wdl;

                    if (wdl >= WDLScore::WIN)
                    {
                        state = ProbeState::ZEROING_BEST_MOVE; // Winning DTZ-zeroing move
                        return wdl;
                    }
                }
            }

            // In case we have already searched all the legal moves we don't have to probe
            // the TB because the stored score could be wrong. For instance TB tables
            // do not contain information on position with Enpassant rights, so in this case
            // the state of probe_wdl_table is wrong. Also in case of only capture
            // moves, for instance here 4K3/4q3/6p1/2k5/6p1/8/8/8 w - - 0 7, we have to
            // return with ZEROING_BEST_MOVE set.
            const bool completed = (   0 != move_list.size ()
                                    && move_count == move_list.size ());

            WDLScore wdl;
            if (completed)
            {
                wdl = best_wdl;
            }
            else
            {
                wdl = probe_table<WDL> (pos, state);
                if (ProbeState::FAILURE == state)
                {
                    return WDLScore::DRAW;
                }
            }

            // DTZ stores a "don't care" wdl if best_wdl is a win
            if (best_wdl >= wdl)
            {
                state = best_wdl > WDLScore::DRAW
                     || completed ?
                            ProbeState::ZEROING_BEST_MOVE :
                            ProbeState::SUCCESS;
                return best_wdl;
            }
            
            state = ProbeState::SUCCESS;
            return wdl;
        }

        /// Check whether there has been at least one repetition of position since the last capture or pawn move.
        bool has_repeated (StateInfo *si)
        {
            while (nullptr != si)
            {
                auto p = std::min (si->clock_ply, si->null_ply);
                if (p < 4)
                {
                    break;
                }

                const auto *psi = si->ptr->ptr;
                do
                {
                    psi = psi->ptr->ptr;
                    // Check first repetition
                    if (psi->posi_key == si->posi_key)
                    {
                        return true;
                    }
                    p -= 2;
                }
                while (p >= 4);
                si = si->ptr;
            }
            return false;
        }

    } // namespace

    /// Probe the DTZ table for a particular position.
    /// If *result != FAILURE, the probe was successful.
    /// The return value is from the point of view of the side to move:
    ///         n < -100 : loss, but draw under 50-move rule
    /// -100 <= n < -1   : loss in n ply (assuming 50-move counter == 0)
    ///         0        : draw
    ///     1 < n <= 100 : win in n ply (assuming 50-move counter == 0)
    ///   100 < n        : win, but draw under 50-move rule
    ///
    /// The return value n can be off by 1: a return value -n can mean a loss
    /// in n+1 ply and a return value +n can mean a win in n+1 ply. This
    /// cannot happen for tables with positions exactly on the "edge" of
    /// the 50-move rule.
    ///
    /// This implies that if dtz > 0 is returned, the position is certainly
    /// a win if dtz + 50-move-counter <= 99. Care must be taken that the engine
    /// picks moves that preserve dtz + 50-move-counter <= 99.
    ///
    /// If n = 100 immediately after a capture or pawn move, then the position
    /// is also certainly a win, and during the whole phase until the next
    /// capture or pawn move, the inequality to be preserved is
    /// dtz + 50-move counter <= 100.
    ///
    /// In short, if a move is available resulting in dtz + 50-move-counter <= 99,
    /// then do not accept moves leading to dtz + 50-move-counter == 100.
    i32      probe_dtz (Position &pos, ProbeState &state)
    {
        auto wdl = search (pos, state, true);

        if (   ProbeState::FAILURE == state
            || WDLScore::DRAW == wdl) // DTZ tables don't store draws
        {
            return 0;
        }

        // DTZ stores a 'don't care' value in this case, or even a plain wrong
        // one as in case the best move is a losing Enpassant, so it cannot be probed.
        if (ProbeState::ZEROING_BEST_MOVE == state)
        {
            return dtz_before_zeroing (wdl);
        }

        i32 dtz = probe_table<DTZ> (pos, state, wdl);

        if (ProbeState::FAILURE == state)
        {
            return 0;
        }

        if (ProbeState::CHANGE_STM != state)
        {
            return (dtz + 100 * (   WDLScore::BLESSED_LOSS == wdl
                                 || WDLScore::CURSED_WIN   == wdl)) * sign (wdl);
        }

        // DTZ stores results for the other side, so we need to do a 1-ply search and
        // find the winning move that minimizes DTZ.
        StateInfo si;
        i32 min_dtz = 0xFFFF;

        for (const auto &vm : MoveList<GenType::LEGAL> (pos))
        {
            bool zeroing = pos.capture (vm.move)
                        || PAWN == ptype (pos[org_sq (vm.move)]);

            pos.do_move (vm.move, si);

            // For zeroing moves we want the dtz of the move _before_ doing it,
            // otherwise we will get the dtz of the next move sequence. Search the
            // position after the move to get the score sign (because even in a
            // winning position we could make a losing capture or going for a draw).
            dtz = zeroing ?
                    -dtz_before_zeroing (search (pos, state, false)) :
                    -probe_dtz (pos, state);

            pos.undo_move (vm.move);

            if (ProbeState::FAILURE == state)
            {
                return 0;
            }

            // Convert state from 1-ply search. Zeroing moves are already accounted
            // by dtz_before_zeroing() that returns the DTZ of the previous move.
            if (!zeroing)
            {
                dtz += sign (dtz);
            }

            // Skip the draws and if we are winning only pick positive dtz
            if (sign (dtz) == sign (wdl))
            {
                min_dtz = std::min (dtz, min_dtz);
            }
        }

        // Special handle a mate position, when there are no legal moves, in this
        // case return value is somewhat arbitrary, so stick to the original TB code
        // that returns -1 in this case.
        return min_dtz == 0xFFFF ? -1 : min_dtz;
    }

    /// Probe the WDL table for a particular position.
    /// If *result != FAILURE, the probe was successful.
    /// The return value is from the point of view of the side to move:
    /// -2 : loss
    /// -1 : loss, but draw under 50-move rule
    ///  0 : draw
    ///  1 : win, but draw under 50-move rule
    ///  2 : win
    WDLScore probe_wdl (Position &pos, ProbeState &state)
    {
        return search (pos, state, false);
    }

    /// Use the DTZ tables to filter out moves that don't preserve the win or draw.
    /// If the position is lost, but DTZ is fairly high, only keep moves that
    /// maximize DTZ.
    ///
    /// A return value false indicates that not all probes were successful and that
    /// no moves were filtered out.
    bool root_probe_dtz (Position &root_pos, RootMoves &root_moves, Value &value)
    {
        assert(0 != root_moves.size ());

        ProbeState state;
        const auto dtz = probe_dtz (root_pos, state);
        if (ProbeState::FAILURE == state)
        {
            return false;
        }

        StateInfo si;
        si.ptr = nullptr;

        // Probe each move
        for (auto &rm : root_moves)
        {
            const auto move = rm[0];
            root_pos.do_move (move, si);

            i32 v = 0;
            if (   0 != root_pos.si->checkers
                && dtz > 0)
            {
                if (0 == MoveList<GenType::LEGAL> (root_pos).size ())
                {
                    v = 1;
                }
            }

            if (v == 0)
            {
                if (0 != si.clock_ply)
                {
                    v = -probe_dtz (root_pos, state);

                    if (v > 0)
                    {
                        ++v;
                    }
                    else
                    if (v < 0)
                    {
                        --v;
                    }
                }
                else
                {
                    v = dtz_before_zeroing (-probe_wdl (root_pos, state));
                }
            }

            root_pos.undo_move (move);

            if (ProbeState::FAILURE == state)
            {
                return false;
            }

            rm.new_value = Value(v);
        }

        // Obtain 50-move counter for the root position.
        i32 clock_ply = nullptr != si.ptr ? si.ptr->clock_ply : 0;
        // Use 50-move counter to determine whether the root position is won, lost or drawn.
        WDLScore wdl;
        if (0 < dtz)
        {
            wdl = +dtz + clock_ply <= 100 ?
                    WDLScore::WIN :
                    WDLScore::CURSED_WIN;
        }
        else
        if (0 > dtz)
        {
            wdl = -dtz + clock_ply <= 100 ?
                    WDLScore::LOSS :
                    WDLScore::BLESSED_LOSS;
        }
        else
        {
            wdl = WDLScore::DRAW;
        }

        // Determine the score to report to the user.

        // If the position is winning or losing, but too few moves left, adjust the
        // score to show how close it is to winning or losing.
        // NOTE: i32(VALUE_EG_PAWN) is used as scaling factor in score_to_uci().
        if (   WDLScore::CURSED_WIN == wdl
            && +100 >= dtz)
        {
            value = Value(+((200 - dtz - clock_ply) * i32(VALUE_EG_PAWN)) / 200);
        }
        else
        if (   WDLScore::BLESSED_LOSS == wdl
            && -100 <= dtz)
        {
            value = Value(-((200 + dtz - clock_ply) * i32(VALUE_EG_PAWN)) / 200);
        }
        else
        {
            value = WDL_To_Value[wdl + 2];
        }

        // Now be a bit smart about filtering out moves.
        size_t size = 0;
        // Winning (or 50-move rule draw)
        if (0 < dtz)
        {
            i32 best = 0xFFFF;
            // Probe each move
            for (const auto &rm : root_moves)
            {
                if (0 < rm.new_value && rm.new_value < best)
                {
                    best = rm.new_value;
                }
            }

            i32 max = best;

            // If the current phase has not seen repetitions, then try all moves
            // that stay safely within the 50-move budget, if there are any.
            if (   best + clock_ply <= 99
                && !has_repeated (si.ptr))
            {
                max = 99 - clock_ply;
            }

            for (size_t i = 0; i < root_moves.size (); ++i)
            {
                if (0 < root_moves[i].new_value && root_moves[i].new_value <= max)
                {
                    root_moves[size++] = root_moves[i];
                }
            }
        }
        else
        // Losing (or 50-move rule draw)
        if (0 > dtz)
        {
            i32 best = 0;
            // Probe each move
            for (const auto &rm : root_moves)
            {
                if (best > rm.new_value)
                {
                    best = rm.new_value;
                }
            }

            // Try all moves, unless we approach or have a 50-move rule draw.
            if (-best * 2 + clock_ply < 100)
            {
                return true;
            }

            for (size_t i = 0; i < root_moves.size (); ++i)
            {
                if (best == root_moves[i].new_value)
                {
                    root_moves[size++] = root_moves[i];
                }
            }
        }
        // Drawing
        else
        {
            // Try all moves that preserve the draw.
            for (size_t i = 0; i < root_moves.size (); ++i)
            {
                if (0 == root_moves[i].new_value)
                {
                    root_moves[size++] = root_moves[i];
                }
            }
        }
        root_moves.resize (size);

        return true;
    }

    /// Use the WDL tables to filter out moves that don't preserve the win or draw.
    /// This is a fall back for the case that some or all DTZ tables are missing.
    ///
    /// A return value false indicates that not all probes were successful and that
    /// no moves were filtered out.
    bool root_probe_wdl (Position &root_pos, RootMoves &root_moves, Value &value)
    {
        ProbeState state;

        const auto wdl = probe_wdl (root_pos, state);

        if (ProbeState::FAILURE == state)
        {
            return false;
        }

        value = WDL_To_Value[wdl + 2];

        StateInfo si;

        i32 best = WDLScore::LOSS;

        // Probe each move
        for (auto &rm : root_moves)
        {
            const auto move = rm[0];
            root_pos.do_move (move, si);
            i32 v = -probe_wdl (root_pos, state);
            root_pos.undo_move (move);

            if (ProbeState::FAILURE == state)
            {
                return false;
            }

            rm.new_value = Value(v);

            if (best < v)
            {
                best = v;
            }
        }

        size_t size = 0;
        for (size_t i = 0; i < root_moves.size (); ++i)
        {
            if (best == root_moves[i].new_value)
            {
                root_moves[size++] = root_moves[i];
            }
        }
        root_moves.resize (size);

        return true;
    }

    /// Initializes TB
    void initialize ()
    {
        static bool initialized = false;
        if (!initialized)
        {
            // MapB1H1H7[] encodes a square below a1-h8 diagonal to 0..27
            i32 code = 0;
            for (auto s : SQ)
            {
                if (off_A1H8 (s) < 0)
                {
                    MapB1H1H7[s] = code++;
                }
            }
            // MapA1D1D4[] encodes a square in the a1-d1-d4 triangle to 0..9
            code = 0;
            vector<Square> diagonal;
            for (auto s : { SQ_A1, SQ_B1, SQ_C1, SQ_D1,
                            SQ_A2, SQ_B2, SQ_C2, SQ_D2,
                            SQ_A3, SQ_B3, SQ_C3, SQ_D3,
                            SQ_A4, SQ_B4, SQ_C4, SQ_D4 })
            {
                if (off_A1H8 (s) < 0)
                {
                    MapA1D1D4[s] = code++;
                }
                else
                if (off_A1H8 (s) == 0)
                {
                    diagonal.push_back (s);
                }
            }
            // Diagonal squares are encoded as last ones
            for (auto s : diagonal)
            {
                MapA1D1D4[s] = code++;
            }
            // MapKK[] encodes all the 461 possible legal positions of two kings where the first is in the a1-d1-d4 triangle.
            // If the first king is on the a1-d4 diagonal, the other one shall not to be above the a1-h8 diagonal.
            vector<pair<i32, Square>> both_on_diagonal;
            code = 0;
            for (i32 idx = 0; idx < 10; ++idx)
            {
                for (auto s1 : { SQ_A1, SQ_B1, SQ_C1, SQ_D1,
                                 SQ_A2, SQ_B2, SQ_C2, SQ_D2,
                                 SQ_A3, SQ_B3, SQ_C3, SQ_D3,
                                 SQ_A4, SQ_B4, SQ_C4, SQ_D4 })
                {
                    if (   MapA1D1D4[s1] == idx
                        && (0 != idx || SQ_B1 == s1)) // SQ_B1 is mapped to 0
                    {
                        for (auto s2 : SQ)
                        {
                            if (contains (PieceAttacks[KING][s1] | s1, s2))
                            {
                                continue; // Illegal position
                            }
                            else
                            if (off_A1H8 (s1) == 0 && off_A1H8 (s2) > 0)
                            {
                                continue; // First on diagonal, second above
                            }
                            else
                            if (off_A1H8 (s1) == 0 && off_A1H8 (s2) == 0)
                            {
                                both_on_diagonal.push_back (std::make_pair (idx, s2));
                            }
                            else
                            {
                                MapKK[idx][s2] = code++;
                            }
                        }
                    }
                }
            }

            // Legal positions with both kings on diagonal are encoded as last ones
            for (auto p : both_on_diagonal)
            {
                MapKK[p.first][p.second] = code++;
            }

            // Binomial[] stores the Binomial Coefficients using Pascal rule. There
            // are Binomial[k][n] ways to choose k elements from a set of n elements.
            Binomial[0][0] = 1;

            for (i32 n = 1; n < 64; ++n) // Squares
            {
                for (i32 k = 0; k < 6 && k <= n; ++k) // Pieces
                {
                    Binomial[k][n] = (k > 0 ? Binomial[k - 1][n - 1] : 0)
                                   + (k < n ? Binomial[k][n - 1] : 0);
                }
            }

            // MapPawns[s] encodes squares a2-h7 to 0..47. This is the number of possible
            // available squares when the leading one is in square. Moreover the pawn with
            // highest MapPawns[] is the leading pawn, the one nearest the edge and,
            // among pawns with same file, the one with lowest rank.
            i32 available_squares = 47; // Available squares when lead pawn is in a2

            // Init the tables for the encoding of leading pawns group:
            // with 6-men TB can have up to 4 leading pawns (KPPPPK).
            for (i32 lead_pawn_count = 1; lead_pawn_count <= 4; ++lead_pawn_count)
            {
                for (auto f : { F_A, F_B, F_C, F_D })
                {
                    // Restart the index at every file because TB table is splitted
                    // by file, so we can reuse the same index for different files.
                    i32 idx = 0;

                    // Sum all possible combinations for a given file, starting with
                    // the leading pawn on rank 2 and increasing the rank.
                    for (auto r : { R_2, R_3, R_4, R_5, R_6, R_7 })
                    {
                        auto sq = f|r;

                        // Compute MapPawns[] at first pass.
                        // If sq is the leading pawn square, any other pawn cannot be
                        // below or more toward the edge of sq. There are 47 available
                        // squares when sq = a2 and reduced by 2 for any rank increase
                        // due to mirroring: sq == a3 -> no a2, h2, so MapPawns[a3] = 45
                        if (lead_pawn_count == 1)
                        {
                            MapPawns[ sq] = available_squares--;
                            MapPawns[!sq] = available_squares--; // Horizontal flip
                        }
                        LeadPawnIdx[lead_pawn_count][sq] = idx;
                        idx += Binomial[lead_pawn_count - 1][MapPawns[sq]];
                    }
                    // After a file is traversed, store the cumulated per-file index
                    LeadPawnsSize[lead_pawn_count][f] = idx;
                }
            }
            initialized = true;
        }

        EntryTable.clear ();
        MaxLimitPiece = 0;

        if (white_spaces (PathString))
        {
            return;
        }

        // PathString Example
        // (Windows)= D:\tb\wdl345;D:\tb\wdl6;D:\tb\dtz345;D:\tb\dtz6
        // (Unix-based OS)= .\tb\wdl345:.\tb\wdl6:.\tb\dtz345:.\tb\dtz6

        constexpr char SepChar =
#       if defined(_WIN32)
            ';';
#       else
            ':';
#       endif

        //TBFile::Paths = split (PathString, SepChar, false, true);
        TBFile::Paths.clear ();
        stringstream ss (PathString);
        string path;
        while (std::getline (ss, path, SepChar))
        {
            if (!white_spaces (path))
            {
                convert_path (path);
                TBFile::Paths.push_back (path);
            }
        }

        for (auto wp1 = PAWN; wp1 < KING; ++wp1)
        {
            EntryTable.insert ({ KING, wp1, KING });

            for (auto bp1 = PAWN; bp1 < KING; ++bp1)
            {
                EntryTable.insert ({ KING, wp1, KING, bp1 });
            }
            for (auto wp2 = PAWN; wp2 <= wp1; ++wp2)
            {
                EntryTable.insert ({ KING, wp1, wp2, KING });

                for (auto bp1 = PAWN; bp1 < KING; ++bp1)
                {
                    EntryTable.insert ({ KING, wp1, wp2, KING, bp1 });
                }
                for (auto wp3 = PAWN; wp3 <= wp2; ++wp3)
                {
                    EntryTable.insert ({ KING, wp1, wp2, wp3, KING });

                    for (auto bp1 = PAWN; bp1 < KING; ++bp1)
                    {
                        EntryTable.insert ({ KING, wp1, wp2, wp3, KING, bp1 });
                    }
                    for (auto wp4 = PAWN; wp4 <= wp3; ++wp4)
                    {
                        EntryTable.insert ({ KING, wp1, wp2, wp3, wp4, KING });

                        for (auto bp1 = PAWN; bp1 < KING; ++bp1)
                        {
                            EntryTable.insert ({ KING, wp1, wp2, wp3, wp4, KING, bp1 });
                        }
                        for (auto wp5 = PAWN; wp5 <= wp4; ++wp5)
                        {
                            EntryTable.insert ({ KING, wp1, wp2, wp3, wp4, wp5, KING });
                        }
                    }
                    for (auto bp1 = PAWN; bp1 < KING; ++bp1)
                    {
                        for (auto bp2 = PAWN; bp2 <= bp1; ++bp2)
                        {
                            EntryTable.insert ({ KING, wp1, wp2, wp3, KING, bp1, bp2 });
                        }
                    }
                }
                for (auto bp1 = PAWN; bp1 <= wp1; ++bp1)
                {
                    for (auto bp2 = PAWN; bp2 <= (wp1 == bp1 ? wp2 : bp1); ++bp2)
                    {
                        EntryTable.insert ({ KING, wp1, wp2, KING, bp1, bp2 });
                    }
                }
            }
        }

        sync_cout << "info string Tablebases found " << EntryTable.size () << sync_endl;
    }
}