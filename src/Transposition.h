#ifndef _TRANSPOSITION_H_INC_
#define _TRANSPOSITION_H_INC_

#include <cstdlib>
#include <iostream>

#include "Type.h"
#include "Zobrist.h"
#include "MemoryHandler.h"
#include "Thread.h"

namespace Transposition {

    union KeySplit
    {
        u64 k;
        u16 u[4];

        u16 key16 () const
        {
            return
                (u[3] ^ u[2]) != 0 ? u[3] ^ u[2] :
                (u[3] ^ u[1]) != 0 ? u[3] ^ u[1] :
                (u[3] ^ u[0]) != 0 ? u[3] ^ u[0] :
                // No Need
                //(u[2] ^ u[1]) != 0 ? u[2] ^ u[1] :
                //(u[2] ^ u[0]) != 0 ? u[2] ^ u[0] :
                //(u[1] ^ u[0]) != 0 ? u[1] ^ u[0] :
                Zobrists::Zobrist::no_tt_key;
        }
    };

    // Transposition::Entry needs 16 byte to be stored
    //
    //  Key--------- 16 bits
    //  Move-------- 16 bits
    //  Value------- 16 bits
    //  Evaluation-- 16 bits
    //  Depth------- 08 bits
    //  Generation-- 06 bits
    //  Bound------- 02 bits
    //  ====================
    //  Total------- 80 bits = 10 bytes
    struct Entry
    {
    private:
        u16 _key16  ;
        u16 _move   ;
        i16 _value  ;
        i16 _eval   ;
        i08 _depth  ;
        u08 _gen_bnd;

        friend class Table;

    public:
        // "Generation" variable distinguish transposition table entries from different searches.
        static u08 Generation;

        //u16   key16 () const { return u16  (_key16); }
        Move  move  () const { return Move (_move);  }
        Value value () const { return Value(_value); }
        Value eval  () const { return Value(_eval);  }
        i16   depth () const { return i16  (_depth); }
        Bound bound () const { return Bound(_gen_bnd & 0x03); }
        //u08   gen   () const { return u08  (_gen_bnd & 0xFC); }

        bool alive () const { return (_gen_bnd & 0xFC) == Generation; }
        // The worth of an entry is calculated as its depth minus 8 times its relative age.
        // Due to packed storage format for generation and its cyclic nature
        // add 0x103 (0x100 + 0x003 (BOUND_EXACT) to keep the lowest two bound bits from affecting the result)
        // to calculate the entry age correctly even after generation overflows into the next cycle.
        u08  worth () const { return u08(_depth - 2*((0x103 + Generation - _gen_bnd) & 0xFC)); }

        void refresh () { _gen_bnd = u08(Generation | (_gen_bnd & 0x03)); }

        void save (
            u64   k,
            Move  m,
            Value v,
            Value e,
            i16   d,
            Bound b)
        {
            const u16 key16 = KeySplit{ k }.key16 ();
            assert(key16 != 0);
            // Preserve any existing move for the position
            if (   key16 != _key16
                || m != MOVE_NONE)
            {
                _move       = u16(m);
            }
            // Preserve more valuable entries
            if (   key16 != _key16
                || d > _depth - 4
                || b == BOUND_EXACT)
            {
                _key16      = key16;
                _value      = i16(v);
                _eval       = i16(e);
                _depth      = i08(d);
                _gen_bnd    = u08(Generation | b);
            }
        }
    };

    const u08 CacheLineSize = 64;
    // Transposition::Cluster needs 32 bytes to be stored
    // 3 x 10 + 2
    struct Cluster
    {
    public:
        // Cluster entry count
        static const u08 EntryCount = 3;

        Entry entries[EntryCount];
        char padding[2]; // Align to a divisor of the cache line size
    };

    // Transposition::Table consists of a power of 2 number of clusters
    // and each cluster consists of Cluster::EntryCount number of entry.
    // Each non-empty entry contains information of exactly one position.
    // Size of a cluster should divide the size of a cache line size,
    // to ensure that clusters never cross cache lines.
    // In case it is less, it should be padded to guarantee always aligned accesses.
    // This ensures best cache performance, as the cacheline is prefetched.
    class Table
    {
    private:
        void    *_blocks        = nullptr;
        Cluster *_clusters      = nullptr;
        size_t   _cluster_count = 0;

        void alloc_aligned_memory (size_t mem_size, u32 alignment);
        void free_aligned_memory ();

    public:
        // Maximum bit of hash for cluster
        static const u08 MaxHashBit = 35;
        // Minimum size of Transposition::Table (4 MB)
        static const u32 MinHashSize = 4;
        // Maximum size of Transposition::Table (1048576 MB = 1048 GB = 1 TB)
        static const u32 MaxHashSize =
#       if defined(BIT64)
            (u64(1) << (MaxHashBit - 20)) * sizeof (Cluster);
#       else
            2048;
#       endif

        static const u32 BufferSize = 0x10000;

        bool retain_hash = false;

        Table () = default;
        Table (const Table&) = delete;
        Table& operator= (const Table&) = delete;
        ~Table ()
        {
            free_aligned_memory ();
        }

        size_t entry_count () const
        {
            return _cluster_count * Cluster::EntryCount;
        }

        // Returns hash size in MB
        u32 size () const
        {
            return u32((_cluster_count * sizeof (Cluster)) >> 20);
        }

        // Reset the entire transposition table with zeroes.
        void clear ()
        {
            if (   !retain_hash
                && _clusters != nullptr)
            {
                std::memset (_clusters, 0x00, _cluster_count * sizeof (Cluster));
                Entry::Generation = 0;
                sync_cout << "info string Hash cleared" << sync_endl;
            }
        }

        // Returns a pointer to the first entry of a cluster given a position.
        // The lower order bits of the key are used to get the index of the cluster inside the table.
        Entry* cluster_entry (Key key) const
        {
            return _clusters[key & (_cluster_count-1)].entries;
        }

        u32 resize (u32 mem_size, bool force = false);
        u32 resize ();

        void auto_resize (u32 mem_size, bool force = false);

        Entry* probe (Key key, bool &tt_hit) const;

        u32 hash_full () const;

        void save (const std::string &hash_fn) const;
        void load (const std::string &hash_fn);

        template<class CharT, class Traits>
        friend std::basic_ostream<CharT, Traits>&
            operator<< (std::basic_ostream<CharT, Traits> &os, const Table &tt)
        {
            u32 mem_size = tt.size ();
            u08 dummy = 0;
            os.write (reinterpret_cast<const CharT*> (&mem_size), sizeof (mem_size));
            os.write (reinterpret_cast<const CharT*> (&dummy), sizeof (dummy));
            os.write (reinterpret_cast<const CharT*> (&dummy), sizeof (dummy));
            os.write (reinterpret_cast<const CharT*> (&dummy), sizeof (dummy));
            os.write (reinterpret_cast<const CharT*> (&Entry::Generation), sizeof (Entry::Generation));
            os.write (reinterpret_cast<const CharT*> (&tt._cluster_count), sizeof (tt._cluster_count));
            for (u32 i = 0; i < tt._cluster_count / BufferSize; ++i)
            {
                os.write (reinterpret_cast<const CharT*> (tt._clusters+i*BufferSize), sizeof (Cluster)*BufferSize);
            }
            return os;
        }

        template<class CharT, class Traits>
        friend std::basic_istream<CharT, Traits>&
            operator>> (std::basic_istream<CharT, Traits> &is,       Table &tt)
        {
            u32 mem_size;
            u08 generation;
            u08 dummy;
            is.read (reinterpret_cast<CharT*> (&mem_size), sizeof (mem_size));
            is.read (reinterpret_cast<CharT*> (&dummy), sizeof (dummy));
            is.read (reinterpret_cast<CharT*> (&dummy), sizeof (dummy));
            is.read (reinterpret_cast<CharT*> (&dummy), sizeof (dummy));
            is.read (reinterpret_cast<CharT*> (&generation), sizeof (generation));
            is.read (reinterpret_cast<CharT*> (&tt._cluster_count), sizeof (tt._cluster_count));
            tt.resize (mem_size);
            Entry::Generation = generation;
            for (u32 i = 0; i < tt._cluster_count / BufferSize; ++i)
            {
                is.read (reinterpret_cast<CharT*> (tt._clusters+i*BufferSize), sizeof (Cluster)*BufferSize);
            }
            return is;
        }
    };
}

// Global Transposition Table
extern Transposition::Table TT;

#endif // _TRANSPOSITION_H_INC_
