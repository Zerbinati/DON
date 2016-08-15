#ifndef _TRANSPOSITION_H_INC_
#define _TRANSPOSITION_H_INC_

#include <cstdlib>

#include "Type.h"
#include "MemoryHandler.h"
#include "Thread.h"

namespace Transposition {

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
        u16   key16 () const { return u16  (_key16); }
        Move  move  () const { return Move (_move);  }
        Value value () const { return Value(_value); }
        Value eval  () const { return Value(_eval);  }
        Depth depth () const { return Depth(_depth); }
        Bound bound () const { return Bound(_gen_bnd & u08( BOUND_EXACT)); }
        u08   gen   () const { return u08  (_gen_bnd & u08(~BOUND_EXACT)); }

        void save (u64 k,
                   Move m,
                   Value v,
                   Value e,
                   Depth d,
                   Bound b,
                   u08 g)
        {
            // Preserve any existing move for the position (key)
            if (  (k >> 0x30) != _key16
                || m != MOVE_NONE)
            {
                _move       = u16(m);
            }
            // Don't overwrite more valuable entries
            if (  (k >> 0x30) != _key16
                || e != VALUE_NONE)
            {
                _eval       = i16(e);
            }
            if (  (k >> 0x30) != _key16
                || d > _depth - DEPTH_4
             /* || g != gen () // Matching non-zero keys are already refreshed by probe() */
                || b == BOUND_EXACT)
            {
                _key16      = u16(k >> 0x30);
                _value      = i16(v);
                _depth      = i08(d);
                _gen_bnd    = u08(g | b);
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
        u08      _generation    = 0;

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

        void clear ();

        // "Generation" variable distinguish transposition table entries from different searches.
        void generation (u16 ply)
        {
            _generation = u08(ply << 2);
        }
        u08  generation () const
        {
            return _generation;
        }

        // Returns a pointer to the first entry of a cluster given a position.
        // The lower order bits of the key are used to get the index of the cluster inside the table.
        Entry* cluster_entry (const Key key) const
        {
            return _clusters[size_t(key) & (_cluster_count-1)].entries;
        }

        u32 resize (u32 mem_size_mb, bool force = false);
        u32 resize ();

        void auto_resize (u32 mem_size_mb, bool force = false);

        Entry* probe (Key key, bool &tt_hit) const;

        u32 hash_full () const;

        void save (const std::string &hash_fn) const;
        void load (const std::string &hash_fn);

        template<class CharT, class Traits>
        friend std::basic_ostream<CharT, Traits>&
            operator<< (std::basic_ostream<CharT, Traits> &os, const Table &tt)
        {
            u32 mem_size_mb = tt.size ();
            u08 dummy = 0;
            os.write (reinterpret_cast<const CharT*> (&mem_size_mb), sizeof (mem_size_mb));
            os.write (reinterpret_cast<const CharT*> (&dummy), sizeof (dummy));
            os.write (reinterpret_cast<const CharT*> (&dummy), sizeof (dummy));
            os.write (reinterpret_cast<const CharT*> (&dummy), sizeof (dummy));
            os.write (reinterpret_cast<const CharT*> (&tt._generation), sizeof (tt._generation));
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
            u32 mem_size_mb;
            u08 generation;
            u08 dummy;
            is.read (reinterpret_cast<CharT*> (&mem_size_mb), sizeof (mem_size_mb));
            is.read (reinterpret_cast<CharT*> (&dummy), sizeof (dummy));
            is.read (reinterpret_cast<CharT*> (&dummy), sizeof (dummy));
            is.read (reinterpret_cast<CharT*> (&dummy), sizeof (dummy));
            is.read (reinterpret_cast<CharT*> (&generation), sizeof (generation));
            is.read (reinterpret_cast<CharT*> (&tt._cluster_count), sizeof (tt._cluster_count));
            tt.resize (mem_size_mb);
            tt._generation = generation;
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
