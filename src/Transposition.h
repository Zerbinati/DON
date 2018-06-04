#ifndef _TRANSPOSITION_H_INC_
#define _TRANSPOSITION_H_INC_

#include "Type.h"
#include "Thread.h"

/// Transposition::Entry needs 16 byte to be stored
///
///  Key        16 bits
///  Move       16 bits
///  Value      16 bits
///  Evaluation 16 bits
///  Depth      08 bits
///  Generation 06 bits
///  Bound      02 bits
///  --------------------
///  Total      80 bits = 10 bytes
struct TEntry
{
private:
    u16 k16;
    u16 m16;
    i16 v16;
    i16 e16;
    i08 d08;
    u08 gb08;

    friend class TCluster;
    friend class TTable;

public:

    Move  move       () const { return Move (m16); }
    Value value      () const { return Value(v16); }
    Value eval       () const { return Value(e16); }
    i16   depth      () const { return i16  (d08); }
    Bound bound      () const { return Bound(gb08 & 0x03); }
    u08   generation () const { return u08  (gb08 & 0xFC); }
    bool  empty      () const { return d08 == DepthEmpty; }

    void save (u64 k, Move m, Value v, Value e, i16 d, Bound b, u08 g)
    {
        // Preserve more valuable entries
        if (   k16 != (k >> 0x30)   // Use the high 16 bits as key inside the cluster
            || MOVE_NONE != m)
        {
            m16 = u16(m);
        }
        if (   k16 != (k >> 0x30)
            || d08 - 4 < d
            || BOUND_EXACT == b)
        {
            k16 = u16(k >> 0x30);
            d08 = i08(d);
            gb08 = u08(g + b);
            v16 = i16(v);
            e16 = i16(e);
        }
        assert(!empty ());
    }
};

/// Size of Transposition entry (10 bytes)
static_assert (sizeof (TEntry) == 10, "Entry size incorrect");

constexpr u32 CacheLineSize = 64;

/// Transposition::Cluster needs 32 bytes to be stored
/// 10 x 3 + 2 = 32
class TCluster
{
public:
    // Cluster entry count
    static constexpr u08 EntryCount = 3;
    static TCluster Empty;

    static void initialize ();

    TEntry entries[EntryCount];
    char padding[2]; // Align to a divisor of the cache line size

    TEntry *probe (u16, bool&, u08);

    void clear ();

    size_t full_entry_count (u08) const;

};

/// Size of Transposition cluster (32 bytes)
static_assert (CacheLineSize % sizeof (TCluster) == 0, "Cluster size incorrect");

/// Transposition::Table consists of a power of 2 number of clusters
/// and each cluster consists of Cluster::EntryCount number of entry.
/// Each non-empty entry contains information of exactly one position.
/// Size of a cluster should divide the size of a cache line size,
/// to ensure that clusters never cross cache lines.
/// In case it is less, it should be padded to guarantee always aligned accesses.
/// This ensures best cache performance, as the cache line is pre-fetched.
class TTable
{
private:
    void alloc_aligned_memory (size_t, u32);
    void free_aligned_memory ();

public:
    // Minimum size of Transposition::Table (4 MB)
    static constexpr u32 MinHashSize = 4;
    // Maximum size of Transposition::Table (131072 MB = 128 GB)
    static constexpr u32 MaxHashSize =
#       if defined(BIT64)
            128 * 1024
#       else
            2 * 1024
#       endif
        ;
    static constexpr u32 BufferSize = 0x10000;

    void *mem;
    TCluster *clusters;
    size_t cluster_count;
    // "Generation" variable distinguish transposition table entries from different searches.
    u08 generation;

    TTable ()
        : mem (nullptr)
        , clusters (nullptr)
        , cluster_count (0)
        , generation (0)
    {}

    TTable (const TTable&) = delete;
    TTable& operator= (const TTable&) = delete;

   ~TTable ()
    {
        free_aligned_memory ();
    }

    size_t cluster_mask () const { return cluster_count - 1; }
    //size_t entry_count () const { return cluster_count * Cluster::EntryCount; }

    /// size() returns hash size in MB
    u32 size () const { return u32((cluster_count * sizeof (TCluster)) >> 20); }

    /// cluster() returns a pointer to the cluster of given a key.
    /// The lower 32 order bits of the key are used to get the index of the cluster inside the table.
    TCluster* cluster (Key key) const { return &clusters[(u32(key) * u64(cluster_count)) >> 0x20]; }

    u32 resize (u32);

    void auto_resize (u32);

    void clear ();

    TEntry* probe (Key, bool&) const;

    u32 hash_full () const;

    void save (const std::string&) const;
    void load (const std::string&);

    template<typename CharT, typename Traits>
    friend std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, const TTable &tt)
    {
        u32 mem_size = tt.size ();
        u08 dummy = 0;
        os.write ((const CharT*)(&mem_size), sizeof (mem_size));
        os.write ((const CharT*)(&dummy), sizeof (dummy));
        os.write ((const CharT*)(&dummy), sizeof (dummy));
        os.write ((const CharT*)(&dummy), sizeof (dummy));
        os.write ((const CharT*)(&tt.generation), sizeof (tt.generation));
        os.write ((const CharT*)(&tt.cluster_count), sizeof (tt.cluster_count));
        for (u32 i = 0; i < tt.cluster_count / BufferSize; ++i)
        {
            os.write ((const CharT*)(tt.clusters+i*BufferSize), sizeof (TCluster)*BufferSize);
        }
        return os;
    }

    template<typename CharT, typename Traits>
    friend std::basic_istream<CharT, Traits>&
        operator>> (std::basic_istream<CharT, Traits> &is,       TTable &tt)
    {
        u32 mem_size;
        u08 dummy;
        is.read ((CharT*)(&mem_size), sizeof (mem_size));
        is.read ((CharT*)(&dummy), sizeof (dummy));
        is.read ((CharT*)(&dummy), sizeof (dummy));
        is.read ((CharT*)(&dummy), sizeof (dummy));
        is.read ((CharT*)(&tt.generation), sizeof (tt.generation));
        is.read ((CharT*)(&tt.cluster_count), sizeof (tt.cluster_count));
        tt.resize (mem_size);
        for (u32 i = 0; i < tt.cluster_count / BufferSize; ++i)
        {
            is.read ((CharT*)(tt.clusters+i*BufferSize), sizeof (TCluster)*BufferSize);
        }
        return is;
    }
};

// Global Transposition Table
extern TTable TT;

#endif // _TRANSPOSITION_H_INC_
