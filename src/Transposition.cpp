#include "Transposition.h"

#include <string>
#include <fstream>

#include "BitBoard.h"
#include "Engine.h"

Transposition::Table  TT;

namespace Transposition {

    using namespace std;

    u08 Entry::Generation = 0;

    // Size of Transposition entry (10 bytes)
    static_assert (sizeof (Entry) == 10, "Entry size incorrect");
    // Size of Transposition cluster (32 bytes)
    static_assert (CacheLineSize % sizeof (Cluster) == 0, "Cluster size incorrect");

    // Alocate the aligned memory
    void Table::alloc_aligned_memory (size_t mem_size, u32 alignment)
    {
        assert((alignment & (alignment-1)) == 0);
        assert((mem_size  & (alignment-1)) == 0);

    #if defined(LPAGES)

        Memory::alloc_memory (_blocks, mem_size, alignment);
        if (_blocks == nullptr)
        {
            return;
        }
        _clusters = reinterpret_cast<Cluster*> ((uintptr_t(_blocks) + alignment-1) & ~uintptr_t(alignment-1));
        assert((uintptr_t(_clusters) & (alignment-1)) == 0);

    #else

        // Need to use malloc provided by C.
        // First need to allocate memory of mem_size + max (alignment, sizeof (void *)).
        // Need 'bytes' because user requested it.
        // Need to add 'alignment' because malloc can give us any address and
        // Need to find multiple of 'alignment', so at maximum multiple
        // of alignment will be 'alignment' bytes away from any location.
        // Need 'sizeof(void *)' for implementing 'aligned_free',
        // since returning modified memory pointer, not given by malloc, to the user,
        // must free the memory allocated by malloc not anything else.
        // So storing address given by malloc just above pointer returning to user.
        // Thats why needed extra space to store that address.
        // Then checking for error returned by malloc, if it returns NULL then 
        // alloc_aligned_memory will fail and return NULL or exit().

        alignment = std::max (u32(sizeof (void *)), alignment);

        _blocks = calloc (mem_size + alignment-1, 1);
        if (_blocks == nullptr)
        {
            std::cerr << "ERROR: Hash memory allocate failed " << (mem_size >> 20) << " MB" << std::endl;
            return;
        }
        sync_cout << "info string Hash " << (mem_size >> 20) << " MB" << sync_endl;
        _clusters = reinterpret_cast<Cluster*> ((uintptr_t(_blocks) + alignment-1) & ~uintptr_t(alignment-1));
        assert((uintptr_t(_clusters) & (alignment-1)) == 0);

    #endif

    }
    // Free the aligned memory
    void Table::free_aligned_memory ()
    {
    #   if defined(LPAGES)
        Memory::free_memory (_blocks);
    #   else
        free (_blocks);
    #   endif
        _blocks = nullptr;
        _clusters = nullptr;
        _cluster_count = 0;
        Entry::Generation = 0;
    }

    // resize(mb) sets the size of the table, measured in mega-bytes.
    // Transposition table consists of a power of 2 number of clusters and
    // each cluster consists of Cluster::EntryCount number of entry.
    u32 Table::resize (u32 mem_size, bool force)
    {
        if (mem_size < MinHashSize)
        {
            mem_size = MinHashSize;
        }
        if (mem_size > MaxHashSize)
        {
            mem_size = MaxHashSize;
        }
        size_t msize = size_t(mem_size) << 20;
        u08 hash_bit = BitBoard::scan_msq (msize / sizeof (Cluster));
        assert(hash_bit <= MaxHashBit);

        size_t cluster_count = size_t(1) << hash_bit;

        msize = cluster_count * sizeof (Cluster);
        if (   force
            || cluster_count != _cluster_count)
        {
            free_aligned_memory ();
            alloc_aligned_memory (msize, CacheLineSize);
            if (_clusters == nullptr)
            {
                return 0;
            }
            _cluster_count = cluster_count;
        }

        return u32(msize >> 20);
    }
    u32 Table::resize ()
    {
        return resize (size (), true);
    }

    void Table::auto_resize (u32 mem_size, bool force)
    {
        if (mem_size == 0)
        {
            mem_size = MaxHashSize;
        }
        auto msize = mem_size;
        while (msize >= MinHashSize)
        {
            if (resize (msize, force) != 0)
            {
                return;
            }
            msize >>= 1;
        }
        Engine::stop (EXIT_FAILURE);
    }
    // probe() looks up the entry in the transposition table.
    // If the position is found, it returns true and a pointer to the found entry.
    // Otherwise, it returns false and a pointer to an empty or least valuable entry to be replaced later.
    Entry* Table::probe (Key key, bool &tt_hit) const
    {
        assert(key != 0);
        const u16 key16 = KeySplit{ key }.key16 ();
        assert(key16 != 0);
        auto *const fte = cluster_entry (key);
        assert(fte != nullptr);
        for (auto *ite = fte+0; ite < fte+Cluster::EntryCount; ++ite)
        {
            if (   ite->_key16 == 0
                || ite->_key16 == key16)
            {
                tt_hit = ite->_key16 != 0;
                if (   tt_hit
                    && !ite->alive ())
                {
                    ite->refresh ();
                }
                return ite;
            }
        }
        tt_hit = false;
        // Find an entry to be replaced according to the replacement strategy
        auto *rte = fte; // Default first
        for (auto *ite = fte+1; ite < fte+Cluster::EntryCount; ++ite)
        {
            // Entry te1 is considered more valuable than Entry te2, if te1.worth() > te2.worth().
            if (  rte->worth ()
                > ite->worth ())
            {
                rte = ite;
            }
        }
        return rte;
    }
    // Returns an approximation of the per-mille of the 
    // all transposition entries during a search which have received
    // at least one write during the current search.
    // It is used to display the "info hashfull ..." information in UCI.
    // "the hash is <x> permill full", the engine should send this info regularly.
    // hash, are using <x>%. of the state of full.
    u32 Table::hash_full () const
    {
        u32 full_count = 0;
        for (const auto *clt = _clusters; clt < _clusters + 1000/Cluster::EntryCount; ++clt)
        {
            const auto *fte = clt->entries;
            for (const auto *ite = fte; ite < fte+Cluster::EntryCount; ++ite)
            {
                if (ite->alive ())
                {
                    ++full_count;
                }
            }
        }
        return full_count;
    }

    void Table::save (const string &hash_fn) const
    {
        ofstream ofs (hash_fn, ios_base::out|ios_base::binary);
        if (ofs.is_open ())
        {
            ofs << *this;
            ofs.close ();
            sync_cout << "info string Hash saved to file \'" << hash_fn << "\'" << sync_endl;
        }
    }

    void Table::load (const string &hash_fn)
    {
        ifstream ifs (hash_fn, ios_base::in|ios_base::binary);
        if (ifs.is_open ())
        {
            ifs >> *this;
            ifs.close ();
            sync_cout << "info string Hash loaded from file \'" << hash_fn << "\'" << sync_endl;
        }
    }

}
