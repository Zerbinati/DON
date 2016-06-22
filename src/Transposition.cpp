#include "Transposition.h"

#include <string>
#include <fstream>

#include "BitBoard.h"
#include "Engine.h"

Transposition::Table  TT;

namespace Transposition {

    using namespace std;

    // Size of Transposition entry (10 bytes)
    static_assert (sizeof (Entry) == 10, "Entry size incorrect");
    // Size of Transposition cluster (32 bytes)
    static_assert (CacheLineSize % sizeof (Cluster) == 0, "Cluster size incorrect");


    void Table::alloc_aligned_memory (size_t mem_size, u32 alignment)
    {
        assert((alignment & (alignment-1)) == 0);
        assert((mem_size  & (alignment-1)) == 0);

    #if defined(LPAGES)

        Memory::alloc_memory (_blocks, mem_size, alignment);
        if (_blocks != nullptr)
        {
            _clusters = reinterpret_cast<Cluster*> ((uintptr_t(_blocks) + alignment-1) & ~uintptr_t(alignment-1));
            assert((uintptr_t(_clusters) & (alignment-1)) == 0);
            return;
        }

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
        if (_blocks != nullptr)
        {
            sync_cout << "info string Hash " << (mem_size >> 20) << " MB" << sync_endl;
            _clusters = reinterpret_cast<Cluster*> ((uintptr_t(_blocks) + alignment-1) & ~uintptr_t(alignment-1));
            assert((uintptr_t(_clusters) & (alignment-1)) == 0);
            return;
        }
        std::cerr << "ERROR: Hash memory allocate failed " << (mem_size >> 20) << " MB" << std::endl;

    #endif

    }

    // resize(mb) sets the size of the table, measured in mega-bytes.
    // Transposition table consists of a power of 2 number of clusters and
    // each cluster consists of ClusterEntryCount number of entry.
    u32 Table::resize (u32 mem_size_mb, bool force)
    {
        if (mem_size_mb < MinHashSize)
        {
            mem_size_mb = MinHashSize;
        }
        if (mem_size_mb > MaxHashSize)
        {
            mem_size_mb = MaxHashSize;
        }

        size_t mem_size = size_t(mem_size_mb) << 20;
        u08 hash_bit = BitBoard::scan_msq (mem_size / sizeof (Cluster));
        assert(hash_bit <= MaxHashBit);

        size_t cluster_count = size_t(1) << hash_bit;

        mem_size  = cluster_count * sizeof (Cluster);

        if (   force
            || cluster_count != _cluster_count)
        {
            free_aligned_memory ();

            alloc_aligned_memory (mem_size, CacheLineSize);

            if (_clusters == nullptr)
            {
                return 0;
            }

            _cluster_count = cluster_count;
            _cluster_mask  = cluster_count-1;
        }

        return u32(mem_size >> 20);
    }

    void Table::auto_size (u32 mem_size_mb, bool force)
    {
        if (mem_size_mb == 0)
        {
            mem_size_mb = MaxHashSize;
        }
        for (u32 msize_mb = mem_size_mb; msize_mb >= MinHashSize; msize_mb >>= 1)
        {
            if (resize (msize_mb, force) != 0)
            {
                return;
            }
        }
        Engine::stop (EXIT_FAILURE);
    }

    // probe() looks up the entry in the transposition table.
    // Returns a pointer to the entry found or NULL if not found.
    Entry* Table::probe (Key key, bool &tt_hit) const
    {
        assert(key != 0);
        const u16 key16 = u16(key >> 0x30);
        auto *const fte = cluster_entry (key);
        assert(fte != nullptr);
        for (auto *ite = fte+0; ite < fte+ClusterEntryCount; ++ite)
        {
            if (   ite->_key16 == 0
                || ite->_key16 == key16)
            {
                tt_hit = ite->_key16 == key16;
                if (   tt_hit
                    && ite->gen () != _generation)
                {
                    ite->_gen_bnd = u08(_generation | ite->bound ()); // Refresh
                }
                return ite;
            }
        }
        // Find an entry to be replaced according to the replacement strategy
        auto *rte = fte;
        auto rem = rte->_depth - 2*(u08(0x100+BOUND_EXACT + _generation - rte->_gen_bnd)&u08(~BOUND_EXACT));
        for (auto *ite = fte+1; ite < fte+ClusterEntryCount; ++ite)
        {
            // Implementation of replacement strategy when a collision occurs
            auto iem = ite->_depth - 2*(u08(0x100+BOUND_EXACT + _generation - ite->_gen_bnd)&u08(~BOUND_EXACT));
            if (rem > iem)
            {
                rem = iem;
                rte = ite;
            }
        }
        tt_hit = false;
        return rte;
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
