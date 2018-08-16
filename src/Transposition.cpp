#include "Transposition.h"

#include <string>
#include <fstream>
#include <iostream>
#include "Engine.h"
#include "MemoryHandler.h"

TTable TT;

using namespace std;

TCluster TCluster::Empty;

void TCluster::initialize ()
{
    std::memset (&Empty, 0x00, sizeof (Empty));
    for (auto *ite = Empty.entries; ite < Empty.entries + EntryCount; ++ite)
    {
        ite->d08 = DepthEmpty;
    }
}

TEntry* TCluster::probe (u16 key16, bool &tt_hit, u08 gen)
{
    // Find an entry to be replaced according to the replacement strategy.
    auto *rte = entries; // Default first
    for (auto *ite = entries; ite < entries + EntryCount; ++ite)
    {
        if (   ite->empty ()
            || ite->k16 == key16)
        {
            if ((tt_hit = !ite->empty ()))
            {
                // Refresh entry.
                if (ite->generation () != gen)
                {
                    ite->gb08 = u08(gen + ite->bound ());
                }
            }
            return ite;
        }
        // Replacement strategy.
        // The worth of an entry is calculated as its depth minus 2 times its relative age.
        // Due to packed storage format for generation and its cyclic nature
        // add 0x103 (0x100 + 3 (BOUND_EXACT) to keep the lowest two bound bits from affecting the result)
        // to calculate the entry age correctly even after generation overflows into the next cycle.
        if (  rte->d08 - ((gen - rte->gb08 + 0x103) & 0xFC) * 2
            > ite->d08 - ((gen - ite->gb08 + 0x103) & 0xFC) * 2)
        {
            rte = ite;
        }
    }
    tt_hit = false;
    return rte;
}

void TCluster::clear ()
{
    std::memcpy (this, &Empty, sizeof (*this));
}

size_t TCluster::full_entry_count (u08 gen) const
{
    size_t count = 0;
    for (const auto *ite = entries; ite < entries + EntryCount; ++ite)
    {
        if (ite->generation () == gen)
        {
            ++count;
        }
    }
    return count;
}

/// TTable::alloc_aligned_memory() allocates the aligned memory
void TTable::alloc_aligned_memory (size_t mem_size, u32 alignment)
{
    assert(0 == (alignment & (alignment-1)));

#if defined(LPAGES)

    Memory::alloc_memory (mem, mem_size, alignment);
    if (nullptr == mem)
    {
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

    mem = malloc (mem_size + alignment-1);
    if (nullptr == mem)
    {
        std::cerr << "ERROR: Hash memory allocate failed " << (mem_size >> 20) << " MB" << std::endl;
        return;
    }
    sync_cout << "info string Hash " << (mem_size >> 20) << " MB" << sync_endl;

#endif

    clusters = (TCluster*)((uintptr_t(mem) + alignment-1) & ~uintptr_t(alignment-1));
    assert(nullptr != clusters
        && 0 == (uintptr_t(clusters) & (alignment-1)));

}
/// TTable::free_aligned_memory() frees the aligned memory
void TTable::free_aligned_memory ()
{
#if defined(LPAGES)
    Memory::free_memory (mem);
#else
    free (mem);
#endif
    mem = nullptr;
    clusters = nullptr;
}

/// TTable::resize() sets the size of the table, measured in mega-bytes.
u32 TTable::resize (u32 mem_size)
{
    mem_size = std::min (std::max (mem_size, MinHashSize), MaxHashSize);
    size_t msize = size_t(mem_size) << 20;

    free_aligned_memory ();
    alloc_aligned_memory (msize, CacheLineSize);

    if (nullptr == clusters)
    {
        return 0;
    }
    cluster_count = msize / sizeof (TCluster);
    clear ();
    return mem_size;
}

/// TTable::auto_resize()
void TTable::auto_resize (u32 mem_size)
{
    for (auto msize =
            0 != mem_size ?
            mem_size :
            MaxHashSize;
            msize >= MinHashSize;
            msize /= 2)
    {
        if (0 != resize (msize))
        {
            return;
        }
    }
    stop (EXIT_FAILURE);
}
/// TTable::clear() clear the entire transposition table in a multi-threaded way.
void TTable::clear ()
{
    assert(0 != cluster_count);
    if (bool(Options["Retain Hash"]))
    {
        return;
    }
    
    std::vector<std::thread> threads;
    auto thread_count = threads_option ();
    for (u32 idx = 0; idx < thread_count; ++idx)
    {
        threads.push_back (std::thread ([this, idx, thread_count]()
                                        {
                                            if (8 <= thread_count)
                                            {
                                                WinProcGroup::bind (idx);
                                            }
                                            size_t stride = cluster_count / thread_count;
                                            auto *pcluster = clusters + idx * stride;
                                            size_t count = idx != thread_count - 1 ?
                                                            stride :
                                                            cluster_count - idx * stride;
                                            for (auto *itc = pcluster; itc < pcluster + count; ++itc)
                                            {
                                                itc->clear ();
                                            }
                                        }));
    }
    for (auto &th : threads)
    {
        th.join ();
    }
    threads.clear ();
    //sync_cout << "info string Hash cleared" << sync_endl;
}

/// TTable::probe() looks up the entry in the transposition table.
/// If the position is found, it returns true and a pointer to the found entry.
/// Otherwise, it returns false and a pointer to an empty or least valuable entry to be replaced later.
TEntry* TTable::probe (Key key, bool &tt_hit) const
{
    return cluster (key)->probe (u16(key >> 0x30), tt_hit, generation);
}
/// TTable::extract_pm_from_tt() extracts ponder move from TT.
Move TTable::extract_pm (Position &pos, Move bm)
{
    assert(MoveList<GenType::LEGAL> (pos).contains (bm));
    StateInfo si;
    pos.do_move (bm, si);
    bool tt_hit;
    auto *tte = probe (pos.si->posi_key, tt_hit);
    Move pm = tt_hit ? tte->move () : MOVE_NONE;
    if (   MOVE_NONE != pm
        && !(   pos.pseudo_legal (pm)
             && pos.legal (pm)))
    {
        pm = MOVE_NONE;
    }
    assert(MOVE_NONE == pm
        || MoveList<GenType::LEGAL> (pos).contains (pm));
    pos.undo_move (bm);
    return pm;
}

/// TTable::hash_full() returns an approximation of the per-mille of the 
/// all transposition entries during a search which have received
/// at least one write during the current search.
/// It is used to display the "info hashfull ..." information in UCI.
/// "the hash is <x> per mill full", the engine should send this info regularly.
/// hash, are using <x>%. of the state of full.
u32 TTable::hash_full () const
{
    size_t full_entry_count = 0;
    const auto cluster_limit = std::min (size_t(1000 / TCluster::EntryCount), cluster_count);
    for (const auto *itc = clusters; itc < clusters + cluster_limit; ++itc)
    {
        full_entry_count += itc->full_entry_count (generation);
    }
    return u32(full_entry_count * 1000 / (cluster_limit * TCluster::EntryCount));
}
/// TTable::save() saves hash to file
void TTable::save (const string &hash_fn) const
{
    if (white_spaces (hash_fn))
    {
        return;
    }
    ofstream ofs (hash_fn, ios_base::out|ios_base::binary);
    if (!ofs.is_open ())
    {
        return;
    }
    ofs << *this;
    ofs.close ();
    sync_cout << "info string Hash saved to file \'" << hash_fn << "\'" << sync_endl;
}
/// TTable::load() loads hash from file
void TTable::load (const string &hash_fn)
{
    if (white_spaces (hash_fn))
    {
        return;
    }
    ifstream ifs (hash_fn, ios_base::in|ios_base::binary);
    if (!ifs.is_open ())
    {
        return;
    }
    ifs >> *this;
    ifs.close ();
    sync_cout << "info string Hash loaded from file \'" << hash_fn << "\'" << sync_endl;
}
