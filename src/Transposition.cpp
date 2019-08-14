#include "Transposition.h"

#include <cstring>
#include <fstream>
#include <iostream>
#include "Engine.h"
#include "MemoryHandler.h"

TTable TT;

using namespace std;

u08 TEntry::Generation;

TEntry* TCluster::probe(u16 key16, bool &hit)
{
    // Find an entry to be replaced according to the replacement strategy.
    auto *rte = entries; // Default first
    for (auto *ite = entries; ite < entries + EntryCount; ++ite)
    {
        if (   ite->empty()
            || ite->k16 == key16)
        {
            ite->refresh(); // Refresh entry.
            return hit = !ite->empty(), ite;
        }
        // Replacement strategy.
        if (  rte->worth()
            > ite->worth())
        {
            rte = ite;
        }
    }
    return hit = false, rte;
}

void TCluster::clear()
{
    std::memset(this, 0, sizeof (*this));
}

size_t TCluster::fresh_entry_count() const
{
    size_t count = 0;
    for (auto const *ite = entries; ite < entries + EntryCount; ++ite)
    {
        if (ite->generation() == TEntry::Generation)
        {
            ++count;
        }
    }
    return count;
}

/// TTable::alloc_aligned_memory() allocates the aligned memory
void TTable::alloc_aligned_memory(size_t mem_size, u32 alignment)
{
    assert(0 == (alignment & (alignment-1)));

#if defined(LPAGES)

    Memory::alloc_memory(mem, mem_size, alignment);
    if (nullptr == mem)
    {
        return;
    }

#else

    // Need to use malloc provided by C.
    // First need to allocate memory of mem_size + max(alignment, sizeof (void *)).
    // Need 'bytes' because user requested it.
    // Need to add 'alignment' because malloc can give us any address and
    // Need to find multiple of 'alignment', so at maximum multiple
    // of alignment will be 'alignment' bytes away from any location.
    // Need 'sizeof (void *)' for implementing 'aligned_free',
    // since returning modified memory pointer, not given by malloc, to the user,
    // must free the memory allocated by malloc not anything else.
    // So storing address given by malloc just above pointer returning to user.
    // Thats why needed extra space to store that address.
    // Then checking for error returned by malloc, if it returns NULL then 
    // alloc_aligned_memory will fail and return NULL or exit().

    alignment = std::max(u32(sizeof (void *)), alignment);

    mem = malloc(mem_size + alignment-1);
    if (nullptr == mem)
    {
        cerr << "ERROR: Hash memory allocate failed " << (mem_size >> 20) << " MB" << endl;
        return;
    }
    sync_cout << "info string Hash " << (mem_size >> 20) << " MB" << sync_endl;

#endif

    clusters = (TCluster*)((uintptr_t(mem) + alignment-1) & ~uintptr_t(alignment-1));
    assert(nullptr != clusters
        && 0 == (uintptr_t(clusters) & (alignment-1)));

}
/// TTable::free_aligned_memory() frees the aligned memory
void TTable::free_aligned_memory()
{
#if defined(LPAGES)
    Memory::free_memory(mem);
#else
    free(mem);
#endif
    mem = nullptr;
    clusters = nullptr;
}

/// TTable::resize() sets the size of the table, measured in MB.
u32 TTable::resize(u32 mem_size)
{
    Threadpool.main_thread()->wait_while_busy();

    mem_size = clamp(mem_size, MinHashSize, MaxHashSize);
    size_t msize = size_t(mem_size) << 20;

    free_aligned_memory();
    alloc_aligned_memory(msize, CacheLineSize);

    if (nullptr == clusters)
    {
        return 0;
    }
    cluster_count = msize / sizeof (TCluster);
    clear();
    return mem_size;
}

/// TTable::auto_resize()
void TTable::auto_resize(u32 mem_size)
{
    auto msize = 0 != mem_size ? mem_size : MaxHashSize;
    while (msize >= MinHashSize)
    {
        if (0 != resize(msize))
        {
            return;
        }
        msize /= 2;
    }
    stop(EXIT_FAILURE);
}
/// TTable::clear() clear the entire transposition table in a multi-threaded way.
void TTable::clear()
{
    assert(0 != cluster_count);
    if (bool(Options["Retain Hash"]))
    {
        return;
    }
    
    vector<NativeThread> threads;
    auto thread_count = option_threads();
    for (u32 idx = 0; idx < thread_count; ++idx)
    {
        threads.emplace_back([this, idx, thread_count]()
                             {
                                 if (8 < thread_count)
                                 {
                                     WinProcGroup::bind(idx);
                                 }
                                 size_t stride = cluster_count / thread_count;
                                 auto *pcluster = clusters + idx * stride;
                                 size_t count = idx != thread_count - 1 ?
                                                 stride :
                                                 cluster_count - idx * stride;
                                 std::memset(pcluster, 0, count * sizeof (TCluster));
                             });
    }
    for (auto &th : threads)
    {
        th.join();
    }
    threads.clear();
    //sync_cout << "info string Hash cleared" << sync_endl;
}

/// TTable::probe() looks up the entry in the transposition table.
/// If the position is found, it returns true and a pointer to the found entry.
/// Otherwise, it returns false and a pointer to an empty or least valuable entry to be replaced later.
TEntry* TTable::probe(Key key, bool &hit) const
{
    return cluster(key)->probe(u16(key >> 0x30), hit);
}
/// TTable::hash_full() returns an approximation of the per-mille of the 
/// all transposition entries during a search which have received
/// at least one write during the current search.
/// It is used to display the "info hashfull ..." information in UCI.
/// "the hash is <x> per mill full", the engine should send this info regularly.
/// hash, are using <x>%. of the state of full.
u32 TTable::hash_full() const
{
    size_t fresh_entry_count = 0;
    const auto cluster_limit = std::min(size_t(1000 / TCluster::EntryCount), cluster_count);
    for (auto const *itc = clusters; itc < clusters + cluster_limit; ++itc)
    {
        fresh_entry_count += itc->fresh_entry_count();
    }
    return u32(fresh_entry_count * 1000 / (cluster_limit * TCluster::EntryCount));
}

/// TTable::extract_opp_move() extracts opponent's move.
Move TTable::extract_opp_move(Position &pos, Move bm) const
{
    assert(MOVE_NONE != bm
        && MoveList<GenType::LEGAL>(pos).contains(bm));

    StateInfo si;
    pos.do_move(bm, si);
    bool tt_hit;
    auto *tte = probe(pos.si->posi_key, tt_hit);
    auto pm = tt_hit ?
                tte->move() :
                MOVE_NONE;
    if (   MOVE_NONE != pm
        && !(   pos.pseudo_legal(pm)
             && pos.legal(pm)))
    {
        pm = MOVE_NONE;
    }
    assert(MOVE_NONE == pm
        || MoveList<GenType::LEGAL>(pos).contains(pm));
    pos.undo_move(bm);

    return pm;
}

/// TTable::save() saves hash to file
void TTable::save(string const &hash_fn) const
{
    if (white_spaces(hash_fn))
    {
        return;
    }
    ofstream ofs(hash_fn, ios_base::out|ios_base::binary);
    if (!ofs.is_open())
    {
        return;
    }
    ofs << *this;
    ofs.close();
    sync_cout << "info string Hash saved to file \'" << hash_fn << "\'" << sync_endl;
}
/// TTable::load() loads hash from file
void TTable::load(string const &hash_fn)
{
    if (white_spaces(hash_fn))
    {
        return;
    }
    ifstream ifs(hash_fn, ios_base::in|ios_base::binary);
    if (!ifs.is_open())
    {
        return;
    }
    ifs >> *this;
    ifs.close();
    sync_cout << "info string Hash loaded from file \'" << hash_fn << "\'" << sync_endl;
}
