#ifndef _SEARCHER_H_INC_
#define _SEARCHER_H_INC_

#include <array>
#include <list>
#include <limits>
#include <type_traits>

#include "MoveGenerator.h"
#include "Position.h"
#include "Type.h"

/// Limit stores information sent by GUI about available time to search the current move.
///  - Maximum time and increment
///  - Maximum depth
///  - Maximum nodes
///  - Maximum mate
///  - Search moves
///  - Infinite analysis mode
///  - Ponder (think while is opponent's side to move) mode
struct Limit
{
public:
    // Clock struct stores the time and inc per move in milli-seconds.
    struct Clock
    {
        TimePoint time;
        TimePoint inc;
    } clock[CLR_NO];        // Search with Clock
    u08       movestogo;    // Search <x> moves to the next time control

    TimePoint movetime;    // Search <x> exact time in milli-seconds
    i16       depth;       // Search <x> depth (plies) only
    u64       nodes;       // Search <x> nodes only
    u08       mate;        // Search mate in <x> moves
    bool      infinite;    // Search until the "stop" command

    Limit ()
        : clock {}
        , movestogo (0)
        , movetime (TimePoint(0))
        , depth (0)
        , nodes (0)
        , mate (0)
        , infinite (false)
    {}

    bool time_mgr_used () const
    {
        return !infinite
            && 0 == movetime
            && DepthZero == depth
            && 0 == nodes
            && 0 == mate;
    }

    bool mate_search () const
    {
        return 0 != mate;
    }
};

/// StatsEntry stores the stats table value. It is usually a number but could
/// be a move or even a nested history. We use a class instead of naked value
/// to directly call history update operator<<() on the entry so to use stats
/// tables at caller sites as simple multi-dim arrays.
template<typename T, i32 D>
class StatsEntry
{
private:

    T entry;

public:

    void operator= (const T &v) { entry = v; }
    T* operator& () { return &entry; }
    T* operator-> () { return &entry; }
    operator const T& () const { return entry; }

    void operator<< (i32 bonus)
    {
        static_assert (D <= std::numeric_limits<T>::max (), "D overflows T");
        assert(abs (bonus) <= D); // Ensure range is [-D, +D]

        entry += T(bonus - entry * abs (bonus) / D);

        assert(abs (entry) <= D);
    }
};

/// Stats is a generic N-dimensional array used to store various statistics.
/// The first template T parameter is the base type of the array,
/// the D parameter limits the range of updates (range is [-D, +D]), and
/// the last parameters (Size and Sizes) encode the dimensions of the array.
template <typename T, i32 D, i32 Size, i32... Sizes>
struct Stats
    : public std::array<Stats<T, D, Sizes...>, Size>
{
    typedef Stats<T, D, Size, Sizes...> stats;

    void fill (const T &v)
    {
        // For standard-layout 'this' points to first struct member
        assert(std::is_standard_layout<stats>::value);

        auto *p = reinterpret_cast<StatsEntry<T, D>*>(this);
        std::fill (p, p + sizeof (*this) / sizeof (*p), v);
    }
};
template <typename T, i32 D, i32 Size>
struct Stats<T, D, Size>
    : public std::array<StatsEntry<T, D>, Size>
{
};

/// ButterflyHistory records how often quiet moves have been successful or unsuccessful
/// during the current search, and is used for reduction and move ordering decisions, indexed by [color][move].
typedef Stats<i16, 10692, CLR_NO, SQ_NO*SQ_NO>              ButterflyHistory;
/// CaptureHistory stores capture history, indexed by [piece][square][captured type]
typedef Stats<i16, 10692, MAX_PIECE, SQ_NO, PT_NO>          CaptureHistory;
/// PieceDestinyHistory is like ButterflyHistory, indexed by [piece][square]
typedef Stats<i16, 29952, MAX_PIECE, SQ_NO>                 PieceDestinyHistory;
/// ContinuationHistory is the combined history of a given pair of moves, usually the current one given a previous one.
/// The nested history table is based on PieceDestinyHistory, indexed by [piece][square]
typedef Stats<PieceDestinyHistory, 0, MAX_PIECE, SQ_NO>     ContinuationHistory;
/// MoveHistory stores moves, indexed by [piece][square]
typedef Stats<Move, 0, MAX_PIECE, SQ_NO>                    MoveHistory;

/// MovePicker class is used to pick one legal moves from the current position.
class MovePicker
{
private:
    enum Stage : u08
    {
        NT_TT, NT_INIT, NT_GOOD_CAPTURES, NT_REFUTATIONS, NT_QUIETS, NT_BAD_CAPTURES,
        EV_TT, EV_INIT, EV_MOVES,
        PC_TT, PC_INIT, PC_CAPTURES,
        QS_TT, QS_INIT, QS_CAPTURES, QS_CHECKS,
    };

    const Position &pos;

    Move tt_move;
    i16 depth;

    const PieceDestinyHistory **pd_histories;

    Value threshold;
    Square recap_sq;

    ValMoves moves;

    std::vector<Move> refutation_moves
        ,             bad_capture_moves;

    u08 stage;
    size_t i;
    ValMove vm;

    template<GenType>
    void value ();

    template<typename Pred>
    bool pick (Pred);

public:
    bool skip_quiets;

    MovePicker () = delete;
    MovePicker (const MovePicker&) = delete;
    MovePicker& operator= (const MovePicker&) = delete;

    MovePicker (const Position&, Move, i16, const PieceDestinyHistory**, const Move*, Move);
    MovePicker (const Position&, Move, i16, const PieceDestinyHistory**, Square);
    MovePicker (const Position&, Move, Value);

    Move next_move ();
};

/// The root of the tree is a PV node.
/// At a PV node all the children have to be investigated.
/// The best move found at a PV node leads to a successor PV node,
/// while all the other investigated children are CUT nodes
/// At a CUT node the child causing a beta cut-off is an ALL node
/// In a perfectly ordered tree only one child of a CUT node has to be explored
/// At an ALL node all the children have to be explored. The successors of an ALL node are CUT nodes
/// NonPV nodes = CUT nodes + ALL nodes
///
/// RootMove class is used for moves at the root of the tree.
/// RootMove stores:
///  - New/Old values
///  - SelDepth
///  - PV (really a refutation table in the case of moves which fail low)
/// Value is normally set at -VALUE_INFINITE for all non-pv moves.
class RootMove
    : public std::list<Move>
{
public:
    Value old_value
        , new_value;

    i16   sel_depth;
    i16   tb_rank;
    Value tb_value;

    explicit RootMove (Move m = MOVE_NONE)
        : std::list<Move> (1, m)
        , old_value (-VALUE_INFINITE)
        , new_value (-VALUE_INFINITE)
        , sel_depth (0)
        , tb_rank (0)
        , tb_value (VALUE_ZERO)
    {}
    RootMove& operator= (const RootMove&) = default;

    bool operator<  (const RootMove &rm) const { return new_value != rm.new_value ? new_value > rm.new_value : old_value >  rm.old_value; }
    bool operator>  (const RootMove &rm) const { return new_value != rm.new_value ? new_value < rm.new_value : old_value <  rm.old_value; }
    //bool operator<= (const RootMove &rm) const { return new_value != rm.new_value ? new_value > rm.new_value : old_value >= rm.old_value; }
    //bool operator>= (const RootMove &rm) const { return new_value != rm.new_value ? new_value < rm.new_value : old_value <= rm.old_value; }
    //bool operator== (const RootMove &rm) const { return front () == rm.front (); }
    //bool operator!= (const RootMove &rm) const { return front () != rm.front (); }

    bool operator== (Move m) const { return front () == m; }
    bool operator!= (Move m) const { return front () != m; }

    void operator+= (Move m) { push_back (m); }
    //void operator-= (Move m) { erase (std::remove (begin (), end (), m), end ()); }

    explicit operator std::string () const;
};

template<typename CharT, typename Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, const RootMove &rm)
{
    os << std::string(rm);
    return os;
}

class RootMoves
    : public std::vector<RootMove>
{
public:
    RootMoves () = default;
    RootMoves (const RootMoves&) = default;
    RootMoves& operator= (const RootMoves&) = default;

    void operator+= (Move m) { emplace_back (m); }
    //void operator-= (Move m) { erase (std::remove (begin (), end (), m), end ()); }
    void operator+= (const RootMove &rm) { push_back (rm); }
    //void operator-= (const RootMove &rm) { erase (std::remove (begin (), end (), rm), end ()); }

    void initialize (const Position&, const std::vector<Move>&);

    explicit operator std::string () const;
};

template<typename CharT, typename Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, const RootMoves &rms)
{
    os << std::string(rms);
    return os;
}

namespace Searcher {

    // Threshold for counter moves based pruning
    constexpr i32 CounterMovePruneThreshold = 0;
    
    extern Limit Limits;

    extern i16 TBProbeDepth;
    extern i32 TBLimitPiece;
    extern bool TBUseRule50;
    extern bool TBHasRoot;

    extern void initialize ();

    extern void clear ();

}

#endif // _SEARCHER_H_INC_
