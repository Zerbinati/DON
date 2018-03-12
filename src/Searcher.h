#ifndef _SEARCHER_H_INC_
#define _SEARCHER_H_INC_

#include <array>
#include <vector>
#include <limits>
#include <type_traits>

#include "MoveGenerator.h"
#include "Position.h"
#include "Type.h"

const u08 MaxKillers = 2;

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
        u64 time;
        u32 inc;
    } clock[CLR_NO];    // Search with Clock
    u08  movestogo;     // Search <x> moves to the next time control

    u64  movetime;      // Search <x> exact time in milli-seconds
    i16  depth;         // Search <x> depth (plies) only
    u64  nodes;         // Search <x> nodes only
    u08  mate;          // Search mate in <x> moves
    bool infinite;      // Search until the "stop" command

    TimePoint start_time;

    Limit ()
    {
        clock[WHITE].time =
        clock[BLACK].time = 0;
        clock[WHITE].inc =
        clock[BLACK].inc = 0;
        movestogo = 0;

        movetime = 0;
        depth = 0;
        nodes = 0;
        mate = 0;
        infinite = false;
    }

    bool use_time_management () const
    {
        return !infinite
            && 0 == movetime
            && 0 == depth
            && 0 == nodes
            && 0 == mate;
    }
};

/// StatsEntry stores the stats table value. It is usually a number but could
/// be a move or even a nested history. We use a class instead of naked value
/// to directly call history update operator<<() on the entry so to use stats
/// tables at caller sites as simple multi-dim arrays.
template<typename T, i32 W, i32 D>
class StatsEntry
{
private:

    static const bool IsInt = std::is_integral<T>::value;
    typedef typename std::conditional<IsInt, i32, T>::type TT;

    T entry;

public:

    T* get () { return &entry; }
    void operator= (const T &v) { entry = v; }
    operator TT() const { return entry; }

    void operator<< (i32 bonus)
    {
        assert(abs (bonus) <= D); // Ensure range is [-W * D, W * D]
        assert(abs (W * D) < std::numeric_limits<T>::max ()); // Ensure we don't overflow

        entry += bonus * W - entry * abs (bonus) / D;

        assert(abs (entry) <= W * D);
    }
};

/// Stats is a generic N-dimensional array used to store various statistics.
/// The first template T parameter is the base type of the array, the W parameter
/// is the weight applied to the bonuses when we update values with the << operator,
/// the D parameter limits the range of updates (range is [-W * D, W * D]), and
/// the last parameters (Size and Sizes) encode the dimensions of the array.
template <typename T, i32 W, i32 D, i32 Size, i32... Sizes>
struct Stats
    : public std::array<Stats<T, W, D, Sizes...>, Size>
{
    T* get () { return this->at (0).get (); }

    void fill (const T &v)
    {
        T *p = get ();
        std::fill (p, p + sizeof (*this) / sizeof (*p), v);
    }
};
template <typename T, i32 W, i32 D, i32 Size>
struct Stats<T, W, D, Size>
    : public std::array<StatsEntry<T, W, D>, Size>
{
    T* get () { return this->at (0).get (); }
};

/// ButterflyHistory records how often quiet moves have been successful or unsuccessful
/// during the current search, and is used for reduction and move ordering decisions.
/// It is indexed by [color][move].
typedef Stats<i16, 32, 324, CLR_NO, SQ_NO*SQ_NO> ButterflyHistory;

/// PieceDestinyHistory is like ButterflyHistory but is indexed by [piece][destiny]
typedef Stats<i16, 32, 936, MAX_PIECE, SQ_NO> PieceDestinyHistory;

/// ContinuationHistory is the combined history of a given pair of moves, usually the current one given a previous one.
/// The nested history table is based on PieceDestinyHistory instead of ButterflyBoards.
typedef Stats<PieceDestinyHistory, 32, 0, MAX_PIECE, SQ_NO> ContinuationHistory;

/// CapturePieceDestinyHistory is indexed by [piece][move][captured piece type]
typedef Stats<i16, 2, 324, MAX_PIECE, SQ_NO*SQ_NO, MAX_PTYPE> CapturePieceDestinyHistory;

/// PieceDestinyMove stores counter moves is indexed by [piece][move]
typedef Stats<Move, 0, 0, MAX_PIECE, SQ_NO*SQ_NO> PieceDestinyMove;

/// MovePicker class is used to pick one legal moves from the current position.
class MovePicker
{
private:
    enum Stage : u08
    {
        NAT_TT, NAT_CAPTURE_INIT, NAT_GOOD_CAPTURES, NAT_QUIET_INIT, NAT_QUIETS, NAT_BAD_CAPTURES,
        EVA_TT, EVA_EVASION_INIT, EVA_EVASIONS,
        PC_TT, PC_CAPTURE_INIT, PC_GOOD_CAPTURES, PC_BAD_CAPTURES,
        QS_TT, QS_CAPTURE_INIT, QS_CAPTURES, QS_CHECK_INIT, QS_CHECKS,
    };

    const i32 MaxValue = 1 << 28;

    const Position &pos;

    Move tt_move;
    i16 depth;
    Value threshold;
    Square recap_sq;

    const PieceDestinyHistory **piece_destiny_history;

    size_t i;
    ValMoves moves;
    std::vector<Move> refutation_moves
        ,             bad_capture_moves;

    u08 stage;

    template<GenType>
    void value ();

    const ValMove& next_max_move ();

public:
    bool pick_quiets;

    MovePicker () = delete;
    //MovePicker (const MovePicker&) = delete;
    MovePicker& operator= (const MovePicker&) = delete;

    MovePicker (const Position&, Move, i16, const PieceDestinyHistory**, const Move*, Move);
    MovePicker (const Position&, Move, i16, Square);
    MovePicker (const Position&, Move, Value);

    Move next_move ();
};

/// Stack keeps the information of the nodes in the tree during the search.
struct Stack
{
public:
    i16   ply;
    Move  played_move;
    Move  excluded_move;
    Move  killer_moves[MaxKillers];
    u08   move_count;
    Value static_eval;
    i32   stat_score;
    PieceDestinyHistory *piece_destiny_history;

    std::vector<Move> pv;
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
    : public std::vector<Move>
{
public:
    Value old_value
        , new_value;
    
    i16 sel_depth;

    explicit RootMove (Move m = MOVE_NONE)
        : std::vector<Move> (1, m)
        , old_value (-VALUE_INFINITE)
        , new_value (-VALUE_INFINITE)
        , sel_depth (0)
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

    void operator+= (Move m) { emplace_back (m); }
    //void operator-= (Move m) { erase (std::remove (begin (), end (), m), end ()); }

    bool extract_ponder_move_from_tt (Position&);

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
    void operator+= (const RootMove &rm) { emplace_back (rm); }
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

    // Threshold for countermoves based pruning
    const i32 CounterMovePruneThreshold = 0;

    extern Limit Limits;

    extern i32 MultiPV;
    //extern i32 MultiPV_cp;

    extern i16 FixedContempt
        ,      ContemptTime 
        ,      ContemptValue;

    extern std::string HashFile;
    extern bool RetainHash;

    extern i16 TBProbeDepth;
    extern i32 TBLimitPiece;
    extern bool TBUseRule50;
    extern bool TBHasRoot;
    extern Value TBValue;

    extern std::string OutputFile;

    extern void initialize ();

    extern void clear ();

}

#endif // _SEARCHER_H_INC_
