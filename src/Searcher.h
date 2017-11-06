#ifndef _SEARCHER_H_INC_
#define _SEARCHER_H_INC_

#include <array>
#include <vector>
#include <limits>

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
    i16  perft;         // Perft depth

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
        perft = 0;
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

/// Table2D is a Generic 2-dimensional array used to store various statistics.
template<u32 Size1, u32 Size2, typename T>
struct Table2D
    : public std::array<std::array<T, Size2>, Size1>
{
    void fill (const T &val)
    {
        T *ptr = &(*this)[0][0];
        std::fill (ptr, ptr + sizeof (*this) / sizeof (*ptr), val);
    }

    void update (T &entry, i32 bonus, const i16 D)
    {
        assert(abs (32*D) < (std::numeric_limits<T>::max)()); // Ensure range is [-32 * D, +32 * D]
        assert(abs (bonus) <= D); // Ensure bonus don't overflow
        entry += T(32*bonus - entry*abs (bonus) / D);
        assert(abs (entry) <= 32*D);
    }
};

/// ButterflyStatTable store stats indexed by [color][move's org and dst squares].
typedef Table2D<CLR_NO, SQ_NO*SQ_NO, i16> ButterflyStatTable;
/// ButterflyHistory records how often quiet moves have been successful or unsuccessful
/// during the current search, and is used for reduction and move ordering decisions.
struct ButterflyHistory
    : public ButterflyStatTable
{
    // Update by color, move (org-dst), bonus
    void update (Color c, Move m, i32 bonus)
    {
        Table2D::update ((*this)[c][move_pp (m)], bonus, 324);
    }
};

/// PieceDestinyStatTable store stats indexed by [piece][destiny].
typedef Table2D<MAX_PIECE, SQ_NO, i16> PieceDestinyStatTable;
/// PieceDestinyHistory is based on PieceDestinyStatTable.
struct PieceDestinyHistory
    : public PieceDestinyStatTable
{
    /// Update by piece, destiny, bonus
    void update (Piece pc, Square dst, i32 bonus)
    {
        Table2D::update ((*this)[pc][dst], bonus, 936);
    }
};

/// ContinuationStatTable is the history of a given pair of moves, usually the current one given a previous one.
/// History table is based on PieceDestinyStatTable instead of ButterflyStatTable.
typedef Table2D<MAX_PIECE, SQ_NO, PieceDestinyHistory> ContinuationStatTable;
typedef ContinuationStatTable ContinuationHistory;

/// PieceDestinyMoveTable stores counter moves indexed by [piece][destiny]
typedef Table2D<MAX_PIECE, SQ_NO, Move> PieceDestinyMoveTable;
typedef PieceDestinyMoveTable PieceDestinyMoveHistory;

/// Table3D is a Generic 3-dimensional array used to store various statistics
template<int Size1, int Size2, int Size3, typename T>
struct Table3D
    : public std::array<std::array<std::array<T, Size3>, Size2>, Size1>
{
    void fill (const T &val)
    {
        T *ptr = &(*this)[0][0][0];
        std::fill (ptr, ptr + sizeof (*this) / sizeof (*ptr), val);
    }

    void update (T &entry, i32 bonus, const i16 D, const i16 W)
    {
        assert (abs (W*D) < (std::numeric_limits<T>::max)()); // Ensure range is [-W * D, +W * D]
        assert (abs (bonus) <= D); // Ensure we don't overflow
        entry += T(W*bonus - entry*abs (bonus) / D);
        assert (abs (entry) <= W*D);
    }
};

/// CapturePieceDestinyTable stores stats indexed by [piece][destiny][captured piece type]
typedef Table3D<MAX_PIECE, SQ_NO, MAX_PTYPE, i16> CapturePieceDestinyStatTable;
/// CapturePieceDestinyHistory is based on CapturePieceDestinyStatTable
struct CapturePieceDestinyHistory
    : public CapturePieceDestinyStatTable
{
    void update (Piece pc, Square dst, PieceType ct, i32 bonus)
    {
        Table3D::update ((*this)[pc][dst][ct], bonus, 324, 2);
    }
};


/// MovePicker class is used to pick one legal moves from the current position.
class MovePicker
{
private:
    const Position &pos;

    Move tt_move;
    Value threshold;
    Square recap_sq;

    const PieceDestinyHistory **piece_destiny_history;

    size_t i;
    ValMoves moves;
    std::vector<Move> killers_moves
        ,             bad_capture_moves;

    u08 stage;

    template<GenType GT>
    void value ();

    const ValMove& next_max_move ();

public:
    bool skip_quiets;

    MovePicker () = delete;
    MovePicker (const MovePicker&) = delete;
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
    bool operator<= (const RootMove &rm) const { return new_value != rm.new_value ? new_value > rm.new_value : old_value >= rm.old_value; }
    bool operator>= (const RootMove &rm) const { return new_value != rm.new_value ? new_value < rm.new_value : old_value <= rm.old_value; }
    bool operator== (const RootMove &rm) const { return new_value == rm.new_value; }
    bool operator!= (const RootMove &rm) const { return new_value != rm.new_value; }

    bool operator== (Move m) const { return front () == m; }
    bool operator!= (Move m) const { return front () != m; }

    void operator+= (Move m) { emplace_back (m); }
    //void operator-= (Move m) { erase (std::remove (begin (), end (), m), end ()); }

    bool extract_ponder_move_from_tt (Position&);
    bool draw (Position&) const;

    explicit operator std::string () const;
};

template<typename CharT, typename Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, const RootMove &root_move)
{
    os << std::string(root_move);
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
    operator<< (std::basic_ostream<CharT, Traits> &os, const RootMoves &root_moves)
{
    os << std::string(root_moves);
    return os;
}

namespace Searcher {

    // Threshold for countermoves based pruning
    const i32 CounterMovePruneThreshold = 0;

    extern Limit Limits;

    extern size_t MultiPV;
    //extern i32   MultiPV_cp;

    extern i16   FixedContempt
        ,        ContemptTime 
        ,        ContemptValue;

    extern std::string HashFile;
    extern bool RetainHash;

    extern bool  OwnBook;
    extern std::string BookFile;
    extern bool  BookMoveBest;
    extern i16   BookUptoMove;

    extern i16   TBProbeDepth;
    extern i32   TBLimitPiece;
    extern bool  TBUseRule50;
    extern bool  TBHasRoot;
    extern Value TBValue;

    extern std::string OutputFile;

    extern void initialize ();

    extern void clear ();
}

#endif // _SEARCHER_H_INC_
