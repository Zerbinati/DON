#ifndef _SEARCHER_H_INC_
#define _SEARCHER_H_INC_

#include <array>
#include <vector>

#include "MoveGenerator.h"
#include "Position.h"
#include "Type.h"

// Limit stores information sent by GUI about available time to search the current move.
//  - Maximum time and increment
//  - Maximum depth
//  - Maximum nodes
//  - Maximum mate
//  - Search moves
//  - Infinite analysis mode
//  - Ponder (think while is opponent's side to move) mode
struct Limit
{
public:
    // Clock struct stores the time and inc per move in milli-seconds.
    struct Clock
    {
        u64 time;
        u32 inc;
    } clock[CLR_NO];    // Search with Clock

    u08   movestogo;    // Search <x> moves to the next time control

    u64   movetime;     // Search <x> exact time in milli-seconds
    i16   depth;        // Search <x> depth (plies) only
    u64   nodes;        // Search <x> nodes only
    u08   mate;         // Search mate in <x> moves
    bool  infinite;     // Search until the "stop" command
    bool  ponder;       // Search on ponder move until the "stop" command
    
    Moves search_moves; // Restrict search to these root moves only

    TimePoint start_time;

    Limit ()
    {
        clock[WHITE].time =
        clock[BLACK].time = 0;
        clock[WHITE].inc =
        clock[BLACK].inc = 0;
        movestogo = 0;
        ponder = false;
        clear_nontime ();
    }

    void clear_nontime ()
    {
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

// StatBoards is a Generic 2-dimensional array used to store various statistics.
template<i32 Size1, i32 Size2, typename T = i32>
struct BoardStats
    : public std::array<std::array<T, Size2>, Size1>
{
    void fill (const T &v)
    {
        T *ptr = &(*this)[0][0];
        std::fill (ptr, ptr + sizeof (*this) / sizeof (*ptr), v);
    }
};

// HistoryStats indexed by [color][move's org and dst squares].
typedef BoardStats<CLR_NO, SQ_NO*SQ_NO> HistoryBoardStats;
// HistoryStats records how often quiet moves have been successful or unsuccessful
// during the current search, and is used for reduction and move ordering decisions.
struct HistoryStats
    : public HistoryBoardStats
{
    // Color, Move, Value
    void update (Color c, Move m, i32 v)
    {
        const i32 D = 324;
        assert(abs (v) <= D); // Consistency check
        auto &e = (*this)[c][move_pp (m)];
        e += v*32 - e*abs (v)/D;
        assert(abs (e) <= 32 * D);
    }
};

// PieceToBoards are addressed by a move's [piece][destiny] information.
typedef BoardStats<MAX_PIECE, SQ_NO> SquareHistoryBoardStats;
// PieceToHistory is like HistoryStats, but is based on SquareHistoryBoardStats.
struct SquareHistoryStats
    : public SquareHistoryBoardStats
{
    // Piece, Destiny, Value
    void update (Piece pc, Square s, i32 v)
    {
        const i32 D = 936;
        assert(abs (v) <= D); // Consistency check
        auto &e = (*this)[pc][s];
        e += v*32 - e*abs (v)/D;
        assert(abs (e) <= 32 * D);
    }
};

// MoveHistoryBoardStats
typedef BoardStats<MAX_PIECE, SQ_NO, SquareHistoryStats> MoveHistoryBoardStats;

// SquareMoveBoardStats stores counter moves indexed by [piece][destiny]
typedef BoardStats<MAX_PIECE, SQ_NO, Move> SquareMoveBoardStats;

// The root of the tree is a PV node.
// At a PV node all the children have to be investigated.
// The best move found at a PV node leads to a successor PV node,
// while all the other investigated children are CUT nodes
// At a CUT node the child causing a beta cut-off is an ALL node
// In a perfectly ordered tree only one child of a CUT node has to be explored
// At an ALL node all the children have to be explored. The successors of an ALL node are CUT nodes
// NonPV nodes = CUT nodes + ALL nodes
//
// RootMove class is used for moves at the root of the tree.
// RootMove stores:
//  - New/Old values
//  - PV (really a refutation table in the case of moves which fail low)
// Value is normally set at -VALUE_INFINITE for all non-pv moves.
class RootMove
    : public Moves
{
public:
    Value old_value
        , new_value;

    explicit RootMove (Move m = MOVE_NONE)
        : Moves (1, m)
        , old_value (-VALUE_INFINITE)
        , new_value (-VALUE_INFINITE)
    {}
    RootMove& operator= (const RootMove&) = default;

    bool operator<  (const RootMove &rm) const { return new_value != rm.new_value ? new_value > rm.new_value : old_value >  rm.old_value; }
    bool operator>  (const RootMove &rm) const { return new_value != rm.new_value ? new_value < rm.new_value : old_value <  rm.old_value; }
    bool operator<= (const RootMove &rm) const { return new_value != rm.new_value ? new_value > rm.new_value : old_value >= rm.old_value; }
    bool operator>= (const RootMove &rm) const { return new_value != rm.new_value ? new_value < rm.new_value : old_value <= rm.old_value; }
    bool operator== (const RootMove &rm) const { return new_value == rm.new_value; }
    bool operator!= (const RootMove &rm) const { return new_value != rm.new_value; }

    bool operator== (Move m) const { return at (0) == m; }
    bool operator!= (Move m) const { return at (0) != m; }

    void operator+= (Move m) { push_back (m); }
    void operator-= (Move m) { erase (std::remove (begin (), end (), m), end ()); }

    bool extract_ponder_move_from_tt (Position &pos);

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

    void operator+= (const RootMove &rm) { push_back (rm); }
    void operator-= (const RootMove &rm) { erase (std::remove (begin (), end (), rm), end ()); }

    void initialize (const Position &pos, const Moves &moves)
    {
        clear ();
        for (const auto &vm : MoveGen::MoveList<GenType::LEGAL> (pos))
        {
            if (   moves.empty ()
                || std::find (moves.begin (), moves.end (), vm.move) != moves.end ())
            {
                *this += RootMove (vm.move);
            }
        }
        shrink_to_fit ();
    }

    explicit operator std::string () const;
};

template<typename CharT, typename Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, const RootMoves &root_moves)
{
    os << std::string(root_moves);
    return os;
}


const u08 MaxKillers = 2;
// Stack keeps the information of the nodes in the tree during the search.
struct Stack
{
public:
    i16   ply;
    Move  current_move;
    Move  killer_moves[MaxKillers];

    Value static_eval;
    i32   statistics;
    u08   move_count;
    Moves pv;

    SquareHistoryStats *m_history;
};

// MovePicker class is used to pick one legal moves from the current position.
class MovePicker
{
private:
    const Position &_pos;
    const Stack *const _ss;

    Move _tt_move;
    Square _recap_sq;
    Value _threshold;
    i16 _depth;
    u08 _stage;
    u08 _index;
    ValMoves _moves;
    Moves _killer_moves
        , _capture_moves;

    template<GenType GT>
    void value ();

    ValMove& swap_best_move (u08 i);

public:
    MovePicker () = delete;
    MovePicker (const MovePicker&) = delete;
    MovePicker& operator= (const MovePicker&) = delete;

    MovePicker (const Position&, Move, const Stack *const&);
    MovePicker (const Position&, Move, const Stack *const&, i16);
    MovePicker (const Position&, Move, Value);

    Move next_move (bool skip_quiets = false);
};

namespace Searcher {

    extern Limit Limits;

    extern u08   MultiPV;
    //extern i32   MultiPV_cp;

    extern i16   FixedContempt
        ,        ContemptTime 
        ,        ContemptValue;

    extern std::string HashFile;
    
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

    template<bool RootNode = true>
    extern u64 perft (Position &pos, i16 depth);

    extern void initialize ();

    extern void clear ();
}

#endif // _SEARCHER_H_INC_
