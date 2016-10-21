#ifndef SEARCHER_H_INC_
#define SEARCHER_H_INC_

#include <atomic>

#include "Type.h"
#include "Position.h"
#include "MoveGenerator.h"

// Limit stores information sent by GUI about available time to search the current move.
//  - Maximum time and increment.
//  - Maximum depth.
//  - Maximum nodes.
//  - Maximum mate.
//  - Search moves.
//  - Infinite analysis mode.
//  - Ponder (think while is opponent's side to move) mode.
struct Limit
{
public:
    // Clock struct stores the Remaining-time and Increment-time per move in milli-seconds
    struct Clock
    {
        TimePoint time  = 0;        // Remaining Time          [milli-seconds]
        TimePoint inc   = 0;        // Increment Time per move [milli-seconds]
    }         clock[CLR_NO];        // Search with Clock
    TimePoint movetime  = 0;        // Search <x> exact time in milli-seconds
    u08       movestogo = 0;        // Search <x> moves to the next time control
    i16       depth     = 0;        // Search <x> depth (plies) only
    u64       nodes     = 0;        // Search <x> nodes only
    u08       mate      = 0;        // Search mate in <x> moves
    bool      infinite  = false;    // Search until the "stop" command
    bool      ponder    = false;    // Search on ponder move until the "stop" command
    MoveVector search_moves;        // Restrict search to these root moves only

    TimePoint start_time = 0;
    TimePoint elapsed_time = 0;

    bool use_time_management () const
    {
        return !infinite
            && movetime == 0
            && depth    == 0
            && nodes    == 0
            && mate     == 0;
    }
};

template<bool CM>
struct PieceValueStats
{
private:
    Value _table[CLR_NO][NONE][SQ_NO];

public:
    void clear ()
    {
        for (auto c = WHITE; c <= BLACK; ++c)
        {
            for (auto pt = PAWN; pt < NONE; ++pt)
            {
                for (auto s = SQ_A1; s <= SQ_H8; ++s)
                {
                    _table[c][pt][s] = VALUE_ZERO;
                }
            }
        }
    }
    // Piece, Destiny
    Value operator() (Piece pc, Square s) const
    {
        assert(pc != NO_PIECE);
        return _table[color (pc)][ptype (pc)][s];
    }
    // Piece, Destiny, Value
    void update (Piece pc, Square s, Value v)
    {
        assert(pc != NO_PIECE);
        i32 x = abs(i32(v));
        if (x < 324)
        {
            auto &e = _table[color (pc)][ptype (pc)][s];
            e = e*(1.0 - double(x) / (CM ? 936 : 324)) + i32(v)*32;
        }
    }
};

typedef PieceValueStats<false>   FPieceValueStats;
typedef PieceValueStats<true >   TPieceValueStats;

struct PieceCMValueStats
{
private:
    TPieceValueStats _table[CLR_NO][NONE][SQ_NO];

public:
    void clear ()
    {
        for (auto c = WHITE; c <= BLACK; ++c)
        {
            for (auto pt = PAWN; pt < NONE; ++pt)
            {
                for (auto s = SQ_A1; s <= SQ_H8; ++s)
                {
                    _table[c][pt][s].clear ();
                }
            }
        }
    }

    TPieceValueStats& operator() (Piece pc, Square s)
    {
        assert(pc != NO_PIECE);
        return _table[color (pc)][ptype (pc)][s];
    }
};

struct ColorValueStats
{
private:
    Value _table[CLR_NO][SQ_NO*SQ_NO];

public:
    void clear ()
    {
        for (auto c = WHITE; c <= BLACK; ++c)
        {
            for (auto m = 0; m < SQ_NO*SQ_NO; ++m)
            {
                _table[c][m] = VALUE_ZERO;
            }
        }
    }

    Value operator() (Color c, Move m) const
    {
        return _table[c][m & 0xFFF];
    }

    void update (Color c, Move m, Value v)
    {
        i32 x = abs(i32(v));
        if (x < 324)
        {
            auto &e = _table[c][m & 0xFFF];
            e = e*(1.0 - double(x) / 324) + i32(v)*32;
        }
    }
};

struct PieceCMoveStats
{
private:
    Move _table[CLR_NO][NONE][SQ_NO];

public:
    void clear ()
    {
        for (auto c = WHITE; c <= BLACK; ++c)
        {
            for (auto pt = PAWN; pt < NONE; ++pt)
            {
                for (auto s = SQ_A1; s <= SQ_H8; ++s)
                {
                    _table[c][pt][s] = MOVE_NONE;
                }
            }
        }
    }

    Move operator() (Piece pc, Square s) const
    {
        assert(ptype (pc) != NONE);
        return _table[color (pc)][ptype (pc)][s];
    }

    void update (Piece pc, Square s, Move cm)
    {
        assert(ptype (pc) != NONE);
        _table[color (pc)][ptype (pc)][s] = cm;
    }
};

// The root of the tree is a PV node.
// At a PV node all the children have to be investigated.
// The best move found at a PV node leads to a successor PV node,
// while all the other investigated children are CUT nodes
// At a CUT node the child causing a beta cut-off is an ALL node.
// In a perfectly ordered tree only one child of a CUT node has to be explored.
// At an ALL node all the children have to be explored. The successors of an ALL node are CUT nodes.
// NonPV nodes = CUT nodes + ALL nodes
//
// RootMove is used for moves at the root of the tree.
// Root move stores:
//  - New/Old values.
//  - PV (really a refutation table in the case of moves which fail low).
// Value is normally set at -VALUE_INFINITE for all non-pv moves.
class RootMove
    : public MoveVector
{
public:
    Value old_value = -VALUE_INFINITE
        , new_value = -VALUE_INFINITE;

    explicit RootMove (Move m = MOVE_NONE)
        : MoveVector (1, m)
    {}
    RootMove& operator= (const RootMove&) = default;

    bool operator<  (const RootMove &rm) const { return new_value >  rm.new_value; }
    bool operator>  (const RootMove &rm) const { return new_value <  rm.new_value; }
    bool operator<= (const RootMove &rm) const { return new_value >= rm.new_value; }
    bool operator>= (const RootMove &rm) const { return new_value <= rm.new_value; }
    bool operator== (const RootMove &rm) const { return new_value == rm.new_value; }
    bool operator!= (const RootMove &rm) const { return new_value != rm.new_value; }

    bool operator== (Move m) const { return at (0) == m; }
    bool operator!= (Move m) const { return at (0) != m; }

    void operator+= (Move m) { push_back (m); }
    void operator-= (Move m) { erase (std::remove (begin (), end (), m), end ()); }

    /*
    void insert_pv_into_tt (Position &pos) const;
    void extract_pv_from_tt (Position &pos);
    */
    bool extract_ponder_move_from_tt (Position &pos);

    explicit operator std::string () const;
};

template<class CharT, class Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, const RootMove &rm)
{
    os << std::string(rm);
    return os;
}

class RootMoveVector
    : public std::vector<RootMove>
{
public:
    RootMoveVector& operator= (const RootMoveVector&) = default;

    void operator+= (const RootMove &rm) { push_back (rm); }
    void operator-= (const RootMove &rm) { erase (std::remove (begin (), end (), rm), end ()); }

    void initialize (const Position &pos, const MoveVector &moves)
    {
        clear ();
        for (const auto &vm : MoveGen::MoveList<LEGAL> (pos))
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

template<class CharT, class Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, const RootMoveVector &root_moves)
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
    Move  current_move
        , exclude_move;
    Move  killer_moves[MaxKillers];

    Value static_eval;
    u08   move_count;
    bool  skip_pruning;
    TPieceValueStats *piece_cm_history;

    MoveVector pv;
};

// MovePicker class is used to pick one legal moves from the current position.
class MovePicker
{
private:
    const Position &_pos;
    const Stack *const _ss = nullptr;

    std::vector<ValMove> _moves;
    std::vector<ValMove> _capture_moves;

    u08     _index      = 0;
    u08     _stage      = 0;

    Move    _tt_move    = MOVE_NONE;
    Square  _recap_sq   = SQ_NO;
    Value   _threshold  = VALUE_ZERO;

    template<GenType GT>
    void value ();

    ValMove& pick_best_move (u08 i);

public:
    MovePicker () = delete;
    MovePicker (const MovePicker&) = delete;
    MovePicker& operator= (const MovePicker&) = delete;

    MovePicker (const Position&, Move, const Stack *const&);
    MovePicker (const Position&, Move, const Stack *const&, i16, Move);
    MovePicker (const Position&, Move, Value);

    Move next_move ();
};

namespace Searcher {

    extern Limit Limits;

    extern std::atomic_bool
                 ForceStop
        ,        PonderhitStop; 

    extern u16   MultiPV;
    //extern i32   MultiPV_cp;

    extern i16   FixedContempt
        ,        ContemptTime 
        ,        ContemptValue;

    extern std::string HashFile;
    
    extern bool  OwnBook;
    extern std::string BookFile;
    extern bool  BookMoveBest;
    extern i16   BookUptoMove;

    extern i16   TBDepthLimit;
    extern i32   TBPieceLimit;
    extern bool  TBUseRule50;
    extern bool  TBHasRoot;

    extern std::string OutputFile;

    template<bool RootNode = true>
    extern u64 perft (Position &pos, i16 depth);

    extern void initialize ();

    extern void clear ();
}

#endif // SEARCHER_H_INC_
