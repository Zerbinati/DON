#ifndef SEARCHER_H_INC_
#define SEARCHER_H_INC_

#include <cstring>
#include <atomic>

#include "Type.h"
#include "Position.h"
#include "MoveGenerator.h"

// Limits stores information sent by GUI about available time to search the current move.
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
        TimePoint time  = 0; // Remaining Time          [milli-seconds]
        TimePoint inc   = 0; // Increment Time per move [milli-seconds]
    }         clock[CLR_NO]; // Clock for both sides
    TimePoint movetime  = 0; // Search <x> exact time in milli-seconds
    u08       movestogo = 0; // Search <x> moves to the next time control
    u08       depth     = 0; // Search <x> depth (plies) only
    u64       nodes     = 0; // Search <x> nodes only
    u08       mate      = 0; // Search mate in <x> moves
    bool      infinite  = false; // Search until the "stop" command
    bool      ponder    = false; // Search on ponder move until the "stop" command
    MoveVector search_moves; // Restrict search to these root moves only

    TimePoint start_time = 0;

    bool use_time_management () const
    {
        return !infinite
            && movetime == 0
            && depth    == 0
            && nodes    == 0
            && mate     == 0;
    }
};

const Value MaxStatsValue = Value(1 << 28);

// The Stats struct stores different statistics.
template<class T, bool CM = false>
struct Stats
{
private:
    T _table[MAX_PIECE][SQ_NO];

    void _clear (Value &v) { v = VALUE_ZERO; }
    void _clear (Stats<Value, false> &vs) { vs.clear (); }
    void _clear (Stats<Value, true > &vs) { vs.clear (); }
    void _clear (Move  &m) { m = MOVE_NONE; }

public:
    T& operator()(Piece pc, Square s)
    {
        return _table[pc][s];
    }
    const T& operator()(Piece pc, Square s) const
    {
        return _table[pc][s];
    }

    void clear ()
    {
        for (i16 pc = 0; pc < MAX_PIECE; ++pc)
        {
            for (auto s = SQ_A1; s <= SQ_H8; ++s)
            {
                _clear (_table[pc][s]);
            }
        }
    }
    void update (Piece pc, Square s, Value v)
    {
        if (abs (v) < 324)
        {
            auto &e = _table[pc][s];
            e = e*(1.0 - (double) abs (v) / (CM ? 936 : 324)) + 32*v;
        }
    }
    void update (Piece pc, Square s, Move m)
    {
        _table[pc][s] = m;
    }
};

// ValueStats stores the value that records how often different moves have been successful/unsuccessful
// during the current search and is used for reduction and move ordering decisions.
typedef Stats<Value, false>     HValueStats;
typedef Stats<Value, true >     CMValueStats;

// CM2DValueStats
typedef Stats<CMValueStats>     CM2DValueStats;

// MoveStats store the move that refute a previous move.
// Entries are stored according only to moving piece and destination square,
// in particular two moves with different origin but same piece and same destination
// will be considered identical.
typedef Stats<Move>             MoveStats;


struct OrgDstStats
{
private:
    Value _table[CLR_NO][SQ_NO][SQ_NO];

public:
    Value& operator()(Color c, Move m)
    {
        return _table[c][org_sq (m)][dst_sq (m)];
    }
    const Value& operator()(Color c, Move m) const
    {
        return _table[c][org_sq (m)][dst_sq (m)];
    }

    void clear ()
    {
        for (auto c = WHITE; c <= BLACK; ++c)
        {
            for (auto s1 = SQ_A1; s1 <= SQ_H8; ++s1)
            {
                for (auto s2 = SQ_A1; s2 <= SQ_H8; ++s2)
                {
                    _table[c][s1][s2] = VALUE_ZERO;
                }
            }
        }
    }
    void update (Color c, Move m, Value v)
    {
        if (abs (v) < 324)
        {
            auto &e = _table[c][org_sq (m)][dst_sq (m)];
            e = e*(1.0 - (double) abs (v) / 324) + 32*v;
        }
    }
};

const u08 MaxKillers = 2;
// The Stack struct keeps track of the information needed to remember from
// nodes shallower and deeper in the tree during the search. Each search thread
// has its own array of Stack objects, indexed by the current ply.
struct Stack
{
    i16   ply;
    Move  current_move
        , exclude_move;
    Move  killer_moves[MaxKillers];

    Value static_eval;
    u08   move_count;
    bool  skip_pruning;
    CMValueStats *counter_move_values;

    MoveVector pv;
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

    extern Depth TBDepthLimit;
    extern i32   TBPieceLimit;
    extern bool  TBUseRule50;
    extern u16   TBHits;
    extern bool  TBHasRoot;

    extern std::string OutputFile;

    // The root of the tree is a PV node.
    // At a PV node all the children have to be investigated.
    // The best move found at a PV node leads to a successor PV node,
    // while all the other investigated children are CUT nodes
    // At a CUT node the child causing a beta cut-off is an ALL node.
    // In a perfectly ordered tree only one child of a CUT node has to be explored.
    // At an ALL node all the children have to be explored. The successors of an ALL node are CUT nodes.
    // NonPV nodes = CUT nodes + ALL nodes

    // RootMove is used for moves at the root of the tree.
    // For each root move stores:
    //  - New/Old values.
    //  - PV (really a refutation table in the case of moves which fail low).
    // Value is normally set at -VALUE_INFINITE for all non-pv moves.
    class RootMove
        : public MoveVector
    {
    public:
        Value new_value = -VALUE_INFINITE
            , old_value = -VALUE_INFINITE;

        explicit RootMove (Move m = MOVE_NONE)
            : MoveVector (1, m)
        {}
        RootMove& operator= (const RootMove&) = default;

        // Descending sort
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

        explicit operator std::string () const
        {
            std::ostringstream oss;
            for (const auto &rm : *this)
            {
                oss << rm << '\n';
            }
            return oss.str ();
        }
    };

    template<class CharT, class Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, const RootMoveVector &root_moves)
    {
        os << std::string(root_moves);
        return os;
    }


    template<bool RootNode = true>
    extern u64 perft (Position &pos, Depth depth);

    extern void initialize ();

    extern void clear ();
}

#endif // SEARCHER_H_INC_
