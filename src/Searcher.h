#ifndef SEARCHER_H_INC_
#define SEARCHER_H_INC_

#include <cstring>
#include <stack>
#include <memory>
#include <atomic>

#include "Type.h"
#include "Position.h"
#include "MoveGenerator.h"

typedef std::stack<StateInfo>       StateStack;
typedef std::unique_ptr<StateStack> StateStackPtr;

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
    };

    Clock clock[CLR_NO];     // Clock for both sides
    TimePoint movetime  = 0; // Search <x> exact time in milli-seconds
    u08       movestogo = 0; // Search <x> moves to the next time control
    u08       depth     = 0; // Search <x> depth (plies) only
    u64       nodes     = 0; // Search <x> nodes only
    u08       mate      = 0; // Search mate in <x> moves
    bool      infinite  = false; // Search until the "stop" command
    bool      ponder    = false; // Search on ponder move until the "stop" command

    MoveVector search_moves; // Restrict search to these root moves only

    TimePoint start_time = 0;

    bool time_management_used () const
    {
        return !infinite
            && movetime == 0
            && depth    == 0
            && nodes    == U64(0)
            && mate     == 0;
    }
};

const u08 Killers = 2;

namespace Searcher {

    // The Stack struct keeps track of the information needed to remember from
    // nodes shallower and deeper in the tree during the search. Each search thread
    // has its own array of Stack objects, indexed by the current ply.
    struct Stack
    {
        //static const size_t Size;

        i16 ply = 0;
        Move current_move = MOVE_NONE
           , exclude_move = MOVE_NONE
           , killer_moves[Killers];

        Value static_eval = VALUE_NONE;
        u08  move_count   = 0;
        bool skip_pruning = false;

        MoveVector pv;

        Stack () = default;
        Stack (const Stack&) = delete;
        Stack& operator= (const Stack&) = delete;
    };

    extern bool Chess960;

    extern StateStackPtr SetupStates;
    extern Limit Limits;

    extern std::atomic_bool ForceStop
        ,                   PonderhitStop; 

    extern u16 MultiPV;
    //extern i32 MultiPV_cp;

    extern i16 FixedContempt
        ,      ContemptTime 
        ,      ContemptValue;

    extern std::string HashFile;
    extern u16 AutoSaveHashTime;
    
    extern bool OwnBook;
    extern std::string BookFile;
    extern bool BookMoveBest;
    extern i16  BookUptoMove;

    extern Depth TBDepthLimit;
    extern i32   TBPieceLimit;
    extern bool  TBUseRule50;
    extern u16   TBHits;
    extern bool  TBHasRoot;

    extern std::string LogFile;

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
    //  - Value[] { new , old }.
    //  - Node count.
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
        bool operator<  (const RootMove &root_move) const { return new_value >  root_move.new_value; }
        bool operator>  (const RootMove &root_move) const { return new_value <  root_move.new_value; }
        bool operator<= (const RootMove &root_move) const { return new_value >= root_move.new_value; }
        bool operator>= (const RootMove &root_move) const { return new_value <= root_move.new_value; }
        bool operator== (const RootMove &root_move) const { return new_value == root_move.new_value; }
        bool operator!= (const RootMove &root_move) const { return new_value != root_move.new_value; }

        bool operator== (Move m) const { return at (0) == m; }
        bool operator!= (Move m) const { return at (0) != m; }

        void operator+= (Move m) { push_back (m); }
        void operator-= (Move m) { erase (std::remove (begin (), end (), m), end ()); }

        void insert_pv_into_tt (Position &pos);
        bool extract_ponder_move_from_tt (Position &pos);

        explicit operator std::string () const;
    };

    template<class CharT, class Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, const RootMove &root_move)
    {
        os << std::string(root_move);
        return os;
    }

    class RootMoveVector
        : public std::vector<RootMove>
    {

    public:
        RootMoveVector& operator= (const RootMoveVector&) = default;

        void operator+= (const RootMove &root_move) { push_back (root_move); }
        void operator-= (const RootMove &root_move) { erase (std::remove (begin (), end (), root_move), end ()); }

        void initialize (const Position &pos, const MoveVector &search_moves)
        {
            clear ();
            for (const auto &vm : MoveGen::MoveList<LEGAL> (pos))
            {
                if (   search_moves.empty ()
                    || std::find (search_moves.begin (), search_moves.end (), vm.move) != search_moves.end ()
                   )
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
                oss << rm << "\n";
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
