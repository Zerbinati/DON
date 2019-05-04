#ifndef _POSITION_H_INC_
#define _POSITION_H_INC_

#include <cstring>
#include <deque>
#include <list>
#include <memory> // For std::unique_ptr
#include <string>
#include "Type.h"
#include "BitBoard.h"
#include "PSQTable.h"
#include "Zobrist.h"

/// Pre-loads the given address in L1/L2 cache.
/// This is a non-blocking function that doesn't stall the CPU
/// waiting for data to be loaded from memory, which can be quite slow.
#if defined(PREFETCH)
#   if defined(_MSC_VER) || defined(__INTEL_COMPILER)

#   include <xmmintrin.h> // Intel and Microsoft header for _mm_prefetch()

inline void prefetch (const void* addr)
{
#   if defined(__INTEL_COMPILER)
    // This hack prevents prefetches from being optimized away by
    // Intel compiler. Both MSVC and gcc seem not be affected by this.
    __asm__ ("");
#   endif
    _mm_prefetch ((const char*)(addr), _MM_HINT_T0);
}

#   else

inline void prefetch (const void* addr)
{
    __builtin_prefetch (addr);
}

#   endif

#else

inline void prefetch (const void*)
{}

#endif

using namespace BitBoard;

/// StateInfo stores information needed to restore a Position object to its previous state when we retract a move.
///
///  - Castling-rights information.
///  - Enpassant square (SQ_NO if no Enpassant capture is possible).
///  - Counter (clock) for detecting 50 move rule draws.
///  - Hash key of the material situation.
///  - Hash key of the pawn structure.
///  - Hash key of the position.
///  - Move played on the last position.
///  - Piece type captured on last position.
///  - Bitboard of all checking pieces.
///  - Pointer to previous StateInfo. 
///  - Hash keys for all previous positions in the game for detecting repetition draws.
struct StateInfo
{
public:
    // ---Copied when making a move---
    Key         matl_key;       // Hash key of materials
    Key         pawn_key;       // Hash key of pawns

    Value       npm[CLR_NO];

    CastleRight castle_rights;  // Castling-rights information
    Square      enpassant_sq;   // Enpassant -> "In passing"
    i16         clock_ply;      // Number of half moves clock since the last pawn advance or any capture
                                // Used to determine if a draw can be claimed under the clock-move rule
    i16         null_ply;

    // ---Not copied when making a move---
    Key         posi_key;       // Hash key of position
    PieceType   capture;        // Piece type captured
    PieceType   promote;        // Piece type promoted
    Bitboard    checkers;       // Checkers
    // Check info
    Bitboard    king_blockers[CLR_NO];// Absolute and Discover Blockers
    Bitboard    king_checkers[CLR_NO];// Absolute and Discover Checkers
    Bitboard    checks[NONE];

    StateInfo  *ptr;            // Previous StateInfo pointer.

    Value non_pawn_material () const
    {
        return npm[WHITE]
             + npm[BLACK];
    }
    Value non_pawn_material (Color c) const
    {
        return npm[c];
    }

    bool can_castle (CastleRight cr) const
    {
        return CR_NONE != (castle_rights & cr);
    }
    CastleRight castle_right (Color c) const
    {
        return castle_rights & (WHITE == c ? CR_WHITE : CR_BLACK);
    }

    void clear ()
    {
        std::memset (this, 0, sizeof (*this));
        enpassant_sq = SQ_NO;
        null_ply = 0;
        capture = NONE;
        promote = NONE;
    }

    void set_check_info (const Position &pos);
};

/// A list to keep track of the position states along the setup moves
/// (from the start position to the position just before the search starts).
/// Needed by 'draw by repetition' detection.
/// Use a std::deque because pointers to elements are not invalidated upon list resizing.
typedef std::unique_ptr<std::deque<StateInfo>> StateListPtr;

class Thread;

/// Position class stores information regarding the board representation:
///  - 64-entry array of pieces, indexed by the square.
///  - Bitboards of each piece type.
///  - Bitboards of each color
///  - Bitboard of occupied square.
///  - List of square for the pieces.
///  - Information about the castling rights.
///  - Initial files of both pairs of rooks, castle path and kings path, this is used to implement the Chess960 castling rules.
///  - Color of side on move.
///  - Ply of the game.
///  - StateInfo pointer for the current status.
class Position
{
private:
    void place_piece (Square, Piece);
    void remove_piece (Square, Piece);
    void move_piece (Square, Square, Piece);

    void set_castle (Color, Square);

    bool can_enpassant (Color, Square, bool = true) const;

    template<PieceType>
    PieceType pick_next_attacker (Bitboard, Square, Square&, Bitboard&, Bitboard&) const;

public:
    Piece       piece[SQ_NO];
    Bitboard    color_bb[CLR_NO];
    Bitboard    type_bb[PT_NO];
    std::list<Square> squares[MAX_PIECE];

    CastleRight castle_right[SQ_NO];

    Square      castle_rook_sq[CLR_NO][CS_NO];
    Bitboard    castle_king_path_bb[CLR_NO][CS_NO];
    Bitboard    castle_rook_path_bb[CLR_NO][CS_NO];

    Score       psq;
    i16         ply;
    Color       active;
    Thread      *thread;

    StateInfo   *si; // Current state information pointer

    static void initialize ();

    Position () = default;
    Position (const Position&) = delete;
    Position& operator= (const Position&) = delete;

    Piece operator[] (Square) const;
    bool empty (Square)  const;

    Bitboard pieces () const;
    Bitboard pieces (Piece) const;
    Bitboard pieces (Color) const;
    Bitboard pieces (PieceType) const;
    template<typename ...PieceTypes>
    Bitboard pieces (PieceType, PieceTypes...) const;
    template<typename ...PieceTypes>
    Bitboard pieces (Color, PieceTypes...) const;

    i32 count () const;
    i32 count (Piece) const;
    i32 count (Color) const;
    i32 count (PieceType) const;
    i32 count (Color, PieceType) const;

    i32 same_color_pawn_count (Color, Color) const;

    //i32 diff (PieceType) const;

    Square square (Piece, u08 = 0) const;

    Key pg_key () const;
    Key posi_move_key (Move) const;

    bool expeded_castle (Color, CastleSide) const;

    i16  move_num () const;
    bool draw (i16) const;
    bool cycled (i16) const;
    bool repeated () const;

    Bitboard attackers_to (Square, Bitboard) const;
    Bitboard attackers_to (Square) const;
    //Bitboard xattackers_to (Square, Bitboard) const;
    //Bitboard xattackers_to (Square) const;
    Bitboard attacks_from (PieceType, Square, Bitboard) const;
    Bitboard attacks_from (PieceType, Square) const;
    Bitboard attacks_from (Square, Bitboard) const;
    Bitboard attacks_from (Square) const;
    template<PieceType>
    Bitboard xattacks_from (Square, Color) const;

    Bitboard slider_blockers (Square, Color, Bitboard, Bitboard&, Bitboard&) const;

    bool pseudo_legal (Move) const;
    bool legal (Move) const;
    bool capture (Move) const;
    bool capture_or_promotion (Move) const;
    bool pawn_advance (Move) const;
    bool gives_check (Move) const;

    PieceType cap_type (Move) const;

    Value exchange (Move) const;
    bool see_ge (Move, Value = VALUE_ZERO) const;

    bool pawn_passed_at (Color, Square) const;
    bool paired_bishop  (Color) const;
    bool opposite_bishops () const;
    bool file_semiopen (Color, File) const;

    void clear ();

    Position& setup (const std::string&, StateInfo&, Thread *const = nullptr);
    Position& setup (const std::string&, Color, StateInfo&);

    void do_move (Move, StateInfo&, bool);
    void do_move (Move, StateInfo&);
    void undo_move (Move);
    void do_null_move (StateInfo&);
    void undo_null_move ();

    void flip ();
    void mirror ();

    std::string fen (bool full = true) const;

    explicit operator std::string () const;

#if !defined(NDEBUG)
    bool ok () const;
#endif

};

inline Piece Position::operator[] (Square s) const
{
    assert(_ok (s));
    return piece[s];
}
inline bool Position::empty (Square s)  const
{
    assert(_ok (s));
    return NO_PIECE == piece[s];
}

inline Bitboard Position::pieces () const
{
    return type_bb[NONE];
}
inline Bitboard Position::pieces (Piece pc) const
{
    return color_bb[color (pc)] & type_bb[ptype (pc)];
}
inline Bitboard Position::pieces (Color c) const
{
    return color_bb[c];
}
inline Bitboard Position::pieces (PieceType pt) const
{
    assert(_ok (pt));
    return type_bb[pt];
}
template<typename ...PieceTypes>
inline Bitboard Position::pieces (PieceType pt, PieceTypes... pts) const
{
    assert(_ok (pt));
    return type_bb[pt] | pieces (pts...);
}
template<typename ...PieceTypes>
inline Bitboard Position::pieces (Color c, PieceTypes... pts) const
{
    return color_bb[c] & pieces (pts...);
}
/// Position::count() counts all
inline i32 Position::count () const
{
    return i32(squares[W_PAWN].size () + squares[B_PAWN].size ()
             + squares[W_NIHT].size () + squares[B_NIHT].size ()
             + squares[W_BSHP].size () + squares[B_BSHP].size ()
             + squares[W_ROOK].size () + squares[B_ROOK].size ()
             + squares[W_QUEN].size () + squares[B_QUEN].size ()
             + squares[W_KING].size () + squares[B_KING].size ());
}
inline i32 Position::count (Piece pc) const
{
    return i32(squares[pc].size ());
}
/// Position::count() counts specific color
inline i32 Position::count (Color c) const
{
    return i32(squares[c|PAWN].size ()
             + squares[c|NIHT].size ()
             + squares[c|BSHP].size ()
             + squares[c|ROOK].size ()
             + squares[c|QUEN].size ()
             + squares[c|KING].size ());
}
/// Position::count() counts specific type
inline i32 Position::count (PieceType pt) const
{
    assert(_ok (pt));
    return i32(squares[WHITE|pt].size () + squares[BLACK|pt].size ());
}
/// Position::count() counts specific color and type
inline i32 Position::count (Color c, PieceType pt) const
{
    assert(_ok (pt));
    return i32(squares[c|pt].size ());
}

inline i32 Position::same_color_pawn_count (Color c, Color s) const
{
    return pop_count (pieces (c, PAWN) & Color_bb[s]);
}

//inline i32 Position::diff (PieceType pt) const
//{
//    return count (WHITE, pt)
//         - count (BLACK, pt);
//}

inline Square Position::square (Piece pc, u08 index) const
{
    assert (_ok (pc));
    assert(squares[pc].size () > index);
    return *std::next (squares[pc].begin (), index);
}

inline Key Position::pg_key () const
{
    return PolyZob.compute_posi_key (*this);
}
/// Position::move_posi_key() computes the new hash key after the given moven.
/// Needed for speculative prefetch.
inline Key Position::posi_move_key (Move m) const
{
    auto org = org_sq (m);
    auto dst = dst_sq (m);
    auto key = si->posi_key;
    if (CASTLE == mtype (m))
    {
        key ^= RandZob.piece_square[active][ROOK][dst]
             ^ RandZob.piece_square[active][ROOK][rel_sq (active, dst > org ? SQ_F1 : SQ_D1)];
    }
    else
    {
        auto cpt = ENPASSANT != mtype (m) ? ptype (piece[dst]) : PAWN;
        if (NONE != cpt)
        {
            key ^= RandZob.piece_square[~active][cpt][ENPASSANT != mtype (m) ? dst : dst - pawn_push (active)];
        }
        else
        if (   PAWN == ptype (piece[org])
            && dst == org + pawn_push (active) * 2)
        {
            auto ep_sq = org + (dst - org) / 2;
            if (can_enpassant (~active, ep_sq, false))
            {
                key ^= RandZob.enpassant[_file (ep_sq)];
            }
        }
    }
    if (SQ_NO != si->enpassant_sq)
    {
        key ^= RandZob.enpassant[_file (si->enpassant_sq)];
    }
    return key
         ^ RandZob.color
         ^ RandZob.piece_square[active][ptype (piece[org])][org]
         ^ RandZob.piece_square[active][PROMOTE != mtype (m) ? ptype (piece[org]) : promote (m)][CASTLE != mtype (m) ? dst : rel_sq (active, dst > org ? SQ_G1 : SQ_C1)]
         ^ RandZob.castle_right[si->castle_rights & (castle_right[org] | castle_right[dst])];
}

inline bool Position::expeded_castle (Color c, CastleSide cs) const
{
    return 0 == (castle_rook_path_bb[c][cs] & pieces ());
}
/// Position::move_num() starts at 1, and is incremented after BLACK's move.
inline i16 Position::move_num () const
{
    return i16(std::max ((ply - (BLACK == active ? 1 : 0)) / 2, 0) + 1);
}

/// Position::attackers_to() finds attackers to the square on occupancy.
inline Bitboard Position::attackers_to (Square s, Bitboard occ) const
{
    return (pieces (BLACK, PAWN) & PawnAttacks[WHITE][s])
         | (pieces (WHITE, PAWN) & PawnAttacks[BLACK][s])
         | (pieces (NIHT)        & PieceAttacks[NIHT][s])
         | (pieces (BSHP, QUEN)  & attacks_bb<BSHP> (s, occ))
         | (pieces (ROOK, QUEN)  & attacks_bb<ROOK> (s, occ))
         | (pieces (KING)        & PieceAttacks[KING][s]);
}
/// Position::attackers_to() finds attackers to the square.
inline Bitboard Position::attackers_to (Square s) const
{
    return attackers_to (s, pieces ());
}

///// Position::xattackers_to() finds attackers to the square on occupancy.
//inline Bitboard Position::xattackers_to (Square s, Bitboard occ) const
//{
//    return (pieces (BLACK, PAWN) & PawnAttacks[WHITE][s])
//         | (pieces (WHITE, PAWN) & PawnAttacks[BLACK][s])
//         | (pieces (NIHT)        & PieceAttacks[NIHT][s])
//         | (pieces (WHITE, BSHP) & attacks_bb<BSHP> (s, occ & ~pieces (WHITE, QUEN, BSHP)))
//         | (pieces (BLACK, BSHP) & attacks_bb<BSHP> (s, occ & ~pieces (BLACK, QUEN, BSHP)))
//         | (pieces (WHITE, ROOK) & attacks_bb<ROOK> (s, occ & ~pieces (WHITE, QUEN, ROOK)))
//         | (pieces (BLACK, ROOK) & attacks_bb<ROOK> (s, occ & ~pieces (BLACK, QUEN, ROOK)))
//         | (pieces (WHITE, QUEN) & attacks_bb<QUEN> (s, occ & ~pieces (WHITE, QUEN)))
//         | (pieces (BLACK, QUEN) & attacks_bb<QUEN> (s, occ & ~pieces (BLACK, QUEN)))
//         | (pieces (KING)        & PieceAttacks[KING][s]);
//}
///// Position::attackers_to() finds attackers to the square.
//inline Bitboard Position::xattackers_to (Square s) const
//{
//    return xattackers_to (s, pieces ());
//}

/// Position::attacks_from() finds attacks of the piecetype from the square on occupancy.
inline Bitboard Position::attacks_from (PieceType pt, Square s, Bitboard occ) const
{
    assert(PAWN != pt);
    switch (pt)
    {
    case NIHT: return PieceAttacks[NIHT][s];
    case BSHP: return attacks_bb<BSHP> (s, occ);
    case ROOK: return attacks_bb<ROOK> (s, occ);
    case QUEN: return attacks_bb<QUEN> (s, occ);
    case KING: return PieceAttacks[KING][s];
    default: assert(false); return 0;
    }
}
/// Position::attacks_from() finds attacks of the piecetype from the square.
inline Bitboard Position::attacks_from (PieceType pt, Square s) const
{
    return attacks_from (pt, s, pieces ());
}
/// Position::attacks_from() finds attacks from the square on occupancy.
inline Bitboard Position::attacks_from (Square s, Bitboard occ) const
{
    switch (ptype (piece[s]))
    {
    case PAWN: return PawnAttacks[color (piece[s])][s];
    case NIHT: return PieceAttacks[NIHT][s];
    case BSHP: return attacks_bb<BSHP> (s, occ);
    case ROOK: return attacks_bb<ROOK> (s, occ);
    case QUEN: return attacks_bb<QUEN> (s, occ);
    case KING: return PieceAttacks[KING][s];
    default: assert(false); return 0;
    }
}
/// Position::attacks_from() finds attacks from the square.
inline Bitboard Position::attacks_from (Square s) const
{
    return attacks_from (s, pieces ());
}

/// Position::xattacks_from() finds xattacks of the piecetype of the color from the square.

template<>
inline Bitboard Position::xattacks_from<NIHT> (Square s, Color) const
{
    return PieceAttacks[NIHT][s];
}
template<>
inline Bitboard Position::xattacks_from<BSHP> (Square s, Color c) const
{
    return attacks_bb<BSHP> (s, pieces () ^ ((pieces (c, QUEN, BSHP) & ~si->king_blockers[c]) | pieces (~c, QUEN)));
}
template<>
inline Bitboard Position::xattacks_from<ROOK> (Square s, Color c) const
{
    return attacks_bb<ROOK> (s, pieces () ^ ((pieces (c, QUEN, ROOK) & ~si->king_blockers[c]) | pieces (~c, QUEN)));
}
template<>
inline Bitboard Position::xattacks_from<QUEN> (Square s, Color c) const
{
    return attacks_bb<QUEN> (s, pieces () ^ ((pieces (c, QUEN)       & ~si->king_blockers[c])));
}

inline bool Position::capture (Move m) const
{
    return ENPASSANT == mtype (m)
        || (   CASTLE != mtype (m)
            && !empty (dst_sq (m)));
}

inline bool Position::capture_or_promotion (Move m) const
{
    return NORMAL != mtype (m) ?
            CASTLE != mtype (m) :
            !empty (dst_sq (m));
}

inline bool Position::pawn_advance (Move m) const
{
    return contains (pieces (PAWN) & Region_bb[~active], org_sq (m));
}

inline PieceType Position::cap_type (Move m) const
{
    return ENPASSANT != mtype (m) ?
            ptype (piece[dst_sq (m)]) :
            PAWN;
}

inline Value Position::exchange (Move m) const
{
    return NORMAL == mtype (m) ?
            PieceValues[MG][ptype (piece[dst_sq (m)])]
          - PieceValues[MG][ptype (piece[org_sq (m)])] :
            VALUE_ZERO;
}

/// Position::pawn_passed_at() check if pawn passed at the given square.
inline bool Position::pawn_passed_at (Color c, Square s) const
{
    return 0 == (pawn_pass_span (c, s) & pieces (~c, PAWN));
}
/// Position::paired_bishop() check the side has pair of opposite color bishops.
inline bool Position::paired_bishop (Color c) const
{
    return 0 != (pieces (c, BSHP) & Color_bb[WHITE])
        && 0 != (pieces (c, BSHP) & Color_bb[BLACK]);
}
inline bool Position::opposite_bishops () const
{
    return 1 == count (WHITE, BSHP)
        && 1 == count (BLACK, BSHP)
        && opposite_colors (square (WHITE|BSHP), square (BLACK|BSHP));
}
inline bool Position::file_semiopen (Color c, File f) const
{
    return 0 == (pieces (c, PAWN) & file_bb (f));
}

inline void Position::do_move (Move m, StateInfo &nsi)
{
    do_move (m, nsi, gives_check (m));
}

inline void Position::place_piece (Square s, Piece pc)
{
    assert(_ok (pc)
        && std::count (squares[pc].begin (), squares[pc].end (), s) == 0);
    color_bb[color (pc)] |= s;
    type_bb[ptype (pc)] |= s;
    type_bb[NONE] |= s;
    squares[pc].push_front (s);
    psq += PSQ[pc][s];
    piece[s] = pc;
}
inline void Position::remove_piece (Square s, Piece pc)
{
    assert(_ok (pc)
        && std::count (squares[pc].begin (), squares[pc].end (), s) == 1);
    color_bb[color (pc)] ^= s;
    type_bb[ptype (pc)] ^= s;
    type_bb[NONE] ^= s;
    squares[pc].remove (s);
    psq -= PSQ[pc][s];
    //piece[s] = NO_PIECE; // Not needed, overwritten by the capturing one
}
inline void Position::move_piece (Square s1, Square s2, Piece pc)
{
    assert(_ok (pc)
        && std::count (squares[pc].begin (), squares[pc].end (), s1) == 1
        && std::count (squares[pc].begin (), squares[pc].end (), s2) == 0);
    Bitboard bb = square_bb (s1) | square_bb (s2);
    color_bb[color (pc)] ^= bb;
    type_bb[ptype (pc)] ^= bb;
    type_bb[NONE] ^= bb;
    std::replace (squares[pc].begin (), squares[pc].end (), s1, s2);
    psq += PSQ[pc][s2] - PSQ[pc][s1];
    piece[s2] = pc;
    piece[s1] = NO_PIECE;
}

template<typename CharT, typename Traits>
inline std::basic_ostream<CharT, Traits>&
operator<< (std::basic_ostream<CharT, Traits> &os, const Position &pos)
{
    os << std::string(pos);
    return os;
}

/// StateInfo::set_check_info() sets check info used for fast check detection.
inline void StateInfo::set_check_info (const Position &pos)
{
    king_checkers[WHITE] = 0;
    king_checkers[BLACK] = 0;
    king_blockers[WHITE] = pos.slider_blockers (pos.square (WHITE|KING), BLACK, 0, king_checkers[WHITE], king_checkers[BLACK]);
    king_blockers[BLACK] = pos.slider_blockers (pos.square (BLACK|KING), WHITE, 0, king_checkers[BLACK], king_checkers[WHITE]);
    assert(/*0 == (king_blockers[WHITE] & pos.pieces (BLACK, QUEN)) &&*/ (attacks_bb<QUEN> (pos.square (WHITE|KING), pos.pieces ()) & king_blockers[WHITE]) == king_blockers[WHITE]);
    assert(/*0 == (king_blockers[BLACK] & pos.pieces (WHITE, QUEN)) &&*/ (attacks_bb<QUEN> (pos.square (BLACK|KING), pos.pieces ()) & king_blockers[BLACK]) == king_blockers[BLACK]);

    checks[PAWN] = PawnAttacks[~pos.active][pos.square (~pos.active|KING)];
    checks[NIHT] = PieceAttacks[NIHT][pos.square (~pos.active|KING)];
    checks[BSHP] = attacks_bb<BSHP> (pos.square (~pos.active|KING), pos.pieces ());
    checks[ROOK] = attacks_bb<ROOK> (pos.square (~pos.active|KING), pos.pieces ());
    checks[QUEN] = checks[BSHP] | checks[ROOK];
    checks[KING] = 0;
}

#if !defined(NDEBUG)
/// _ok() Check the validity of FEN string.
inline bool _ok (const std::string &fen)
{
    Position pos;
    StateInfo si;
    return !white_spaces (fen)
        && pos.setup (fen, si, nullptr).ok ();
}
#endif

#endif // _POSITION_H_INC_
