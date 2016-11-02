#ifndef _POSITION_H_INC_
#define _POSITION_H_INC_

#include <deque>

#include "BitBoard.h"
#include "Zobrist.h"

class Position;
using namespace BitBoard;

// StateInfo stores information needed to restore a Position object to its previous state
// when we retract a move. Whenever a move is made on the board (by calling do_move),
// a StateInfo object must be passed as a parameter.
//
//  - Castling-rights information.
//  - En-passant square (SQ_NO if no en passant capture is possible).
//  - Counter (clock) for detecting 50 move rule draws.
//  - Hash key of the material situation.
//  - Hash key of the pawn structure.
//  - Hash key of the position.
//  - Move played on the last position.
//  - Piece type captured on last position.
//  - Bitboard of all checking pieces.
//  - Pointer to previous StateInfo. 
//  - Hash keys for all previous positions in the game for detecting repetition draws.
struct StateInfo
{
public:
    // ---Copied when making a move---
    Key         matl_key;       // Hash key of materials.
    Key         pawn_key;       // Hash key of pawns.

    Value       non_pawn_matl[CLR_NO];
    Score       psq_score;

    CastleRight castle_rights;  // Castling-rights information.
    Square      en_passant_sq;  // En-passant -> "In passing"
    u08         clock_ply;      // Number of halfmoves clock since the last pawn advance or any capture.
                                // Used to determine if a draw can be claimed under the clock-move rule.
    u08         null_ply;   

    // ---Not copied when making a move---
    Key         posi_key;       // Hash key of position.
    PieceType   capture_type;   // Piece type captured.
    Bitboard    checkers;       // Checkers.

    // Check info
    Bitboard king_blockers[CLR_NO];// Absolute and Discover Blockers
    Bitboard king_checkers[CLR_NO];// Absolute and Discover Checkers
    Bitboard checks[NONE];

    StateInfo   *ptr;           // Previous StateInfo.

    void clear ()
    {
        std::memset (this, 0x00, sizeof (*this));
    }

    void set_check_info (const Position &pos);
};

typedef std::deque<StateInfo> StateList;

namespace Threading {
    class Thread;
}
using namespace Threading;

// Position class stores information regarding the board representation:
//  - 64-entry array of pieces, indexed by the square.
//  - Bitboards of each piece type.
//  - Bitboards of each color
//  - Bitboard of all occupied squares.
//  - List of squares for the pieces.
//  - Count of the pieces.
//  - ----------x-----------
//  - Color of side on move.
//  - Ply of the game.
//  - Nodes visited during search.
//  - StateInfo object for the base status.
//  - StateInfo pointer for the current status.
//  - Information about the castling rights.
//  - Initial files of both pairs of rooks, castle path and kings path, this is used to implement the Chess960 castling rules.
class Position
{
private:
    void place_piece (Square s, Color c, PieceType pt);
    void place_piece (Square s, Piece p);
    void remove_piece (Square s);
    void move_piece (Square s1, Square s2);

    void set_castle (Color c, Square rook_org);

    bool can_en_passant (Color c, Square ep_sq, bool move_done = true) const;

    template<bool Do> void do_castling (Square king_org, Square &king_dst, Square &rook_org, Square &rook_dst);

    template<PieceType PT> PieceType pick_least_val_att (Square dst, Bitboard c_attackers, Bitboard &mocc, Bitboard &attackers) const;

public:
    static u08  DrawClockPly;
    static bool Chess960;
    
    Piece       board[SQ_NO];
    Bitboard    color_bb[CLR_NO];
    Bitboard    types_bb[MAX_PTYPE];
    SquareVector squares[CLR_NO][NONE];

    Color       active;
    i16         ply;
    u64         nodes;

    CastleRight castle_mask[SQ_NO];
    Square      castle_rook[CR_NO];
    Bitboard    castle_path[CR_NO];
    Bitboard    king_path[CR_NO];

    Thread      *thread;
    StateInfo   *si; // Current state information pointer

    Position () = default;
    Position (const Position&) = delete;
    Position& operator= (const Position &pos) = delete;

    Piece operator[] (Square s) const;
    bool empty  (Square s)  const;

    Bitboard pieces () const;
    Bitboard pieces (Color c) const;
    Bitboard pieces (PieceType pt) const;
    Bitboard pieces (Color c, PieceType pt) const;
    Bitboard pieces (PieceType p1, PieceType p2) const;
    Bitboard pieces (Color c, PieceType p1, PieceType p2) const;

    template<PieceType PT> i32 count () const;
    template<PieceType PT> i32 count (Color c) const;
    i32 count (Color c, PieceType pt) const;

    Square square (Color c, PieceType pt, i08 index = 0) const;

    Key poly_key () const;
    Key move_posi_key (Move m) const;

    CastleRight can_castle (Color c) const;
    CastleRight can_castle (CastleRight cr) const;

    bool impeded_castle (CastleRight cr) const;

    i16  move_num () const;
    bool draw     () const;
    bool repeated () const;
    Phase phase   () const;

    bool see_ge (Move m, Value v) const;

    Bitboard attackers_to (Square s, Color c, Bitboard occ) const;
    Bitboard attackers_to (Square s, Color c) const;
    Bitboard attackers_to (Square s, Bitboard occ) const;
    Bitboard attackers_to (Square s) const;

    template<Color Own>
    Bitboard slider_blockers (Square s, Bitboard ex_attackers, Bitboard &pinners, Bitboard &discovers) const;
    Bitboard abs_blockers (Color c) const;
    Bitboard dsc_blockers (Color c) const;
    Bitboard abs_checkers (Color c) const;
    Bitboard dsc_checkers (Color c) const;

    bool pseudo_legal   (Move m) const;
    bool legal          (Move m) const;
    bool en_passant     (Move m) const;
    bool capture        (Move m) const;
    bool promotion      (Move m) const;
    bool capture_or_promotion (Move m) const;
    bool gives_check    (Move m) const;

    bool pawn_passed_at (Color c, Square s) const;
    bool paired_bishop  (Color c) const;
    bool opposite_bishops ()    const;

    void clear ();

    Position& setup (const std::string &ff, StateInfo &si, Thread *const th = nullptr, bool full = true);
    Position& setup (const std::string &code, StateInfo &si, Color c);

    void do_move (Move m, StateInfo &si, bool gives_check);
    void undo_move (Move m);
    void do_null_move (StateInfo &si);
    void undo_null_move ();

    void flip ();
    void mirror ();

    std::string fen (bool full = true) const;

    explicit operator std::string () const;

#if !defined(NDEBUG)
    bool ok (u08 *step = nullptr) const;
#endif

};

inline Piece Position::operator[] (Square s) const
{
    assert(_ok (s));
    return board[s];
}
inline bool Position::empty  (Square s)  const
{
    assert(_ok (s));
    return board[s] == NO_PIECE;
}

inline Bitboard Position::pieces () const { return types_bb[NONE]; }
inline Bitboard Position::pieces (Color c) const { return color_bb[c]; }
inline Bitboard Position::pieces (PieceType pt) const { return types_bb[pt]; }
inline Bitboard Position::pieces (Color c, PieceType pt) const { return color_bb[c]&types_bb[pt]; }
inline Bitboard Position::pieces (PieceType pt1, PieceType pt2) const { return types_bb[pt1]|types_bb[pt2]; }
inline Bitboard Position::pieces (Color c, PieceType pt1, PieceType pt2) const { return color_bb[c]&(types_bb[pt1]|types_bb[pt2]); }

// Count specific piece
template<PieceType PT> inline i32 Position::count () const { return i32(squares[WHITE][PT].size () + squares[BLACK][PT].size ()); }
// Count total pieces
template<> inline i32 Position::count<NONE> () const
{
    return i32(squares[WHITE][PAWN].size () + squares[BLACK][PAWN].size ()
             + squares[WHITE][NIHT].size () + squares[BLACK][NIHT].size ()
             + squares[WHITE][BSHP].size () + squares[BLACK][BSHP].size ()
             + squares[WHITE][ROOK].size () + squares[BLACK][ROOK].size ()
             + squares[WHITE][QUEN].size () + squares[BLACK][QUEN].size ()
             + squares[WHITE][KING].size () + squares[BLACK][KING].size ());
}
// Count specific piece of color
template<PieceType PT> inline i32 Position::count (Color c) const { return i32(squares[c][PT].size ()); }
// Count total pieces of color
template<> inline i32 Position::count<NONE> (Color c) const
{
    return i32(squares[c][PAWN].size ()
             + squares[c][NIHT].size ()
             + squares[c][BSHP].size ()
             + squares[c][ROOK].size ()
             + squares[c][QUEN].size ()
             + squares[c][KING].size ());
}

inline i32 Position::count (Color c, PieceType pt) const { return i32(squares[c][pt].size ()); }

inline Square Position::square (Color c, PieceType pt, i08 index) const
{
    assert(i08(squares[c][pt].size ()) > index);
    return squares[c][pt][index];
}

inline Key Position::poly_key () const { return PolyZob.compute_posi_key (*this); }
// Computes the new hash key after the given moven. Needed for speculative prefetch.
// It doesn't recognize special moves like castling, en-passant and promotions.
inline Key Position::move_posi_key (Move m) const
{
    auto org = org_sq (m);
    auto dst = dst_sq (m);
    auto mpt = ptype (board[org]);
    assert(!empty (org)
        && color (board[org]) == active
        && mpt != NONE);

    auto ppt = promotion (m) ? promote (m) : mpt;
    Key key = si->posi_key;
    if (mtype (m) == CASTLE)
    {
        key ^=
             Zob.piece_square_keys[active][ROOK][dst]
            ^Zob.piece_square_keys[active][ROOK][rel_sq (active, dst > org ? SQ_F1 : SQ_D1)];
        dst = rel_sq (active, dst > org ? SQ_G1 : SQ_C1);
    }
    else
    {
        auto cpt = en_passant (m) ? PAWN : ptype (board[dst]);
        if (cpt != NONE)
        {
            key ^= Zob.piece_square_keys[~active][cpt][en_passant (m) ? dst - pawn_push (active) : dst];
        }
    }
    key ^=
          Zob.color_key
        ^ Zob.piece_square_keys[active][ppt][dst]
        ^ Zob.piece_square_keys[active][mpt][org];
    
    i32 cr;
    if (   si->castle_rights != CR_NONE
        && (cr = (  castle_mask[org]
                  | castle_mask[dst])) != 0)
    {
        Bitboard b = si->castle_rights & cr;
        while (b != 0)
        {
            key ^= (*Zob.castle_right_keys)[pop_lsq (b)];
        }
    }
    if (si->en_passant_sq != SQ_NO)
    {
        key ^= Zob.en_passant_keys[_file (si->en_passant_sq)];
    }
    if (   mpt == PAWN
        && (u08(dst) ^ u08(org)) == 16)
    {
        auto ep_sq = org + (dst - org) / 2;
        if (can_en_passant (~active, ep_sq, false))
        {
            key ^= Zob.en_passant_keys[_file (ep_sq)];
        }
    }
    return key;
}

inline CastleRight Position::can_castle (Color c) const { return si->castle_rights & castle_right (c); }
inline CastleRight Position::can_castle (CastleRight cr) const { return si->castle_rights & cr; }

inline bool Position::impeded_castle (CastleRight cr) const { return (castle_path[cr] & pieces ()) != 0; }
// move_num starts at 1, and is incremented after BLACK's move.
inline i16  Position::move_num () const { return i16(std::max ((ply - (active == BLACK ? 1 : 0))/2, 0) + 1); }
// Calculates the phase interpolating total non-pawn material between endgame and midgame limits.
inline Phase Position::phase () const
{
    return Phase(i32(std::min (std::max (si->non_pawn_matl[WHITE] + si->non_pawn_matl[BLACK], VALUE_ENDGAME), VALUE_MIDGAME)
                        - VALUE_ENDGAME) * PHASE_MIDGAME / (VALUE_MIDGAME - VALUE_ENDGAME));
}
// Attackers to the square 's' by color 'c' on occupancy 'occ'
inline Bitboard Position::attackers_to (Square s, Color c, Bitboard occ) const
{
    return (  (PawnAttacks[~c][s]        & pieces (PAWN))
            | (PieceAttacks[NIHT][s]     & pieces (NIHT))
            | (attacks_bb<BSHP> (s, occ) & pieces (BSHP, QUEN))
            | (attacks_bb<ROOK> (s, occ) & pieces (ROOK, QUEN))
            | (PieceAttacks[KING][s]     & pieces (KING))) & pieces (c);
}
// Attackers to the square 's' by color 'c'
inline Bitboard Position::attackers_to (Square s, Color c) const
{
    return attackers_to (s, c, pieces ());
}
// Attackers to the square 's' on occupancy 'occ'
inline Bitboard Position::attackers_to (Square s, Bitboard occ) const
{
    return (  (PawnAttacks[WHITE][s]     & pieces (BLACK, PAWN))
            | (PawnAttacks[BLACK][s]     & pieces (WHITE, PAWN))
            | (PieceAttacks[NIHT][s]     & pieces (NIHT))
            | (attacks_bb<BSHP> (s, occ) & pieces (BSHP, QUEN))
            | (attacks_bb<ROOK> (s, occ) & pieces (ROOK, QUEN))
            | (PieceAttacks[KING][s]     & pieces (KING)));
}
// Attackers to the square 's'
inline Bitboard Position::attackers_to (Square s) const
{
    return attackers_to (s, pieces ());
}
// Absolute blockers are friend pieces, that blocks the check to friend king.
inline Bitboard Position::abs_blockers (Color c) const
{
    return si->king_blockers[ c] & pieces (c);
}
// Discovered blockers are friend pieces, that blocks the check to enemy king.
inline Bitboard Position::dsc_blockers (Color c) const
{
    return si->king_blockers[~c] & pieces (c);
}
// Absolute checkers are friend pieces, that give the check when enemy piece is moved.
inline Bitboard Position::abs_checkers (Color c) const
{
    return si->king_checkers[~c] & pieces (c);
}
// Discovered checkers are friend pieces, that give the check when own piece is moved.
inline Bitboard Position::dsc_checkers (Color c) const
{
    return si->king_checkers[ c] & pieces (c);
}

// Check if pawn passed at the given square
inline bool Position::pawn_passed_at (Color c, Square s) const
{
    return (pawn_pass_span (c, s) & pieces (~c, PAWN)) == 0;
}
// Check the side has pair of opposite color bishops
inline bool Position::paired_bishop (Color c) const
{
    for (i08 pc = 1; pc < i08(count<BSHP> (c)); ++pc)
    {
        if (opposite_colors (square (c, BSHP, pc-1), square (c, BSHP, pc)))
        {
            return true;
        }
    }
    return false;
}
inline bool Position::opposite_bishops () const
{
    return count<BSHP> (WHITE) == 1
        && count<BSHP> (BLACK) == 1
        && opposite_colors (square (WHITE, BSHP), square (BLACK, BSHP));
}
inline bool Position::en_passant (Move m) const
{
    return mtype (m) == ENPASSANT
        && (pieces (active, PAWN) & org_sq (m)) != 0
        && si->en_passant_sq == dst_sq (m)
        && empty (si->en_passant_sq);
}
inline bool Position::capture (Move m) const
{
    // Castling is encoded as "king captures the rook"
    return (   (   mtype (m) == NORMAL
                || promotion (m))
            && (pieces (~active) & dst_sq (m)) != 0)
        || en_passant (m);
}
inline bool Position::promotion (Move m) const
{
    return mtype (m) == PROMOTE
        && (pieces (active, PAWN) & org_sq (m)) != 0
        && rel_rank (active, org_sq (m)) == R_7;
}
inline bool Position::capture_or_promotion (Move m) const
{
    return (   mtype (m) == NORMAL
            && (pieces (~active) & dst_sq (m)) != 0)
        || en_passant (m)
        || promotion (m);
}

inline void Position::place_piece (Square s, Color c, PieceType pt)
{
    //assert(empty (s)); // Not needed, in case of remove_piece()
    board[s] = (c|pt);
    Bitboard bb = square_bb (s);
    color_bb[c]    |= bb;
    types_bb[pt]   |= bb;
    types_bb[NONE] |= bb;

    squares[c][pt].push_back (s);
}
inline void Position::place_piece (Square s, Piece p)
{
    assert(_ok (p));
    place_piece (s, color (p), ptype (p));
}
inline void Position::remove_piece (Square s)
{
    assert(!empty (s));
    auto c  = color (board[s]);
    auto pt = ptype (board[s]);
    //board[s] = NO_PIECE; // Not needed, overwritten by the capturing one
    Bitboard bb = square_bb (s);
    color_bb[c]    ^= bb;
    types_bb[pt]   ^= bb;
    types_bb[NONE] ^= bb;

    auto &v = squares[c][pt];
    assert(!v.empty ());
    if (v.size () > 1)
    {
        std::swap (*std::find (v.begin (), v.end (), s), v.back ());
    }
    v.pop_back ();
}
inline void Position::move_piece (Square s1, Square s2)
{
    assert(!empty (s1));
    auto c  = color (board[s1]);
    auto pt = ptype (board[s1]);
    board[s2] = board[s1];
    board[s1] = NO_PIECE;
    Bitboard bb = square_bb (s1) ^ square_bb (s2);
    color_bb[c]    ^= bb;
    types_bb[pt]   ^= bb;
    types_bb[NONE] ^= bb;

    auto &v = squares[c][pt];
    assert(!v.empty ());
    v[v.size () > 1 ? std::find (v.begin (), v.end (), s1) - v.begin () : 0] = s2;
}
// do_castling() is a helper used to do/undo a castling move.
// This is a bit tricky, especially in Chess960.
template<bool Do> inline void Position::do_castling (Square king_org, Square &king_dst, Square &rook_org, Square &rook_dst)
{
    bool king_side = king_dst > king_org;
    // Move the piece. The tricky Chess960 castle is handled earlier
    rook_org = king_dst; // castle is always encoded as "King captures friendly Rook"
    king_dst = rel_sq (active, king_side ? SQ_G1 : SQ_C1);
    rook_dst = rel_sq (active, king_side ? SQ_F1 : SQ_D1);
    // Remove both pieces first since squares could overlap in chess960
    remove_piece (Do ? king_org : king_dst);
    remove_piece (Do ? rook_org : rook_dst);
    board[Do ? king_org : king_dst] =
    board[Do ? rook_org : rook_dst] = NO_PIECE; // Not done by remove_piece()
    place_piece (Do ? king_dst : king_org, active, KING);
    place_piece (Do ? rook_dst : rook_org, active, ROOK);
}

template<class CharT, class Traits>
inline std::basic_ostream<CharT, Traits>&
operator<< (std::basic_ostream<CharT, Traits> &os, const Position &pos)
{
    os << std::string(pos);
    return os;
}

inline void StateInfo::set_check_info (const Position &pos)
{
    king_checkers[WHITE] =
    king_checkers[BLACK] = 0;
    king_blockers[WHITE] = pos.slider_blockers<WHITE> (pos.square (WHITE, KING), 0, king_checkers[WHITE], king_checkers[BLACK]);
    king_blockers[BLACK] = pos.slider_blockers<BLACK> (pos.square (BLACK, KING), 0, king_checkers[BLACK], king_checkers[WHITE]);

    checks[PAWN] = PawnAttacks[~pos.active][pos.square (~pos.active, KING)];
    checks[NIHT] = PieceAttacks[NIHT][pos.square (~pos.active, KING)];
    checks[BSHP] = attacks_bb<BSHP> (pos.square (~pos.active, KING), pos.pieces ());
    checks[ROOK] = attacks_bb<ROOK> (pos.square (~pos.active, KING), pos.pieces ());
    checks[QUEN] = checks[BSHP] | checks[ROOK];
    checks[KING] = 0;
}

#if !defined(NDEBUG)
// Check the validity of FEN string
inline bool _ok (const std::string &fen, bool full = true)
{
    StateInfo si;
    return Position().setup (fen, si, nullptr, full).ok ();
}
#endif

#endif // _POSITION_H_INC_
