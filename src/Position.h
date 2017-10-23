#ifndef _POSITION_H_INC_
#define _POSITION_H_INC_

#include <cassert>
#include <deque>
#include <memory> // For std::unique_ptr
#include <string>
#include <list>

#include "BitBoard.h"
#include "Zobrist.h"
#include "Type.h"

class Position;
using namespace BitBoard;

/// StateInfo stores information needed to restore a Position object to its previous state
/// when we retract a move. Whenever a move is made on the board (by calling do_move),
/// a StateInfo object must be passed as a parameter.
///
///  - Castling-rights information.
///  - En-passant square (SQ_NO if no en passant capture is possible).
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
    Key         posi_key;       // Hash key of position
    Key         matl_key;       // Hash key of materials
    Key         pawn_key;       // Hash key of pawns

    Value       non_pawn_matl[CLR_NO];
    Score       psq_score;

    CastleRight castle_rights;  // Castling-rights information
    Square      en_passant_sq;  // En-passant -> "In passing"
    u08         clock_ply;      // Number of halfmoves clock since the last pawn advance or any capture
                                // Used to determine if a draw can be claimed under the clock-move rule
    u08         null_ply;
    
    // ---Not copied when making a move---
    PieceType   capture;        // Piece type captured
    bool        promotion;      // Promotion
    Bitboard    checkers;       // Checkers
    // Check info
    Bitboard    king_blockers[CLR_NO];// Absolute and Discover Blockers
    Bitboard    king_checkers[CLR_NO];// Absolute and Discover Checkers
    Bitboard    checks[NONE];

    StateInfo  *ptr;            // Previous StateInfo pointer.

    Value non_pawn_material () const
    {
        return non_pawn_matl[WHITE]
             + non_pawn_matl[BLACK];
    }
    Value non_pawn_material (Color c) const
    {
        return non_pawn_matl[c];
    }

    void set_check_info (const Position &pos);
};

/// A list to keep track of the position states along the setup moves
/// (from the start position to the position just before the search starts).
/// Needed by 'draw by repetition' detection.
/// Use a std::deque because pointers to elements are not invalidated upon list resizing.
typedef std::unique_ptr<std::deque<StateInfo>> StateListPtr;

namespace Threading {
    class Thread;
}
using namespace Threading;

/// Position class stores information regarding the board representation:
///  - 64-entry array of pieces, indexed by the square.
///  - Bitboards of each piece type.
///  - Bitboards of each color
///  - Bitboard of all occupied squares.
///  - List of squares for the pieces.
///  - Information about the castling rights.
///  - Initial files of both pairs of rooks, castle path and kings path, this is used to implement the Chess960 castling rules.
///  - Color of side on move.
///  - Ply of the game.
///  - StateInfo pointer for the current status.
class Position
{
private:
    void place_piece (Square, Color, PieceType);
    void place_piece (Square, Piece);
    void remove_piece (Square);
    void move_piece (Square, Square);

    void set_castle (Color, CastleSide);

    bool can_en_passant (Color, Square, bool = true) const;

    template<bool Do>
    void do_castling (Square, Square&, Square&, Square&);

    template<PieceType PT>
    PieceType pick_least_val_att (Square, Bitboard, Bitboard&, Bitboard&) const;

public:
    static bool Chess960;
    static u08  DrawClockPly;
    
    Piece    board[SQ_NO];
    Bitboard color_bb[CLR_NO];
    Bitboard types_bb[MAX_PTYPE];
    std::list<Square> squares[CLR_NO][NONE];

    CastleRight castle_mask[SQ_NO];

    Square   castle_rook[CLR_NO][CS_NO];
    Bitboard castle_path[CLR_NO][CS_NO];
    Bitboard king_path  [CLR_NO][CS_NO];

    Color active;
    i16   ply;
    
    Thread *thread;

    StateInfo *si; // Current state information pointer

    Position () = default;
    Position (const Position&) = delete;
    Position& operator= (const Position&) = delete;

    Piece operator[] (Square s) const;
    bool empty  (Square s)  const;

    Bitboard pieces () const;
    Bitboard pieces (Color) const;
    Bitboard pieces (PieceType) const;
    Bitboard pieces (Color, PieceType) const;
    Bitboard pieces (PieceType, PieceType) const;
    Bitboard pieces (Color, PieceType, PieceType) const;

    template<PieceType PT> i32 count () const;
    template<PieceType PT> i32 count (Color) const;
    i32 count (Color, PieceType) const;

    template<PieceType PT> Square square (Color, i08 = 0) const;

    Key pg_key () const;
    Key move_posi_key (Move) const;

    bool can_castle (Color) const;
    bool can_castle (Color, CastleSide) const;
    bool has_castleright (CastleRight) const;

    bool expeded_castle (Color, CastleSide) const;

    i16  move_num () const;
    bool draw (i16) const;

    bool see_ge (Move, Value = VALUE_ZERO) const;

    Bitboard attackers_to (Square, Color, Bitboard) const;
    Bitboard attackers_to (Square, Color) const;
    Bitboard attackers_to (Square, Bitboard) const;
    Bitboard attackers_to (Square) const;

    Bitboard slider_blockers (Color, Square, Bitboard, Bitboard&, Bitboard&) const;
    Bitboard abs_blockers (Color) const;
    Bitboard dsc_blockers (Color) const;
    Bitboard abs_checkers (Color) const;
    Bitboard dsc_checkers (Color) const;

    bool pseudo_legal (Move) const;
    bool legal (Move) const;
    bool en_passant (Move) const;
    bool capture (Move) const;
    bool promotion (Move) const;
    bool capture_or_promotion (Move) const;
    bool gives_check (Move) const;

    PieceType cap_type  (Move) const;

    bool pawn_passed_at (Color, Square) const;
    bool paired_bishop  (Color) const;
    bool opposite_bishops () const;

    void clear ();

    Position& setup (const std::string&, StateInfo&, Thread *const = nullptr, bool = true);
    Position& setup (const std::string&, StateInfo&, Color);
    
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
    return board[s];
}
inline bool Position::empty  (Square s)  const
{
    assert(_ok (s));
    return board[s] == NO_PIECE;
}

inline Bitboard Position::pieces () const
{
    return types_bb[NONE];
}
inline Bitboard Position::pieces (Color c) const
{
    return color_bb[c];
}
inline Bitboard Position::pieces (PieceType pt) const
{
    return types_bb[pt];
}
inline Bitboard Position::pieces (Color c, PieceType pt) const
{
    return color_bb[c]&types_bb[pt];
}
inline Bitboard Position::pieces (PieceType pt1, PieceType pt2) const
{
    return types_bb[pt1]|types_bb[pt2];
}
inline Bitboard Position::pieces (Color c, PieceType pt1, PieceType pt2) const
{
    return color_bb[c]&(types_bb[pt1]|types_bb[pt2]);
}

/// Position::count<>() counts specific piece
template<PieceType PT> inline i32 Position::count () const
{
    assert(PT < NONE);
    return i32(squares[WHITE][PT].size ()
             + squares[BLACK][PT].size ());
}
/// Position::count<NONE>() counts total pieces
template<> inline i32 Position::count<NONE> () const
{
    return i32(squares[WHITE][PAWN].size () + squares[BLACK][PAWN].size ()
             + squares[WHITE][NIHT].size () + squares[BLACK][NIHT].size ()
             + squares[WHITE][BSHP].size () + squares[BLACK][BSHP].size ()
             + squares[WHITE][ROOK].size () + squares[BLACK][ROOK].size ()
             + squares[WHITE][QUEN].size () + squares[BLACK][QUEN].size ()
             + squares[WHITE][KING].size () + squares[BLACK][KING].size ());
}
/// Position::count<>() counts specific piece of color
template<PieceType PT> inline i32 Position::count (Color c) const
{
    assert(PT < NONE);
    return i32(squares[c][PT].size ());
}
/// Position::count<NONE>() counts total pieces of color
template<> inline i32 Position::count<NONE> (Color c) const
{
    return i32(squares[c][PAWN].size ()
             + squares[c][NIHT].size ()
             + squares[c][BSHP].size ()
             + squares[c][ROOK].size ()
             + squares[c][QUEN].size ()
             + squares[c][KING].size ());
}
/// Position::count() counts specific piece of color and type
inline i32 Position::count (Color c, PieceType pt) const
{
    assert(pt < NONE);
    return i32(squares[c][pt].size ());
}

template<PieceType PT> inline Square Position::square (Color c, i08 index) const
{
    assert(PT < NONE);
    assert(i08(squares[c][PT].size ()) > index);
    return *(index == 0 ?
             squares[c][PT].begin () : 
             std::next (squares[c][PT].begin (), index));
}

inline Key Position::pg_key () const
{
    return PolyZob.compute_posi_key (*this);
}
/// Position::move_posi_key() computes the new hash key after the given moven.
/// Needed for speculative prefetch.
inline Key Position::move_posi_key (Move m) const
{
    auto org = org_sq (m);
    auto dst = dst_sq (m);
    auto mpt = ptype (board[org]);
    assert(!empty (org)
        && color (board[org]) == active
        && NONE != mpt);
    
    auto key = si->posi_key;
    auto ppt = PROMOTE != mtype (m) ? mpt : promote (m);
    if (CASTLE == mtype (m))
    {
        key ^= RandZob.piece_square_keys[active][ROOK][dst]
             ^ RandZob.piece_square_keys[active][ROOK][rel_sq (active, dst > org ? SQ_F1 : SQ_D1)];
    }
    else
    {
        if (   NORMAL == mtype (m)
            && PAWN == mpt
            && 16 == (u08(dst) ^ u08(org)))
        {
            auto ep_sq = org + (dst - org) / 2;
            if (can_en_passant (~active, ep_sq, false))
            {
                key ^= RandZob.en_passant_keys[_file (ep_sq)];
            }
        }
        auto cpt = ENPASSANT != mtype (m) ? ptype (board[dst]) : PAWN;
        if (NONE != cpt)
        {
            key ^= RandZob.piece_square_keys[~active][cpt][ENPASSANT != mtype (m) ?
                                                                dst :
                                                                dst - pawn_push (active)];
        }
    }
    auto b = si->castle_rights & (castle_mask[org]|castle_mask[dst]);
    if (CR_NONE != b)
    {
        if (CR_NONE != (b & CR_WKING)) key ^= RandZob.castle_right_keys[WHITE][CS_KING];
        if (CR_NONE != (b & CR_WQUEN)) key ^= RandZob.castle_right_keys[WHITE][CS_QUEN];
        if (CR_NONE != (b & CR_BKING)) key ^= RandZob.castle_right_keys[BLACK][CS_KING];
        if (CR_NONE != (b & CR_BQUEN)) key ^= RandZob.castle_right_keys[BLACK][CS_QUEN];
    }
    return key
         ^ RandZob.color_key
         ^ RandZob.piece_square_keys[active][ppt][CASTLE != mtype (m) ? dst : rel_sq (active, dst > org ? SQ_G1 : SQ_C1)]
         ^ RandZob.piece_square_keys[active][mpt][org]
         ^ (SQ_NO != si->en_passant_sq ? RandZob.en_passant_keys[_file (si->en_passant_sq)] : 0);
}

inline bool Position::can_castle (Color c) const
{
    return CR_NONE != (si->castle_rights & castle_right (c));
}
inline bool Position::can_castle (Color c, CastleSide cs) const
{
    return CR_NONE != (si->castle_rights & castle_right (c, cs));
}
inline bool Position::has_castleright (CastleRight cr) const
{
    return CR_NONE != (si->castle_rights & cr);
}

inline bool Position::expeded_castle (Color c, CastleSide cs) const
{
    return 0 == (castle_path[c][cs] & pieces ());
}
/// Position::move_num() starts at 1, and is incremented after BLACK's move.
inline i16  Position::move_num () const
{
    return i16(std::max ((ply - (BLACK == active ? 1 : 0))/2, 0) + 1);
}
/// Position::attackers_to() finds attackers to the square by color on occupancy.
inline Bitboard Position::attackers_to (Square s, Color c, Bitboard occ) const
{
    return (pieces (c, PAWN) & PawnAttacks[~c][s])
         | (pieces (c, NIHT) & PieceAttacks[NIHT][s])
         | (0 != (pieces (c, BSHP, QUEN) & PieceAttacks[BSHP][s]) ? pieces (c, BSHP, QUEN) & attacks_bb<BSHP> (s, occ) : 0)
         | (0 != (pieces (c, ROOK, QUEN) & PieceAttacks[ROOK][s]) ? pieces (c, ROOK, QUEN) & attacks_bb<ROOK> (s, occ) : 0)
         | (pieces (c, KING) & PieceAttacks[KING][s]);
}
/// Position::attackers_to() finds attackers to the square by color.
inline Bitboard Position::attackers_to (Square s, Color c) const
{
    return attackers_to (s, c, pieces ());
}
/// Position::attackers_to() finds attackers to the square on occupancy.
inline Bitboard Position::attackers_to (Square s, Bitboard occ) const
{
    return (pieces (BLACK, PAWN) & PawnAttacks[WHITE][s])
         | (pieces (WHITE, PAWN) & PawnAttacks[BLACK][s])
         | (pieces (NIHT)        & PieceAttacks[NIHT][s])
         | (0 != (pieces (BSHP, QUEN) & PieceAttacks[BSHP][s]) ? pieces (BSHP, QUEN) & attacks_bb<BSHP> (s, occ) : 0)
         | (0 != (pieces (ROOK, QUEN) & PieceAttacks[ROOK][s]) ? pieces (ROOK, QUEN) & attacks_bb<ROOK> (s, occ) : 0)
         | (pieces (KING)        & PieceAttacks[KING][s]);
}
/// Position::attackers_to() finds attackers to the square.
inline Bitboard Position::attackers_to (Square s) const
{
    return attackers_to (s, pieces ());
}

/// Position::abs_blockers() find absolute blockers (friend pieces), that blocks the check to friend king.
inline Bitboard Position::abs_blockers (Color c) const
{
    return si->king_blockers[ c] & pieces (c);
}
/// Position::dsc_blockers() finds discovered blockers (friend pieces), that blocks the check to enemy king.
inline Bitboard Position::dsc_blockers (Color c) const
{
    return si->king_blockers[~c] & pieces (c);
}
/// Position::abs_checkers() find absolute checkers (friend pieces), that give the check when enemy piece is moved.
inline Bitboard Position::abs_checkers (Color c) const
{
    return si->king_checkers[~c] & pieces (c);
}
/// Position::dsc_checkers() finds discovered checkers (friend pieces), that give the check when friend piece is moved.
inline Bitboard Position::dsc_checkers (Color c) const
{
    return si->king_checkers[ c] & pieces (c);
}

/// Position::pawn_passed_at() check if pawn passed at the given square.
inline bool Position::pawn_passed_at (Color c, Square s) const
{
    return 0 == (pawn_pass_span (c, s) & pieces (~c, PAWN));
}
/// Position::paired_bishop() check the side has pair of opposite color bishops.
inline bool Position::paired_bishop (Color c) const
{
    //for (u08 pc = 1; pc < count<BSHP> (c); ++pc)
    //{
    //    if (opposite_colors (square<BSHP> (c, pc-1), square<BSHP> (c, pc)))
    //    {
    //        return true;
    //    }
    //}
    //return false;
    return 0 != (pieces (c, BSHP) & Color_bb[WHITE])
        && 0 != (pieces (c, BSHP) & Color_bb[BLACK]);
}
inline bool Position::opposite_bishops () const
{
    return 1 == count<BSHP> (WHITE)
        && 1 == count<BSHP> (BLACK)
        //&& opposite_colors (square<BSHP> (WHITE), square<BSHP> (BLACK));
        && (   (   0 != (pieces (WHITE, BSHP) & Color_bb[WHITE])
                && 0 != (pieces (BLACK, BSHP) & Color_bb[BLACK]))
            || (   0 != (pieces (WHITE, BSHP) & Color_bb[BLACK])
                && 0 != (pieces (BLACK, BSHP) & Color_bb[WHITE])));
}
inline bool Position::en_passant (Move m) const
{
    return ENPASSANT == mtype (m)
        && contains (pieces (active, PAWN), org_sq (m))
        && empty (dst_sq (m))
        && si->en_passant_sq == dst_sq (m);
}
inline bool Position::capture (Move m) const
{
    // Castling is encoded as "king captures the rook"
    return (   (   NORMAL == mtype (m)
                || promotion (m))
            && contains (pieces (~active), dst_sq (m)))
        || en_passant (m);
}
inline bool Position::promotion (Move m) const
{
    return PROMOTE == mtype (m)
        && contains (pieces (active, PAWN), org_sq (m))
        && rel_rank (active, org_sq (m)) == R_7;
}
inline bool Position::capture_or_promotion (Move m) const
{
    return (   NORMAL == mtype (m)
            && contains (pieces (~active), dst_sq (m)))
        || en_passant (m)
        || promotion (m);
}
inline PieceType Position::cap_type (Move m) const
{
    return ENPASSANT != mtype (m) ?
               ptype (board[dst_sq (m)]) :
               PAWN;
}

inline void Position::do_move (Move m, StateInfo &nsi)
{
    do_move (m, nsi, gives_check (m));
}

inline void Position::place_piece (Square s, Color c, PieceType pt)
{
    //assert(empty (s)); // Not needed, in case of remove_piece()
    board[s] = (c|pt);
    color_bb[c] |= s;
    types_bb[pt] |= s;
    types_bb[NONE] |= s;

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
    color_bb[c] ^= s;
    types_bb[pt] ^= s;
    types_bb[NONE] ^= s;

    squares[c][pt].remove (s);
}
inline void Position::move_piece (Square s1, Square s2)
{
    assert(!empty (s1));
    auto c  = color (board[s1]);
    auto pt = ptype (board[s1]);
    board[s2] = board[s1];
    board[s1] = NO_PIECE;
    Bitboard bb = square_bb (s1) ^ square_bb (s2);
    color_bb[c] ^= bb;
    types_bb[pt] ^= bb;
    types_bb[NONE] ^= bb;

    std::replace (squares[c][pt].begin (), squares[c][pt].end (), s1, s2);
}
/// do_castling() is a helper used to do/undo a castling move.
/// This is a bit tricky, especially in Chess960.
template<bool Do>
inline void Position::do_castling (Square king_org, Square &king_dst, Square &rook_org, Square &rook_dst)
{
    rook_org = king_dst; // Castling is always encoded as "King captures friendly Rook"
    king_dst = rel_sq (active, rook_org > king_org ? SQ_G1 : SQ_C1);
    rook_dst = rel_sq (active, rook_org > king_org ? SQ_F1 : SQ_D1);
    // Remove both pieces first since squares could overlap in chess960
    remove_piece (Do ? king_org : king_dst);
    remove_piece (Do ? rook_org : rook_dst);
    board[Do ? king_org : king_dst] =
    board[Do ? rook_org : rook_dst] = NO_PIECE; // Not done by remove_piece()
    place_piece (Do ? king_dst : king_org, active, KING);
    place_piece (Do ? rook_dst : rook_org, active, ROOK);
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
    king_blockers[WHITE] = pos.slider_blockers (WHITE, pos.square<KING> (WHITE), 0, king_checkers[WHITE], king_checkers[BLACK]);
    king_blockers[BLACK] = pos.slider_blockers (BLACK, pos.square<KING> (BLACK), 0, king_checkers[BLACK], king_checkers[WHITE]);

    checks[PAWN] = PawnAttacks[~pos.active][pos.square<KING> (~pos.active)];
    checks[NIHT] = PieceAttacks[NIHT][pos.square<KING> (~pos.active)];
    checks[BSHP] = attacks_bb<BSHP> (pos.square<KING> (~pos.active), pos.pieces ());
    checks[ROOK] = attacks_bb<ROOK> (pos.square<KING> (~pos.active), pos.pieces ());
    checks[QUEN] = checks[BSHP] | checks[ROOK];
    checks[KING] = 0;
}

#if !defined(NDEBUG)
/// _ok() Check the validity of FEN string.
inline bool _ok (const std::string &fen, bool full = true)
{
    StateInfo si;
    return !white_spaces (fen)
        && Position().setup (fen, si, nullptr, full).ok ();
}
#endif

#endif // _POSITION_H_INC_
