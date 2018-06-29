#ifndef _POSITION_H_INC_
#define _POSITION_H_INC_

#include <cassert>
#include <deque>
#include <list>
#include <memory> // For std::unique_ptr
#include <string>
#include "BitBoard.h"
#include "PSQT.h"
#include "Type.h"
#include "Zobrist.h"

using namespace BitBoard;

/// StateInfo stores information needed to restore a Position object to its previous state
/// when we retract a move. Whenever a move is made on the board (by calling do_move),
/// a StateInfo object must be passed as a parameter.
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
    Key         posi_key;       // Hash key of position
    Key         matl_key;       // Hash key of materials
    Key         pawn_key;       // Hash key of pawns

    Value       npm[CLR_NO];

    CastleRight castle_rights;  // Castling-rights information
    Square      enpassant_sq;   // Enpassant -> "In passing"
    u08         clock_ply;      // Number of half moves clock since the last pawn advance or any capture
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
    bool can_castle (Color c) const
    {
        return can_castle (castle_right (c));
    }
    bool can_castle (Color c, CastleSide cs) const
    {
        return can_castle (castle_right (c, cs));
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
    void place_piece_on (Square, Piece);
    void remove_piece_on (Square);
    void move_piece_on_to (Square, Square);

    void set_castle (Color, Square);

    bool can_enpassant (Color, Square, bool = true) const;

    void do_castling (Square, Square&, Square&, Square&);
    void undo_castling (Square, Square&, Square&, Square&);

    PieceType pick_least_val_att (PieceType, Square, Bitboard, Bitboard&, Bitboard&) const;

public:
    Piece    board[SQ_NO];
    Bitboard color_bb[CLR_NO];
    Bitboard types_bb[PT_NO];
    std::list<Square> squares[CLR_NO][NONE];

    CastleRight castle_mask[SQ_NO];

    Square   castle_rook[CLR_NO][CS_NO];
    Bitboard castle_path[CLR_NO][CS_NO];
    Bitboard king_path  [CLR_NO][CS_NO];

    Color active;
    i16   ply;

    Score psq;

    Thread *thread;

    StateInfo *si; // Current state information pointer

    static void initialize ();

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

    i32 count () const;
    i32 count (Color) const;
    i32 count (PieceType) const;
    i32 count (Color, PieceType) const;

    template<PieceType>
    Square square (Color, i08 = 0) const;

    Key pg_key () const;
    Key posi_move_key (Move) const;

    bool expeded_castle (Color, CastleSide) const;

    i16  move_num () const;
    bool draw (i16) const;
    bool cycled (i16) const;
    bool repeated () const;

    bool see_ge (Move, Value = VALUE_ZERO) const;

    Bitboard attackers_to (Square, Color, Bitboard) const;
    Bitboard attackers_to (Square, Color) const;
    Bitboard attackers_to (Square, Bitboard) const;
    Bitboard attackers_to (Square) const;

    Bitboard slider_blockers (Color, Square, Bitboard, Bitboard&, Bitboard&) const;
    Bitboard abs_blockers (Color) const;
    Bitboard dsc_blockers (Color) const;
    //Bitboard abs_checkers (Color) const;
    //Bitboard dsc_checkers (Color) const;

    bool pseudo_legal (Move) const;
    bool legal (Move) const;
    bool enpassant (Move) const;
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
    return NO_PIECE == board[s];
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
    assert(PAWN <= pt && pt <= KING);
    return types_bb[pt];
}
inline Bitboard Position::pieces (Color c, PieceType pt) const
{
    assert(PAWN <= pt && pt <= KING);
    return color_bb[c]&types_bb[pt];
}
inline Bitboard Position::pieces (PieceType pt1, PieceType pt2) const
{
    assert(PAWN <= pt1 && pt1 <= KING);
    assert(PAWN <= pt2 && pt2 <= KING);
    return types_bb[pt1]|types_bb[pt2];
}
inline Bitboard Position::pieces (Color c, PieceType pt1, PieceType pt2) const
{
    assert(PAWN <= pt1 && pt1 <= KING);
    assert(PAWN <= pt2 && pt2 <= KING);
    return color_bb[c]&(types_bb[pt1]|types_bb[pt2]);
}
/// Position::count() counts all
inline i32 Position::count () const
{
    return i32(squares[WHITE][PAWN].size () + squares[BLACK][PAWN].size ()
             + squares[WHITE][NIHT].size () + squares[BLACK][NIHT].size ()
             + squares[WHITE][BSHP].size () + squares[BLACK][BSHP].size ()
             + squares[WHITE][ROOK].size () + squares[BLACK][ROOK].size ()
             + squares[WHITE][QUEN].size () + squares[BLACK][QUEN].size ()
             + squares[WHITE][KING].size () + squares[BLACK][KING].size ());
}
/// Position::count() counts specific color
inline i32 Position::count (Color c) const
{
    return i32(squares[c][PAWN].size ()
             + squares[c][NIHT].size ()
             + squares[c][BSHP].size ()
             + squares[c][ROOK].size ()
             + squares[c][QUEN].size ()
             + squares[c][KING].size ());
}
/// Position::count() counts specific type
inline i32 Position::count (PieceType pt) const
{
    assert(PAWN <= pt && pt <= KING);
    return i32(squares[WHITE][pt].size () + squares[BLACK][pt].size ());
}
/// Position::count() counts specific color and type
inline i32 Position::count (Color c, PieceType pt) const
{
    assert(PAWN <= pt && pt <= KING);
    return i32(squares[c][pt].size ());
}

template<PieceType PT>
inline Square Position::square (Color c, i08 index) const
{
    static_assert (PAWN <= PT && PT <= KING, "PT incorrect");
    assert(i08(squares[c][PT].size ()) > index);
    return 0 == index ?
             *squares[c][PT].begin () : 
             *std::next (squares[c][PT].begin (), index);
}

inline Key Position::pg_key () const
{
    return PolyZob.compute_posi_key (*this);
}
/// Position::move_posi_key() computes the new hash key after the given moven.
/// Needed for speculative prefetch.
inline Key Position::posi_move_key (Move m) const
{
    const auto org = org_sq (m);
    const auto dst = dst_sq (m);
    assert(contains (pieces (active), org));

    auto key = si->posi_key;
    auto ppt = PROMOTE != mtype (m) ?
                ptype (board[org]) :
                promote (m);
    if (CASTLE == mtype (m))
    {
        key ^= RandZob.piece_square[active][ROOK][dst]
             ^ RandZob.piece_square[active][ROOK][rel_sq (active, dst > org ? SQ_F1 : SQ_D1)];
    }
    else
    {
        if (   NORMAL == mtype (m)
            && PAWN == ptype (board[org])
            && 16 == (u08(dst) ^ u08(org)))
        {
            const auto ep_sq = org + (dst - org) / 2;
            if (can_enpassant (~active, ep_sq, false))
            {
                key ^= RandZob.enpassant[_file (ep_sq)];
            }
        }
        auto cpt = ENPASSANT != mtype (m) ?
                    ptype (board[dst]) :
                    PAWN;
        if (NONE != cpt)
        {
            key ^= RandZob.piece_square[~active][cpt][ENPASSANT != mtype (m) ?
                                                                dst :
                                                                dst - pawn_push (active)];
        }
    }
    auto b = si->castle_rights & (castle_mask[org]|castle_mask[dst]);
    if (CR_NONE != b)
    {
        if (CR_NONE != (b & CR_WKING)) key ^= RandZob.castle_right[WHITE][CS_KING];
        if (CR_NONE != (b & CR_WQUEN)) key ^= RandZob.castle_right[WHITE][CS_QUEN];
        if (CR_NONE != (b & CR_BKING)) key ^= RandZob.castle_right[BLACK][CS_KING];
        if (CR_NONE != (b & CR_BQUEN)) key ^= RandZob.castle_right[BLACK][CS_QUEN];
    }
    return key
         ^ RandZob.color
         ^ RandZob.piece_square[active][ppt][CASTLE != mtype (m) ? dst : rel_sq (active, dst > org ? SQ_G1 : SQ_C1)]
         ^ RandZob.piece_square[active][ptype (board[org])][org]
         ^ (SQ_NO != si->enpassant_sq ? RandZob.enpassant[_file (si->enpassant_sq)] : 0);
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
///// Position::abs_checkers() find absolute checkers (friend pieces), that give the check when enemy piece is moved.
//inline Bitboard Position::abs_checkers (Color c) const
//{
//    return si->king_checkers[~c] & pieces (c);
//}
///// Position::dsc_checkers() finds discovered checkers (friend pieces), that give the check when friend piece is moved.
//inline Bitboard Position::dsc_checkers (Color c) const
//{
//    return si->king_checkers[ c] & pieces (c);
//}

/// Position::pawn_passed_at() check if pawn passed at the given square.
inline bool Position::pawn_passed_at (Color c, Square s) const
{
    return 0 == (pawn_pass_span (c, s) & pieces (~c, PAWN));
}
/// Position::paired_bishop() check the side has pair of opposite color bishops.
inline bool Position::paired_bishop (Color c) const
{
    //for (u08 pc = 1; pc < count (c, BSHP); ++pc)
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
    return 1 == count (WHITE, BSHP)
        && 1 == count (BLACK, BSHP)
        //&& opposite_colors (square<BSHP> (WHITE), square<BSHP> (BLACK));
        && (   (   0 != (pieces (WHITE, BSHP) & Color_bb[WHITE])
                && 0 != (pieces (BLACK, BSHP) & Color_bb[BLACK]))
            || (   0 != (pieces (WHITE, BSHP) & Color_bb[BLACK])
                && 0 != (pieces (BLACK, BSHP) & Color_bb[WHITE])));
}
inline bool Position::enpassant (Move m) const
{
    return ENPASSANT == mtype (m)
        && contains (pieces (active, PAWN), org_sq (m))
        && empty (dst_sq (m))
        && si->enpassant_sq == dst_sq (m);
}
inline bool Position::capture (Move m) const
{
    // Castling is encoded as "king captures the rook"
    return (   (   NORMAL == mtype (m)
                || promotion (m))
            && contains (pieces (~active), dst_sq (m)))
        || enpassant (m);
}
inline bool Position::promotion (Move m) const
{
    return PROMOTE == mtype (m)
        && contains (pieces (active, PAWN), org_sq (m))
        && R_7 == rel_rank (active, org_sq (m));
}
inline bool Position::capture_or_promotion (Move m) const
{
    return (   NORMAL == mtype (m)
            && contains (pieces (~active), dst_sq (m)))
        || enpassant (m)
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

inline void Position::place_piece_on (Square s, Piece pc)
{
    //assert(empty (s)); // Not needed, in case of remove_piece_on()
    assert(_ok (pc));
    color_bb[color (pc)] |= s;
    types_bb[ptype (pc)] |= s;
    types_bb[NONE] |= s;
    squares[color (pc)][ptype (pc)].emplace_back (s);
    psq += PSQT[color (pc)][ptype (pc)][s];
    board[s] = pc;
}
inline void Position::remove_piece_on (Square s)
{
    assert(!empty (s));
    color_bb[color (board[s])] ^= s;
    types_bb[ptype (board[s])] ^= s;
    types_bb[NONE] ^= s;
    squares[color (board[s])][ptype (board[s])].remove (s);
    psq -= PSQT[color (board[s])][ptype (board[s])][s];
    //board[s] = NO_PIECE; // Not needed, overwritten by the capturing one
}
inline void Position::move_piece_on_to (Square s1, Square s2)
{
    assert(!empty (s1)
        && std::count (squares[color (board[s1])][ptype (board[s1])].begin (),
                       squares[color (board[s1])][ptype (board[s1])].end (), s1) == 1);
    Bitboard bb = square_bb (s1) ^ square_bb (s2);
    color_bb[color (board[s1])] ^= bb;
    types_bb[ptype (board[s1])] ^= bb;
    types_bb[NONE] ^= bb;
    std::replace (squares[color (board[s1])][ptype (board[s1])].begin (),
                  squares[color (board[s1])][ptype (board[s1])].end (), s1, s2);
    psq += PSQT[color (board[s1])][ptype (board[s1])][s2]
         - PSQT[color (board[s1])][ptype (board[s1])][s1];
    board[s2] = board[s1];
    board[s1] = NO_PIECE;
}

/// do_castling()
inline void Position::do_castling (Square king_org, Square &king_dst, Square &rook_org, Square &rook_dst)
{
    rook_org = king_dst; // Castling is always encoded as "King captures friendly Rook"
    king_dst = rel_sq (active, rook_org > king_org ? SQ_G1 : SQ_C1);
    rook_dst = rel_sq (active, rook_org > king_org ? SQ_F1 : SQ_D1);
    // Remove both pieces first since squares could overlap in chess960
    remove_piece_on (king_org);
    remove_piece_on (rook_org);
    board[king_org] =
    board[rook_org] = NO_PIECE; // Not done by remove_piece_on()
    place_piece_on (king_dst, active|KING);
    place_piece_on (rook_dst, active|ROOK);
}
/// undo_castling()
inline void Position::undo_castling (Square king_org, Square &king_dst, Square &rook_org, Square &rook_dst)
{
    rook_org = king_dst; // Castling is always encoded as "King captures friendly Rook"
    king_dst = rel_sq (active, rook_org > king_org ? SQ_G1 : SQ_C1);
    rook_dst = rel_sq (active, rook_org > king_org ? SQ_F1 : SQ_D1);
    // Remove both pieces first since squares could overlap in chess960
    remove_piece_on (king_dst);
    remove_piece_on (rook_dst);
    board[king_dst] =
    board[rook_dst] = NO_PIECE; // Not done by remove_piece_on()
    place_piece_on (king_org, active|KING);
    place_piece_on (rook_org, active|ROOK);
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
