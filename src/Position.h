#ifndef _POSITION_H_INC_
#define _POSITION_H_INC_

#include "BitBoard.h"
#include "Zobrist.h"

class Position;
using namespace BitBoard;

extern const Value PieceValues[PH_NO][MAX_PTYPE];

// Check the validity of FEN string
extern bool _ok (const std::string &fen, bool c960 = false, bool full = true);

// StateInfo stores information needed to restore a Position object to its previous state
// when we retract a move. Whenever a move is made on the board (by calling do_move),
// a StateInfo object must be passed as a parameter.
//
//  - Castling-rights information for both sides.
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
    // Copied when making a move
    Key         matl_key;   //= 0;          // Hash key of materials.
    Key         pawn_key;   //= 0;          // Hash key of pawns.

    Value       non_pawn_matl[CLR_NO];
    Score       psq_score;  //= SCORE_ZERO;

    CastleRight castle_rights;//= CR_NONE;  // Castling-rights information for both side.
    Square      en_passant_sq;//= SQ_NO;    // En-passant -> "In passing"
    u08         clock_ply;  //= 0;          // Number of halfmoves clock since the last pawn advance or any capture.
                                            // Used to determine if a draw can be claimed under the clock-move rule.
    u08         null_ply;   //= 0;

    // Not copied when making a move
    Key         posi_key;   //= 0;          // Hash key of position.
    Move        last_move;  //= MOVE_NONE;  // Move played on the previous position.
    PieceType   capture_type;//= NONE;      // Piece type captured.
    Bitboard    checkers;   //= 0;          // Checkers bitboard.

    StateInfo   *ptr;       //= nullptr;    // Previous StateInfo.

    StateInfo () = default;
};

// CheckInfo struct is initialized at constructor time
// and stores critical information used to detect if a move gives check.
//
//  - Checking squares from which the enemy king can be checked
//  - Pinned pieces.
//  - Check discoverer pieces.
//  - Enemy king square.
struct CheckInfo
{
public:
    Bitboard checking_bb[NONE];     // Checking squares from which the enemy king can be checked
    Bitboard pinneds;    //= 0;     // Pinned pieces
    Bitboard discoverers;//= 0;     // Check discoverer pieces
    Square   king_sq;    //= SQ_NO; // Enemy king square

    CheckInfo () = delete;
    explicit CheckInfo (const Position &pos);
};

namespace Threading {
    class Thread;
}

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
//  - Information about the castling rights for both sides.
//  - Initial files of both pairs of rooks, castle path and kings path, this is used to implement the Chess960 castling rules.
//  - Chess960 info
class Position
{
private:
    Piece       _board[SQ_NO];  // Board for storing pieces.

    Bitboard    _color_bb[CLR_NO];
    Bitboard    _types_bb[MAX_PTYPE];

    SquareVector _piece_sq[CLR_NO][NONE];

    CastleRight _castle_mask[SQ_NO];
    Square      _castle_rook[CR_NO];
    Bitboard    _castle_path[CR_NO];
    Bitboard    _king_path  [CR_NO];

    Color       _active;
    bool        _chess960;
    i16         _game_ply;
    u64         _game_nodes;

    StateInfo   *_si; // Current state information pointer
    Threading::Thread  *_thread;

    // ------------------------

    void place_piece (Square s, Color c, PieceType pt);
    void place_piece (Square s, Piece p);
    void remove_piece (Square s);
    void move_piece (Square s1, Square s2);

    void set_castle (Color c, Square rook_org);

    bool can_en_passant (Square ep_sq) const;
    bool can_en_passant (File ep_f) const;

    template<bool Do>
    void do_castling (Square king_org, Square &king_dst, Square &rook_org, Square &rook_dst);

    template<PieceType PT>
    PieceType pick_least_val_att (Square dst, Bitboard stm_attackers, Bitboard &mocc, Bitboard &attackers) const;

public:
    static u08  DrawClockPly;

    static void initialize ();

    Position () = default;
    Position (const Position&) = delete;
    Position& operator= (const Position &pos) = delete;

    Piece operator[] (Square s) const;
    //Bitboard operator[] (Color  c) const;
    //Bitboard operator[] (PieceType pt) const;
    const SquareVector& operator[] (Piece p)  const;

    bool empty  (Square s)  const;

    Bitboard pieces () const;
    Bitboard pieces (Color c) const;
    Bitboard pieces (PieceType pt) const;
    Bitboard pieces (Color c, PieceType pt) const;
    Bitboard pieces (PieceType p1, PieceType p2) const;
    Bitboard pieces (Color c, PieceType p1, PieceType p2) const;

    template<PieceType PT>
    i32      count  () const;
    template<PieceType PT>
    i32      count  (Color c) const;
    i32      count  (Color c, PieceType pt) const;

    template<PieceType PT>
    const SquareVector& squares (Color c) const;
    template<PieceType PT>
    Square square (Color c, i32 index = 0) const;

    CastleRight castle_rights () const;
    Square en_passant_sq () const;

    u08  clock_ply () const;
    Move last_move () const;
    PieceType capture_type () const;
    //Piece  capture_piece () const;  // Last piece captured
    Bitboard checkers () const;

    Key matl_key () const;
    Key pawn_key () const;
    Key posi_key () const;
    Key poly_key () const;
    Key move_posi_key (Move m) const;

    Value non_pawn_material (Color c) const;

    Score psq_score () const;

    CastleRight can_castle (Color c) const;
    CastleRight can_castle (CastleRight cr) const;

    Square   castle_rook (CastleRight cr) const;
    Bitboard castle_path (CastleRight cr) const;
    Bitboard king_path   (CastleRight cr) const;
    bool  castle_impeded (CastleRight cr) const;

    Color   active   () const;
    bool    chess960 () const;
    i16     game_ply () const;
    i16     move_num () const;
    bool    draw     () const;
    bool    repeated () const;

    u64   game_nodes ()  const;
    //void  game_nodes (u64 nodes);
    Phase game_phase ()  const;

    Threading::Thread* thread   ()  const;

    bool ok (i08 *failed_step = nullptr) const;

    Value see      (Move m) const;
    Value see_sign (Move m) const;

    Bitboard attackers_to (Square s, Color c, Bitboard occ) const;
    Bitboard attackers_to (Square s, Color c) const;
    Bitboard attackers_to (Square s, Bitboard occ) const;
    Bitboard attackers_to (Square s) const;

    Bitboard slider_blockers (Square s, Bitboard sliders, Bitboard target) const;
    Bitboard checkers    (Color c) const;
    Bitboard pinneds     (Color c) const;
    Bitboard discoverers (Color c) const;

    bool pseudo_legal   (Move m) const;
    bool legal          (Move m, Bitboard pinned) const;
    bool legal          (Move m) const;
    bool capture        (Move m) const;
    bool promotion (Move m) const;
    bool capture_or_promotion (Move m) const;
    bool en_passant     (Move m) const;
    bool gives_check    (Move m, const CheckInfo &ci) const;
    //bool gives_checkmate (Move m, const CheckInfo &ci) const;
    bool advanced_pawn_push (Move m)    const;

    bool passed_pawn  (Color c, Square s) const;
    bool bishops_pair (Color c) const;
    bool opposite_bishops ()    const;

    Value compute_non_pawn_material (Color c) const;

    void clear ();

    bool setup (const std::string &fen_str, StateInfo &si, Threading::Thread *const th = nullptr, bool c960 = false, bool full = true);

    void do_move (Move m, StateInfo &si, bool gives_check);
    void do_move (const std::string &can, StateInfo &si);
    void undo_move ();
    void do_null_move (StateInfo &si);
    void undo_null_move ();

    void flip ();

    std::string fen (bool c960 = false, bool full = true) const;

    explicit operator std::string () const;

};

// -------------------------------

inline Piece Position::operator[] (Square s) const { return _board[s]; }
//inline Bitboard Position::operator[] (Color  c) const { return _color_bb[c];  }
//inline Bitboard Position::operator[] (PieceType pt) const { return _types_bb[pt]; }
inline const SquareVector& Position::operator[] (Piece  p) const { return _piece_sq[color (p)][ptype (p)]; }

inline bool Position::empty  (Square s)  const { return _board[s] == NO_PIECE; }

inline Bitboard Position::pieces () const { return _types_bb[NONE]; }
inline Bitboard Position::pieces (Color c) const { return _color_bb[c]; }
inline Bitboard Position::pieces (PieceType pt) const { return _types_bb[pt]; }
inline Bitboard Position::pieces (Color c,   PieceType pt) const { return _color_bb[c]&_types_bb[pt]; }
inline Bitboard Position::pieces (PieceType p1, PieceType p2) const { return _types_bb[p1]|_types_bb[p2]; }
inline Bitboard Position::pieces (Color c, PieceType p1, PieceType p2) const { return _color_bb[c]&(_types_bb[p1]|_types_bb[p2]); }

template<PieceType PT>
// Count specific piece
inline i32 Position::count () const { return i32(_piece_sq[WHITE][PT].size () + _piece_sq[BLACK][PT].size ()); }
template<>
// Count total pieces
inline i32 Position::count<NONE> () const
{
    return i32(_piece_sq[WHITE][PAWN].size () + _piece_sq[BLACK][PAWN].size ()
             + _piece_sq[WHITE][NIHT].size () + _piece_sq[BLACK][NIHT].size ()
             + _piece_sq[WHITE][BSHP].size () + _piece_sq[BLACK][BSHP].size ()
             + _piece_sq[WHITE][ROOK].size () + _piece_sq[BLACK][ROOK].size ()
             + _piece_sq[WHITE][QUEN].size () + _piece_sq[BLACK][QUEN].size ()
             + _piece_sq[WHITE][KING].size () + _piece_sq[BLACK][KING].size ());
}
template<>
// Count non-pawn pieces
inline i32 Position::count<NONPAWN> () const
{
    return i32(_piece_sq[WHITE][NIHT].size () + _piece_sq[BLACK][NIHT].size ()
             + _piece_sq[WHITE][BSHP].size () + _piece_sq[BLACK][BSHP].size ()
             + _piece_sq[WHITE][ROOK].size () + _piece_sq[BLACK][ROOK].size ()
             + _piece_sq[WHITE][QUEN].size () + _piece_sq[BLACK][QUEN].size ());
}
template<PieceType PT>
// Count specific piece of color
inline i32 Position::count (Color c) const { return i32(_piece_sq[c][PT].size ()); }
template<>
// Count total pieces of color
inline i32 Position::count<NONE> (Color c) const
{
    return i32(_piece_sq[c][PAWN].size ()
             + _piece_sq[c][NIHT].size ()
             + _piece_sq[c][BSHP].size ()
             + _piece_sq[c][ROOK].size ()
             + _piece_sq[c][QUEN].size ()
             + _piece_sq[c][KING].size ());
}
template<>
// Count non-pawn pieces of color
inline i32 Position::count<NONPAWN> (Color c) const
{
    return i32(_piece_sq[c][NIHT].size ()
             + _piece_sq[c][BSHP].size ()
             + _piece_sq[c][ROOK].size ()
             + _piece_sq[c][QUEN].size ());
}
inline i32 Position::count (Color c, PieceType pt) const { return i32(_piece_sq[c][pt].size ()); }

template<PieceType PT>
inline const SquareVector& Position::squares (Color c) const { return _piece_sq[c][PT]; }
template<PieceType PT>
inline Square Position::square (Color c, i32 index) const
{
    assert(i32(_piece_sq[c][PT].size ()) > index);
    return _piece_sq[c][PT][index];
}

// Castling rights for both side
inline CastleRight Position::castle_rights () const { return _si->castle_rights; }
// Target square in algebraic notation. If there's no en passant target square is "-"
inline Square Position::en_passant_sq () const { return _si->en_passant_sq; }
// Number of halfmoves clock since the last pawn advance or any capture.
// used to determine if a draw can be claimed under the clock-move rule.
inline u08 Position::clock_ply () const { return _si->clock_ply; }
inline Move Position::last_move () const { return _si->last_move; }
inline PieceType Position::capture_type () const { return _si->capture_type; }
//inline Piece  Position::capture_piece () const { return _si->capture_type != NONE ? _active|_si->capture_type : NO_PIECE; }
inline Bitboard Position::checkers () const { return _si->checkers; }

inline Key Position::matl_key () const { return _si->matl_key; }
inline Key Position::pawn_key () const { return _si->pawn_key; }
inline Key Position::posi_key () const { return _si->posi_key; }
inline Key Position::poly_key () const { return PolyZob.compute_posi_key (*this); }
// move_posi_key() computes the new hash key after the given moven. Needed for speculative prefetch.
// It doesn't recognize special moves like castling, en-passant and promotions.
inline Key Position::move_posi_key (Move m) const
{
    auto org = org_sq (m);
    auto dst = dst_sq (m);
    auto mpt = ptype (_board[org]);
    assert(!empty (org)
          && color (_board[org]) == _active
          && mpt != NONE);

    auto ppt = mtype (m) == PROMOTE
           && _board[org] == (_active|PAWN) ?
            promote (m) : mpt;
    PieceType cpt;
    Square    cap;
    if (en_passant (m))
    {
        cpt = PAWN;
        cap = dst - pawn_push (_active);
    }
    else
    {
        cpt = ptype (_board[dst]);
        cap = dst;
    }

    Key key = _si->posi_key ^ Zob.act_side
        ^ Zob.piece_square[_active][mpt][org]
        ^ Zob.piece_square[_active][ppt][dst];
    if (cpt != NONE)
    {
        key ^= Zob.piece_square[~_active][cpt][cap];
    }
    return key;
}

inline Score  Position::psq_score () const { return _si->psq_score; }
// Incremental piece-square evaluation
inline Value  Position::non_pawn_material (Color c) const { return _si->non_pawn_matl[c]; }

inline CastleRight Position::can_castle (Color c) const { return _si->castle_rights & mk_castle_right (c); }
inline CastleRight Position::can_castle (CastleRight cr) const { return _si->castle_rights & cr; }

inline Square   Position::castle_rook (CastleRight cr) const { return _castle_rook[cr]; }
inline Bitboard Position::castle_path (CastleRight cr) const { return _castle_path[cr]; }
inline Bitboard Position::king_path   (CastleRight cr) const { return _king_path[cr]; }

inline bool  Position::castle_impeded (CastleRight cr) const { return (_castle_path[cr] & pieces ()) != 0; }
// Color of the side on move
inline Color Position::active  () const { return _active; }
inline bool Position::chess960 () const { return _chess960; }
// game_ply starts at 0, and is incremented after every move.
// game_ply  = max ((move_num - 1) * 2, 0) + (active == BLACK)
inline i16  Position::game_ply () const { return _game_ply; }
// move_num starts at 1, and is incremented after BLACK's move.
// move_num = max ((game_ply - (active == BLACK)) / 2, 0) + 1
inline i16  Position::move_num () const { return i16(std::max ((_game_ply - (_active == BLACK ? 1 : 0)) / 2, 0) + 1); }
// Nodes visited
inline u64  Position::game_nodes () const { return _game_nodes; }
//inline void Position::game_nodes (u64 nodes) { _game_nodes = nodes; }
// game_phase() calculates the phase interpolating total
// non-pawn material between endgame and midgame limits.
inline Phase Position::game_phase () const
{
    return Phase(
        i32(std::max (std::min (_si->non_pawn_matl[WHITE] + _si->non_pawn_matl[BLACK], VALUE_MIDGAME), VALUE_ENDGAME) - VALUE_ENDGAME) * i32(PHASE_MIDGAME) /
        i32(VALUE_MIDGAME - VALUE_ENDGAME));
}

inline Threading::Thread* Position::thread () const { return _thread; }

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

// Checkers are enemy pieces that give the direct Check to friend King of color 'c'
inline Bitboard Position::checkers (Color c) const
{
    return attackers_to (square<KING> (c), ~c);
}
// Pinneds are friend pieces, that save the friend king from enemy pinners.
inline Bitboard Position::pinneds (Color c) const
{
    return slider_blockers (square<KING> ( c), pieces (~c), pieces (c));
}
// Discoverers are candidate friend pieces, that give the discover check to enemy king when moved.
inline Bitboard Position::discoverers (Color c) const
{
    return slider_blockers (square<KING> (~c), pieces ( c), pieces (c));
}
inline bool Position::passed_pawn (Color c, Square s) const
{
    return (pieces (~c, PAWN) & pawn_pass_span (c, s)) == 0;
}
// bishops_pair(c) check the side has pair of opposite color bishops
inline bool Position::bishops_pair (Color c) const
{
    for (i32 pc = 1; pc < count<BSHP> (c); ++pc)
    {
        if (opposite_colors (square<BSHP> (c, pc-1), square<BSHP> (c, pc)))
        {
            return true;
        }
    }
    return false;
}
// opposite_bishops() check the opposite sides have opposite bishops
inline bool Position::opposite_bishops () const
{
    return count<BSHP> (WHITE) == 1
        && count<BSHP> (BLACK) == 1
        && opposite_colors (square<BSHP> (WHITE), square<BSHP> (BLACK));
}
inline bool Position::legal (Move m) const { return legal (m, pinneds (_active)); }
// capture(m) checks move is capture
inline bool Position::capture (Move m) const
{
    // Castling is encoded as "king captures the rook"
    return ((mtype (m) == NORMAL || promotion (m)) && (pieces (~_active) & dst_sq (m)) != 0)
        || en_passant (m);
}
// promotion(m) checks move is promotion
inline bool Position::promotion (Move m) const
{
    return mtype (m) == PROMOTE
        && _board[org_sq (m)] == (_active|PAWN)
        && rel_rank (_active, dst_sq (m)) == R_8;
}
// capture_or_promotion(m) checks move is capture or promotion
inline bool Position::capture_or_promotion (Move m) const
{
    return (mtype (m) == NORMAL && (pieces (~_active) & dst_sq (m)) != 0)
        || en_passant (m)
        || promotion (m);
}
// en_passant(m) checks move is en-passant
inline bool Position::en_passant (Move m) const
{
    return mtype (m) == ENPASSANT
        && _board[org_sq (m)] == (_active|PAWN)
        && _si->en_passant_sq == dst_sq (m)
        && empty (dst_sq (m));
}
// advanced_pawn_push(m) checks move is advanced pawn push
inline bool Position::advanced_pawn_push (Move m) const
{
    return _board[org_sq (m)] == (_active|PAWN)
        && rel_rank (_active, org_sq (m)) > R_4;
}

inline void  Position::place_piece (Square s, Color c, PieceType pt)
{
    //assert(empty (s));
    _board[s] = (c | pt);

    auto bb = Square_bb[s];
    _color_bb[c]    |= bb;
    _types_bb[pt]   |= bb;
    _types_bb[NONE] |= bb;

    auto &v = _piece_sq[c][pt];
    v.push_back (s);
}
inline void  Position::place_piece (Square s, Piece p)
{
    assert(_ok (p));
    place_piece (s, color (p), ptype (p));
}
inline void  Position::remove_piece (Square s)
{
    //assert(!empty (s));
    auto c  = color (_board[s]);
    auto pt = ptype (_board[s]);
    //_board[s] = NO_PIECE; // Not needed, overwritten by the capturing one

    auto bb = ~Square_bb[s];
    _color_bb[c]    &= bb;
    _types_bb[pt]   &= bb;
    _types_bb[NONE] &= bb;

    auto &v = _piece_sq[c][pt];
    assert(!v.empty ());
    if (v.size () > 1)
    {
        std::swap (*std::find (v.begin (), v.end (), s), v.back ());
    }
    v.pop_back ();
}
inline void  Position::move_piece (Square s1, Square s2)
{
    //assert(!empty (s1));
    //assert( empty (s2));
    auto c  = color (_board[s1]);
    auto pt = ptype (_board[s1]);

    _board[s2] = _board[s1];
    _board[s1] = NO_PIECE;

    auto bb = Square_bb[s1] ^ Square_bb[s2];
    _color_bb[c]    ^= bb;
    _types_bb[pt]   ^= bb;
    _types_bb[NONE] ^= bb;

    auto &v = _piece_sq[c][pt];
    assert(!v.empty ());
    v[v.size () > 1 ? std::find (v.begin (), v.end (), s1) - v.begin () : 0] = s2;
}
// do_castling() is a helper used to do/undo a castling move.
// This is a bit tricky, especially in Chess960.
template<bool Do>
inline void Position::do_castling (Square king_org, Square &king_dst, Square &rook_org, Square &rook_dst)
{
    // Move the piece. The tricky Chess960 castle is handled earlier
    rook_org = king_dst; // castle is always encoded as "King captures friendly Rook"
    king_dst = rel_sq (_active, king_dst > king_org ? SQ_G1 : SQ_C1);
    rook_dst = rel_sq (_active, king_dst > king_org ? SQ_F1 : SQ_D1);
    // Remove both pieces first since squares could overlap in chess960
    remove_piece (Do ? king_org : king_dst);
    remove_piece (Do ? rook_org : rook_dst);
    _board[Do ? king_org : king_dst] = _board[Do ? rook_org : rook_dst] = NO_PIECE; // Not done by remove_piece()
    place_piece (Do ? king_dst : king_org, _active, KING);
    place_piece (Do ? rook_dst : rook_org, _active, ROOK);
}

template<class CharT, class Traits>
inline std::basic_ostream<CharT, Traits>&
operator<< (std::basic_ostream<CharT, Traits> &os, const Position &pos)
{
    os << std::string(pos);
    return os;
}

// ----------------------------------------------
// CheckInfo constructor
inline CheckInfo::CheckInfo (const Position &pos)
{
    king_sq     = pos.square<KING> (~pos.active ());
    pinneds     = pos.pinneds (pos.active ());
    discoverers = pos.discoverers (pos.active ());

    checking_bb[PAWN] = PawnAttacks[~pos.active ()][king_sq];
    checking_bb[NIHT] = PieceAttacks[NIHT][king_sq];
    checking_bb[BSHP] = attacks_bb<BSHP> (king_sq, pos.pieces ());
    checking_bb[ROOK] = attacks_bb<ROOK> (king_sq, pos.pieces ());
    checking_bb[QUEN] = checking_bb[BSHP] | checking_bb[ROOK];
    checking_bb[KING] = 0;
}

#endif // _POSITION_H_INC_
