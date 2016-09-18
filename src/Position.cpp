#include "Position.h"

#include "Transposition.h"
#include "PieceSquare.h"
#include "MoveGenerator.h"
#include "Thread.h"
#include "Notation.h"

using namespace std;
using namespace BitBoard;
using namespace PieceSquare;
using namespace Transposition;
using namespace MoveGen;
using namespace Threading;
using namespace Notation;

u08  Position::DrawClockPly = 100;
bool Position::Chess960     = false;

// Checks whether position is drawn by: Clock Ply Rule, Repetition.
// It does not detect Insufficient materials and Stalemate.
bool Position::draw () const
{
    // Draw by Clock Ply Rule?
    // Not in check or in check have legal moves 
    if (   _si->clock_ply >= DrawClockPly
        && (   _si->checkers == 0
            || MoveList<LEGAL> (*this).size () != 0))
    {
        return true;
    }
    // Draw by Repetition?
    const auto *psi = _si;
    for (auto ply = std::min (psi->clock_ply, psi->null_ply);
              ply >= 2;
              ply -= 2)
    {
        psi = psi->ptr->ptr;
        // Check first repetition
        if (psi->posi_key == _si->posi_key)
        {
            return true;
        }
    }
    return false;
}
// Check whether there has been at least one repetition of position since the last capture or pawn move.
bool Position::repeated () const
{
    for (const auto *bsi = _si;
            bsi != nullptr;
            bsi = bsi->ptr)
    {
        const auto *psi = bsi;
        for (auto ply = std::min (psi->clock_ply, psi->null_ply);
                  ply >= 2;
                  ply -= 2)
        {
            psi = psi->ptr->ptr;
            // Check first repetition
            if (psi->posi_key == bsi->posi_key)
            {
                return true;
            }
        }
    }
    return false;
}
// Helper function used by see() to locate the least valuable attacker for the side to move,
// remove the attacker just found from the bitboards and scan for new X-ray attacks behind it.
template<PieceType PT>
PieceType Position::pick_least_val_att (Square dst, Bitboard c_attackers, Bitboard &mocc, Bitboard &attackers) const
{
    Bitboard b = c_attackers & pieces (PT);
    if (b != 0)
    {
        mocc ^= b & ~(b - 1);

        switch (PT)
        {
        case PAWN:
        case BSHP:
            attackers |= pieces (BSHP, QUEN) & attacks_bb<BSHP> (dst, mocc);
            break;
        case ROOK:
            attackers |= pieces (ROOK, QUEN) & attacks_bb<ROOK> (dst, mocc);
            break;
        case QUEN:
            attackers |= (pieces (BSHP, QUEN) & attacks_bb<BSHP> (dst, mocc))
                      |  (pieces (ROOK, QUEN) & attacks_bb<ROOK> (dst, mocc));
            break;
        case NIHT:
            break;
        }

        attackers &= mocc; // After X-ray that may add already processed pieces
        return PT;
    }

    return pick_least_val_att<PieceType(PT+1)> (dst, c_attackers, mocc, attackers);
}
template<>
PieceType Position::pick_least_val_att<KING> (Square, Bitboard, Bitboard&, Bitboard&) const
{
    return KING; // No need to update bitboards, it is the last cycle
}

// Static Exchange Evaluator (SEE): It tries to estimate the material gain or loss resulting from a move.
Value Position::see (Move m) const
{
    assert(m != MOVE_NONE);

    auto org = org_sq (m);
    auto dst = dst_sq (m);
    assert(!empty (org));

    auto c = color (_board[org]);

    Value gain_values[32];
    Bitboard mocc;

    switch (mtype (m))
    {
    case CASTLE:
        // Castle moves are implemented as king capturing the rook so cannot be
        // handled correctly. Simply return 0 that is always the correct value
        // unless in the rare case the rook ends up under attack.
        return VALUE_ZERO;
        break;

    case ENPASSANT:
        assert(_board[org] == (_active|PAWN));
        // Remove the captured pawn
        mocc = pieces () - org - (dst - pawn_push (c));
        gain_values[0] = PieceValues[MG][PAWN];
        break;

    default:
        assert(_board[org] != NO_PIECE);
        mocc = pieces () - org;
        gain_values[0] = PieceValues[MG][ptype (_board[dst])];
        break;
    }
    // For the case when captured piece is a pinner
    mocc -= dst;

    // Find all attackers to the destination square, with the moving piece
    // removed, but possibly an X-ray attacker added behind it.
    Bitboard attackers = attackers_to (dst, mocc) & mocc;

    Bitboard c_attackers;
    c = ~c;
    c_attackers = attackers & pieces (c);
    // Don't allow pinned pieces to attack pieces except the king as long all pinners are on their original square.
    // When a pinner moves to the exchange-square or get captured on it, we fall back to standard SEE behaviour.
    if (   (c_attackers & abs_pinneds (c)) != 0
        && (_si->pinners[c] & mocc) == _si->pinners[c])
    {
        c_attackers &= ~abs_pinneds (c);
    }

    if (c_attackers != 0)
    {
        // The destination square is defended, which makes things rather more
        // difficult to compute. Proceed by building up a "swap list" containing
        // the material gain or loss at each stop in a sequence of captures to the
        // destination square, where the sides alternately capture, and always
        // capture with the least valuable piece. After each capture, look for
        // new X-ray attacks from behind the capturing piece.
        auto captured = ptype (_board[org]);

        i16 depth = 1;
        do {
            assert(depth < 32);

            // Add the new entry to the swap list
            gain_values[depth] = PieceValues[MG][captured] - gain_values[depth - 1];

            // Locate and remove the next least valuable attacker
            captured = pick_least_val_att<PAWN> (dst, c_attackers, mocc, attackers);

            c = ~c;
            c_attackers = attackers & pieces (c);
            if (   captured != KING // for resolving Bxf2 on fen: r2qk2r/pppb1ppp/2np4/1Bb5/4n3/5N2/PPP2PPP/RNBQR1K1 b kq - 1 1
                && (c_attackers & abs_pinneds (c)) != 0
                && (_si->pinners[c] & mocc) == _si->pinners[c])
            {
                c_attackers &= ~abs_pinneds (c);
            }

            ++depth;
        } while (   c_attackers != 0
                 // Stop before a king capture
                 && (captured != KING || (--depth, false)));

        // Having built the swap list, negamax through it to find the best
        // achievable score from the point of view of the side to move.
        while (--depth != 0)
        {
            // Find minimum gain
            if (gain_values[depth - 1] > -gain_values[depth])
            {
                gain_values[depth - 1] = -gain_values[depth];
            }
        }
    }

    return gain_values[0];
}
// Sign of SSE
Value Position::see_sign (Move m) const
{
    assert(m != MOVE_NONE);
    // If SEE cannot be negative because captured piece value is not less then capturing one.
    // Note that king moves always return here because king value is set to VALUE_ZERO.
    return
           PieceValues[MG][ptype (_board[org_sq (m)])]
        <= PieceValues[MG][ptype (_board[dst_sq (m)])] ?
            VALUE_KNOWN_WIN :
            see (m);
}
// Returns a bitboard of all the pieces that are blocking attacks on the square 's' from 'sliders'.
// A piece blocks a slider if removing that piece from the board would result in a position where square 's' is attacked by the 'sliders'.
// For example, a king-attack blocking piece can be either a pinned or a discovered check piece,
// according if its color is the opposite or the same of the color of the slider.
Bitboard Position::slider_blockers (Square s, Bitboard sliders, Bitboard &pinners) const
{
    Bitboard blockers = pinners = 0;
    Bitboard defenders = pieces (color (_board[s]));
    // Snipers are sliders that attack 's' in x-ray
    Bitboard snipers =
          sliders
        & (  (pieces (BSHP, QUEN) & PieceAttacks[BSHP][s])
           | (pieces (ROOK, QUEN) & PieceAttacks[ROOK][s]));
    while (snipers != 0)
    {
        auto sniper_sq = pop_lsq (snipers);
        Bitboard b = pieces () & between_bb (s, sniper_sq);
        if (!more_than_one (b))
        {
            blockers |= b;
            if ((b & defenders) != 0)
            {
                pinners += sniper_sq;
            }
        }
    }
    return blockers;
}
// Tests whether a random move is pseudo-legal.
// It is used to validate moves from TT that can be corrupted
// due to SMP concurrent access or hash position key aliasing.
bool Position::pseudo_legal (Move m) const
{
    assert(m != MOVE_NONE);
    auto org = org_sq (m);
    auto dst = dst_sq (m);
    // If the org square is not occupied by a piece belonging to the side to move,
    // then the move is obviously not legal.
    if ((pieces (_active) & org) == 0)
    {
        return false;
    }
    if (   ((pieces (_active)|pieces (KING)) & dst) != 0
        && mtype (m) != CASTLE)
    {
        return false;
    }

    auto mpt = ptype (_board[org]);
    auto cap = dst;

    switch (mtype (m))
    {
    case NORMAL:
    {
        // Is not a promotion, so promotion piece must be empty
        assert(promote (m) - NIHT == PAWN);
    }
        break;

    case CASTLE:
    {
        // Check whether the destination square is attacked by the opponent.
        // Castling moves are checked for legality during move generation.
        if (!(   mpt == KING
              && rel_rank (_active, org) == R_1
              && rel_rank (_active, dst) == R_1
              && _board[dst] == (_active|ROOK)
              && _si->checkers == 0
              && (_si->castle_rights & mk_castle_right (_active, dst > org ? CS_KING : CS_QUEN)) != CR_NONE
              && !impeded_castle (mk_castle_right (_active, dst > org ? CS_KING : CS_QUEN))))
        {
            return false;
        }

        // Castle is always encoded as "King captures friendly Rook"
        bool king_side = dst > org;
        assert(dst == castle_rook (mk_castle_right (_active, king_side ? CS_KING : CS_QUEN)));
        dst = rel_sq (_active, king_side ? SQ_G1 : SQ_C1);
        // Check king's path for attackers
        auto step = king_side ? DEL_E : DEL_W;
        for (auto s = dst; s != org; s -= step)
        {
            if (attackers_to (s, ~_active) != 0)
            {
                return false;
            }
        }
        return true; // No capture
    }
        break;

    case ENPASSANT:
    {
        if (!(   mpt == PAWN
              && empty (dst)
              && _si->en_passant_sq == dst
              && rel_rank (_active, org) == R_5
              && rel_rank (_active, dst) == R_6))
        {
            return false;
        }
        cap -= pawn_push (_active);
        if (_board[cap] != (~_active|PAWN))
        {
            return false;
        }
    }
        break;

    case PROMOTE:
    {
        if (!(   mpt == PAWN
              && rel_rank (_active, org) == R_7
              && rel_rank (_active, dst) == R_8
              && (NIHT <= promote (m) && promote (m) <= QUEN)))
        {
            return false;
        }
    }
        break;
    }
    // The captured square cannot be occupied by a friendly piece or kings
    if (((pieces (_active)|pieces (KING)) & cap) != 0)
    {
        return false;
    }

    // Handle the special case of a piece move
    if (mpt == PAWN)
    {
        // In case of non-promotional moves origin & destination cannot be on the 7th/2nd & 8th/1st rank.
        if (   mtype (m) != PROMOTE
            && (   rel_rank (_active, org) == R_7
                || rel_rank (_active, dst) == R_8))
        {
            return false;
        }
        if (   // Not a single push
               !(   empty (dst)
                 && (org + pawn_push (_active) == dst))
            && // Not a normal capture
               !(   mtype (m) != ENPASSANT
                 && ((pieces (~_active) & PawnAttacks[_active][org]) & dst) != 0)
               // Not an enpassant capture
            && !(   mtype (m) == ENPASSANT
                 && _si->en_passant_sq == dst
                 && ((~pieces () & PawnAttacks[_active][org]) & dst) != 0
                 && _board[cap] == (~_active|PAWN))
               // Not a double push
            && !(   rel_rank (_active, org) == R_2
                 && rel_rank (_active, dst) == R_4
                 && empty (dst)
                 && empty (dst - pawn_push (_active))
                 && (org + pawn_push (_active)*2 == dst)))
        {
            return false;
        }
    }
    else
    {
        Bitboard attacks = 0;
        switch (mpt)
        {
        case NIHT: attacks = PieceAttacks[NIHT][org];           break;
        case BSHP: attacks = attacks_bb<BSHP> (org, pieces ()); break;
        case ROOK: attacks = attacks_bb<ROOK> (org, pieces ()); break;
        case QUEN: attacks = attacks_bb<QUEN> (org, pieces ()); break;
        case KING: attacks = PieceAttacks[KING][org];           break;
        default: assert(false); break;
        }
        if ((attacks & dst) == 0)
        {
            return false;
        }
    }

    // Evasions generator already takes care to avoid some kind of illegal moves
    // and legal() relies on this. So have to take care that the
    // same kind of moves are filtered out here.
    if (_si->checkers != 0)
    {
        // In case of king moves under check, remove king so to catch
        // as invalid moves like B1A1 when opposite queen is on C1.
        if (mpt == KING)
        {
            // Remove 'org' but not place 'dst'
            return attackers_to (dst, ~_active, pieces () - org) == 0;
        }
        // Double check? In this case a king move is required
        if (!more_than_one (_si->checkers))
        {
            return en_passant (m) ?
                // Move must be a capture of the checking en-passant pawn or a blocking evasion of the checking piece
                (_si->checkers & cap) != 0 || (between_bb (scan_lsq (_si->checkers), square<KING> (_active)) & dst) != 0 :
                // Move must be a capture of the checking piece or a blocking evasion of the checking piece
                ((_si->checkers | between_bb (scan_lsq (_si->checkers), square<KING> (_active))) & dst) != 0;
        }
        return false;
    }
    return true;
}
// Tests whether a pseudo-legal move is legal
bool Position::legal (Move m) const
{
    assert(m != MOVE_NONE);

    auto org = org_sq (m);
    auto dst = dst_sq (m);
    assert((pieces (_active) & org) != 0);

    auto mpt = ptype (_board[org]);

    switch (mtype (m))
    {
    case NORMAL:
    {
        // Only king moves to non attacked squares, sliding check x-rays the king
        // In case of king moves under check have to remove king so to catch
        // as invalid moves like B1-A1 when opposite queen is on SQ_C1.
        // check whether the destination square is attacked by the opponent.
        if (mpt == KING)
        {
            // Remove 'org' but not place 'dst'
            return attackers_to (dst, ~_active, pieces () - org) == 0;
        }
    }
    // NOTE: no break
    case PROMOTE:
    {
        assert(mtype (m) == NORMAL
            || (   mtype (m) == PROMOTE
                && mpt == PAWN));
        // A non-king move is legal if and only if it is not pinned or
        // it is moving along the ray towards or away from the king or
        // it is a blocking evasion or a capture of the checking piece.
        return (abs_pinneds (_active) & org) == 0
            || sqrs_aligned (org, dst, square<KING> (_active));
    }
        break;

    case CASTLE:
    {
        // Castling moves are checked for legality during move generation.
        return mpt == KING
            && _board[dst] == (_active|ROOK);
    }
        break;

    case ENPASSANT:
    {
        // En-passant captures are a tricky special case. Because they are rather uncommon,
        // do it simply by testing whether the king is attacked after the move is made.
        auto cap = dst - pawn_push (_active);

        assert(mpt == PAWN
            && rel_rank (_active, org) == R_5
            && rel_rank (_active, dst) == R_6
            && empty (dst)
            && dst == _si->en_passant_sq
            && _board[cap] == (~_active|PAWN));

        // If any attacker then in check and not legal move
        return (   (pieces (~_active, BSHP, QUEN) & PieceAttacks[BSHP][square<KING> (_active)]) == 0
                || (pieces (~_active, BSHP, QUEN) & attacks_bb<BSHP> (square<KING> (_active), pieces () - org - cap + dst)) == 0)
            && (   (pieces (~_active, ROOK, QUEN) & PieceAttacks[ROOK][square<KING> (_active)]) == 0
                || (pieces (~_active, ROOK, QUEN) & attacks_bb<ROOK> (square<KING> (_active), pieces () - org - cap + dst)) == 0);
    }
        break;
    }
    return false;
}
// Tests whether a pseudo-legal move gives a check
bool Position::gives_check  (Move m) const
{
    auto org = org_sq (m);
    auto dst = dst_sq (m);
    assert((pieces (_active) & org) != 0);
    
    auto mpt = ptype (_board[org]);

    if (    // Direct check ?
           ((checks (mpt) & dst) != 0)
            // Discovered check ?
        || (   (dsc_checkers (_active) & org) != 0
            && !sqrs_aligned (org, dst, square<KING> (~_active))))
    {
        return true;
    }

    switch (mtype (m))
    {
    case NORMAL:
    {
        return false;
    }
        break;

    case CASTLE:
    {
        // Castling with check ?
        auto rook_org = dst; // 'King captures the rook' notation
        dst           = rel_sq (_active, dst > org ? SQ_G1 : SQ_C1);
        auto rook_dst = rel_sq (_active, dst > org ? SQ_F1 : SQ_D1);
        // First x-ray check then full check
        return (PieceAttacks[ROOK][rook_dst] & square<KING> (~_active)) != 0
            && (attacks_bb<ROOK> (rook_dst, pieces () - org - rook_org + dst + rook_dst) & square<KING> (~_active)) != 0;
    }
        break;

    case ENPASSANT:
    {
        // En-passant capture with check ?
        // already handled the case of direct checks and ordinary discovered check,
        // the only case need to handle is the unusual case of a discovered check through the captured pawn.
        return (   (pieces (_active, BSHP, QUEN) & PieceAttacks[BSHP][square<KING> (~_active)]) != 0
                && (pieces (_active, BSHP, QUEN) & attacks_bb<BSHP> (square<KING> (~_active), pieces () - org - (_file (dst)|_rank (org)) + dst)) != 0)
            || (   (pieces (_active, ROOK, QUEN) & PieceAttacks[ROOK][square<KING> (~_active)]) != 0
                && (pieces (_active, ROOK, QUEN) & attacks_bb<ROOK> (square<KING> (~_active), pieces () - org - (_file (dst)|_rank (org)) + dst)) != 0);
    }
        break;

    case PROMOTE:
    {
        // Promotion with check ?
        Bitboard attacks = 0;
        switch (promote (m))
        {
        case NIHT: attacks = PieceAttacks[NIHT][dst];                       break;
        case BSHP: attacks = attacks_bb<BSHP> (dst, pieces () - org + dst); break;
        case ROOK: attacks = attacks_bb<ROOK> (dst, pieces () - org + dst); break;
        case QUEN: attacks = attacks_bb<QUEN> (dst, pieces () - org + dst); break;
        default: assert(false); break;
        }
        return (attacks & square<KING> (~_active)) != 0;
    }
        break;
    }
    return false;
}

// Computes the total non-pawn middle
// game material value for the given side. Material values are updated
// incrementally during the search, this function is only used while
// initializing a new Position object.
Value Position::compute_non_pawn_material (Color c) const
{
    auto npm_value = VALUE_ZERO;
    for (auto pt = NIHT; pt <= QUEN; ++pt)
    {
        npm_value += PieceValues[MG][pt] * count (c, pt);
    }
    return npm_value;
}

// Clear the position
void Position::clear ()
{
    for (auto s = SQ_A1; s <= SQ_H8; ++s)
    {
        _board[s] = NO_PIECE;
        _castle_mask[s] = CR_NONE;
    }
    for (auto c = WHITE; c <= BLACK; ++c)
    {
        _color_bb[c] = 0;
    }
    for (auto pt = PAWN; pt <= NONE; ++pt)
    {
        _types_bb[pt] = 0;
    }
    for (auto c = WHITE; c <= BLACK; ++c)
    {
        for (auto pt = PAWN; pt <= KING; ++pt)
        {
            _piece_sq[c][pt].clear ();
        }
    }
    for (auto r = 0; r < CR_NO; ++r)
    {
        _castle_rook[r] = SQ_NO;
        _castle_path[r] = 0;
        _king_path[r]   = 0;
    }

    _active = CLR_NO;
    _ply    = 0;
    _nodes  = 0;
    _thread = nullptr;
}

// Set the castling for the particular color & rook
void Position::set_castle (Color c, Square rook_org)
{
    auto king_org = square<KING> (c);
    assert(king_org != rook_org);

    auto cr = mk_castle_right (c, rook_org > king_org ? CS_KING : CS_QUEN);
    auto king_dst = rel_sq (c, rook_org > king_org ? SQ_G1 : SQ_C1);
    auto rook_dst = rel_sq (c, rook_org > king_org ? SQ_F1 : SQ_D1);

    _si->castle_rights     |= cr;
    _castle_mask[king_org] |= cr;
    _castle_mask[rook_org] |= cr;
    _castle_rook[cr] = rook_org;

    for (auto s = std::min (king_org, king_dst); s <= std::max (king_org, king_dst); ++s)
    {
        if (   king_org != s
            && rook_org != s)
        {
            _castle_path[cr] += s;
            _king_path[cr] += s;
        }
    }
    for (auto s = std::min (rook_org, rook_dst); s <= std::max (rook_org, rook_dst); ++s)
    {
        if (   king_org != s
            && rook_org != s)
        {
            _castle_path[cr] += s;
        }
    }
}
// Tests the en-passant square
bool Position::can_en_passant (Square ep_sq) const
{
    assert(_ok (ep_sq));
    assert(rel_rank (_active, ep_sq) == R_6);

    auto cap = ep_sq - pawn_push (_active);
    if (   (pieces (~_active, PAWN) & cap) == 0
        || _board[cap] != (~_active|PAWN))
    {
        return false;
    }
    // En-passant attackers
    auto attackers = pieces (_active, PAWN) & PawnAttacks[~_active][ep_sq];
    assert(pop_count (attackers) <= 2);
    if (attackers != 0)
    {
        MoveVector moves;
        while (attackers != 0)
        {
            moves.push_back (mk_move<ENPASSANT> (pop_lsq (attackers), ep_sq));
        }
        // Check en-passant is legal for the position
        auto mocc = pieces () + ep_sq - cap;
        for (auto m : moves)
        {
            if (   (   (pieces (~_active, BSHP, QUEN) & PieceAttacks[BSHP][square<KING> (_active)]) == 0
                    || (pieces (~_active, BSHP, QUEN) & attacks_bb<BSHP> (square<KING> (_active), mocc - org_sq (m))) == 0)
                && (   (pieces (~_active, ROOK, QUEN) & PieceAttacks[ROOK][square<KING> (_active)]) == 0
                    || (pieces (~_active, ROOK, QUEN) & attacks_bb<ROOK> (square<KING> (_active), mocc - org_sq (m))) == 0))
            {
                return true;
            }
        }
    }
    return false;
}
bool Position::can_en_passant (File ep_f) const
{
    return can_en_passant (ep_f | rel_rank (_active, R_6));
}
// A FEN string defines a particular position using only the ASCII character set.
// A FEN string contains six fields separated by a space.
// 1) Piece placement (from white's perspective).
//    Each rank is described, starting with rank 8 and ending with rank 1;
//    within each rank, the contents of each square are described from file A through file H.
//    Following the Standard Algebraic Notation (SAN),
//    each piece is identified by a single letter taken from the standard English names.
//    White pieces are designated using upper-case letters ("PNBRQK") while
//    Black pieces are designated using lower-case letters ("pnbrqk").
//    Blank squares are noted using digits 1 through 8 (the number of blank squares),
//    and "/" separates ranks.
// 2) Active color. "w" means white, "b" means black - moves next.
// 3) Castling availability. If neither side can castle, this is "-". 
//    Otherwise, this has one or more letters:
//    "K" (White can castle  Kingside).
//    "Q" (White can castle Queenside).
//    "k" (Black can castle  Kingside).
//    "q" (Black can castle Queenside).
//    In Chess 960 file "a-h" is used.
// 4) En passant target square (in algebraic notation).
//    If there's no en passant target square, this is "-".
//    If a pawn has just made a 2-square move, this is the position "behind" the pawn.
//    This is recorded regardless of whether there is a pawn in position to make an en passant capture.
// 5) Halfmove clock. This is the number of halfmoves since the last pawn advance or capture.
//    This is used to determine if a draw can be claimed under the fifty-move rule.
// 6) Fullmove number. The number of the full move.
//    It starts at 1, and is incremented after Black's move.
Position& Position::setup (const string &ff, StateInfo &si, Thread *const th, bool full)
{
    istringstream iss (ff);
    iss >> std::noskipws;

    std::memset (&si, 0x00, sizeof (StateInfo));
    clear ();
    _si = &si;

    u08 token;
    // 1. Piece placement on Board
    size_t idx;
    auto f = F_A;
    auto r = R_8;
    while (   iss >> token
           && !isspace (token)
           && f <= F_NO
           && r >= R_1)
    {
        if (isdigit (token))
        {
            f += (token - '0');
        }
        else
        if (   isalpha (token)
            && (idx = PieceChar.find (token)) != string::npos)
        {
            place_piece (f|r, Piece(idx));
            ++f;
        }
        else
        {
            assert(token == '/');
            f = F_A;
            --r;
        }
    }

    assert(square<KING> (WHITE) != SQ_NO
        && square<KING> (BLACK) != SQ_NO);

    // 2. Active color
    iss >> token;
    _active = Color(ColorChar.find (token));

    // 3. Castling availability
    iss >> token;
    while (   iss >> token
           && !isspace (token))
    {
        Square r_sq;

        Color c = isupper (token) ? WHITE : BLACK;
        token = char(tolower (token));
        if (token == 'k')
        {
            for (r_sq  = rel_sq (c, SQ_H1);
                 r_sq >= rel_sq (c, SQ_A1)
              && _board[r_sq] != (c|ROOK);
                 --r_sq) {}
        }
        else
        if (token == 'q')
        {
            for (r_sq  = rel_sq (c, SQ_A1);
                 r_sq <= rel_sq (c, SQ_H1)
              && _board[r_sq] != (c|ROOK);
                 ++r_sq) {}
        }
        else
        // Chess960
        if ('a' <= token && token <= 'h')
        {
            r_sq = to_file (token) | rel_rank (c, R_1);
        }
        else
        {
            assert(token == '-');
            continue;
        }

        assert(_board[r_sq] == (c|ROOK));
        set_castle (c, r_sq);
    }
    
    // 4. En-passant square. Ignore if no pawn capture is possible
    auto ep_sq = SQ_NO;
    u08 file, rank;
    if (   (iss >> file && ('a' <= file && file <= 'h'))
        && (iss >> rank && ('3' == rank || rank == '6')))
    {
        if (can_en_passant (to_square (file, rank)))
        {
            ep_sq = to_square (file, rank);
        }
    }
    _si->en_passant_sq = ep_sq;

    // 5-6. Clock ply and Game move count
    i16 clk_ply = 0
      , moves  = 1;
    if (full)
    {
        iss >> std::skipws
            >> clk_ply
            >> moves;

        if (_si->en_passant_sq != SQ_NO)
        {
            clk_ply = 0;
        }
        // Rule 50 draw case
        assert(clk_ply <= 100);

        // Handle common problem move-num = 0.
        if (moves <= 0)
        {
            moves = 1;
        }
    }

    // Convert from game-move starting from 1 to game-ply starting from 0,
    _ply = i16(2*(moves - 1) + (_active == BLACK ? 1 : 0));

    _si->matl_key = Zob.compute_matl_key (*this);
    _si->pawn_key = Zob.compute_pawn_key (*this);
    _si->posi_key = Zob.compute_posi_key (*this);
    _si->psq_score = compute_psq_score (*this);
    _si->non_pawn_matl[WHITE] = compute_non_pawn_material (WHITE);
    _si->non_pawn_matl[BLACK] = compute_non_pawn_material (BLACK);
    _si->clock_ply = u08(clk_ply);
    _si->capture_type = NONE;
    _si->checkers = attackers_to (square<KING> (_active), ~_active);
    _si->set_check_info (*this);
    _thread = th;

    return *this;
}
// Overload to initialize the position object with the given endgame code string like "KBPKN".
// It is manily an helper to get the material key out of an endgame code.
// Position is not playable, indeed is not even guaranteed to be legal.
Position& Position::setup (const string &code, StateInfo &si, Color c)
{
    assert(0 < code.length () && code.length () <= 8);
    assert(code[0] == 'K');
    assert(code.find ('K', 1) != string::npos);

    string sides[CLR_NO] =
    {
        code.substr (   code.find ('K', 1)), // Weak
        code.substr (0, code.find ('K', 1)), // Strong
    };

    to_lower (sides[c]);

    string fen = sides[0] + char(8 - sides[0].length () + '0') + "/8/8/8/8/8/8/"
               + sides[1] + char(8 - sides[1].length () + '0') + " w - - 0 1";

    setup (fen, si, nullptr, false);
    return *this;
}

#undef do_capture

#define do_capture()                                                        \
    remove_piece (cap);                                                     \
    if (cpt == PAWN)                                                        \
    {                                                                       \
        _si->pawn_key ^= Zob.piece_square_key[pasive][cpt][cap];            \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        _si->non_pawn_matl[pasive] -= PieceValues[MG][cpt];                 \
    }                                                                       \
    _si->matl_key ^= Zob.piece_square_key[pasive][cpt][count (pasive, cpt)];\
    key ^= Zob.piece_square_key[pasive][cpt][cap];                          \
    _si->psq_score -= PSQ[pasive][cpt][cap];

// Do the natural-move
void Position::do_move (Move m, StateInfo &si, bool gives_check)
{
    assert(m != MOVE_NONE);
    assert(&si != _si);

    Key key = _si->posi_key ^ Zob.color_key;
    // Copy some fields of old state info to new state info object except
    // the ones which are going to be recalculated from scratch anyway, 
    std::memcpy (&si, _si, offsetof(StateInfo, posi_key));
    // Point state pointer to point to the new state.
    si.ptr = _si;
    _si    = &si;

    auto pasive = ~_active;

    auto org = org_sq (m);
    auto dst = dst_sq (m);

    assert((pieces (_active) & org) != 0);
    assert((pieces (_active) & dst) == 0
        || mtype (m) == CASTLE);

    auto mpt = ptype (_board[org]);
    assert(mpt != NONE);

    auto cap = dst;
    auto cpt = NONE;

    auto ppt = mpt;
    // Do move according to move type
    switch (mtype (m))
    {
    case NORMAL:
    {
        cpt = ptype (_board[cap]);
        assert(promote (m) - NIHT == PAWN
            && cpt != KING);

        if (cpt != NONE)
        {
            do_capture ();
            _si->clock_ply = 0;
        }
        else
        {
            if (mpt == PAWN)
            {
                _si->clock_ply = 0;
            }
            else
            {
                _si->clock_ply++;
            }
        }

        move_piece (org, dst);

        if (mpt == PAWN)
        {
            _si->pawn_key ^=
                 Zob.piece_square_key[_active][mpt][dst]
                ^Zob.piece_square_key[_active][mpt][org];
        }
    }
        break;

    case CASTLE:
    {
        assert(mpt == KING
            && _board[org] == (_active|KING)
            && _board[dst] == (_active|ROOK));

        Square rook_org, rook_dst;
        do_castling<true> (org, dst, rook_org, rook_dst);

        key ^=
             Zob.piece_square_key[_active][ROOK][rook_dst]
            ^Zob.piece_square_key[_active][ROOK][rook_org];

        _si->psq_score +=
             PSQ[_active][ROOK][rook_dst]
            -PSQ[_active][ROOK][rook_org];

        _si->clock_ply++;
    }
        break;

    case ENPASSANT:
    {
        assert(mpt == PAWN
            && dst == _si->en_passant_sq
            && empty (dst)
            && rel_rank (_active, org) == R_5
            && rel_rank (_active, dst) == R_6);

        cap -= pawn_push (_active);
        cpt = PAWN;
        assert(!empty (cap)
            && _board[cap] == (pasive|cpt));

        do_capture ();
        _board[cap] = NO_PIECE; // Not done by remove_piece()

        assert(_si->clock_ply == 0); // As pawn is the last piece moved

        move_piece (org, dst);

        _si->pawn_key ^=
             Zob.piece_square_key[_active][mpt][dst]
            ^Zob.piece_square_key[_active][mpt][org];
    }
        break;

    case PROMOTE:
    {
        cpt = ptype (_board[cap]);
        assert(mpt == PAWN
            && rel_rank (_active, org) == R_7
            && rel_rank (_active, dst) == R_8
            && cpt != PAWN && cpt != KING);

        if (cpt != NONE)
        {
            do_capture ();
        }
        _si->clock_ply = 0;

        ppt = promote (m);
        assert(NIHT <= ppt && ppt <= QUEN);
        // Replace the pawn with the Promoted piece
        remove_piece (org);
        _board[org] = NO_PIECE; // Not done by remove_piece()
        place_piece (dst, _active, ppt);

        _si->matl_key ^=
             Zob.piece_square_key[_active][mpt][count (_active, mpt)]
            ^Zob.piece_square_key[_active][ppt][count (_active, ppt) - 1];

        _si->pawn_key ^=
             Zob.piece_square_key[_active][mpt][org];

        _si->non_pawn_matl[_active] += PieceValues[MG][ppt];
    }
        break;
    }

    key ^=
         Zob.piece_square_key[_active][ppt][dst]
        ^Zob.piece_square_key[_active][mpt][org];

    _si->psq_score +=
         PSQ[_active][ppt][dst]
        -PSQ[_active][mpt][org];

    // Update castling rights if needed
    if (   _si->castle_rights != CR_NONE
        && (_castle_mask[org] | _castle_mask[dst]) != CR_NONE)
    {
        i32 cr = _castle_mask[org]
               | _castle_mask[dst];
        Bitboard b = _si->castle_rights & cr;
        _si->castle_rights &= ~cr;
        while (b != 0)
        {
            key ^= (*Zob.castle_right_key)[pop_lsq (b)];
        }
    }

    assert(attackers_to (square<KING> (_active), pasive) == 0);

    // Calculate checkers bitboard (if move is check)
    _si->checkers = gives_check ? attackers_to (square<KING> (pasive), _active) : 0;

    // Switch sides
    _active = pasive;

    // Reset en-passant square
    if (_si->en_passant_sq != SQ_NO)
    {
        key ^= Zob.en_passant_key[_file (_si->en_passant_sq)];
        _si->en_passant_sq = SQ_NO;
    }
    // If the moving piece is a pawn check for en-passant square
    if (mpt == PAWN)
    {
        // Set en-passant square if the moved pawn can be captured
        if ((u08(dst) ^ u08(org)) == DEL_NN)
        {
            auto ep_sq = org + (dst - org) / 2;
            if (can_en_passant (ep_sq))
            {
                _si->en_passant_sq = ep_sq;
                key ^= Zob.en_passant_key[_file (ep_sq)];
            }
        }
    }
    // Update state information
    _si->posi_key     = key;
    _si->last_move    = m;
    _si->capture_type = cpt;
    ++_si->null_ply;
    // Update king attacks used for fast check detection
    _si->set_check_info (*this);
    ++_ply;
    ++_nodes;

    assert(ok ());
}
#undef do_capture
// Undo the last natural-move
void Position::undo_move ()
{
    assert(_si->ptr != nullptr);
    auto m = _si->last_move;
    assert(m != MOVE_NONE);

    auto org = org_sq (m);
    auto dst = dst_sq (m);

    _active = ~_active;

    assert(empty (org)
        || mtype (m) == CASTLE);
    assert(_si->capture_type != KING);

    auto cap = dst;

    // Undo move according to move type
    switch (mtype (m))
    {
    case NORMAL:
    {
        move_piece (dst, org);
    }
        break;

    case CASTLE:
    {
        assert(_si->capture_type == NONE);

        Square rook_org, rook_dst;
        do_castling<false> (org, dst, rook_org, rook_dst);
    }
        break;

    case ENPASSANT:
    {
        cap -= pawn_push (_active);
        assert(_board[dst] == (_active|PAWN)
            && rel_rank (_active, org) == R_5
            && rel_rank (_active, dst) == R_6
            && _si->ptr->en_passant_sq == dst
            && _si->capture_type == PAWN
            && empty (cap));

        move_piece (dst, org);
    }
        break;

    case PROMOTE:
    {
        assert(promote (m) == ptype (_board[dst])
            && rel_rank (_active, org) == R_7
            && rel_rank (_active, dst) == R_8
            && (NIHT <= promote (m) && promote (m) <= QUEN));

        remove_piece (dst);
        _board[dst] = NO_PIECE; // Not done by remove_piece()
        place_piece (org, _active, PAWN);
    }
        break;
    }
    // Restore the captured piece
    if (_si->capture_type != NONE)
    {
        place_piece (cap, ~_active, _si->capture_type);
    }

    // Point state pointer back to the previous state
    _si = _si->ptr;
    --_ply;

    assert(ok ());
}
// Do the null-move
void Position::do_null_move (StateInfo &si)
{
    assert(&si != _si);
    assert(_si->checkers == 0);

    // Full copy here
    std::memcpy (&si, _si, sizeof (StateInfo));
    // Point state pointer to point to the new state.
    si.ptr = _si;
    _si    = &si;

    if (_si->en_passant_sq != SQ_NO)
    {
        _si->posi_key ^= Zob.en_passant_key[_file (_si->en_passant_sq)];
        _si->en_passant_sq = SQ_NO;
    }
    _active = ~_active;
    _si->posi_key ^= Zob.color_key;
    _si->clock_ply++;
    _si->null_ply = 0;
    _si->set_check_info (*this);

    assert(ok ());
}
// Undo the last null-move
void Position::undo_null_move ()
{
    assert(_si->ptr != nullptr);
    assert(_si->checkers == 0);

    _active = ~_active;
    _si    = _si->ptr;

    assert(ok ());
}
// Flips position with the white and black sides reversed.
// This is only useful for debugging especially for finding evaluation symmetry bugs.
void Position::flip ()
{
    istringstream iss (fen (true));
    string ff, token;
    // 1. Piece placement
    for (auto r = R_8; r >= R_1; --r)
    {
        std::getline (iss, token, r > R_1 ? '/' : ' ');
        toggle (token);
        ff.insert (0, token + (!white_spaces (ff) ? "/" : " "));
    }
    // 2. Active color
    iss >> token;
    ff += (token[0] == 'w' ? 'b' :
           token[0] == 'b' ? 'w' : '-');
    ff += ' ';
    // 3. Castling availability
    iss >> token;
    toggle (token);
    ff += token + ' ';
    // 4. En-passant square
    iss >> token;
    ff += (token[0] == '-' ? token : token.replace (1, 1, token[1] == '3' ? "6" :
                                                          token[1] == '6' ? "3" : "-"));
    // 5-6. Half and full moves
    std::getline (iss, token);
    ff += token;

    setup (ff, *_si, _thread, true);

    assert(ok ());
}
// Returns the fen of position
string Position::fen (bool full) const
{
    ostringstream oss;

    for (auto r = R_8; r >= R_1; --r)
    {
        for (auto f = F_A; f <= F_H; ++f)
        {
            i16 empty_count;
            for (empty_count = 0;
                    f <= F_H
                 && empty (f|r); ++f)
            {
                ++empty_count;
            }
            if (empty_count != 0)
            {
                oss << empty_count;
            }
            if (f <= F_H)
            {
                oss << _board[f|r];
            }
        }
        if (r > R_1)
        {
            oss << '/';
        }
    }

    oss << ' ' << _active << ' ';

    if (can_castle (CR_ANY) != CR_NONE)
    {
        if (can_castle (CR_WHITE) != CR_NONE)
        {
            if (can_castle (CR_WKING) != CR_NONE)
            {
                oss << (Chess960 ? to_char (_file (castle_rook (Castling<WHITE, CS_KING>::Right)), false) : 'K');
            }
            if (can_castle (CR_WQUEN) != CR_NONE)
            {
                oss << (Chess960 ? to_char (_file (castle_rook (Castling<WHITE, CS_QUEN>::Right)), false) : 'Q');
            }
        }
        if (can_castle (CR_BLACK) != CR_NONE)
        {
            if (can_castle (CR_BKING) != CR_NONE)
            {
                oss << (Chess960 ? to_char (_file (castle_rook (Castling<BLACK, CS_KING>::Right)),  true) : 'k');
            }
            if (can_castle (CR_BQUEN) != CR_NONE)
            {
                oss << (Chess960 ? to_char (_file (castle_rook (Castling<BLACK, CS_QUEN>::Right)),  true) : 'q');
            }
        }
    }
    else
    {
        oss << '-';
    }

    oss << ' ' << (_si->en_passant_sq != SQ_NO ? to_string (_si->en_passant_sq) : "-");

    if (full)
    {
        oss << ' ' << i16(_si->clock_ply) << ' ' << move_num ();
    }

    return oss.str ();
}
// Returns an ASCII representation of the position to be
// printed to the standard output
Position::operator string () const
{
    ostringstream oss;
    oss << " +---+---+---+---+---+---+---+---+\n";
    for (auto r = R_8; r >= R_1; --r)
    {
        oss << to_char (r) << "| ";
        for (auto f = F_A; f <= F_H; ++f)
        {
            oss << _board[f|r] << " | ";
        }
        oss << '\n' << " +---+---+---+---+---+---+---+---+\n";
    }
    for (auto f = F_A; f <= F_H; ++f)
    {
        oss << "   " << Notation::to_char (f, false);
    }

    oss << '\n'
        << "FEN: " << fen (true) << '\n'
        << "Key: " << std::setfill ('0') << std::hex << std::uppercase << std::setw (16)
        << _si->posi_key << std::nouppercase << std::dec << std::setfill (' ') << '\n';
    oss << "Checkers: ";
    for (Bitboard b = _si->checkers; b != 0; )
    {
        oss << pop_lsq (b) << ' ';
    }
    oss << '\n';
    return oss.str ();
}

#if !defined(NDEBUG)
// Performs some consistency checks for the position, helpful for debugging.
bool Position::ok (i08 *failed_step) const
{
    static const bool Fast = true;

    enum Step : u08
    {
        BASIC,
        PIECE,
        BITBOARD,
        STATE,
        LIST,
        CASTLING
    };

    for (i08 step = BASIC; step <= (Fast ? BASIC : CASTLING); ++step)
    {
        if (failed_step != nullptr)
        {
            *failed_step = step;
        }

        if (step == BASIC)
        {
            if (   (   _active != WHITE
                    && _active != BLACK)
                || (count<KING> (WHITE) != 1 || _board[square<KING> (WHITE)] != W_KING)
                || (count<KING> (BLACK) != 1 || _board[square<KING> (BLACK)] != B_KING)
                || (count<NONE> () > 32 || count<NONE> () != pop_count (pieces ()))
                || (   _si->en_passant_sq != SQ_NO
                    && (   rel_rank (_active, _si->en_passant_sq) != R_6
                        || !can_en_passant (_si->en_passant_sq)))
                || _si->clock_ply > DrawClockPly)
            {
                return false;
            }
        }
        if (step == PIECE)
        {
            if (   std::count (_board, _board + SQ_NO, W_KING) != 1
                || std::count (_board, _board + SQ_NO, B_KING) != 1
                || attackers_to (square<KING> (~_active), _active) != 0
                || pop_count (_si->checkers) > 2)
            {
                return false;
            }
        }
        if (step == BITBOARD)
        {
            if (   (pieces (WHITE) & pieces (BLACK)) != 0
                || (pieces (WHITE) | pieces (BLACK)) != pieces ()
                || (pieces (WHITE) ^ pieces (BLACK)) != pieces ())
            {
                return false;
            }

            for (auto pt1 = PAWN; pt1 <= KING; ++pt1)
            {
                for (auto pt2 = PAWN; pt2 <= KING; ++pt2)
                {
                    if (   pt1 != pt2
                        && (pieces (pt1) & pieces (pt2)) != 0)
                    {
                        return false;
                    }
                }
            }
            if (   (pieces (PAWN)|pieces (NIHT)|pieces (BSHP)|pieces (ROOK)|pieces (QUEN)|pieces (KING))    // pieces ()
                != (pieces (PAWN)^pieces (NIHT)^pieces (BSHP)^pieces (ROOK)^pieces (QUEN)^pieces (KING)))
            {
                return false;
            }

            if (((R1_bb|R8_bb) & pieces (PAWN)) != 0)
            {
                return false;
            }

            for (auto c = WHITE; c <= BLACK; ++c)
            {
                // Too many Piece of color
                if (   count<NONE> (c) > 16
                    || count<NONE> (c) != pop_count (pieces (c)))
                {
                    return false;
                }
                // check if the number of Pawns plus the number of
                // extra Queens, Rooks, Bishops, Knights exceeds 8
                // (which can result only by promotion)
                if (           (count (c, PAWN)
                    + std::max (count (c, NIHT)-2, 0)
                    + std::max (count (c, BSHP)-2, 0)
                    + std::max (count (c, ROOK)-2, 0)
                    + std::max (count (c, QUEN)-1, 0)) > 8)
                {
                    return false; // Too many Promoted Piece of color
                }

                if (           (count (c, PAWN)
                    + std::max (pop_count (pieces (c, BSHP) & Color_bb[WHITE])-1, 0)
                    + std::max (pop_count (pieces (c, BSHP) & Color_bb[BLACK])-1, 0)) > 8)
                {
                    return false; // Too many Promoted Bishop of color
                }

                // There should be one and only one KING of color
                if (   pieces (c, KING) == 0
                    || more_than_one (pieces (c, KING)))
                {
                    return false;
                }
            }
        }
        if (step == STATE)
        {
            if (   _si->matl_key != Zob.compute_matl_key (*this)
                || _si->pawn_key != Zob.compute_pawn_key (*this)
                || _si->posi_key != Zob.compute_posi_key (*this)
                || _si->psq_score != compute_psq_score (*this)
                || _si->non_pawn_matl[WHITE] != compute_non_pawn_material (WHITE)
                || _si->non_pawn_matl[BLACK] != compute_non_pawn_material (BLACK))
            {
                return false;
            }
        }
        if (step == LIST)
        {
            for (auto c = WHITE; c <= BLACK; ++c)
            {
                for (auto pt = PAWN; pt <= KING; ++pt)
                {
                    if (count (c, pt) != pop_count (pieces (c, pt)))
                    {
                        return false;
                    }

                    for (auto i = 0; i < count (c, pt); ++i)
                    {
                        if (   !_ok  (_piece_sq[c][pt][i])
                            || _board[_piece_sq[c][pt][i]] != (c|pt))
                        {
                            return false;
                        }
                    }
                }
            }
        }
        if (step == CASTLING)
        {
            for (auto c = WHITE; c <= BLACK; ++c)
            {
                for (auto cs = CS_KING; cs <= CS_QUEN; ++cs)
                {
                    auto cr = mk_castle_right (c, cs);
                    if (   can_castle (cr) != CR_NONE
                        && (   _board[castle_rook (cr)] != (c|ROOK)
                            || _castle_mask[castle_rook (cr)] != cr
                            || (_castle_mask[square<KING> (c)] & cr) != cr))
                    {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}
#endif
