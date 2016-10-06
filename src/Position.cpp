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
    if (   si->clock_ply >= DrawClockPly
        && (   si->checkers == 0
            || MoveList<LEGAL> (*this).size () != 0))
    {
        return true;
    }
    // Draw by Repetition?
    const auto *psi = si;
    for (auto p = std::min (psi->clock_ply, psi->null_ply);
              p >= 2;
              p -= 2)
    {
        psi = psi->ptr->ptr;
        // Check first repetition
        if (psi->posi_key == si->posi_key)
        {
            return true;
        }
    }
    return false;
}
// Check whether there has been at least one repetition of position since the last capture or pawn move.
bool Position::repeated () const
{
    for (const auto *bsi = si;
            bsi != nullptr;
            bsi = bsi->ptr)
    {
        const auto *psi = bsi;
        for (auto p = std::min (psi->clock_ply, psi->null_ply);
                  p >= 2;
                  p -= 2)
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
template<PieceType PT> PieceType Position::pick_least_val_att (Square dst, Bitboard c_attackers, Bitboard &mocc, Bitboard &attackers) const
{
    Bitboard b = c_attackers & pieces (PT);
    if (b != 0)
    {
        mocc ^= b & ~(b - 1);

        if (   PT == PAWN
            || PT == BSHP)
        {
            attackers |= pieces (BSHP, QUEN) & attacks_bb<BSHP> (dst, mocc);
        }
        else
        if (PT == ROOK)
        {
            attackers |= pieces (ROOK, QUEN) & attacks_bb<ROOK> (dst, mocc);
        }
        else
        if (PT == QUEN)
        {
            attackers |= (pieces (BSHP, QUEN) & attacks_bb<BSHP> (dst, mocc))
                      |  (pieces (ROOK, QUEN) & attacks_bb<ROOK> (dst, mocc));
        }
        // Remove already processed pieces in X-ray
        attackers &= mocc;
        return PT;
    }

    return pick_least_val_att<PieceType(PT+1)> (dst, c_attackers, mocc, attackers);
}
template<> PieceType Position::pick_least_val_att<KING> (Square, Bitboard, Bitboard&, Bitboard&) const
{
    return KING; // No need to update bitboards, it is the last cycle
}

// Static Exchange Evaluator (SEE): It tries to estimate the material gain or loss resulting from a move.
Value Position::see (Move m) const
{
    assert(_ok (m));
    if (mtype (m) == CASTLE)
    {
        // Castle moves are implemented as king capturing the rook so cannot be
        // handled correctly. Simply return 0 that is always the correct value
        // unless in the rare case the rook ends up under attack.
        return VALUE_ZERO;
    }

    auto org = org_sq (m);
    auto dst = dst_sq (m);
    assert(!empty (org));

    vector<Value> gain_values;
    gain_values.reserve (8);

    Bitboard mocc;

    if (mtype (m) == ENPASSANT)
    {
        assert(board[org] == (active|PAWN));
        mocc = pieces () - org - (dst - pawn_push (active));
        gain_values.push_back (PieceValues[MG][PAWN]);
    }
    else
    {
        assert(board[org] != NO_PIECE);
        mocc = pieces () - org;
        gain_values.push_back (PieceValues[MG][ptype (board[dst])]);
    }

    // Find all attackers to the destination square, with the moving piece
    // removed, but possibly an X-ray attacker added behind it.
    Bitboard attackers = attackers_to (dst, mocc) & mocc;
    // For the case when captured piece is a pinner
    mocc -= dst;
    // The first victim
    auto victim = ptype (board[org]);
    auto c = ~color (board[org]);
    Bitboard c_attackers = attackers & pieces (c);
    // Don't allow pinned pieces to attack pieces except the king as long all pinners are on their original square.
    if (   c_attackers != 0
        && (si->pinners[c] & ~mocc) == 0)
    {
        assert(victim != KING);
        c_attackers &= ~abs_pinneds (c);
    }

    // The destination square is defended, which makes things rather more difficult to compute.
    // Proceed by building up a "gain list" containing the material gain or loss at each stop in
    // a sequence of captures to the destination square, where the sides alternately capture,
    // and always capture with the least valuable piece.
    // After each capture, look for new X-ray attacks from behind the capturing piece.
    while (c_attackers != 0)
    {
        assert(gain_values.size () < 32);

        // Add the new entry to the swap list
        gain_values.push_back (PieceValues[MG][victim] - gain_values[gain_values.size () - 1]);
        // Locate and remove the next least valuable attacker
        victim = pick_least_val_att<PAWN> (dst, c_attackers, mocc, attackers);
        c = ~c;
        c_attackers = attackers & pieces (c);

        if (victim != KING)
        {
            // for resolving Bxf2 on fen: r2qk2r/pppb1ppp/2np4/1Bb5/4n3/5N2/PPP2PPP/RNBQR1K1 b kq - 1 1
            if (   c_attackers != 0
                && (si->pinners[c] & ~mocc) == 0)
            {
                c_attackers &= ~abs_pinneds (c);
            }
        }
        else
        {
            if (c_attackers != 0)
            {
                // Stop before a king capture
                gain_values.pop_back ();
                break;
            }
        }
    }
    // Having built the gain list, negamax through it to find the best
    // achievable score from the point of view of the side to move.
    auto depth = i08(gain_values.size () - 1);
    while (depth != 0)
    {
        // Find minimum gain
        if (gain_values[depth - 1] > -gain_values[depth])
        {
            gain_values[depth - 1] = -gain_values[depth];
        }
        --depth;
    }

    return gain_values[0];
}
// Sign of SSE
Value Position::see_sign (Move m) const
{
    assert(_ok (m));
    // If SEE cannot be negative because captured piece value is not less then capturing one.
    // Note that king moves always return here because king value is set to VALUE_ZERO.
    return PieceValues[MG][ptype (board[org_sq (m)])]
        <= PieceValues[MG][ptype (board[dst_sq (m)])] ?
            VALUE_KNOWN_WIN :
            see (m);
}
// Returns a bitboard of all the pieces that are blocking attacks on the square 's' from sliders in 'attackers'.
// A piece blocks a slider if removing that piece from the board would result in a position where square 's' is attacked by the sliders in 'attackers'.
// For example, a king-attack blocking piece can be either a pinned or a discovered check piece,
// according if its color is the opposite or the same of the color of the slider.
Bitboard Position::slider_blockers (Square s, Bitboard defenders, Bitboard attackers, Bitboard &pinners) const
{
    Bitboard blockers = pinners = 0;
    // Snipers are attackers that are aligned on 's' in x-ray
    Bitboard snipers =
          attackers
        & (  (pieces (BSHP, QUEN) & PieceAttacks[BSHP][s])
           | (pieces (ROOK, QUEN) & PieceAttacks[ROOK][s]));
    while (snipers != 0)
    {
        auto sniper_sq = pop_lsq (snipers);
        Bitboard b = pieces () & between_bb (s, sniper_sq);
        if (   b != 0
            && !more_than_one (b))
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
    assert(_ok (m));
    auto org = org_sq (m);
    auto dst = dst_sq (m);
    // If the org square is not occupied by a piece belonging to the side to move,
    // then the move is obviously not legal.
    if ((pieces (active) & org) == 0)
    {
        return false;
    }
    if (   ((pieces (active)|pieces (KING)) & dst) != 0
        && mtype (m) != CASTLE)
    {
        return false;
    }

    auto mpt = ptype (board[org]);
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
              && rel_rank (active, org) == R_1
              && rel_rank (active, dst) == R_1
              && board[dst] == (active|ROOK)
              && si->checkers == 0
              && (si->castle_rights & castle_right (active, dst > org ? CS_KING : CS_QUEN)) != CR_NONE
              && !impeded_castle (castle_right (active, dst > org ? CS_KING : CS_QUEN))))
        {
            return false;
        }
        // Castle is always encoded as "King captures friendly Rook"
        assert(dst == castle_rook[castle_right (active, dst > org ? CS_KING : CS_QUEN)]);
        Bitboard b = king_path[castle_right (active, dst > org ? CS_KING : CS_QUEN)];
        // Check king's path for attackers
        while (b != 0)
        {
            if (attackers_to (pop_lsq (b), ~active) != 0)
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
              && si->en_passant_sq == dst
              && rel_rank (active, org) == R_5
              && rel_rank (active, dst) == R_6))
        {
            return false;
        }
        cap -= pawn_push (active);
        if (board[cap] != (~active|PAWN))
        {
            return false;
        }
    }
        break;

    case PROMOTE:
    {
        if (!(   mpt == PAWN
              && rel_rank (active, org) == R_7
              && rel_rank (active, dst) == R_8
              && (NIHT <= promote (m) && promote (m) <= QUEN)))
        {
            return false;
        }
    }
        break;
    }
    // The captured square cannot be occupied by a friendly piece or kings
    if (((pieces (active)|pieces (KING)) & cap) != 0)
    {
        return false;
    }

    // Handle the special case of a piece move
    if (mpt == PAWN)
    {
        // In case of non-promotional moves origin & destination cannot be on the 7th/2nd & 8th/1st rank.
        if (   mtype (m) != PROMOTE
            && (   rel_rank (active, org) == R_7
                || rel_rank (active, dst) == R_8))
        {
            return false;
        }
        if (    // Not a single push
               !(   (   mtype (m) == NORMAL
                     || mtype (m) == PROMOTE)
                 && empty (dst)
                 && (org + pawn_push (active) == dst))
                // Not a normal capture
            && !(   (   mtype (m) == NORMAL
                     || mtype (m) == PROMOTE)
                 && ((pieces (~active) & PawnAttacks[active][org]) & dst) != 0)
                // Not an enpassant capture
            && !(   mtype (m) == ENPASSANT
                 && si->en_passant_sq == dst
                 && ((~pieces () & PawnAttacks[active][org]) & dst) != 0
                 && board[cap] == (~active|PAWN))
                // Not a double push
            && !(   mtype (m) == NORMAL
                 && rel_rank (active, org) == R_2
                 && rel_rank (active, dst) == R_4
                 && empty (dst)
                 && empty (dst - pawn_push (active))
                 && (org + pawn_push (active)*2 == dst)))
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
    if (si->checkers != 0)
    {
        // In case of king moves under check, remove king so to catch
        // as invalid moves like B1A1 when opposite queen is on C1.
        if (mpt == KING)
        {
            // Remove 'org' but not place 'dst'
            return attackers_to (dst, ~active, pieces () - org) == 0;
        }
        // Double check? In this case a king move is required
        if (!more_than_one (si->checkers))
        {
            return en_passant (m) ?
                // Move must be a capture of the checking en-passant pawn or a blocking evasion of the checking piece
                (si->checkers & cap) != 0 || (between_bb (scan_lsq (si->checkers), square (active, KING)) & dst) != 0 :
                // Move must be a capture of the checking piece or a blocking evasion of the checking piece
                ((si->checkers | between_bb (scan_lsq (si->checkers), square (active, KING))) & dst) != 0;
        }
        return false;
    }
    return true;
}
// Tests whether a pseudo-legal move is legal
bool Position::legal (Move m) const
{
    assert(_ok (m));
    auto org = org_sq (m);
    auto dst = dst_sq (m);
    assert((pieces (active) & org) != 0);

    auto mpt = ptype (board[org]);

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
            return attackers_to (dst, ~active, pieces () - org) == 0;
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
        return (abs_pinneds (active) & org) == 0
            || sqrs_aligned (org, dst, square (active, KING));
    }
        break;

    case CASTLE:
    {
        // Castling moves are checked for legality during move generation.
        return mpt == KING
            && board[dst] == (active|ROOK);
    }
        break;

    case ENPASSANT:
    {
        // En-passant captures are a tricky special case. Because they are rather uncommon,
        // do it simply by testing whether the king is attacked after the move is made.
        auto cap = dst - pawn_push (active);

        assert(mpt == PAWN
            && rel_rank (active, org) == R_5
            && rel_rank (active, dst) == R_6
            && empty (dst)
            && dst == si->en_passant_sq
            && board[cap] == (~active|PAWN));

        Bitboard mocc = pieces () - org - cap + dst;
        // If any attacker then in check and not legal move
        return (   (pieces (~active, BSHP, QUEN) & PieceAttacks[BSHP][square (active, KING)]) == 0
                || (pieces (~active, BSHP, QUEN) & attacks_bb<BSHP> (square (active, KING), mocc)) == 0)
            && (   (pieces (~active, ROOK, QUEN) & PieceAttacks[ROOK][square (active, KING)]) == 0
                || (pieces (~active, ROOK, QUEN) & attacks_bb<ROOK> (square (active, KING), mocc)) == 0);
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
    assert((pieces (active) & org) != 0);
    
    if (    // Direct check ?
           (si->checks[ptype (board[org])] & dst) != 0
            // Discovered check ?
        || (   (si->check_blockers[~active] & org) != 0
            && !sqrs_aligned (org, dst, square (~active, KING))))
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
        dst           = rel_sq (active, dst > org ? SQ_G1 : SQ_C1);
        auto rook_dst = rel_sq (active, dst > org ? SQ_F1 : SQ_D1);
        // First x-ray check then full check
        return (PieceAttacks[ROOK][rook_dst] & square (~active, KING)) != 0
            && (attacks_bb<ROOK> (rook_dst, pieces () - org - rook_org + dst + rook_dst) & square (~active, KING)) != 0;
    }
        break;

    case ENPASSANT:
    {
        // En-passant capture with check ?
        // already handled the case of direct checks and ordinary discovered check,
        // the only case need to handle is the unusual case of a discovered check through the captured pawn.
        return (   (pieces (active, BSHP, QUEN) & PieceAttacks[BSHP][square (~active, KING)]) != 0
                && (pieces (active, BSHP, QUEN) & attacks_bb<BSHP> (square (~active, KING), pieces () - org - (_file (dst)|_rank (org)) + dst)) != 0)
            || (   (pieces (active, ROOK, QUEN) & PieceAttacks[ROOK][square (~active, KING)]) != 0
                && (pieces (active, ROOK, QUEN) & attacks_bb<ROOK> (square (~active, KING), pieces () - org - (_file (dst)|_rank (org)) + dst)) != 0);
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
        return (attacks & square (~active, KING)) != 0;
    }
        break;
    }
    return false;
}

// Clear the position
void Position::clear ()
{
    for (auto s = SQ_A1; s <= SQ_H8; ++s)
    {
        board[s] = NO_PIECE;
        castle_mask[s] = CR_NONE;
    }
    for (auto c = WHITE; c <= BLACK; ++c)
    {
        color_bb[c] = 0;
    }
    for (auto pt = PAWN; pt <= NONE; ++pt)
    {
        types_bb[pt] = 0;
    }
    for (auto c = WHITE; c <= BLACK; ++c)
    {
        for (auto pt = PAWN; pt <= KING; ++pt)
        {
            squares[c][pt].clear ();
        }
    }
    for (auto r = 0; r < CR_NO; ++r)
    {
        castle_rook[r] = SQ_NO;
        castle_path[r] = 0;
        king_path[r]   = 0;
    }

    active = CLR_NO;
    ply    = 0;
    nodes  = 0;
    thread = nullptr;
}

// Set the castling for the particular color & rook
void Position::set_castle (Color c, Square rook_org)
{
    auto king_org = square (c, KING);
    assert(king_org != rook_org);

    auto cr = castle_right (c, rook_org > king_org ? CS_KING : CS_QUEN);
    auto king_dst = rel_sq (c, rook_org > king_org ? SQ_G1 : SQ_C1);
    auto rook_dst = rel_sq (c, rook_org > king_org ? SQ_F1 : SQ_D1);

    si->castle_rights     |= cr;
    castle_mask[king_org] |= cr;
    castle_mask[rook_org] |= cr;
    castle_rook[cr] = rook_org;

    for (auto s = std::min (king_org, king_dst); s <= std::max (king_org, king_dst); ++s)
    {
        if (   king_org != s
            && rook_org != s)
        {
            castle_path[cr] += s;
            king_path[cr] += s;
        }
    }
    for (auto s = std::min (rook_org, rook_dst); s <= std::max (rook_org, rook_dst); ++s)
    {
        if (   king_org != s
            && rook_org != s)
        {
            castle_path[cr] += s;
        }
    }
}
// Tests the en-passant square
bool Position::can_en_passant (Square ep_sq) const
{
    assert(ep_sq != SQ_NO);
    assert(rel_rank (active, ep_sq) == R_6);

    auto cap = ep_sq - pawn_push (active);
    if ((pieces (~active, PAWN) & cap) == 0)
    {
        return false;
    }
    Bitboard mocc = pieces () + ep_sq - cap;
    // En-passant attackers
    Bitboard attackers = pieces (active, PAWN) & PawnAttacks[~active][ep_sq];
    assert(pop_count (attackers) <= 2);
    while (attackers != 0)
    {
        auto org = pop_lsq (attackers);
        // Check en-passant is legal for the position
        if (   (   (pieces (~active, BSHP, QUEN) & PieceAttacks[BSHP][square (active, KING)]) == 0
                || (pieces (~active, BSHP, QUEN) & attacks_bb<BSHP> (square (active, KING), mocc - org)) == 0)
            && (   (pieces (~active, ROOK, QUEN) & PieceAttacks[ROOK][square (active, KING)]) == 0
                || (pieces (~active, ROOK, QUEN) & attacks_bb<ROOK> (square (active, KING), mocc - org)) == 0))
        {
            return true;
        }
    }
    return false;
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
Position& Position::setup (const string &ff, StateInfo &nsi, Thread *const th, bool full)
{
    istringstream iss (ff);
    iss >> std::noskipws;

    std::memset (&nsi, 0x00, sizeof (StateInfo));
    clear ();
    si = &nsi;

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

    assert(square (WHITE, KING) != SQ_NO
        && square (BLACK, KING) != SQ_NO);

    // 2. Active color
    iss >> token;
    active = Color(ColorChar.find (token));

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
              && board[r_sq] != (c|ROOK);
                 --r_sq) {}
        }
        else
        if (token == 'q')
        {
            for (r_sq  = rel_sq (c, SQ_A1);
                 r_sq <= rel_sq (c, SQ_H1)
              && board[r_sq] != (c|ROOK);
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

        assert(board[r_sq] == (c|ROOK));
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
    si->en_passant_sq = ep_sq;

    // 5-6. Clock ply and Game move count
    i16 clk_ply = 0
      , moves  = 1;
    if (full)
    {
        iss >> std::skipws
            >> clk_ply
            >> moves;

        if (si->en_passant_sq != SQ_NO)
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

    // Convert from moves starting from 1 to ply starting from 0,
    ply = i16(2*(moves - 1) + (active == BLACK ? 1 : 0));

    si->matl_key = Zob.compute_matl_key (*this);
    si->pawn_key = Zob.compute_pawn_key (*this);
    si->posi_key = Zob.compute_posi_key (*this);
    si->psq_score = compute_psq (*this);
    si->non_pawn_matl[WHITE] = compute_npm<WHITE> (*this);
    si->non_pawn_matl[BLACK] = compute_npm<BLACK> (*this);
    si->clock_ply = u08(clk_ply);
    si->capture_type = NONE;
    si->checkers = attackers_to (square (active, KING), ~active);
    si->set_check_info (*this);
    thread = th;

    return *this;
}
// Overload to initialize the position object with the given endgame code string like "KBPKN".
// It is manily an helper to get the material key out of an endgame code.
// Position is not playable, indeed is not even guaranteed to be legal.
Position& Position::setup (const string &code, StateInfo &nsi, Color c)
{
    assert(0 < code.length () && code.length () <= 8);
    assert(code[0] == 'K');
    assert(code.find ('K', 1) != string::npos);

    string sides[CLR_NO] =
    {
        code.substr (   code.find ('K', 1)), // Weak
        code.substr (0, code.find ('K', 1))  // Strong
    };

    to_lower (sides[c]);

    string fen = sides[0] + char(8 - sides[0].length () + '0') + "/8/8/8/8/8/8/"
               + sides[1] + char(8 - sides[1].length () + '0') + " w - - 0 1";

    setup (fen, nsi, nullptr, false);
    return *this;
}

#undef do_capture

#define do_capture()                                                        \
    remove_piece (cap);                                                     \
    if (cpt == PAWN)                                                        \
    {                                                                       \
        si->pawn_key ^= Zob.piece_square_keys[pasive][cpt][cap];            \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        si->non_pawn_matl[pasive] -= PieceValues[MG][cpt];                  \
    }                                                                       \
    si->matl_key ^= Zob.piece_square_keys[pasive][cpt][count (pasive, cpt)];\
    key ^= Zob.piece_square_keys[pasive][cpt][cap];                         \
    si->psq_score -= PSQ[pasive][cpt][cap];

// Do the natural-move
void Position::do_move (Move m, StateInfo &nsi, bool is_check)
{
    assert(_ok (m));
    assert(&nsi != si);

    Key key = si->posi_key ^ Zob.color_key;
    // Copy some fields of old state info to new state info object except
    // the ones which are going to be recalculated from scratch anyway, 
    std::memcpy (&nsi, si, offsetof(StateInfo, posi_key));
    // Point state pointer to point to the new state.
    nsi.ptr = si;
    si = &nsi;

    auto pasive = ~active;

    auto org = org_sq (m);
    auto dst = dst_sq (m);

    assert((pieces (active) & org) != 0);
    assert((pieces (active) & dst) == 0
        || mtype (m) == CASTLE);

    auto mpt = ptype (board[org]);
    assert(mpt != NONE);

    auto cap = dst;
    auto cpt = NONE;

    auto ppt = mpt;
    // Do move according to move type
    switch (mtype (m))
    {
    case NORMAL:
    {
        cpt = ptype (board[cap]);
        assert(promote (m) - NIHT == PAWN
            && cpt != KING);

        if (cpt != NONE)
        {
            do_capture();
            si->clock_ply = 0;
        }
        else
        {
            if (mpt == PAWN)
            {
                si->clock_ply = 0;
            }
            else
            {
                ++si->clock_ply;
            }
        }

        move_piece (org, dst);

        if (mpt == PAWN)
        {
            si->pawn_key ^=
                 Zob.piece_square_keys[active][mpt][dst]
                ^Zob.piece_square_keys[active][mpt][org];
        }
    }
        break;

    case CASTLE:
    {
        assert(mpt == KING
            && board[org] == (active|KING)
            && board[dst] == (active|ROOK));

        Square rook_org, rook_dst;
        do_castling<true> (org, dst, rook_org, rook_dst);

        key ^=
             Zob.piece_square_keys[active][ROOK][rook_dst]
            ^Zob.piece_square_keys[active][ROOK][rook_org];

        si->psq_score +=
             PSQ[active][ROOK][rook_dst]
            -PSQ[active][ROOK][rook_org];

        ++si->clock_ply;
    }
        break;

    case ENPASSANT:
    {
        assert(mpt == PAWN
            && dst == si->en_passant_sq
            && empty (dst)
            && rel_rank (active, org) == R_5
            && rel_rank (active, dst) == R_6);

        cap -= pawn_push (active);
        cpt = PAWN;
        assert(!empty (cap)
            && board[cap] == (pasive|cpt));

        do_capture();
        board[cap] = NO_PIECE; // Not done by remove_piece()

        assert(si->clock_ply == 0); // As pawn is the last piece moved

        move_piece (org, dst);

        si->pawn_key ^=
             Zob.piece_square_keys[active][mpt][dst]
            ^Zob.piece_square_keys[active][mpt][org];
    }
        break;

    case PROMOTE:
    {
        cpt = ptype (board[cap]);
        assert(mpt == PAWN
            && rel_rank (active, org) == R_7
            && rel_rank (active, dst) == R_8
            && cpt != PAWN && cpt != KING);

        if (cpt != NONE)
        {
            do_capture();
        }
        si->clock_ply = 0;

        ppt = promote (m);
        assert(NIHT <= ppt && ppt <= QUEN);
        // Replace the pawn with the promoted piece
        remove_piece (org);
        board[org] = NO_PIECE; // Not done by remove_piece()
        place_piece (dst, active, ppt);

        si->matl_key ^=
             Zob.piece_square_keys[active][mpt][count (active, mpt)]
            ^Zob.piece_square_keys[active][ppt][count (active, ppt) - 1];

        si->pawn_key ^=
             Zob.piece_square_keys[active][mpt][org];

        si->non_pawn_matl[active] += PieceValues[MG][ppt];
    }
        break;
    }

    key ^=
         Zob.piece_square_keys[active][ppt][dst]
        ^Zob.piece_square_keys[active][mpt][org];

    si->psq_score +=
         PSQ[active][ppt][dst]
        -PSQ[active][mpt][org];

    i32 cr;
    // Update castling rights if needed
    if (   si->castle_rights != CR_NONE
        && (cr = (  castle_mask[org]
                  | castle_mask[dst])) != 0)
    {
        Bitboard b = si->castle_rights & cr;
        while (b != 0)
        {
            key ^= (*Zob.castle_right_keys)[pop_lsq (b)];
        }
        si->castle_rights &= ~cr;
    }

    assert(attackers_to (square (active, KING), pasive) == 0);

    // Calculate checkers bitboard (if move is check)
    si->checkers = is_check ? attackers_to (square (pasive, KING), active) : 0;

    // Switch sides
    active = pasive;

    // Reset en-passant square
    if (si->en_passant_sq != SQ_NO)
    {
        key ^= Zob.en_passant_keys[_file (si->en_passant_sq)];
        si->en_passant_sq = SQ_NO;
    }
    // If the moving piece is a pawn check for en-passant square
    if (mpt == PAWN)
    {
        // Set en-passant square if the moved pawn can be captured
        if ((u08(dst) ^ u08(org)) == 16)
        {
            auto ep_sq = org + (dst - org) / 2;
            if (can_en_passant (ep_sq))
            {
                si->en_passant_sq = ep_sq;
                key ^= Zob.en_passant_keys[_file (ep_sq)];
            }
        }
    }
    // Update state information
    si->posi_key     = key;
    si->capture_type = cpt;
    ++si->null_ply;
    // Set check info used for fast check detection
    si->set_check_info (*this);
    ++ply;
    ++nodes;

    assert(ok ());
}
#undef do_capture
// Undo the last natural-move
void Position::undo_move (Move m)
{
    assert(si->ptr != nullptr);
    assert(_ok (m));
    auto org = org_sq (m);
    auto dst = dst_sq (m);

    active = ~active;

    assert(empty (org)
        || mtype (m) == CASTLE);
    assert(si->capture_type != KING);

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
        assert(si->capture_type == NONE);

        Square rook_org, rook_dst;
        do_castling<false> (org, dst, rook_org, rook_dst);
    }
        break;

    case ENPASSANT:
    {
        cap -= pawn_push (active);
        assert(board[dst] == (active|PAWN)
            && rel_rank (active, org) == R_5
            && rel_rank (active, dst) == R_6
            && si->ptr->en_passant_sq == dst
            && si->capture_type == PAWN
            && empty (cap));

        move_piece (dst, org);
    }
        break;

    case PROMOTE:
    {
        assert(promote (m) == ptype (board[dst])
            && rel_rank (active, org) == R_7
            && rel_rank (active, dst) == R_8
            && (NIHT <= promote (m) && promote (m) <= QUEN));

        remove_piece (dst);
        board[dst] = NO_PIECE; // Not done by remove_piece()
        place_piece (org, active, PAWN);
    }
        break;
    }
    // Restore the captured piece
    if (si->capture_type != NONE)
    {
        assert(empty (cap));
        place_piece (cap, ~active, si->capture_type);
    }

    // Point state pointer back to the previous state
    si = si->ptr;
    --ply;

    assert(ok ());
}
// Do the null-move
void Position::do_null_move (StateInfo &nsi)
{
    assert(&nsi != si);
    assert(si->checkers == 0);

    // Full copy here
    std::memcpy (&nsi, si, sizeof (StateInfo));
    // Point state pointer to point to the new state.
    nsi.ptr = si;
    si = &nsi;

    if (si->en_passant_sq != SQ_NO)
    {
        si->posi_key ^= Zob.en_passant_keys[_file (si->en_passant_sq)];
        si->en_passant_sq = SQ_NO;
    }
    active = ~active;
    si->posi_key ^= Zob.color_key;
    ++si->clock_ply;
    si->null_ply = 0;
    // Set check info used for fast check detection
    si->set_check_info (*this);

    assert(ok ());
}
// Undo the last null-move
void Position::undo_null_move ()
{
    assert(si->ptr != nullptr);
    assert(si->checkers == 0);

    active = ~active;
    si = si->ptr;

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

    setup (ff, *si, thread, true);

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
                oss << board[f|r];
            }
        }
        if (r > R_1)
        {
            oss << '/';
        }
    }

    oss << ' ' << active << ' ';

    if (can_castle (CR_ANY) != CR_NONE)
    {
        if (can_castle (CR_WHITE) != CR_NONE)
        {
            if (can_castle (CR_WKING) != CR_NONE)
            {
                oss << (Chess960 ? to_char (_file (castle_rook[Castling<WHITE, CS_KING>::Right]), false) : 'K');
            }
            if (can_castle (CR_WQUEN) != CR_NONE)
            {
                oss << (Chess960 ? to_char (_file (castle_rook[Castling<WHITE, CS_QUEN>::Right]), false) : 'Q');
            }
        }
        if (can_castle (CR_BLACK) != CR_NONE)
        {
            if (can_castle (CR_BKING) != CR_NONE)
            {
                oss << (Chess960 ? to_char (_file (castle_rook[Castling<BLACK, CS_KING>::Right]),  true) : 'k');
            }
            if (can_castle (CR_BQUEN) != CR_NONE)
            {
                oss << (Chess960 ? to_char (_file (castle_rook[Castling<BLACK, CS_QUEN>::Right]),  true) : 'q');
            }
        }
    }
    else
    {
        oss << '-';
    }

    oss << ' ' << (si->en_passant_sq != SQ_NO ? to_string (si->en_passant_sq) : "-");

    if (full)
    {
        oss << ' ' << i16(si->clock_ply) << ' ' << move_num ();
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
            oss << board[f|r] << " | ";
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
        << si->posi_key << std::nouppercase << std::dec << std::setfill (' ') << '\n';
    oss << "Checkers: ";
    Bitboard b = si->checkers;
    while (b != 0)
    {
        oss << pop_lsq (b) << ' ';
    }
    oss << '\n';
    return oss.str ();
}

#if !defined(NDEBUG)
// Performs some consistency checks for the position, helpful for debugging.
bool Position::ok (u08 *failed_step) const
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

    for (u08 step = BASIC; step <= (Fast ? BASIC : CASTLING); ++step)
    {
        if (failed_step != nullptr)
        {
            *failed_step = step;
        }

        if (step == BASIC)
        {
            if (   (   active != WHITE
                    && active != BLACK)
                || (count<KING> (WHITE) != 1 || board[square (WHITE, KING)] != W_KING)
                || (count<KING> (BLACK) != 1 || board[square (BLACK, KING)] != B_KING)
                || (count<NONE> () > 32 || count<NONE> () != pop_count (pieces ()))
                || (   si->en_passant_sq != SQ_NO
                    && (   rel_rank (active, si->en_passant_sq) != R_6
                        || !can_en_passant (si->en_passant_sq)))
                || si->clock_ply > DrawClockPly)
            {
                return false;
            }
        }
        if (step == PIECE)
        {
            if (   std::count (board, board + SQ_NO, W_KING) != 1
                || std::count (board, board + SQ_NO, B_KING) != 1
                || attackers_to (square (~active, KING), active) != 0
                || pop_count (si->checkers) > 2)
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
            if (   si->matl_key != Zob.compute_matl_key (*this)
                || si->pawn_key != Zob.compute_pawn_key (*this)
                || si->posi_key != Zob.compute_posi_key (*this)
                || si->psq_score != compute_psq (*this)
                || si->non_pawn_matl[WHITE] != compute_npm<WHITE> (*this)
                || si->non_pawn_matl[BLACK] != compute_npm<BLACK> (*this))
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
                        if (   !_ok  (squares[c][pt][i])
                            || board[squares[c][pt][i]] != (c|pt))
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
                    auto cr = castle_right (c, cs);
                    if (   can_castle (cr) != CR_NONE
                        && (   board[castle_rook[cr]] != (c|ROOK)
                            || castle_mask[castle_rook[cr]] != cr
                            || (castle_mask[square (c, KING)] & cr) != cr))
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
