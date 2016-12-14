#include "Position.h"

#include "PieceSquare.h"
#include "Transposition.h"
#include "MoveGenerator.h"
#include "Thread.h"
#include "TBsyzygy.h"
#include "Notation.h"

using namespace std;
using namespace BitBoard;
using namespace PieceSquare;
using namespace Transposition;
using namespace MoveGen;
using namespace Threading;
using namespace TBSyzygy;
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
    const auto *psi = si->ptr->ptr;
    for (auto p = si->draw_ply;
              p >= 4;
              p -= 2)
    {
        psi = psi->ptr->ptr;
        if (psi->posi_key == si->posi_key)
        {
            return psi->draw;
        }
    }
    return false;
}
// Helper function used by see_ge() to locate the least valuable attacker for the side to move,
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
bool Position::see_ge (Move m, Value v) const
{
    assert(_ok (m));
    auto org = org_sq (m);
    auto dst = dst_sq (m);

    Value balance; // Values of the pieces taken by own's minus opp's

    auto c = color (board[org]);
    Bitboard mocc;
    switch (mtype (m))
    {
    case CASTLE:
        // Castle moves are implemented as king capturing the rook so cannot be handled correctly.
        // Simply assume the SEE value is VALUE_ZERO that is always correct unless in the rare case the rook ends up under attack.
        return VALUE_ZERO >= v;
    case ENPASSANT:
        assert((pieces (c, PAWN) & org) != 0);
        mocc = square_bb (dst - pawn_push (c));
        balance = PieceValues[MG][PAWN];
    default:
        assert((pieces (c) & org) != 0);
        mocc = 0;
        balance = PieceValues[MG][ptype (board[dst])];
    }

    if (balance < v)
    {
        return false;
    }

    // The first victim
    auto victim = ptype (board[org]);
    if (victim == KING)
    {
        return true;
    }

    balance -= PieceValues[MG][victim];
    if (balance >= v)
    {
        return true;
    }

    bool profit = true; // True if the opponent is to move
    mocc ^= pieces () ^ org ^ dst;
    // Find all attackers to the destination square, with the moving piece
    // removed, but possibly an X-ray attacker added behind it.
    Bitboard attackers = attackers_to (dst, mocc) & mocc;
    Bitboard c_attackers;
    while (attackers != 0)
    {
        c = ~c;
        c_attackers = attackers & pieces (c);

        // Don't allow pinned pieces to attack pieces except the king
        // as long all pinners are on their original square.
        // for resolving Bxf2 on fen: r2qk2r/pppb1ppp/2np4/1Bb5/4n3/5N2/PPP2PPP/RNBQR1K1 b kq - 1 1
        if (   c_attackers != 0
            && (abs_checkers (~c) & mocc) != 0)
        {
            c_attackers &= ~si->king_blockers[c];
        }
        // If move is a discovered check, the only possible defensive capture on
        // the destination square is a capture by the king to evade the check.
        if (   c_attackers != 0
            && (dsc_checkers (~c) & mocc) != 0
            && (dsc_blockers (~c) & mocc) == 0)
        {
            c_attackers &= pieces (KING);
        }

        if (c_attackers == 0)
        {
            return profit;
        }

        // Locate and remove the next least valuable attacker
        victim = pick_least_val_att<PAWN> (dst, c_attackers, mocc, attackers);
        if (victim == KING)
        {
            return profit == ((attackers & pieces (~c)) != 0);
        }

        balance +=
            profit ?
                +PieceValues[MG][victim] :
                -PieceValues[MG][victim];

        profit = !profit;
        if (profit == (balance >= v))
        {
            return profit;
        }
    }
    return profit;
}

// Returns a bitboard of all the pieces that are blocking attacks on the square 's' from sliders in 'attackers'.
// A piece blocks a slider if removing that piece from the board would result in a position where square 's' is attacked by the sliders in 'attackers'.
// For example, a king-attack blocking piece can be either absolute or discovered blocked piece,
// according if its color is the opposite or the same of the color of the sliders in 'attackers'.
template<Color Own>
Bitboard Position::slider_blockers (Square s, Bitboard ex_attackers, Bitboard &pinners, Bitboard &discovers) const
{
    static const auto Opp  = Own == WHITE ? BLACK : WHITE;

    Bitboard blockers = 0;
    Bitboard defenders = pieces (Own);
    Bitboard attackers = pieces (Opp) ^ ex_attackers;
    // Snipers are attackers that are aligned on 's' in x-ray
    Bitboard snipers =
          attackers
        & (  (pieces (BSHP, QUEN) & PieceAttacks[BSHP][s])
           | (pieces (ROOK, QUEN) & PieceAttacks[ROOK][s]));
    Bitboard hurdle = (defenders | (attackers ^ snipers));
    Bitboard b;
    while (snipers != 0)
    {
        auto sniper_sq = pop_lsq (snipers);
        b = hurdle & between_bb (s, sniper_sq);
        if (   b != 0
            && !more_than_one (b))
        {
            blockers |= b;
            
            if ((b & defenders) != 0)
            {
                pinners |= sniper_sq;
            }
            else
            {
                discovers |= sniper_sq;
            }
        }
    }
    return blockers;
}
// Explicit template instantiations
template Bitboard Position::slider_blockers<WHITE> (Square, Bitboard, Bitboard&, Bitboard&) const;
template Bitboard Position::slider_blockers<BLACK> (Square, Bitboard, Bitboard&, Bitboard&) const;

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
    
    auto mt = mtype (m);
    auto mpt = ptype (board[org]);
    auto cap = dst;
    switch (mt)
    {
    case NORMAL:
    {
        // Is not a promotion, so promotion piece must be empty
        assert(promote (m) - NIHT == PAWN);
    }
        break;
    case CASTLE:
    {
        auto cr = castle_right (active, dst > org ? CS_KING : CS_QUEN);
        // Check whether the destination square is attacked by the opponent.
        // Castling moves are checked for legality during move generation.
        if (!(   mpt == KING
              && rel_rank (active, org) == R_1
              && rel_rank (active, dst) == R_1
              && (pieces (active, ROOK) & dst) != 0
              && si->checkers == 0
              && (si->castle_rights & cr) != CR_NONE
              && !impeded_castle (cr)))
        {
            return false;
        }
        // Castle is always encoded as "King captures friendly Rook"
        assert(dst == castle_rook[cr]);
        Bitboard b = king_path[cr];
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
        if ((pieces (~active, PAWN) & cap) == 0)
        {
            return false;
        }
    }
        break;
    case PROMOTE:
    {
        assert(NIHT <= promote (m) && promote (m) <= QUEN);
        if (!(   mpt == PAWN
              && rel_rank (active, org) == R_7
              && rel_rank (active, dst) == R_8))
        {
            return false;
        }
    }
        break;
    }
    // The captured square cannot be occupied by a friendly piece (mt != CASTLE)
    if ((pieces (active) & cap) != 0)
    {
        return false;
    }

    // Handle the special case of a piece move
    if (mpt == PAWN)
    {
        // In case of non-promotional moves origin & destination cannot be on the 7th/2nd & 8th/1st rank.
        if (   mt != PROMOTE
            && (   rel_rank (active, org) == R_7
                || rel_rank (active, dst) == R_8))
        {
            return false;
        }
        if (    // Not a single push
               !(   (   mt == NORMAL
                     || mt == PROMOTE)
                 && empty (dst)
                 && (org + pawn_push (active) == dst))
                // Not a normal capture
            && !(   (   mt == NORMAL
                     || mt == PROMOTE)
                 && ((pieces (~active) & PawnAttacks[active][org]) & dst) != 0)
                // Not an enpassant capture
            && !(   mt == ENPASSANT
                 && si->en_passant_sq == dst
                 && ((~pieces () & PawnAttacks[active][org]) & dst) != 0
                 && (pieces (~active, PAWN) & cap) != 0)
                // Not a double push
            && !(   mt == NORMAL
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
        switch (mpt)
        {
        case NIHT:
            if ((PieceAttacks[NIHT][org]           & dst) == 0) { return false; } break;
        case BSHP:
            if ((attacks_bb<BSHP> (org, pieces ()) & dst) == 0) { return false; } break;
        case ROOK:
            if ((attacks_bb<ROOK> (org, pieces ()) & dst) == 0) { return false; } break;
        case QUEN:
            if ((attacks_bb<QUEN> (org, pieces ()) & dst) == 0) { return false; } break;
        case KING:
            if ((PieceAttacks[KING][org]           & dst) == 0) { return false; } break;
        default: assert(false); break;
        }
    }

    // Evasions generator already takes care to avoid some kind of illegal moves and legal() relies on this.
    // So have to take care that the same kind of moves are filtered out here.
    if (si->checkers != 0)
    {
        // In case of king moves under check, remove king so to catch
        // as invalid moves like B1A1 when opposite queen is on C1.
        if (mpt == KING)
        {
            return attackers_to (dst, ~active, pieces () ^ org) == 0;
        }
        // Double check? In this case a king move is required
        if (!more_than_one (si->checkers))
        {
            return mt != ENPASSANT ?
                // Move must be a capture of the checking piece or a blocking evasion of the checking piece
                ((si->checkers | between_bb (scan_lsq (si->checkers), square (active, KING))) & dst) != 0 :
                // Move must be a capture of the checking en-passant pawn or a blocking evasion of the checking piece
                (si->checkers & cap) != 0 || (between_bb (scan_lsq (si->checkers), square (active, KING)) & dst) != 0;
        }
        return false;
    }
    return true;
}
// Tests whether a pseudo-legal move is legal
bool Position::legal (Move m) const
{
    assert(_ok (m));
    assert((pieces (active) & org_sq (m)) != 0);

    switch (mtype (m))
    {
    case NORMAL:
    {
        // Only king moves to non attacked squares, sliding check x-rays the king
        // In case of king moves under check have to remove king so to catch
        // as invalid moves like B1-A1 when opposite queen is on SQ_C1.
        // check whether the destination square is attacked by the opponent.
        if ((pieces (KING) & org_sq (m)) != 0)
        {
            return attackers_to (dst_sq (m), ~active, pieces () ^ org_sq (m)) == 0;
        }
    }
    // NOTE: no break
    case PROMOTE:
    {
        // A non-king move is legal if and only if it is not pinned or
        // it is moving along the ray towards or away from the king or
        // it is a blocking evasion or a capture of the checking piece.
        return (si->king_blockers[active] & org_sq (m)) == 0
            || sqrs_aligned (org_sq (m), dst_sq (m), square (active, KING));
    }
        break;
    case CASTLE:
    {
        // Castling moves are checked for legality during move generation.
        assert((pieces (active, KING) & org_sq (m)) != 0
            && (pieces (active, ROOK) & dst_sq (m)) != 0);
        return true;
    }
        break;
    case ENPASSANT:
    {
        // En-passant captures are a tricky special case. Because they are rather uncommon,
        // do it simply by testing whether the king is attacked after the move is made.
        assert((pieces (active, PAWN) & org_sq (m)) != 0
            && rel_rank (active, org_sq (m)) == R_5
            && rel_rank (active, dst_sq (m)) == R_6
            && empty (dst_sq (m))
            && dst_sq (m) == si->en_passant_sq
            && (pieces (~active, PAWN) & (dst_sq (m) - pawn_push (active))) != 0);
        auto fk_sq = square (active, KING);
        Bitboard mocc = (pieces () ^ org_sq (m) ^ (dst_sq (m) - pawn_push (active))) | dst_sq (m);
        // If any attacker then in check and not legal move
        return (   (pieces (~active, BSHP, QUEN) & PieceAttacks[BSHP][fk_sq]) == 0
                || (pieces (~active, BSHP, QUEN) & attacks_bb<BSHP> (fk_sq, mocc)) == 0)
            && (   (pieces (~active, ROOK, QUEN) & PieceAttacks[ROOK][fk_sq]) == 0
                || (pieces (~active, ROOK, QUEN) & attacks_bb<ROOK> (fk_sq, mocc)) == 0);
    }
        break;
    }
    return false;
}
// Tests whether a pseudo-legal move gives a check
bool Position::gives_check (Move m) const
{
    auto org = org_sq (m);
    auto dst = dst_sq (m);
    assert((pieces (active) & org) != 0);
    
    if (    // Direct check ?
           (si->checks[ptype (board[org])] & dst) != 0
            // Discovered check ?
        || (   (si->king_blockers[~active] & org) != 0
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
        bool king_side = dst > org;
        auto rook_org = dst; // 'King captures the rook' notation
        dst           = rel_sq (active, king_side ? SQ_G1 : SQ_C1);
        auto rook_dst = rel_sq (active, king_side ? SQ_F1 : SQ_D1);
        // First x-ray check then full check
        return (PieceAttacks[ROOK][rook_dst] & square (~active, KING)) != 0
            && (attacks_bb<ROOK> (rook_dst, (pieces () ^ org ^ rook_org) | dst | rook_dst) & square (~active, KING)) != 0;
    }
        break;
    case ENPASSANT:
    {
        // En-passant capture with check ?
        // already handled the case of direct checks and ordinary discovered check,
        // the only case need to handle is the unusual case of a discovered check through the captured pawn.
        auto ek_sq = square (~active, KING);
        Bitboard mocc = (pieces () ^ org ^ (_file (dst)|_rank (org))) | dst;
        return (   (pieces (active, BSHP, QUEN) & PieceAttacks[BSHP][ek_sq]) != 0
                && (pieces (active, BSHP, QUEN) & attacks_bb<BSHP> (ek_sq, mocc)) != 0)
            || (   (pieces (active, ROOK, QUEN) & PieceAttacks[ROOK][ek_sq]) != 0
                && (pieces (active, ROOK, QUEN) & attacks_bb<ROOK> (ek_sq, mocc)) != 0);
    }
        break;
    case PROMOTE:
    {
        // Promotion with check ?
        switch (promote (m))
        {
        case NIHT: return (PieceAttacks[NIHT][dst] & square (~active, KING)) != 0; break;
        case BSHP: return (attacks_bb<BSHP> (dst, pieces () ^ org) & square (~active, KING)) != 0; break;
        case ROOK: return (attacks_bb<ROOK> (dst, pieces () ^ org) & square (~active, KING)) != 0; break;
        case QUEN: return (attacks_bb<QUEN> (dst, pieces () ^ org) & square (~active, KING)) != 0; break;
        default: assert(false); break;
        }
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

    nodes = 0;
}
// Set the castling right
void Position::set_castle (Color c, CastleSide cs)
{
    assert(cs == CS_KING
        || cs == CS_QUEN);
    bool king_side = cs == CS_KING;

    auto king_org = square (c, KING);
    Square rook_org;
    for (rook_org = king_side ? rel_sq (c, SQ_H1) : rel_sq (c, SQ_A1);
         king_side ? rook_org >= rel_sq (c, SQ_A1) : rook_org <= rel_sq (c, SQ_H1);
         king_side ? --rook_org : ++rook_org)
    {
        assert((pieces (c, KING) & rook_org) == 0);
        if ((pieces (c, ROOK) & rook_org) != 0)
        {
            break;
        }
    }
    assert((pieces (c, ROOK) & rook_org) != 0);

    auto king_dst = rel_sq (c, king_side ? SQ_G1 : SQ_C1);
    auto rook_dst = rel_sq (c, king_side ? SQ_F1 : SQ_D1);
    auto cr = castle_right (c, cs);
    si->castle_rights     |= cr;
    castle_mask[king_org] |= cr;
    castle_mask[rook_org] |= cr;
    castle_rook[cr] = rook_org;

    for (auto s = std::min (king_org, king_dst); s <= std::max (king_org, king_dst); ++s)
    {
        if (   s != king_org
            && s != rook_org)
        {
            castle_path[cr] |= s;
            king_path[cr] |= s;
        }
    }
    for (auto s = std::min (rook_org, rook_dst); s <= std::max (rook_org, rook_dst); ++s)
    {
        if (   s != king_org
            && s != rook_org)
        {
            castle_path[cr] |= s;
        }
    }
}
// Tests the en-passant square
bool Position::can_en_passant (Color c, Square ep_sq, bool move_done) const
{
    assert(ep_sq != SQ_NO);
    assert(rel_rank (c, ep_sq) == R_6);
    auto cap = move_done ?
                ep_sq - pawn_push (c) :
                ep_sq + pawn_push (c);
    if ((pieces (~c, PAWN) & cap) == 0)//board[cap] != (~c|PAWN)
    {
        return false;
    }
    auto fk_sq = square (c, KING);
    Bitboard mocc = (pieces () ^ cap) | ep_sq;
    // En-passant attackers
    Bitboard attackers = PawnAttacks[~c][ep_sq] & pieces (c, PAWN);
    assert(pop_count (attackers) <= 2);
    while (attackers != 0)
    {
        auto org = pop_lsq (attackers);
        assert((mocc & org) != 0);
        // Check en-passant is legal for the position
        if (   (   (pieces (~c, BSHP, QUEN) & PieceAttacks[BSHP][fk_sq]) == 0
                || (pieces (~c, BSHP, QUEN) & attacks_bb<BSHP> (fk_sq, mocc ^ org)) == 0)
            && (   (pieces (~c, ROOK, QUEN) & PieceAttacks[ROOK][fk_sq]) == 0
                || (pieces (~c, ROOK, QUEN) & attacks_bb<ROOK> (fk_sq, mocc ^ org)) == 0))
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
//    This is recorded only if there really is a pawn that might have advanced two squares
//    and if there is a pawn in position to make an en passant capture legally!!!. 
// 5) Halfmove clock. This is the number of halfmoves since the last pawn advance or capture.
//    This is used to determine if a draw can be claimed under the fifty-move rule.
// 6) Fullmove number. The number of the full move.
//    It starts at 1, and is incremented after Black's move.
Position& Position::setup (const string &ff, StateInfo &nsi, Thread *const th, bool full)
{
    istringstream iss (ff);
    iss >> std::noskipws;

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

    si->castle_rights = CR_NONE;
    // 3. Castling availability
    iss >> token;
    while (   iss >> token
           && !isspace (token))
    {
        Color c = isupper (token) ? WHITE : BLACK;
        token = char(tolower (token));
        if (token == 'k')
        {
            set_castle (c, CS_KING);
        }
        else
        if (token == 'q')
        {
            set_castle (c, CS_QUEN);
        }
        else
        // Chess960
        if ('a' <= token && token <= 'h')
        {
            set_castle (c, (to_file (token) | rel_rank (c, R_1)) > square (c, KING) ? CS_KING : CS_QUEN);
        }
        else
        {
            assert(token == '-');
            continue;
        }
    }
    
    // 4. En-passant square. Ignore if no pawn capture is possible
    si->en_passant_sq = SQ_NO;
    u08 file, rank;
    if (   (iss >> file && ('a' <= file && file <= 'h'))
        && (iss >> rank && ('3' == rank || rank == '6')))
    {
        auto ep_sq = to_square (file, rank);
        if (can_en_passant (active, ep_sq))
        {
            si->en_passant_sq = ep_sq;
        }
    }

    // 5-6. Clock ply and Game move count
    i16   clk_ply = 0
        , moves   = 1;
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

    si->posi_key = Zob.compute_posi_key (*this);
    si->matl_key = Zob.compute_matl_key (*this);
    si->pawn_key = Zob.compute_pawn_key (*this);
    si->psq_score = compute_psq (*this);
    si->non_pawn_matl[WHITE] = compute_npm<WHITE> (*this);
    si->non_pawn_matl[BLACK] = compute_npm<BLACK> (*this);
    si->clock_ply = u08(clk_ply);
    si->draw_ply = 0;
    si->draw = false;
    si->capture = NONE;
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

// set_draw() set the value of si->draw - the boolean meaning the position occurred before in the game.
// This function is called for positions up to the root, so recursion into some of them becomes possible during search.
void Position::set_draw ()
{
    StateInfo* psi = si;
    bool draw = false;
    for (auto p = si->draw_ply;
              p >= 2;
              p -=2)
    {
        psi = psi->ptr->ptr;
        if (psi->posi_key == si->posi_key)
        {
            draw = true;
            break;
        }
    }
    si->draw = draw;
}

// Do the natural-move
void Position::do_move (Move m, StateInfo &nsi, bool is_check)
{
    assert(_ok (m));
    assert(&nsi != si);

    // Copy some fields of old state info to new state info object 
    std::memcpy (&nsi, si, offsetof(StateInfo, capture));
    nsi.ptr = si;
    si = &nsi;

    auto pasive = ~active;

    auto org = org_sq (m);
    auto dst = dst_sq (m);
    auto mt  = mtype (m);
    assert((pieces (active) & org) != 0);
    assert((pieces (active) & dst) == 0
        || mt == CASTLE);

    auto mpt = ptype (board[org]);
    assert(mpt != NONE);
    auto ppt = mpt;
    auto cap = mt != ENPASSANT ? dst : dst - pawn_push (active);
    ++si->clock_ply;
    ++si->draw_ply;
    si->capture = mt != CASTLE ? ptype (board[cap]) : NONE;
    if (si->capture != NONE)
    {
        assert(capture (m));
        remove_piece (cap);
        if (si->capture == PAWN)
        {
            si->pawn_key ^= Zob.piece_square_keys[pasive][PAWN][cap];
        }
        else
        {
            si->non_pawn_matl[pasive] -= PieceValues[MG][si->capture];
        }
        si->matl_key ^= Zob.piece_square_keys[pasive][si->capture][count (pasive, si->capture)];
        si->posi_key ^= Zob.piece_square_keys[pasive][si->capture][cap];
        si->psq_score -= PSQ[pasive][si->capture][cap];
        si->clock_ply = 0;
        si->draw_ply = 0;
    }
    // Reset en-passant square
    if (si->en_passant_sq != SQ_NO)
    {
        assert(si->clock_ply <= 1);
        si->posi_key ^= Zob.en_passant_keys[_file (si->en_passant_sq)];
        si->en_passant_sq = SQ_NO;
    }
    switch (mt)
    {
    case NORMAL:
    {
        assert(promote (m) - NIHT == PAWN
            && si->capture != KING);

        move_piece (org, dst);

        if (mpt == PAWN)
        {
            si->pawn_key ^=
                  Zob.piece_square_keys[active][PAWN][dst]
                ^ Zob.piece_square_keys[active][PAWN][org];
            // If pawn with double push
            if ((u08 (dst) ^ u08 (org)) == 16)
            {
                // Set en-passant square if the moved pawn can be captured
                auto ep_sq = org + (dst - org) / 2;
                if (can_en_passant (pasive, ep_sq))
                {
                    si->posi_key ^= Zob.en_passant_keys[_file (ep_sq)];
                    si->en_passant_sq = ep_sq;
                }
            }
            si->clock_ply = 0;
            si->draw_ply = 0;
        }
    }
        break;
    case CASTLE:
    {
        assert(mpt == KING
            && (pieces (active, KING) & org) != 0
            && (pieces (active, ROOK) & dst) != 0);

        Square rook_org, rook_dst;
        do_castling<true> (org, dst, rook_org, rook_dst);
        si->posi_key ^=
              Zob.piece_square_keys[active][ROOK][rook_dst]
            ^ Zob.piece_square_keys[active][ROOK][rook_org];
        si->psq_score +=
              PSQ[active][ROOK][rook_dst]
            - PSQ[active][ROOK][rook_org];
    }
        break;
    case ENPASSANT:
    {
        assert(mpt == PAWN
            //&& dst == si->en_passant_sq // Already reset si->en_passant_sq
            && empty (dst)
            && !empty (cap)
            && rel_rank (active, org) == R_5
            && rel_rank (active, dst) == R_6);

        board[cap] = NO_PIECE; // Not done by remove_piece()

        assert(si->clock_ply <= 1);
        si->clock_ply = 0;
        si->draw_ply = 0;
        move_piece (org, dst);
        si->pawn_key ^=
              Zob.piece_square_keys[active][PAWN][dst]
            ^ Zob.piece_square_keys[active][PAWN][org];
    }
        break;
    case PROMOTE:
    {
        assert(mpt == PAWN
            && rel_rank (active, org) == R_7
            && rel_rank (active, dst) == R_8);

        si->clock_ply = 0;
        si->draw_ply = 0;
        ppt = promote (m);
        assert(NIHT <= ppt && ppt <= QUEN);
        // Replace the pawn with the promoted piece
        remove_piece (org);
        board[org] = NO_PIECE; // Not done by remove_piece()
        place_piece (dst, active, ppt);
        si->matl_key ^=
              Zob.piece_square_keys[active][PAWN][count (active, mpt)]
            ^ Zob.piece_square_keys[active][ppt][count (active, ppt) - 1];
        si->pawn_key ^=
              Zob.piece_square_keys[active][PAWN][org];
        si->non_pawn_matl[active] += PieceValues[MG][ppt];
    }
        break;
    }

    si->posi_key ^=
          Zob.piece_square_keys[active][ppt][dst]
        ^ Zob.piece_square_keys[active][mpt][org];
    si->psq_score +=
          PSQ[active][ppt][dst]
        - PSQ[active][mpt][org];

    // Update castling rights
    auto b = si->castle_rights & (  castle_mask[org]
                                  | castle_mask[dst]);
    if (b != CR_NONE)
    {
        si->castle_rights &= ~b;
        if ((b & CR_WKING) != CR_NONE) si->posi_key ^= Zob.castle_right_keys[WHITE][CS_KING];
        if ((b & CR_WQUEN) != CR_NONE) si->posi_key ^= Zob.castle_right_keys[WHITE][CS_QUEN];
        if ((b & CR_BKING) != CR_NONE) si->posi_key ^= Zob.castle_right_keys[BLACK][CS_KING];
        if ((b & CR_BQUEN) != CR_NONE) si->posi_key ^= Zob.castle_right_keys[BLACK][CS_QUEN];
    }

    assert(attackers_to (square (active, KING), pasive) == 0);

    // Calculate checkers bitboard (if move is check)
    si->checkers = is_check ? attackers_to (square (pasive, KING), active) : 0;
    assert(!is_check || si->checkers != 0);

    // Switch sides
    active = pasive;
    si->posi_key ^= Zob.color_key;

    // Draw at first repetition
    si->draw = true;
    si->set_check_info (*this);
    ++ply;
    ++nodes;

    assert(ok ());
}
// Undo the last natural-move
void Position::undo_move (Move m)
{
    assert(si->ptr != nullptr);
    assert(_ok (m));
    auto org = org_sq (m);
    auto dst = dst_sq (m);

    active = ~active;
    auto mt = mtype (m);
    assert(empty (org)
        || mt == CASTLE);
    assert(si->capture != KING);

    auto cap = mt != ENPASSANT ? dst : dst - pawn_push (active);
    switch (mt)
    {
    case NORMAL:
    {
        move_piece (dst, org);
    }
        break;
    case CASTLE:
    {
        assert(si->capture == NONE);

        Square rook_org, rook_dst;
        do_castling<false> (org, dst, rook_org, rook_dst);
    }
        break;
    case ENPASSANT:
    {
        assert((pieces (active, PAWN) & dst) != 0
            && rel_rank (active, org) == R_5
            && rel_rank (active, dst) == R_6
            && si->ptr->en_passant_sq == dst
            && si->capture == PAWN
            && empty (cap));

        move_piece (dst, org);
    }
        break;
    case PROMOTE:
    {
        assert(promote (m) == ptype (board[dst])
            && rel_rank (active, org) == R_7
            && rel_rank (active, dst) == R_8);

        remove_piece (dst);
        board[dst] = NO_PIECE; // Not done by remove_piece()
        place_piece (org, active, PAWN);
    }
        break;
    }
    // Restore the captured piece
    if (si->capture != NONE)
    {
        assert(empty (cap));
        place_piece (cap, ~active, si->capture);
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

    std::memcpy (&nsi, si, sizeof (nsi));
    nsi.ptr = si;
    si = &nsi;
    // Reset en-passant square
    if (si->en_passant_sq != SQ_NO)
    {
        si->posi_key ^= Zob.en_passant_keys[_file (si->en_passant_sq)];
        si->en_passant_sq = SQ_NO;
    }
    ++si->clock_ply;

    active = ~active;
    si->posi_key ^= Zob.color_key;

    si->capture = NONE;
    si->draw_ply = 0;
    // Draw at first repetition
    si->draw = true;
    assert(si->checkers == 0);
    si->set_check_info (*this);

    assert(ok ());
}
// Undo the last null-move
void Position::undo_null_move ()
{
    assert(si->ptr != nullptr);
    assert(si->capture == NONE);
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
        std::getline (iss, token, r != R_1 ? '/' : ' ');
        toggle (token);
        token += r != R_8 ? '/' : ' ';
        ff = token + ff;
    }
    // 2. Active color
    iss >> token;
    ff += token[0] == 'w' ? 'b' :
          token[0] == 'b' ? 'w' : '-';
    ff += ' ';
    // 3. Castling availability
    iss >> token;
    toggle (token);
    ff += token;
    ff += ' ';
    // 4. En-passant square
    iss >> token;
    ff += token[0] == '-' ?
          token :
          token.replace (1, 1, token[1] == '3' ? "6" :
                               token[1] == '6' ? "3" : "-");
    // 5-6. Half and full moves
    std::getline (iss, token);
    ff += token;

    setup (ff, *si, thread, true);

    assert(ok ());
}
void Position::mirror ()
{
    istringstream iss (fen (true));
    string ff, token;
    // 1. Piece placement
    for (auto r = R_8; r >= R_1; --r)
    {
        std::getline (iss, token, r != R_1 ? '/' : ' ');
        std::reverse (token.begin (), token.end ());
        token += r != R_1 ? '/' : ' ';
        ff = ff + token;
    }
    // 2. Active color
    iss >> token;
    ff += token;
    ff += ' ';
    // 3. Castling availability
    iss >> token;
    // Swap castling
    if (token[0] != '-')
    {
        for (auto &ch : token)
        {
            ch = Chess960 ?
                    to_char (~to_file (char(tolower (ch))), islower (ch)) :
                    tolower (ch) == 'k' ? (islower (ch) ? 'q' : 'Q') :
                    tolower (ch) == 'q' ? (islower (ch) ? 'k' : 'K') : '-';
        }
    }
    ff += token;
    ff += ' ';
    // 4. En-passant square
    iss >> token;
    ff += token[0] == '-' ?
          token :
          token.replace (0, 1, string(1, to_char (~to_file (token[0]))));
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
        if (r != R_1)
        {
            oss << '/';
        }
    }

    oss << ' ' << active << ' ';

    if (can_castle (CR_ANY) != CR_NONE)
    {
        if (can_castle (CR_WKING) != CR_NONE)
        {
            oss << (Chess960 ? to_char (_file (castle_rook[Castling<WHITE, CS_KING>::Right]), false) : 'K');
        }
        if (can_castle (CR_WQUEN) != CR_NONE)
        {
            oss << (Chess960 ? to_char (_file (castle_rook[Castling<WHITE, CS_QUEN>::Right]), false) : 'Q');
        }
        if (can_castle (CR_BKING) != CR_NONE)
        {
            oss << (Chess960 ? to_char (_file (castle_rook[Castling<BLACK, CS_KING>::Right]),  true) : 'k');
        }
        if (can_castle (CR_BQUEN) != CR_NONE)
        {
            oss << (Chess960 ? to_char (_file (castle_rook[Castling<BLACK, CS_QUEN>::Right]),  true) : 'q');
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
        oss << "\n +---+---+---+---+---+---+---+---+\n";
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
    for (Bitboard b = si->checkers; b != 0;)
    {
        oss << pop_lsq (b) << ' ';
    }
    if (   MaxLimitPiece >= count<NONE> ()
        && can_castle (CR_ANY) == CR_NONE)
    {
        StateInfo st;
        Position pos;
        pos.setup (fen (), st, thread);
        ProbeState wdl_state; WDLScore wdl = probe_wdl (pos, wdl_state);
        ProbeState dtz_state; i32      dtz = probe_dtz (pos, dtz_state);
        oss << "\nTablebases WDL: " << std::setw (4) << wdl << " (" << wdl_state << ")"
            << "\nTablebases DTZ: " << std::setw (4) << dtz << " (" << dtz_state << ")";
    }
    oss << '\n';
    return oss.str ();
}

#if !defined(NDEBUG)
// Performs some consistency checks for the position, helpful for debugging.
bool Position::ok (u08 *step) const
{
    static const bool Fast = true;
    //enum Step : u08
    //{
    //    BASIC,
    //    BITBOARD,
    //    LIST,
    //    CASTLING,
    //    STATEINFO,
    //};

    u08 s = 0;
    {
        if (   (   active != WHITE
                && active != BLACK)
            || (count<KING> (WHITE) != 1 || !_ok (square (WHITE, KING)) || board[square (WHITE, KING)] != W_KING)
            || (count<KING> (BLACK) != 1 || !_ok (square (BLACK, KING)) || board[square (BLACK, KING)] != B_KING)
            || (   count<NONE> () > 32
                || count<NONE> () != pop_count (pieces ())))
        {
            if (step != nullptr) *step = s;
            return false;
        }
    }
    ++s;
    {
        if (   std::count (board, board + SQ_NO, W_KING) != 1
            || std::count (board, board + SQ_NO, B_KING) != 1
            || pop_count (attackers_to (square (~active, KING),  active)) != 0
            || pop_count (attackers_to (square ( active, KING), ~active)) > 2)
        {
            if (step != nullptr) *step = s;
            return false;
        }
        if (   (pieces (WHITE) & pieces (BLACK)) != 0
            || (pieces (WHITE) | pieces (BLACK)) != pieces ()
            || (pieces (WHITE) ^ pieces (BLACK)) != pieces ())
        {
            if (step != nullptr) *step = s;
            return false;
        }
        for (auto pt1 = PAWN; pt1 <= KING; ++pt1)
        {
            for (auto pt2 = PAWN; pt2 <= KING; ++pt2)
            {
                if (   pt1 != pt2
                    && (pieces (pt1) & pieces (pt2)) != 0)
                {
                    if (step != nullptr) *step = s;
                    return false;
                }
            }
        }
        if (   (pieces (PAWN)|pieces (NIHT)|pieces (BSHP)|pieces (ROOK)|pieces (QUEN)|pieces (KING))
            != (pieces (PAWN)^pieces (NIHT)^pieces (BSHP)^pieces (ROOK)^pieces (QUEN)^pieces (KING)))
        {
            if (step != nullptr) *step = s;
            return false;
        }
        // Pawns on rank1 and rank8
        if ((pieces (PAWN) & (R1_bb|R8_bb)) != 0)
        {
            if (step != nullptr) *step = s;
            return false;
        }

        for (auto c = WHITE; c <= BLACK; ++c)
        {
            // Too many Piece of color
            if (   count<NONE> (c) > 16
                || count<NONE> (c) != pop_count (pieces (c)))
            {
                if (step != nullptr) *step = s;
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
                if (step != nullptr) *step = s;
                return false;
            }

            if (           (count (c, PAWN)
                + std::max (pop_count (pieces (c, BSHP) & Color_bb[WHITE])-1, 0)
                + std::max (pop_count (pieces (c, BSHP) & Color_bb[BLACK])-1, 0)) > 8)
            {
                if (step != nullptr) *step = s;
                return false;
            }

            // There should be one and only one KING of color
            if (   pieces (c, KING) == 0
                || more_than_one (pieces (c, KING)))
            {
                if (step != nullptr) *step = s;
                return false;
            }
        }
    }
    if (Fast)
    {
        return true;
    }
    ++s;
    {
        for (auto c = WHITE; c <= BLACK; ++c)
        {
            for (auto pt = PAWN; pt <= KING; ++pt)
            {
                if (count (c, pt) != pop_count (pieces (c, pt)))
                {
                    if (step != nullptr) *step = s;
                    return false;
                }

                for (auto i = 0; i < count (c, pt); ++i)
                {
                    if (   !_ok  (squares[c][pt][i])
                        || board[squares[c][pt][i]] != (c|pt))
                    {
                        if (step != nullptr) *step = s;
                        return false;
                    }
                }
            }
        }
    }
    ++s;
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
                    if (step != nullptr) *step = s;
                    return false;
                }
            }
        }
    }
    ++s;
    {
        if (   si->matl_key != Zob.compute_matl_key (*this)
            || si->pawn_key != Zob.compute_pawn_key (*this)
            || si->posi_key != Zob.compute_posi_key (*this)
            || si->psq_score != compute_psq (*this)
            || si->non_pawn_matl[WHITE] != compute_npm<WHITE> (*this)
            || si->non_pawn_matl[BLACK] != compute_npm<BLACK> (*this)
            || si->checkers != attackers_to (square (active, KING), ~active)
            || (   si->clock_ply > DrawClockPly
                || (   si->capture != NONE
                    && si->clock_ply != 0))
            || (   si->en_passant_sq != SQ_NO
                && (   rel_rank (active, si->en_passant_sq) != R_6
                    || !can_en_passant (active, si->en_passant_sq))))
        {
            if (step != nullptr) *step = s;
            return false;
        }
    }
    
    return true;
}
#endif
