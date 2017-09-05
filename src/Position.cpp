#include "Position.h"

#include "MoveGenerator.h"
#include "Notation.h"
#include "PSQT.h"
#include "TBsyzygy.h"
#include "Thread.h"
#include "Transposition.h"

using namespace std;
using namespace BitBoard;
using namespace MoveGen;
using namespace Notation;
using namespace PSQT;
using namespace TBSyzygy;
using namespace Threading;
using namespace Transposition;

bool Position::Chess960 = false;
u08  Position::DrawClockPly = 100;

/// Position::draw() checks whether position is drawn by: Clock Ply Rule, Repetition.
/// It does not detect Insufficient materials and Stalemate.
bool Position::draw (i16 pp) const
{
    // Draw by Clock Ply Rule?
    // Not in check or in check have legal moves 
    if (   si->clock_ply >= DrawClockPly
        && (   0 == si->checkers
            || 0 != MoveList<GenType::LEGAL> (*this).size ()))
    {
        return true;
    }

    // Draw by Repetition?
    const auto *psi = si->ptr->ptr;
    bool repeated = false;
    for (u08 p = 4; p <= std::min (si->clock_ply, si->null_ply); p += 2)
    {
        psi = psi->ptr->ptr;
        if (psi->posi_key == si->posi_key)
        {
            // Root ply is 1, so return a draw score
            // If repeats once earlier but strictly after the root, or
            // If repeats twice before or at the root.
            if (   repeated
                || pp > p + 1)
            {
                return true;
            }
            repeated = true;
        }
    }
    return false;
}

/// Position::pick_least_val_att() helper function used by see_ge() to locate the least valuable attacker for the side to move,
/// remove the attacker just found from the bitboards and scan for new X-ray attacks behind it.
template<PieceType PT>
PieceType Position::pick_least_val_att (Square dst, Bitboard c_attackers, Bitboard &mocc, Bitboard &attackers) const
{
    Bitboard b = c_attackers & pieces (PT);
    if (0 != b)
    {
        mocc ^= b & ~(b - 1);

        if (   (   PAWN == PT
                || BSHP == PT
                || QUEN == PT)
            && 0 != (b = mocc & pieces (BSHP, QUEN) & PieceAttacks[BSHP][dst]))
        {
            attackers |= b & attacks_bb<BSHP> (dst, mocc);
        }
        if (   (   ROOK == PT
                || QUEN == PT)
            && 0 != (b = mocc & pieces (ROOK, QUEN) & PieceAttacks[ROOK][dst]))
        {
            attackers |= b & attacks_bb<ROOK> (dst, mocc);
        }
        // Remove already processed pieces in x-ray.
        attackers &= mocc;
        return PT;
    }

    return pick_least_val_att<PieceType(PT+1)> (dst, c_attackers, mocc, attackers);
}
template<> PieceType Position::pick_least_val_att<KING> (Square, Bitboard, Bitboard&, Bitboard&) const
{
    return KING; // No need to update bitboards, it is the last cycle.
}

/// Position::see_ge() Static Exchange Evaluator (SEE): It tries to estimate the material gain or loss resulting from a move.
bool Position::see_ge (Move m, Value threshold) const
{
    assert(_ok (m));

    // Only deal with normal moves, assume others pass a simple see
    if (NORMAL != mtype (m))
    {
        return VALUE_ZERO >= threshold;
    }

    auto org = org_sq (m);
    auto dst = dst_sq (m);

    auto balance = PieceValues[MG][ptype (board[dst])]; // Values of the pieces taken by own's minus opp's
    if (balance < threshold)
    {
        return false;
    }

    auto victim = ptype (board[org]);
    assert(PAWN <= victim && victim <= KING);
    balance -= PieceValues[MG][victim];
    if (balance >= threshold)
    {
        return true;
    }

    bool profit = true; // True if the opponent is to move
    
    auto c = color (board[org]);
    Bitboard mocc = pieces () ^ org ^ dst;
    // Find all attackers to the destination square, with the moving piece
    // removed, but possibly an X-ray attacker added behind it.
    Bitboard attackers = attackers_to (dst, mocc) & mocc;
    Bitboard c_attackers;
    while (0 != attackers)
    {
        c = ~c;
        c_attackers = attackers & pieces (c);

        Bitboard b;
        // Don't allow pinned pieces to attack pieces except the king
        // as long all pinners are on their original square.
        // for resolving Bxf2 on fen: r2qk2r/pppb1ppp/2np4/1Bb5/4n3/5N2/PPP2PPP/RNBQR1K1 b kq - 1 1
        if (   0 != c_attackers
            && 0 != (b = abs_checkers (~c) & mocc))
        {
            while (0 != b)
            {
                c_attackers &= ~between_bb (pop_lsq (b), square<KING> (c));
            }
        }
        
        // If move is a discovered check, the only possible defensive capture on
        // the destination square is a capture by the king to evade the check.
        if (   0 != c_attackers
            && 0 != (b = dsc_checkers (~c) & mocc))
        {
            while (0 != b)
            {
                if (0 == (between_bb (pop_lsq (b), square<KING> (c)) & mocc))
                {
                    c_attackers &= pieces (c, KING);
                    break;
                }
            }
        }

        if (0 == c_attackers)
        {
            return profit;
        }

        // Locate and remove the next least valuable attacker.
        victim = pick_least_val_att<PAWN> (dst, c_attackers, mocc, attackers);
        if (KING == victim)
        {
            return profit == (0 != (attackers & pieces (~c)));
        }

        balance +=
            profit ?
                +PieceValues[MG][victim] :
                -PieceValues[MG][victim];

        profit = !profit;
        if (profit == (balance >= threshold))
        {
            return profit;
        }
    }
    return profit;
}

/// Position::slider_blockers() returns a bitboard of all the pieces that are blocking attacks on the square.
/// A piece blocks a slider if removing that piece from the board would result in a position where square is attacked by the sliders in 'attackers'.
/// For example, a king-attack blocking piece can be either absolute or discovered blocked piece,
/// according if its color is the opposite or the same of the color of the sliders in 'attackers'.
template<Color Own>
Bitboard Position::slider_blockers (Square s, Bitboard ex_attackers, Bitboard &pinners, Bitboard &discovers) const
{
    const auto Opp = WHITE == Own ? BLACK : WHITE;

    Bitboard blockers = 0;
    Bitboard defenders = pieces (Own);
    Bitboard attackers = pieces (Opp) ^ ex_attackers;
    // Snipers are attackers that are aligned on square in x-ray.
    Bitboard snipers =
          attackers
        & (  (pieces (BSHP, QUEN) & PieceAttacks[BSHP][s])
           | (pieces (ROOK, QUEN) & PieceAttacks[ROOK][s]));
    Bitboard hurdle = (defenders | (attackers ^ snipers));
    Bitboard b;
    while (0 != snipers)
    {
        auto sniper_sq = pop_lsq (snipers);
        b = hurdle & between_bb (s, sniper_sq);
        if (   0 != b
            && !more_than_one (b))
        {
            blockers |= b;
            
            if (0 != (b & defenders))
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

/// Position::pseudo_legal() tests whether a random move is pseudo-legal.
/// It is used to validate moves from TT that can be corrupted
/// due to SMP concurrent access or hash position key aliasing.
bool Position::pseudo_legal (Move m) const
{
    assert(_ok (m));
    auto org = org_sq (m);
    auto dst = dst_sq (m);
    // If the org square is not occupied by a piece belonging to the side to move,
    // then the move is obviously not legal.
    if (!contains (pieces (active), org))
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
        // Is not a promotion, so promotion piece must be empty.
        assert(PAWN == (promote (m) - NIHT));
    }
        break;
    case CASTLE:
    {
        auto cs = dst > org ? CS_KING : CS_QUEN;
        // Check whether the destination square is attacked by the opponent.
        // Castling moves are checked for legality during move generation.
        if (!(   KING == mpt
              && rel_rank (active, org) == R_1
              && rel_rank (active, dst) == R_1
              && contains (pieces (active, ROOK), dst)
              && can_castle (active, cs)
              && expeded_castle (active, cs)
              && 0 == si->checkers))
        {
            return false;
        }
        // Castle is always encoded as "King captures friendly Rook".
        assert(dst == castle_rook[active][cs]);
        Bitboard b = king_path[active][cs];
        // Check king's path for attackers.
        while (0 != b)
        {
            if (0 != attackers_to (pop_lsq (b), ~active))
            {
                return false;
            }
        }
        auto king_dst = rel_sq (active, dst > org ? SQ_G1 : SQ_C1);
        // Chess960
        // For instance an enemy queen in SQ_A1 when castling rook is in SQ_B1.
        if (   0 != (b = pieces (~active, ROOK, QUEN) & rank_bb (king_dst))
            && 0 != (b & attacks_bb<ROOK> (king_dst, pieces () ^ dst)))
        {
            return false;
        }
        return true; // No capture
    }
        break;
    case ENPASSANT:
    {
        cap -= pawn_push (active);
        if (!(   PAWN == mpt
              && rel_rank (active, org) == R_5
              && rel_rank (active, dst) == R_6
              && si->en_passant_sq == dst
              && empty (dst)
              && contains (pieces (~active, PAWN), cap)))
        {
            return false;
        }
    }
        break;
    case PROMOTE:
    {
        assert(NIHT <= promote (m) && promote (m) <= QUEN);
        if (!(   PAWN == mpt
              && rel_rank (active, org) == R_7
              && rel_rank (active, dst) == R_8))
        {
            return false;
        }
    }
        break;
    }
    // The captured square cannot be occupied by a friendly piece (CASTLE != mt)
    if (contains (pieces (active), cap))
    {
        return false;
    }

    // Handle the special case of a piece move
    if (PAWN == mpt)
    {
        // In case of non-promotional moves origin & destination cannot be on the 7th/2nd & 8th/1st rank.
        if (   PROMOTE != mt
            && (   rel_rank (active, org) == R_7
                || rel_rank (active, dst) == R_8))
        {
            return false;
        }
        if (    // Single push
               !(   (   NORMAL == mt
                     || PROMOTE == mt)
                 && empty (dst)
                 && org + pawn_push (active) == dst)
                // Normal capture
            && !(   (   NORMAL == mt
                     || PROMOTE == mt)
                 && contains (pieces (~active) & PawnAttacks[active][org], dst))
                // Enpassant capture
            && !(   ENPASSANT == mt
                 && si->en_passant_sq == dst
                 && empty (dst)
                 && contains (pieces (~active, PAWN), cap))
                // Double push
            && !(   NORMAL == mt
                 && rel_rank (active, org) == R_2
                 && rel_rank (active, dst) == R_4
                 && empty (dst - pawn_push (active))
                 && empty (dst)
                 && org + pawn_push (active)*2 == dst))
        {
            return false;
        }
    }
    else
    {
        switch (mpt)
        {
        case NIHT: if (   !contains (PieceAttacks[NIHT][org]          , dst)) { return false; }
            break;
        case BSHP: if (   !contains (PieceAttacks[BSHP][org]          , dst)
                       || !contains (attacks_bb<BSHP> (org, pieces ()), dst)) { return false; }
            break;
        case ROOK: if (   !contains (PieceAttacks[ROOK][org]          , dst)
                       || !contains (attacks_bb<ROOK> (org, pieces ()), dst)) { return false; }
            break;
        case QUEN: if (   !contains (PieceAttacks[QUEN][org]          , dst)
                       || !contains (attacks_bb<QUEN> (org, pieces ()), dst)) { return false; }
            break;
        case KING: if (   !contains (PieceAttacks[KING][org]          , dst)) { return false; }
            break;
        default: assert(false);
            break;
        }
    }

    // Evasions generator already takes care to avoid some kind of illegal moves and legal() relies on this.
    // So have to take care that the same kind of moves are filtered out here.
    if (0 != si->checkers)
    {
        // In case of king moves under check, remove king so to catch
        // as invalid moves like B1A1 when opposite queen is on C1.
        if (KING == mpt)
        {
            return 0 == attackers_to (dst, ~active, pieces () ^ org);
        }
        // Double check? In this case a king move is required
        if (!more_than_one (si->checkers))
        {
            return ENPASSANT != mt ?
                // Move must be a capture of the checking piece or a blocking evasion of the checking piece
                   contains (si->checkers | between_bb (scan_lsq (si->checkers), square<KING> (active)), dst) :
                // Move must be a capture of the checking en-passant pawn or a blocking evasion of the checking piece
                   (0 != (si->checkers & pieces (~active, PAWN)) && contains (si->checkers, cap))
                || contains (between_bb (scan_lsq (si->checkers), square<KING> (active)), dst);
        }
        return false;
    }
    return true;
}
/// Position::legal() tests whether a pseudo-legal move is legal.
bool Position::legal (Move m) const
{
    assert(_ok (m));
    assert(contains (pieces (active), org_sq (m)));

    switch (mtype (m))
    {
    case NORMAL:
    {
        // Only king moves to non attacked squares, sliding check x-rays the king
        // In case of king moves under check have to remove king so to catch
        // as invalid moves like B1-A1 when opposite queen is on SQ_C1.
        // check whether the destination square is attacked by the opponent.
        if (contains (pieces (KING), org_sq (m)))
        {
            return 0 == attackers_to (dst_sq (m), ~active, pieces () ^ org_sq (m));
        }
    }
    // NOTE: no break
    case PROMOTE:
    {
        // A non-king move is legal if and only if it is not pinned or
        // it is moving along the ray towards or away from the king or
        // it is a blocking evasion or a capture of the checking piece.
        return !contains (si->king_blockers[active], org_sq (m))
            || sqrs_aligned (org_sq (m), dst_sq (m), square<KING> (active));
    }
        break;
    case CASTLE:
    {
        // Castling moves are checked for legality during move generation.
        assert(contains (pieces (active, KING), org_sq (m))
            && contains (pieces (active, ROOK), dst_sq (m)));
        return true;
    }
        break;
    case ENPASSANT:
    {
        // En-passant captures are a tricky special case. Because they are rather uncommon,
        // do it simply by testing whether the king is attacked after the move is made.
        assert(contains (pieces (active, PAWN), org_sq (m))
            && rel_rank (active, org_sq (m)) == R_5
            && rel_rank (active, dst_sq (m)) == R_6
            && empty (dst_sq (m))
            && si->en_passant_sq == dst_sq (m)
            && contains (pieces (~active, PAWN), dst_sq (m) - pawn_push (active)));
        auto fk_sq = square<KING> (active);
        Bitboard mocc = (pieces () ^ org_sq (m) ^ (dst_sq (m) - pawn_push (active))) | dst_sq (m);
        Bitboard b;
        // If any attacker then in check and not legal move.
        return (   0 == (b = pieces (~active, BSHP, QUEN) & PieceAttacks[BSHP][fk_sq])
                || 0 == (b & attacks_bb<BSHP> (fk_sq, mocc)))
            && (   0 == (b = pieces (~active, ROOK, QUEN) & PieceAttacks[ROOK][fk_sq])
                || 0 == (b & attacks_bb<ROOK> (fk_sq, mocc)));
    }
        break;
    }
    return false;
}
/// Position::gives_check() tests whether a pseudo-legal move gives a check.
bool Position::gives_check (Move m) const
{
    auto org = org_sq (m);
    auto dst = dst_sq (m);
    assert(contains (pieces (active), org));
    
    if (    // Direct check ?
           contains (si->checks[ptype (board[org])], dst)
            // Discovered check ?
        || (   contains (dsc_blockers (active), org)
            && !sqrs_aligned (org, dst, square<KING> (~active))))
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
        // Castling with check?
        auto ek_sq = square<KING> (~active);
        auto king_org = org;
        auto rook_org = dst; // 'King captures the rook' notation
        /*change*/dst = rel_sq (active, rook_org > king_org ? SQ_G1 : SQ_C1);
        auto rook_dst = rel_sq (active, rook_org > king_org ? SQ_F1 : SQ_D1);
        // First x-ray check then full check
        return contains (PieceAttacks[ROOK][rook_dst]                                   , ek_sq)
            && contains (attacks_bb<ROOK> (rook_dst, (pieces () ^ org ^ rook_org) | dst), ek_sq);
    }
        break;
    case ENPASSANT:
    {
        // En-passant capture with check?
        // already handled the case of direct checks and ordinary discovered check,
        // the only case need to handle is the unusual case of a discovered check through the captured pawn.
        auto ek_sq = square<KING> (~active);
        Bitboard mocc = (pieces () ^ org ^ (_file (dst)|_rank (org))) | dst;
        Bitboard b;
        return (   0 != (b = pieces (active, BSHP, QUEN) & PieceAttacks[BSHP][ek_sq])
                && 0 != (b & attacks_bb<BSHP> (ek_sq, mocc)))
            || (   0 != (b = pieces (active, ROOK, QUEN) & PieceAttacks[ROOK][ek_sq])
                && 0 != (b & attacks_bb<ROOK> (ek_sq, mocc)));
    }
        break;
    case PROMOTE:
    {
        // Promotion with check?
        auto ek_sq = square<KING> (~active);
        switch (promote (m))
        {
        case NIHT: return contains (PieceAttacks[NIHT][dst]                , ek_sq);
            break;
        case BSHP: return contains (PieceAttacks[BSHP][dst]                , ek_sq)
                       && contains (attacks_bb<BSHP> (dst, pieces () ^ org), ek_sq);
            break;
        case ROOK: return contains (PieceAttacks[ROOK][dst]                , ek_sq)
                       && contains (attacks_bb<ROOK> (dst, pieces () ^ org), ek_sq);
            break;
        case QUEN: return contains (PieceAttacks[QUEN][dst]                , ek_sq)
                       && contains (attacks_bb<QUEN> (dst, pieces () ^ org), ek_sq);
            break;
        default: assert(false); break;
        }
    }
        break;
    }
    return false;
}
/// Position::clear() clear the position.
void Position::clear ()
{
    for (auto s : SQ)
    {
        board[s] = NO_PIECE;
        castle_mask[s] = CR_NONE;
    }
    for (auto pt : { PAWN, NIHT, BSHP, ROOK, QUEN, KING, NONE })
    {
        types_bb[pt] = 0;
    }
    for (auto c : { WHITE, BLACK })
    {
        color_bb[c] = 0;
        for (auto pt : { PAWN, NIHT, BSHP, ROOK, QUEN, KING })
        {
            squares[c][pt].clear ();
        }
        for (auto cs : { CS_KING, CS_QUEN })
        {
            castle_rook[c][cs] = SQ_NO;
            castle_path[c][cs] = 0;
            king_path  [c][cs] = 0;
        }
    }
}
/// Position::set_castle() set the castling right.
void Position::set_castle (Color c, CastleSide cs)
{
    auto king_org = square<KING> (c);
    assert(rel_rank (c, king_org) == R_1);
    auto rook_org = castle_rook[c][cs];
    assert(contains (pieces (c, ROOK), rook_org)
        && rel_rank (c, rook_org) == R_1);

    auto king_dst = rel_sq (c, rook_org > king_org ? SQ_G1 : SQ_C1);
    auto rook_dst = rel_sq (c, rook_org > king_org ? SQ_F1 : SQ_D1);
    auto cr = castle_right (c, cs);
    si->castle_rights     |= cr;
    castle_mask[king_org] |= cr;
    castle_mask[rook_org] |= cr;

    for (auto s = std::min (king_org, king_dst); s <= std::max (king_org, king_dst); ++s)
    {
        if (s != king_org)
        {
            king_path[c][cs] |= s;
        }
        if (   s != king_org
            && s != rook_org)
        {
            castle_path[c][cs] |= s;
        }
    }
    for (auto s = std::min (rook_org, rook_dst); s <= std::max (rook_org, rook_dst); ++s)
    {
        if (   s != king_org
            && s != rook_org)
        {
            castle_path[c][cs] |= s;
        }
    }
}
/// Position::can_en_passant() Can the en-passant possible.
bool Position::can_en_passant (Color c, Square ep_sq, bool move_done) const
{
    assert(ep_sq != SQ_NO);
    assert(rel_rank (c, ep_sq) == R_6);
    auto cap = move_done ?
                ep_sq - pawn_push (c) :
                ep_sq + pawn_push (c);
    if (!contains (pieces (~c, PAWN), cap))
    {
        return false;
    }
    
    // En-passant attackers
    Bitboard attackers = PawnAttacks[~c][ep_sq] & pieces (c, PAWN);
    assert(pop_count (attackers) <= 2);
    Bitboard mocc = (pieces () ^ cap) | ep_sq;
    auto fk_sq = square<KING> (c);
    Bitboard bq = pieces (~c, BSHP, QUEN) & PieceAttacks[BSHP][fk_sq];
    Bitboard rq = pieces (~c, ROOK, QUEN) & PieceAttacks[ROOK][fk_sq];
    while (0 != attackers)
    {
        auto org = pop_lsq (attackers);
        assert(contains (mocc, org));
        // Check en-passant is legal for the position
        if (   (0 == bq || 0 == (bq & attacks_bb<BSHP> (fk_sq, mocc ^ org)))
            && (0 == rq || 0 == (rq & attacks_bb<ROOK> (fk_sq, mocc ^ org))))
        {
            return true;
        }
    }
    return false;
}

/// Position::setup() initializes the position object with the given FEN string.
/// This function is not very robust - make sure that input FENs are correct,
/// this is assumed to be the responsibility of the GUI.
Position& Position::setup (const string &ff, StateInfo &nsi, Thread *const th, bool full)
{
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

    istringstream iss (ff);
    iss >> std::noskipws;

    clear ();
    si = &nsi;

    u08 token;
    // 1. Piece placement on Board
    size_t idx;
    i08 f = F_A;
    i08 r = R_8;
    while (   iss >> token
           && f <= F_NO
           && r >= R_1)
    {
        if (isdigit (token))
        {
            f += token - '0';
        }
        else
        if (   isalpha (token)
            && (idx = PieceChar.find (token)) != string::npos)
        {
            place_piece (File(f)|Rank(r), Piece(idx));
            ++f;
        }
        else
        if (isspace (token))
        {
            break;
        }
        else
        {
            assert(token == '/');
            f = F_A;
            --r;
        }
    }
    assert(SQ_NO != square<KING> (WHITE)
        && SQ_NO != square<KING> (BLACK));

    // 2. Active color
    iss >> token;
    active = Color(ColorChar.find (token));

    si->castle_rights = CR_NONE;
    // 3. Castling availability
    iss >> token;
    while (   iss >> token
           && !isspace (token))
    {
        Square rook_org;
        Color c = isupper (token) ? WHITE : BLACK;
        token = char(tolower (token));
        if ('k' == token)
        {
            assert(rel_rank (c, square<KING> (c)) == R_1);
            for (rook_org = rel_sq (c, SQ_H1); rook_org >= rel_sq (c, SQ_A1); --rook_org)
            {
                assert(!contains (pieces (c, KING), rook_org));
                if (contains (pieces (c, ROOK), rook_org))
                {
                    break;
                }
            }
            assert(contains (pieces (c, ROOK), rook_org)
                && rook_org > square<KING> (c));
            castle_rook[c][CS_KING] = rook_org;
            set_castle (c, CS_KING);
        }
        else
        if ('q' == token)
        {
            assert(rel_rank (c, square<KING> (c)) == R_1);
            for (rook_org = rel_sq (c, SQ_A1); rook_org <= rel_sq (c, SQ_H1); ++rook_org)
            {
                assert(!contains (pieces (c, KING), rook_org));
                if (contains (pieces (c, ROOK), rook_org))
                {
                    break;
                }
            }
            assert(contains (pieces (c, ROOK), rook_org)
                && rook_org < square<KING> (c));
            castle_rook[c][CS_QUEN] = rook_org;
            set_castle (c, CS_QUEN);
        }
        else
        // Chess960
        if ('a' <= token && token <= 'h')
        {
            assert(rel_rank (c, square<KING> (c)) == R_1);
            rook_org = to_file (token)|_rank (square<KING> (c));
            auto cs = rook_org > square<KING> (c) ? CS_KING : CS_QUEN;
            castle_rook[c][cs] = rook_org;
            set_castle (c, cs);
        }
        else
        {
            assert('-' == token);
            continue;
        }
    }
    
    // 4. En-passant square. Ignore if no pawn capture is possible.
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

    // 5-6. Clock ply and Game move count.
    i16   clock_ply = 0
        , moves = 1;
    if (full)
    {
        iss >> std::skipws
            >> clock_ply
            >> moves;

        if (SQ_NO != si->en_passant_sq)
        {
            clock_ply = 0;
        }
        // Rule 50 draw case.
        assert(clock_ply <= 100);

        // Handle common problem move-num = 0.
        if (moves <= 0)
        {
            moves = 1;
        }
    }

    // Convert from moves starting from 1 to ply starting from 0.
    ply = i16(2*(moves - 1) + (BLACK == active ? 1 : 0));

    si->posi_key = RandZob.compute_posi_key (*this);
    si->matl_key = RandZob.compute_matl_key (*this);
    si->pawn_key = RandZob.compute_pawn_key (*this);
    si->psq_score = compute_psq (*this);
    si->non_pawn_matl[WHITE] = compute_npm<WHITE> (*this);
    si->non_pawn_matl[BLACK] = compute_npm<BLACK> (*this);
    si->clock_ply = u08(clock_ply);
    si->null_ply = 0;
    si->capture = NONE;
    si->promotion = false;
    si->checkers = attackers_to (square<KING> (active), ~active);
    si->set_check_info (*this);
    thread = th;
    return *this;
}
/// Position::setup() initializes the position object with the given endgame code string like "KBPKN".
/// It is manily an helper to get the material key out of an endgame code.
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
    
    string fen = "8/" + sides[WHITE] + char(8 - sides[WHITE].length () + '0') + "/8/8/8/8/"
               + sides[BLACK] + char(8 - sides[BLACK].length () + '0') + "/8 w - -";
    
    setup (fen, nsi, nullptr, false);
    
    return *this;
}

/// Position::do_move() makes a move, and saves all information necessary
/// to a StateInfo object. The move is assumed to be legal. Pseudo-legal
/// moves should be filtered out before this function is called.
void Position::do_move (Move m, StateInfo &nsi, bool is_check)
{
    assert(_ok (m));
    assert(&nsi != si);

    thread->nodes.fetch_add (1, std::memory_order::memory_order_relaxed);
    // Copy some fields of old state info to new state info object.
    std::memcpy (&nsi, si, offsetof(StateInfo, capture));
    nsi.ptr = si;
    si = &nsi;

    auto pasive = ~active;

    auto org = org_sq (m);
    auto dst = dst_sq (m);
    auto mt = mtype (m);
    assert(contains (pieces (active), org)
        && (!contains (pieces (active), dst)
         || CASTLE == mt));

    auto mpt = ptype (board[org]);
    assert(NONE != mpt);
    auto ppt = mpt;
    auto cap = ENPASSANT != mt ?
                dst :
                dst - pawn_push (active);
    ++si->clock_ply;
    ++si->null_ply;
    si->capture = CASTLE != mt ? ptype (board[cap]) : NONE;
    if (NONE != si->capture)
    {
        assert(capture (m));
        remove_piece (cap);
        if (PAWN == si->capture)
        {
            si->pawn_key ^= RandZob.piece_square_keys[pasive][PAWN][cap];
            prefetch2 (thread->pawn_table.get (si->pawn_key));
        }
        else
        {
            si->non_pawn_matl[pasive] -= PieceValues[MG][si->capture];
        }
        si->matl_key ^= RandZob.piece_square_keys[pasive][si->capture][count (pasive, si->capture)];
        prefetch (thread->matl_table.get (si->matl_key));

        si->posi_key ^= RandZob.piece_square_keys[pasive][si->capture][cap];
        si->psq_score -= PSQ[pasive][si->capture][cap];
        si->clock_ply = 0;
    }
    // Reset en-passant square.
    if (SQ_NO != si->en_passant_sq)
    {
        assert(si->clock_ply <= 1);
        si->posi_key ^= RandZob.en_passant_keys[_file (si->en_passant_sq)];
        si->en_passant_sq = SQ_NO;
    }
    switch (mt)
    {
    case NORMAL:
    {
        assert(PAWN == (promote (m) - NIHT)
            && KING != si->capture);

        si->promotion = false;
        move_piece (org, dst);
        if (PAWN == mpt)
        {
            si->pawn_key ^=
                  RandZob.piece_square_keys[active][PAWN][dst]
                ^ RandZob.piece_square_keys[active][PAWN][org];
            prefetch2 (thread->pawn_table.get (si->pawn_key));
            // Double push pawn
            if (16 == (u08(dst) ^ u08(org)))
            {
                // Set en-passant square if the moved pawn can be captured
                auto ep_sq = org + (dst - org) / 2;
                if (can_en_passant (pasive, ep_sq))
                {
                    si->posi_key ^= RandZob.en_passant_keys[_file (ep_sq)];
                    si->en_passant_sq = ep_sq;
                }
            }
            si->clock_ply = 0;
        }
    }
        break;
    case CASTLE:
    {
        assert(KING == mpt
            && contains (pieces (active, KING), org)
            && contains (pieces (active, ROOK), dst));

        si->promotion = false;
        Square rook_org, rook_dst;
        do_castling<true> (org, dst, rook_org, rook_dst);
        si->posi_key ^=
              RandZob.piece_square_keys[active][ROOK][rook_dst]
            ^ RandZob.piece_square_keys[active][ROOK][rook_org];
        si->psq_score +=
              PSQ[active][ROOK][rook_dst]
            - PSQ[active][ROOK][rook_org];
    }
        break;
    case ENPASSANT:
    {
        board[cap] = NO_PIECE; // Not done by remove_piece()

        // NOTE:: some condition already set so may not work.
        assert(PAWN == mpt
            && rel_rank (active, org) == R_5
            && rel_rank (active, dst) == R_6
            && empty (dst));

        assert(si->clock_ply <= 1);
        si->clock_ply = 0;
        si->promotion = false;
        move_piece (org, dst);
        si->pawn_key ^=
              RandZob.piece_square_keys[active][PAWN][dst]
            ^ RandZob.piece_square_keys[active][PAWN][org];
        prefetch2 (thread->pawn_table.get (si->pawn_key));
    }
        break;
    case PROMOTE:
    {
        assert(PAWN == mpt
            && rel_rank (active, org) == R_7
            && rel_rank (active, dst) == R_8);
        ppt = promote (m);
        assert(NIHT <= ppt && ppt <= QUEN);

        si->clock_ply = 0;
        si->promotion = true;
        // Replace the pawn with the promoted piece.
        remove_piece (org);
        board[org] = NO_PIECE; // Not done by remove_piece()
        place_piece (dst, active, ppt);
        si->matl_key ^=
              RandZob.piece_square_keys[active][PAWN][count (active, mpt)]
            ^ RandZob.piece_square_keys[active][ppt][count (active, ppt) - 1];
        prefetch (thread->matl_table.get (si->matl_key));

        si->pawn_key ^=
              RandZob.piece_square_keys[active][PAWN][org];
        prefetch2 (thread->pawn_table.get (si->pawn_key));
        si->non_pawn_matl[active] += PieceValues[MG][ppt];
    }
        break;
    }

    si->posi_key ^=
          RandZob.piece_square_keys[active][ppt][dst]
        ^ RandZob.piece_square_keys[active][mpt][org];
    si->psq_score +=
          PSQ[active][ppt][dst]
        - PSQ[active][mpt][org];

    // Update castling rights.
    auto b = si->castle_rights & (castle_mask[org]|castle_mask[dst]);
    if (CR_NONE != b)
    {
        if (CR_NONE != (b & CR_WKING)) si->posi_key ^= RandZob.castle_right_keys[WHITE][CS_KING];
        if (CR_NONE != (b & CR_WQUEN)) si->posi_key ^= RandZob.castle_right_keys[WHITE][CS_QUEN];
        if (CR_NONE != (b & CR_BKING)) si->posi_key ^= RandZob.castle_right_keys[BLACK][CS_KING];
        if (CR_NONE != (b & CR_BQUEN)) si->posi_key ^= RandZob.castle_right_keys[BLACK][CS_QUEN];
        si->castle_rights &= ~b;
    }

    assert(0 == attackers_to (square<KING> (active), pasive));

    // Calculate checkers bitboard (if move is check).
    si->checkers = is_check ? attackers_to (square<KING> (pasive), active) : 0;
    assert(!is_check
        || 0 != si->checkers);

    // Switch sides.
    active = pasive;
    si->posi_key ^= RandZob.color_key;

    si->set_check_info (*this);
    ++ply;

    assert(ok ());
}
/// Position::undo_move() unmakes a move. When it returns, the position should
/// be restored to exactly the same state as before the move was made.
void Position::undo_move (Move m)
{
    assert(nullptr != si->ptr);
    assert(_ok (m));
    auto org = org_sq (m);
    auto dst = dst_sq (m);

    active = ~active;
    auto mt = mtype (m);
    assert(empty (org)
        || CASTLE == mt);
    assert(KING != si->capture);

    auto cap = ENPASSANT != mt ?
                dst :
                dst - pawn_push (active);
    switch (mt)
    {
    case NORMAL:
    {
        move_piece (dst, org);
    }
        break;
    case CASTLE:
    {
        assert(NONE == si->capture);

        Square rook_org, rook_dst;
        do_castling<false> (org, dst, rook_org, rook_dst);
    }
        break;
    case ENPASSANT:
    {
        assert(rel_rank (active, org) == R_5
            && rel_rank (active, dst) == R_6
            && si->ptr->en_passant_sq == dst
            && PAWN == si->capture
            && empty (cap)
            && contains (pieces (active, PAWN), dst));

        move_piece (dst, org);
    }
        break;
    case PROMOTE:
    {
        assert(rel_rank (active, org) == R_7
            && rel_rank (active, dst) == R_8
            && contains (pieces (active, promote (m)), dst));

        remove_piece (dst);
        board[dst] = NO_PIECE; // Not done by remove_piece()
        place_piece (org, active, PAWN);
    }
        break;
    }
    // Restore the captured piece.
    if (NONE != si->capture)
    {
        assert(empty (cap));
        place_piece (cap, ~active, si->capture);
    }

    // Point state pointer back to the previous state.
    si = si->ptr;
    --ply;

    assert(ok ());
}
/// Position::do_null_move() makes a 'null move'.
// It flips the side to move without executing any move on the board.
void Position::do_null_move (StateInfo &nsi)
{
    assert(&nsi != si);
    assert(0 == si->checkers);

    std::memcpy (&nsi, si, sizeof (nsi));
    nsi.ptr = si;
    si = &nsi;
    // Reset en-passant square.
    if (SQ_NO != si->en_passant_sq)
    {
        si->posi_key ^= RandZob.en_passant_keys[_file (si->en_passant_sq)];
        si->en_passant_sq = SQ_NO;
    }
    ++si->clock_ply;
    si->null_ply = 0;

    active = ~active;
    si->posi_key ^= RandZob.color_key;

    si->capture = NONE;
    assert(0 == si->checkers);
    si->set_check_info (*this);

    assert(ok ());
}
/// Position::do_null_move() unmakes a 'null move'.
void Position::undo_null_move ()
{
    assert(nullptr != si->ptr);
    assert(NONE == si->capture);
    assert(0 == si->checkers);

    active = ~active;
    si = si->ptr;

    assert(ok ());
}
/// Position::flip() flips position with the white and black sides reversed.
/// This is only useful for debugging especially for finding evaluation symmetry bugs.
void Position::flip ()
{
    istringstream iss (fen (true));
    string ff, token;
    // 1. Piece placement
    for (auto r : { R_8, R_7, R_6, R_5, R_4, R_3, R_2, R_1 })
    {
        std::getline (iss, token, r > R_1 ? '/' : ' ');
        toggle (token);
        token += r < R_8 ? "/" : " ";
        ff = token + ff;
    }
    // 2. Active color
    iss >> token;
    ff += token[0] == 'w' ? "b" :
          token[0] == 'b' ? "w" : "-";
    ff += " ";
    // 3. Castling availability
    iss >> token;
    toggle (token);
    ff += token;
    ff += " ";
    // 4. En-passant square
    iss >> token;
    ff += token[0] == '-' ?
          token :
          token.replace (1, 1, token[1] == '3' ? "6" :
                               token[1] == '6' ? "3" : "-");
    // 5-6. Half and full moves
    std::getline (iss, token);
    ff += token;

    setup (ff, *si, thread);

    assert(ok ());
}
/// Position::mirror() mirrors position.
void Position::mirror ()
{
    istringstream iss (fen (true));
    string ff, token;
    // 1. Piece placement
    for (auto r : { R_8, R_7, R_6, R_5, R_4, R_3, R_2, R_1 })
    {
        std::getline (iss, token, r > R_1 ? '/' : ' ');
        std::reverse (token.begin (), token.end ());
        token += r > R_1 ? "/" : " ";
        ff = ff + token;
    }
    // 2. Active color
    iss >> token;
    ff += token;
    ff += " ";
    // 3. Castling availability
    iss >> token;
    // Swap castling.
    if (token[0] != '-')
    {
        for (auto &ch : token)
        {
            ch = Chess960 ?
                    to_char (~to_file (char(tolower (i32(ch)))), islower (i32(ch))) :
                    tolower (ch) == 'k' ? (islower (i32(ch)) ? 'q' : 'Q') :
                    tolower (ch) == 'q' ? (islower (i32(ch)) ? 'k' : 'K') : '-';
        }
    }
    ff += token;
    ff += " ";
    // 4. En-passant square
    iss >> token;
    ff += token[0] == '-' ?
          token :
          token.replace (0, 1, string(1, to_char (~to_file (token[0]))));
    // 5-6. Half and full moves
    std::getline (iss, token);
    ff += token;

    setup (ff, *si, thread);

    assert(ok ());
}

/// Position::fen() returns a FEN representation of the position.
/// In case of Chess960 the Shredder-FEN notation is used.
string Position::fen (bool full) const
{
    ostringstream oss;

    for (auto r : { R_8, R_7, R_6, R_5, R_4, R_3, R_2, R_1 })
    {
        for (auto f = F_A; f <= F_H; ++f)
        {
            i16 empty_count;
            for (empty_count = 0; f <= F_H && empty (f|r); ++f)
            {
                ++empty_count;
            }
            if (0 != empty_count)
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

    oss << " " << active << " ";

    if (has_castleright (CR_ANY))
    {
        if (can_castle (WHITE, CS_KING)) oss << (Chess960 ? to_char (_file (castle_rook[WHITE][CS_KING]), false) : 'K');
        if (can_castle (WHITE, CS_QUEN)) oss << (Chess960 ? to_char (_file (castle_rook[WHITE][CS_QUEN]), false) : 'Q');
        if (can_castle (BLACK, CS_KING)) oss << (Chess960 ? to_char (_file (castle_rook[BLACK][CS_KING]),  true) : 'k');
        if (can_castle (BLACK, CS_QUEN)) oss << (Chess960 ? to_char (_file (castle_rook[BLACK][CS_QUEN]),  true) : 'q');
    }
    else
    {
        oss << '-';
    }

    oss << " " << (SQ_NO != si->en_passant_sq ? to_string (si->en_passant_sq) : "-");

    if (full)
    {
        oss << " " << i16(si->clock_ply) << " " << move_num ();
    }

    return oss.str ();
}
/// Position::operator string () returns an ASCII representation of the position.
Position::operator string () const
{
    ostringstream oss;
    oss << " +---+---+---+---+---+---+---+---+\n";
    for (auto r : { R_8, R_7, R_6, R_5, R_4, R_3, R_2, R_1 })
    {
        oss << to_char (r) << "| ";
        for (auto f : { F_A, F_B, F_C, F_D, F_E, F_F, F_G, F_H })
        {
            oss << board[f|r] << " | ";
        }
        oss << "\n +---+---+---+---+---+---+---+---+\n";
    }
    for (auto f : { F_A, F_B, F_C, F_D, F_E, F_F, F_G, F_H })
    {
        oss << "   " << Notation::to_char (f, false);
    }

    oss << "\n"
        << "FEN: " << fen (true) << "\n"
        << "Key: " << std::setfill ('0') << std::hex << std::uppercase << std::setw (16)
        << si->posi_key << std::nouppercase << std::dec << std::setfill (' ') << "\n";
    oss << "Checkers: ";
    for (Bitboard b = si->checkers; 0 != b; )
    {
        oss << pop_lsq (b) << " ";
    }
    if (   MaxLimitPiece >= count<NONE> ()
        && !has_castleright (CR_ANY))
    {
        StateInfo st;
        Position pos;
        pos.setup (fen (), st, thread);
        ProbeState wdl_state; auto wdl = probe_wdl (pos, wdl_state);
        ProbeState dtz_state; auto dtz = probe_dtz (pos, dtz_state);
        oss << "\nTablebases WDL: " << std::setw (4) << wdl << " (" << wdl_state << ")"
            << "\nTablebases DTZ: " << std::setw (4) << dtz << " (" << dtz_state << ")";
    }
    oss << "\n";
    return oss.str ();
}

#if !defined(NDEBUG)
/// Position::ok() performs some consistency checks for the position,
/// and raises an asserts if something wrong is detected.
bool Position::ok () const
{
    const bool Fast = true;
    
    //    BASIC     =0
    //    BITBOARD  =1
    //    SQUARELIST=2
    //    CASTLING  =3
    //    STATEINFO =4

    if (   (   active != WHITE
            && active != BLACK)
        || (1 != count<KING> (WHITE) || !_ok (square<KING> (WHITE)) || W_KING != board[square<KING> (WHITE)])
        || (1 != count<KING> (BLACK) || !_ok (square<KING> (BLACK)) || B_KING != board[square<KING> (BLACK)])
        || 1 != std::count (board, board + SQ_NO, W_KING)
        || 1 != std::count (board, board + SQ_NO, B_KING)
        || (   32 < count<NONE> ()
            || pop_count (pieces ()) != count<NONE> ()))
    {
        assert(0 && "Position OK: BASIC");
        return false;
    }

    if (   (pieces (WHITE) & pieces (BLACK)) != 0
        || (pieces (WHITE) | pieces (BLACK)) != pieces ()
        || (pieces (WHITE) ^ pieces (BLACK)) != pieces ()
        || (pieces (PAWN)|pieces (NIHT)|pieces (BSHP)|pieces (ROOK)|pieces (QUEN)|pieces (KING))
        != (pieces (PAWN)^pieces (NIHT)^pieces (BSHP)^pieces (ROOK)^pieces (QUEN)^pieces (KING))
        || 0 != (pieces (PAWN) & (R1_bb|R8_bb)) // Pawns on Rank1 and Rank8
        || 0 != pop_count (attackers_to (square<KING> (~active),  active))
        || 2 <  pop_count (attackers_to (square<KING> ( active), ~active)))
    {
        assert(0 && "Position OK: BITBOARD");
        return false;
    }
    for (auto pt1 : { PAWN, NIHT, BSHP, ROOK, QUEN, KING })
    {
        for (auto pt2 : { PAWN, NIHT, BSHP, ROOK, QUEN, KING })
        {
            if (   pt1 != pt2
                && 0 != (pieces (pt1) & pieces (pt2)))
            {
                assert(0 && "Position OK: BITBOARD");
                return false;
            }
        }
    }
    for (auto c : { WHITE, BLACK })
    {
        // Too many Piece of color
        if (   16 < count<NONE> (c)
            || pop_count (pieces (c)) != count<NONE> (c)
            || 0 == pieces (c, KING)
            || more_than_one (pieces (c, KING)))
        {
            assert(0 && "Position OK: BITBOARD");
            return false;
        }
        // Check if the number of Pawns plus the number of extra Queens, Rooks, Bishops, Knights exceeds 8
        if (   (        (count<PAWN> (c)
             + std::max (count<NIHT> (c)-2, 0)
             + std::max (count<BSHP> (c)-2, 0)
             + std::max (count<ROOK> (c)-2, 0)
             + std::max (count<QUEN> (c)-1, 0)) > 8)
            || (        (count<PAWN> (c)
             + std::max (pop_count (pieces (c, BSHP) & Color_bb[WHITE])-1, 0)
             + std::max (pop_count (pieces (c, BSHP) & Color_bb[BLACK])-1, 0)) > 8))
        {
            assert(0 && "Position OK: BITBOARD");
            return false;
        }
    }

    if (Fast)
    {
        return true;
    }

    for (auto c : { WHITE, BLACK })
    {
        for (auto pt : { PAWN, NIHT, BSHP, ROOK, QUEN, KING })
        {
            if (count (c, pt) != pop_count (pieces (c, pt)))
            {
                assert(0 && "Position OK: SQUARELIST");
                return false;
            }
            for (auto i = 0; i < count (c, pt); ++i)
            {
                if (   !_ok (squares[c][pt][i])
                    || board[squares[c][pt][i]] != (c|pt))
                {
                    assert(0 && "Position OK: SQUARELIST");
                    return false;
                }
            }
        }
    }

    for (auto c : { WHITE, BLACK })
    {
        for (auto cs : { CS_KING, CS_QUEN })
        {
            auto cr = castle_right (c, cs);
            if (   can_castle (c, cs)
                && (   board[castle_rook[c][cs]] != (c|ROOK)
                    || castle_mask[castle_rook[c][cs]] != cr
                    || (castle_mask[square<KING> (c)] & cr) != cr))
            {
                assert(0 && "Position OK: CASTLING");
                return false;
            }
        }
    }

    if (   si->matl_key != RandZob.compute_matl_key (*this)
        || si->pawn_key != RandZob.compute_pawn_key (*this)
        || si->posi_key != RandZob.compute_posi_key (*this)
        || si->psq_score != compute_psq (*this)
        || si->non_pawn_matl[WHITE] != compute_npm<WHITE> (*this)
        || si->non_pawn_matl[BLACK] != compute_npm<BLACK> (*this)
        || si->checkers != attackers_to (square<KING> (active), ~active)
        || (   si->clock_ply > DrawClockPly
            || (   NONE != si->capture
                && 0 != si->clock_ply))
        || (   SQ_NO != si->en_passant_sq
            && (   rel_rank (active, si->en_passant_sq) != R_6
                || !can_en_passant (active, si->en_passant_sq))))
    {
        assert(0 && "Position OK: STATEINFO");
        return false;
    }

    return true;
}
#endif
