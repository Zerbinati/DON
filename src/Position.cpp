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

#if !defined(NDEBUG)
bool _ok (const string &fen, bool c960, bool full)
{
    Position pos;
    StateInfo si;
    pos.setup (fen, si, nullptr, c960, full);
    return pos.ok ();
}
#endif

u08 DrawClockPly = 100;

// draw() checks whether position is drawn by: Clock Ply Rule, Repetition.
// It does not detect Insufficient materials and Stalemate.
bool Position::draw () const
{
    // Draw by Clock Ply Rule?
    // Not in check or in check have legal moves 
    if (   _si->clock_ply >= DrawClockPly
        && (_si->checkers == 0 || MoveList<LEGAL> (*this).size () != 0))
    {
        return true;
    }
    // Draw by Repetition?
    const auto *psi = _si;
    auto ply = std::min (psi->clock_ply, psi->null_ply);
    if (ply < 4)
    {
        return false;
    }
    psi = psi->ptr->ptr;
    ply -= 2;
    while (ply >= 2)
    {
        psi = psi->ptr->ptr;
        ply -= 2;
        // Check first repetition
        if (psi->posi_key == _si->posi_key)
        {
            return true;
        }
    }

    return false;
}
// repeated() check whether there has been at least one repetition of position since the last capture or pawn move.
bool Position::repeated () const
{
    const auto *bsi = _si;
    while (bsi != nullptr)
    {
        const auto *psi = bsi;
        auto ply = std::min (psi->clock_ply, psi->null_ply);
        if (ply < 4)
        {
            return false;
        }
        psi = psi->ptr->ptr;
        ply -= 2;
        while (ply >= 2)
        {
            psi = psi->ptr->ptr;
            ply -= 2;
            if (psi->posi_key == bsi->posi_key)
            {
                return true;
            }
        }
        bsi = bsi->ptr;
    }
    return false;
}
#if !defined(NDEBUG)
// ok() performs some consistency checks for the position, helpful for debugging.
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
            if (   (_active != WHITE && _active != BLACK)
                || _board[square<KING> (WHITE)] != W_KING
                || _board[square<KING> (BLACK)] != B_KING
                || count<NONE> () > 32
                || count<NONE> () != pop_count (pieces ())
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
                    if (pt1 != pt2 && (pieces (pt1) & pieces (pt2)))
                    {
                        return false;
                    }
                }
            }
            if (   (pieces (PAWN)|pieces (NIHT)|pieces (BSHP)|pieces (ROOK)|pieces (QUEN)|pieces (KING)) != pieces ()
                || (pieces (PAWN)^pieces (NIHT)^pieces (BSHP)^pieces (ROOK)^pieces (QUEN)^pieces (KING)) != pieces ())
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
                    + std::max (pop_count (pieces (c, BSHP) & Liht_bb)-1, 0)
                    + std::max (pop_count (pieces (c, BSHP) & Dark_bb)-1, 0)) > 8)
                {
                    return false; // Too many Promoted Bishop of color
                }

                // There should be one and only one KING of color
                if (pieces (c, KING) == 0 || more_than_one (pieces (c, KING)))
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

                    if (can_castle (cr) == CR_NONE)
                    {
                        continue;
                    }
                    if (   _board[castle_rook (cr)] != (c|ROOK)
                        || _castle_mask[castle_rook (cr)] != cr
                        || (_castle_mask[square<KING> (c)] & cr) != cr)
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
// pick_least_val_att() is a helper function used by see()
// to locate the least valuable attacker for the side to move,
// remove the attacker just found from the bitboards and
// scan for new X-ray attacks behind it.
template<PieceType PT>
PieceType Position::pick_least_val_att (Square dst, Bitboard stm_attackers, Bitboard &mocc, Bitboard &attackers) const
{
    auto b = stm_attackers & pieces (PT);
    if (b != 0)
    {
        mocc ^= b & ~(b - 1);

        switch (PT)
        {
        case PAWN:
        case BSHP:
            attackers |= (pieces (BSHP, QUEN) & attacks_bb<BSHP> (dst, mocc));
            break;
        case ROOK:
            attackers |= (pieces (ROOK, QUEN) & attacks_bb<ROOK> (dst, mocc));
            break;
        case QUEN:
            attackers |= (pieces (BSHP, QUEN) & attacks_bb<BSHP> (dst, mocc))
                      |  (pieces (ROOK, QUEN) & attacks_bb<ROOK> (dst, mocc));
            break;
        default:
            break;
        }

        attackers &= mocc; // After X-ray that may add already processed pieces
        return PT;
    }

    return pick_least_val_att<PieceType(PT+1)> (dst, stm_attackers, mocc, attackers);
}
template<>
PieceType Position::pick_least_val_att<KING> (Square, Bitboard, Bitboard&, Bitboard&) const
{
    return KING; // No need to update bitboards, it is the last cycle
}

// see() is a Static Exchange Evaluator (SEE):
// It tries to estimate the material gain or loss resulting from a move.
Value Position::see (Move m) const
{
    assert(_ok (m));

    auto org = org_sq (m);
    auto dst = dst_sq (m);
    assert(!empty (org));

    // Side to move
    auto stm = color (_board[org]);

    // Gain list
    Value gain_list[32];
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
        mocc = pieces () - org - (dst - pawn_push (stm));
        gain_list[0] = PieceValues[MG][PAWN];
        break;

    default:
        assert(_board[org] != NO_PIECE);
        mocc = pieces () - org;
        gain_list[0] = PieceValues[MG][ptype (_board[dst])];
        break;
    }

    // Find all attackers to the destination square, with the moving piece
    // removed, but possibly an X-ray attacker added behind it.
    auto attackers = attackers_to (dst, mocc) & mocc;

    // If the opponent has any attackers
    stm = ~stm;
    auto stm_attackers = attackers & pieces (stm);

    if (stm_attackers != 0)
    {
        // The destination square is defended, which makes things rather more
        // difficult to compute. Proceed by building up a "swap list" containing
        // the material gain or loss at each stop in a sequence of captures to the
        // destination square, where the sides alternately capture, and always
        // capture with the least valuable piece. After each capture, look for
        // new X-ray attacks from behind the capturing piece.
        auto captured = ptype (_board[org]);

        i08 depth = 1;
        do {
            assert(depth < 32);

            // Add the new entry to the swap list
            gain_list[depth] = PieceValues[MG][captured] - gain_list[depth - 1];

            // Locate and remove the next least valuable attacker
            captured = pick_least_val_att<PAWN> (dst, stm_attackers, mocc, attackers);

            stm = ~stm;
            stm_attackers = attackers & pieces (stm);

            ++depth;
        } while (   stm_attackers != 0
                 // Stop before a king capture
                 && (captured != KING || (--depth, false)));

        // Having built the swap list, negamax through it to find the best
        // achievable score from the point of view of the side to move.
        while (--depth != 0)
        {
            // Find minimum gain
            if (gain_list[depth - 1] > -gain_list[depth])
            {
                gain_list[depth - 1] = -gain_list[depth];
            }
        }
    }

    return gain_list[0];
}
// see_sign()
Value Position::see_sign (Move m) const
{
    assert(_ok (m));
    // If SEE cannot be negative because captured piece value is not less then capturing one.
    // Note that king moves always return here because king value is set to VALUE_ZERO.
    return (  PieceValues[MG][ptype (_board[org_sq (m)])]
           <= PieceValues[MG][ptype (_board[dst_sq (m)])]) ?
        VALUE_KNOWN_WIN :
        see (m);
}
// slider_blockers() returns a bitboard of all the pieces in 'target' that
// are blocking attacks on the square 's' from 'sliders'.
// A piece blocks a slider if removing that piece from the board would result in a position
// where square 's' is attacked by the 'sliders'.
Bitboard Position::slider_blockers (Square s, Bitboard sliders, Bitboard target) const
{
    Bitboard slide_blockers = 0;
    // Pinners are sliders that attack 's' when a pinned piece is removed
    Bitboard pinners =
          sliders
        & (  (pieces (BSHP, QUEN) & PieceAttacks[BSHP][s])
           | (pieces (ROOK, QUEN) & PieceAttacks[ROOK][s]));
    while (pinners != 0)
    {
        Bitboard blocker = between_bb (s, pop_lsq (pinners)) & pieces ();
        if (   !more_than_one (blocker)
            && (blocker & target) != 0)
        {
            slide_blockers |= blocker;
        }
    }
    return slide_blockers;
}
// pseudo_legal() tests whether a random move is pseudo-legal.
// It is used to validate moves from TT that can be corrupted
// due to SMP concurrent access or hash position key aliasing.
bool Position::pseudo_legal (Move m) const
{
    assert(_ok (m));
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
              && !castle_impeded (mk_castle_right (_active, dst > org ? CS_KING : CS_QUEN))))
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
              && _si->en_passant_sq == dst
              && rel_rank (_active, org) == R_5
              && rel_rank (_active, dst) == R_6
              && empty (dst)))
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

    default:
    {
        assert(false);
        return false;
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
        auto org_rel_rank = rel_rank (_active, org);
        auto dst_rel_rank = rel_rank (_active, dst);
        // In case of any moves origin & destination cannot be on the 1st/8th & 1st/2nd rank.
        if (   org_rel_rank == R_1
            || dst_rel_rank == R_2
            || org_rel_rank == R_8
            || dst_rel_rank == R_1)
        {
            return false;
        }
        // In case of non-promotional moves origin & destination cannot be on the 7th/2nd & 8th/1st rank.
        if (   mtype (m) != PROMOTE
            && (   org_rel_rank == R_7
                || dst_rel_rank == R_8))
        {
            return false;
        }
        if (   // Not a capture
               !(((PawnAttacks[_active][org] & pieces (~_active)) & dst) != 0)
               // Not an enpassant capture
            && !(   mtype (m) == ENPASSANT 
                 && ((PawnAttacks[_active][org] & ~pieces ()) & dst) != 0
                 && _si->en_passant_sq == dst
                 && _board[cap] == (~_active|PAWN))
               // Not a single push
            && !(   empty (dst)
                 && (org + pawn_push (_active) == dst))
               // Not a double push
            && !(   org_rel_rank == R_2
                 && dst_rel_rank == R_4
                 && empty (dst)
                 && empty (dst - pawn_push (_active))
                 && (org + 2*pawn_push (_active) == dst)))
        {
            return false;
        }
    }
    else
    {
        if ((attacks_bb (_board[org], org, pieces ()) & dst) == 0)
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
        if (more_than_one (_si->checkers))
        {
            return false;
        }
        return en_passant (m) ?
            // Move must be a capture of the checking en-passant pawn or a blocking evasion of the checking piece
            (_si->checkers & cap) != 0 || (between_bb (scan_lsq (_si->checkers), square<KING> (_active)) & dst) != 0 :
            // Move must be a capture of the checking piece or a blocking evasion of the checking piece
            ((_si->checkers | between_bb (scan_lsq (_si->checkers), square<KING> (_active))) & dst) != 0;
    }
    return true;
}
// legal() tests whether a pseudo-legal move is legal
bool Position::legal (Move m, Bitboard pinneds) const
{
    assert(_ok (m));
    assert(pinneds == abs_pinneds (_active));

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
        return  pinneds == 0
            || (pinneds & org) == 0
            || sqrs_aligned (org, dst, square<KING> (_active));
    }
        break;

    case CASTLE:
    {
        // Castling moves are checked for legality during move generation.
        return mpt == KING && _board[dst] == (_active|ROOK);
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

        auto mocc = pieces () - org - cap + dst;
        // If any attacker then in check and not legal move
        return (pieces (~_active, BSHP, QUEN) & attacks_bb<BSHP> (square<KING> (_active), mocc)) == 0
            && (pieces (~_active, ROOK, QUEN) & attacks_bb<ROOK> (square<KING> (_active), mocc)) == 0;
    }
        break;

    default:
    {
        assert(false);
        return false;
    }
        break;
    }
}
// gives_check() tests whether a pseudo-legal move gives a check
bool Position::gives_check  (Move m, const CheckInfo &ci) const
{
    assert(ci.check_discoverers == check_discoverers (_active));

    auto org = org_sq (m);
    auto dst = dst_sq (m);
    assert((pieces (_active) & org) != 0);

    if (// Is there a Direct check ?
           (ci.checking_bb[ptype (_board[org])] & dst) != 0
        // Is there a Discovered check ?
        // For discovery check we need to verify also direction
        || (   (ci.check_discoverers & org) != 0
            && !sqrs_aligned (org, dst, ci.king_sq)))
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
        return (PieceAttacks[ROOK][rook_dst] & ci.king_sq) != 0
            && (attacks_bb<ROOK> (rook_dst, pieces () - org - rook_org + dst + rook_dst) & ci.king_sq) != 0;
    }
        break;

    case ENPASSANT:
    {
        // En-passant capture with check ?
        // already handled the case of direct checks and ordinary discovered check,
        // the only case need to handle is the unusual case of a discovered check through the captured pawn.
        auto cap = _file (dst)|_rank (org);
        auto mocc = pieces () - org - cap + dst;
        // If any attacker then in check
        return (pieces (_active, BSHP, QUEN) & attacks_bb<BSHP> (ci.king_sq, mocc)) != 0
            || (pieces (_active, ROOK, QUEN) & attacks_bb<ROOK> (ci.king_sq, mocc)) != 0;
    }
        break;

    case PROMOTE:
    {
        // Promotion with check ?
        return (attacks_bb (Piece(promote (m)), dst, pieces () - org + dst) & ci.king_sq) != 0;
    }
        break;

    default:
    {
        assert(false);
        return false;
    }
        break;
    }
}
//// gives_checkmate() tests whether a pseudo-legal move gives a checkmate
//bool Position::gives_checkmate (Move m, const CheckInfo &ci) const
//{
//    bool checkmate = false;
//    Position pos;
//    StateInfo si;
//    pos.setup (fen (_chess960), si, _thread, _chess960);
//    if (pos.gives_check (m, ci))
//    {
//        StateInfo si;
//        pos.do_move (m, si, true);
//        checkmate = MoveList<LEGAL> (pos).size () == 0;
//        pos.undo_move ();
//    }
//    return checkmate;
//}

// compute_non_pawn_material() computes the total non-pawn middle
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

// clear() clear the position
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
    for (auto r = 0; r <= CR_ANY; ++r)
    {
        _castle_rook[r] = SQ_NO;
        _castle_path[r] = 0;
        _king_path[r]   = 0;
    }

    _active     = CLR_NO;
    _ply        = 0;
    _nodes      = 0;
    _chess960   = false;
    _thread     = nullptr;
}

// set_castle() set the castling for the particular color & rook
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

    for (auto s = std::min (rook_org, rook_dst); s <= std::max (rook_org, rook_dst); ++s)
    {
        if (king_org != s && rook_org != s)
        {
            _castle_path[cr] += s;
        }
    }
    for (auto s = std::min (king_org, king_dst); s <= std::max (king_org, king_dst); ++s)
    {
        if (king_org != s && rook_org != s)
        {
            _castle_path[cr] += s;
            _king_path[cr] += s;
        }
    }
}
// can_en_passant() tests the en-passant square
bool Position::can_en_passant (Square ep_sq) const
{
    assert(_ok (ep_sq));
    assert(rel_rank (_active, ep_sq) == R_6);

    auto cap = ep_sq - pawn_push (_active);
    if (//(pieces (~_active, PAWN) & cap) == 0
        _board[cap] != (~_active|PAWN))
    {
        return false;
    }
    // En-passant attackes
    auto attacks = pieces (_active, PAWN) & PawnAttacks[~_active][ep_sq];
    assert(pop_count (attacks) <= 2);
    if (attacks != 0)
    {
        MoveVector moves;
        while (attacks != 0)
        {
            moves.push_back (mk_move<ENPASSANT> (pop_lsq (attacks), ep_sq));
        }
        // Check en-passant is legal for the position
        auto occ = pieces () + ep_sq - cap;
        for (auto m : moves)
        {
            auto mocc = occ - org_sq (m);
            if (   (pieces (~_active, BSHP, QUEN) & attacks_bb<BSHP> (square<KING> (_active), mocc)) == 0
                && (pieces (~_active, ROOK, QUEN) & attacks_bb<ROOK> (square<KING> (_active), mocc)) == 0)
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
//    White pieces are designated using upper-case letters ("PNBRQK") while Black take lowercase ("pnbrqk").
//    Blank squares are noted using digits 1 through 8 (the number of blank squares),
//    and "/" separates ranks.
// 2) Active color. "w" means white, "b" means black - moves next,.
// 3) Castling availability. If neither side can castle, this is "-". 
//    Otherwise, this has one or more letters:
//    "K" (White can castle  Kingside),
//    "Q" (White can castle Queenside),
//    "k" (Black can castle  Kingside),
//    "q" (Black can castle Queenside).
// 4) En passant target square (in algebraic notation).
//    If there's no en passant target square, this is "-".
//    If a pawn has just made a 2-square move, this is the position "behind" the pawn.
//    This is recorded regardless of whether there is a pawn in position to make an en passant capture.
// 5) Halfmove clock. This is the number of halfmoves since the last pawn advance or capture.
//    This is used to determine if a draw can be claimed under the fifty-move rule.
// 6) Fullmove number. The number of the full move.
//    It starts at 1, and is incremented after Black's move.
bool Position::setup (const string &fen_, StateInfo &si, Thread *const th, bool c960, bool full)
{
    assert(!white_spaces (fen_));

    istringstream iss (fen_);
    iss >> std::noskipws;

    clear ();
    std::memset (&si, 0x00, sizeof (StateInfo));
    _si = &si;

    u08 ch;
    // 1. Piece placement on Board
    size_t idx;
    auto f = F_A;
    auto r = R_8;
    while (   iss >> ch
           && !isspace (ch)
           && f <= F_NO
           && r >= R_1)
    {
        if (isdigit (ch))
        {
            f += (ch - '0');
        }
        else
        if (   isalpha (ch)
            && (idx = PieceChar.find (ch)) != string::npos)
        {
            place_piece (f|r, Piece(idx));
            ++f;
        }
        else
        if (ch == '/')
        {
            f = F_A;
            --r;
        }
        else
        {
            assert(false);
            return false;
        }
    }

    assert(square<KING> (WHITE) != SQ_NO
        && square<KING> (BLACK) != SQ_NO);

    // 2. Active color
    iss >> ch;
    _active = Color(ColorChar.find (ch));

    // 3. Castling rights availability
    // Compatible with 3 standards:
    // 1-Normal FEN standard,
    // 2-Shredder-FEN that uses the letters of the columns on which the rooks began the game instead of KQkq
    // 3-X-FEN standard that, in case of Chess960, if an inner rook is associated with the castling right, the castling
    // tag is replaced by the file letter of the involved rook, as for the Shredder-FEN.
    iss >> ch;
    while (   iss >> ch
           && !isspace (ch))
    {
        Square r_sq;
        auto c = isupper (ch) ? WHITE : BLACK;

        if (ch == '-')
        {
            iss >> ch;
            break;
        }

        if (c960)
        {
            if (!('a' <= tolower (ch) && tolower (ch) <= 'h'))
            {
                assert(false);
                return false;
            }
            r_sq = to_file (char(tolower (ch))) | rel_rank (c, R_1);
        }
        else
        {
            switch (char(toupper (ch)))
            {
            case 'K':
                for (r_sq  = rel_sq (c, SQ_H1); r_sq >= rel_sq (c, SQ_A1) && _board[r_sq] != (c|ROOK); --r_sq) {}
                break;
            case 'Q':
                for (r_sq  = rel_sq (c, SQ_A1); r_sq <= rel_sq (c, SQ_H1) && _board[r_sq] != (c|ROOK); ++r_sq) {}
                break;
            default:
                assert(false);
                return false;
                break;
            }
        }

        if (_board[r_sq] != (c|ROOK))
        {
            assert(false);
            return false;
        }
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

    // 5-6. clock ply and game-move count
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
        //// Rule 50 draw case
        //if (clk_ply > 100)
        //{
        //    assert(false);
        //    return false;
        //}

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
    _si->checkers = checkers (_active);
    
    _chess960 = c960;
    _thread   = th;

    return true;
}

#undef do_capture

#define do_capture()                                                     \
    remove_piece (cap);                                                  \
    if (cpt == PAWN)                                                     \
    {                                                                    \
        _si->pawn_key ^= Zob.piece_square[pasive][cpt][cap];             \
    }                                                                    \
    else                                                                 \
    {                                                                    \
        _si->non_pawn_matl[pasive] -= PieceValues[MG][cpt];              \
    }                                                                    \
    _si->matl_key ^= Zob.piece_square[pasive][cpt][count (pasive, cpt)]; \
    key ^= Zob.piece_square[pasive][cpt][cap];                           \
    _si->psq_score -= PSQ[pasive][cpt][cap];

// do_move() do the natural-move
void Position::do_move (Move m, StateInfo &si, bool gives_check)
{
    assert(_ok (m));
    assert(&si != _si);

    Key key = _si->posi_key ^ Zob.act_side;
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
    assert(_ok (mpt));

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

        if (_ok (cpt))
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
                 Zob.piece_square[_active][mpt][dst]
                ^Zob.piece_square[_active][mpt][org];
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
             Zob.piece_square[_active][ROOK][rook_dst]
            ^Zob.piece_square[_active][ROOK][rook_org];

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
             Zob.piece_square[_active][mpt][dst]
            ^Zob.piece_square[_active][mpt][org];
    }
        break;

    case PROMOTE:
    {
        cpt = ptype (_board[cap]);
        assert(mpt == PAWN
            && rel_rank (_active, org) == R_7
            && rel_rank (_active, dst) == R_8
            && cpt != PAWN
            && cpt != KING);

        if (_ok (cpt))
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
             Zob.piece_square[_active][mpt][count (_active, mpt)]
            ^Zob.piece_square[_active][ppt][count (_active, ppt) - 1];

        _si->pawn_key ^=
             Zob.piece_square[_active][mpt][org];

        _si->non_pawn_matl[_active] += PieceValues[MG][ppt];
    }
        break;

    default:
    {
        assert(false);
        return;
    }
        break;
    }

    key ^=
         Zob.piece_square[_active][ppt][dst]
        ^Zob.piece_square[_active][mpt][org];

    _si->psq_score +=
         PSQ[_active][ppt][dst]
        -PSQ[_active][mpt][org];

    // Update castling rights if needed
    if (   _si->castle_rights != CR_NONE
        && (_castle_mask[org] | _castle_mask[dst]) != CR_NONE)
    {
        i32 cr = _si->castle_rights & (_castle_mask[org] | _castle_mask[dst]);
        Bitboard b = cr;
        _si->castle_rights &= ~cr;
        while (b != 0)
        {
            key ^= (*Zob.castle_right)[pop_lsq (b)];
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
        key ^= Zob.en_passant[_file (_si->en_passant_sq)];
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
                key ^= Zob.en_passant[_file (ep_sq)];
            }
        }
    }
    // Update state information
    _si->posi_key     = key;
    _si->last_move    = m;
    _si->capture_type = cpt;
    ++_si->null_ply;
    ++_ply;
    ++_nodes;

    assert(ok ());
}
#undef do_capture
// do_move() do the natural-move (CAN)
void Position::do_move (const string &can, StateInfo &si)
{
    auto m = move_from_can (can, *this);
    if (m != MOVE_NONE)
    {
        do_move (m, si, gives_check (m, CheckInfo (*this)));
    }
}
// undo_move() undo the last natural-move
void Position::undo_move ()
{
    assert(_si->ptr != nullptr);
    auto m = _si->last_move;
    assert(_ok (m));

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

    default:
    {
        assert(false);
        return;
    }
        break;
    }
    // Restore the captured piece
    if (_ok (_si->capture_type))
    {
        place_piece (cap, ~_active, _si->capture_type);
    }

    // Point state pointer back to the previous state
    _si = _si->ptr;
    --_ply;

    assert(ok ());
}
// do_null_move() do the null-move
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
        _si->posi_key ^= Zob.en_passant[_file (_si->en_passant_sq)];
        _si->en_passant_sq = SQ_NO;
    }
    _si->posi_key ^= Zob.act_side;
    _si->clock_ply++;
    _si->null_ply = 0;

    _active = ~_active;

    assert(ok ());
}
// undo_null_move() undo the last null-move
void Position::undo_null_move ()
{
    assert(_si->ptr != nullptr);
    assert(_si->checkers == 0);

    _active = ~_active;
    _si    = _si->ptr;

    assert(ok ());
}
// flip() flips position with the white and black sides reversed.
// This is only useful for debugging especially for finding evaluation symmetry bugs.
void Position::flip ()
{
    string flip_fen, token;
    istringstream iss (fen ());
    // 1. Piece placement
    for (auto rank = R_8; rank >= R_1; --rank)
    {
        std::getline (iss, token, rank > R_1 ? '/' : ' ');
        flip_fen.insert (0, token + (!white_spaces (flip_fen) ? "/" : " "));
    }
    // 2. Active color
    iss >> token;
    flip_fen += (token == "w" ? "B" : "W"); // Will be lowercased later
    flip_fen += ' ';
    // 3. Castling availability
    iss >> token;
    flip_fen += token + ' ';
    toggle (flip_fen);
    // 4. En-passant square
    iss >> token;
    flip_fen += (token == "-" ? token : token.replace (1, 1, token[1] == '3' ? "6" :
                                                             token[1] == '6' ? "3" : "-"));
    // 5-6. Half and full moves
    std::getline (iss, token);
    flip_fen += token;

    setup (flip_fen, *_si, _thread, _chess960, true);

    assert(ok ());
}
// fen()
string Position::fen (bool c960, bool full) const
{
    ostringstream oss;

    for (auto r = R_8; true; --r)
    {
        for (auto f = F_A; true; ++f)
        {
            i16 empty_count = 0;
            while (   f <= F_H
                   && empty (f|r))
            {
                ++empty_count;
                ++f;
            }
            if (empty_count != 0)
            {
                oss << empty_count;
            }
            if (f <= F_H)
            {
                oss << _board[f|r];
            }
            if (f >= F_H)
            {
                break;
            }
        }
        if (r == R_1)
        {
            break;
        }
        oss << '/';
    }

    oss << ' ' << _active << ' ';

    if (can_castle (CR_ANY) != CR_NONE)
    {
        if (_chess960 || c960)
        {
            if (can_castle (CR_WHITE) != CR_NONE)
            {
                if (can_castle (CR_WKING) != CR_NONE) oss << to_char (_file (castle_rook (Castling<WHITE, CS_KING>::Right)), false);
                if (can_castle (CR_WQUEN) != CR_NONE) oss << to_char (_file (castle_rook (Castling<WHITE, CS_QUEN>::Right)), false);
            }
            if (can_castle (CR_BLACK) != CR_NONE)
            {
                if (can_castle (CR_BKING) != CR_NONE) oss << to_char (_file (castle_rook (Castling<BLACK, CS_KING>::Right)), true);
                if (can_castle (CR_BQUEN) != CR_NONE) oss << to_char (_file (castle_rook (Castling<BLACK, CS_QUEN>::Right)), true);
            }
        }
        else
        {
            if (can_castle (CR_WHITE) != CR_NONE)
            {
                if (can_castle (CR_WKING) != CR_NONE) oss << 'K';
                if (can_castle (CR_WQUEN) != CR_NONE) oss << 'Q';
            }
            if (can_castle (CR_BLACK) != CR_NONE)
            {
                if (can_castle (CR_BKING) != CR_NONE) oss << 'k';
                if (can_castle (CR_BQUEN) != CR_NONE) oss << 'q';
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
// string() returns an ASCII representation of the position to be
// printed to the standard output
Position::operator string () const
{
    const string Edge = " +---+---+---+---+---+---+---+---+\n";
    const string Row1 = "| . |   | . |   | . |   | . |   |\n" + Edge;
    const string Row2 = "|   | . |   | . |   | . |   | . |\n" + Edge;

    auto sboard = Edge;

    for (auto r = R_8; r >= R_1; --r)
    {
        sboard += to_char (r) + ((r % 2) != 0 ? Row1 : Row2);
    }
    for (auto f = F_A; f <= F_H; ++f)
    {
        sboard += "   ";
        sboard += to_char (f, false);
    }

    auto occ = pieces ();
    while (occ != 0)
    {
        auto s = pop_lsq (occ);
        sboard[3 + i32((Row1.length () + 1) * (7.5 - _rank (s)) + 4 * _file (s))] = PieceChar[_board[s]];
    }

    ostringstream oss;

    oss << sboard << '\n' << '\n';

    oss << "FEN: " << fen () << '\n'
        << "Key: " << std::setfill ('0') << std::hex << std::uppercase << std::setw (16)
        << _si->posi_key << std::nouppercase << std::dec << std::setfill (' ') << '\n';

    oss << "Checkers: ";
    auto chkrs = _si->checkers;
    if (chkrs != 0)
    {
        while (chkrs != 0)
        {
            auto chk_sq = pop_lsq (chkrs);
            oss << (WHITE|ptype (_board[chk_sq])) << chk_sq << ' ';
        }
    }
    else
    {
        oss << "<none>";
    }

    return oss.str ();
}
