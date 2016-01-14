#include "Position.h"

#include <iomanip>
#include <sstream>

#include "Transposition.h"
#include "MoveGenerator.h"
#include "Thread.h"
#include "Notation.h"

using namespace std;
using namespace BitBoard;
using namespace Transposition;
using namespace MoveGen;
using namespace Threading;
using namespace Notation;

const Value PieceValues[PH_NO][MAX_PTYPE] =
{
    { VALUE_MG_PAWN, VALUE_MG_NIHT, VALUE_MG_BSHP, VALUE_MG_ROOK, VALUE_MG_QUEN, VALUE_ZERO, VALUE_ZERO },
    { VALUE_EG_PAWN, VALUE_EG_NIHT, VALUE_EG_BSHP, VALUE_EG_ROOK, VALUE_EG_QUEN, VALUE_ZERO, VALUE_ZERO }
};

bool _ok (const string &fen, bool c960, bool full)
{
    return Position (fen, nullptr, c960, full).ok ();
}

#define S(mg, eg) mk_score (mg, eg)
// PSQ_Bonus[piece-type][rank][file/2] contains Piece-Square scores.
// For each piece type on a given square a (midgame, endgame) score pair is assigned.
// Table is defined for files A..D and white side: it is symmetric for black side and
// second half of the files.
const Score Position::PSQ_Bonus[NONE][R_NO][F_NO/2] =
{
    { // Pawn
        { S(  0, 0), S(  0, 0), S(  0, 0), S( 0, 0) },
        { S(-19, 5), S(  1,-4), S(  7, 8), S( 3,-2) },
        { S(-26,-6), S( -7,-5), S( 19, 5), S(24, 4) },
        { S(-25, 1), S(-14, 3), S( 20,-8), S(35,-3) },
        { S(-14, 6), S(  0, 9), S(  3, 7), S(21,-6) },
        { S(-14, 6), S(-13,-5), S( -6, 2), S(-2, 4) },
        { S(-12, 1), S( 15,-9), S( -8, 1), S(-4,18) },
        { S(  0, 0), S(  0, 0), S(  0, 0), S( 0, 0) }
    },
    { // Knight
        { S(-143, -97), S(-96,-82), S(-80,-46), S(-73,-14) },
        { S( -83, -69), S(-43,-55), S(-21,-17), S(-10,  9) },
        { S( -71, -50), S(-22,-39), S(  0, -8), S(  9, 28) },
        { S( -25, -41), S( 18,-25), S( 43,  7), S( 47, 38) },
        { S( -26, -46), S( 16,-25), S( 38,  2), S( 50, 41) },
        { S( -11, -55), S( 37,-38), S( 56, -8), S( 71, 27) },
        { S( -62, -64), S(-17,-50), S(  5,-24), S( 14, 13) },
        { S(-195,-110), S(-66,-90), S(-42,-50), S(-29,-13) }
    },
    { // Bishop
        { S(-54,-68), S(-23,-40), S(-35,-46), S(-44,-28) },
        { S(-30,-43), S( 10,-17), S(  2,-23), S( -9, -5) },
        { S(-19,-32), S( 17, -9), S( 11,-13), S(  1,  8) },
        { S(-21,-36), S( 18,-13), S( 11,-15), S(  0,  7) },
        { S(-21,-36), S( 14,-14), S(  6,-17), S( -1,  3) },
        { S(-27,-35), S(  6,-13), S(  2,-10), S( -8,  1) },
        { S(-33,-44), S(  7,-21), S( -4,-22), S(-12, -4) },
        { S(-45,-65), S(-21,-42), S(-29,-46), S(-39,-27) }
    },
    { // Rook
        { S(-25, 0), S(-16, 0), S(-16, 0), S(-9, 0) },
        { S(-21, 0), S( -8, 0), S( -3, 0), S( 0, 0) },
        { S(-21, 0), S( -9, 0), S( -4, 0), S( 2, 0) },
        { S(-22, 0), S( -6, 0), S( -1, 0), S( 2, 0) },
        { S(-22, 0), S( -7, 0), S(  0, 0), S( 1, 0) },
        { S(-21, 0), S( -7, 0), S(  0, 0), S( 2, 0) },
        { S(-12, 0), S(  4, 0), S(  8, 0), S(12, 0) },
        { S(-23, 0), S(-15, 0), S(-11, 0), S(-5, 0) }
    },
    { // Queen
        { S( 0,-70), S(-3,-57), S(-4,-41), S(-1,-29) },
        { S(-4,-58), S( 6,-30), S( 9,-21), S( 8, -4) },
        { S(-2,-39), S( 6,-17), S( 9, -7), S( 9,  5) },
        { S(-1,-29), S( 8, -5), S(10,  9), S( 7, 17) },
        { S(-3,-27), S( 9, -5), S( 8, 10), S( 7, 23) },
        { S(-2,-40), S( 6,-16), S( 8,-11), S(10,  3) },
        { S(-2,-54), S( 7,-30), S( 7,-21), S( 6, -7) },
        { S(-1,-75), S(-4,-54), S(-1,-44), S( 0,-30) }
    },
    { // King
        { S(291, 28), S(344, 76), S(294,103), S(219,112) },
        { S(289, 70), S(329,119), S(263,170), S(205,159) },
        { S(226,109), S(271,164), S(202,195), S(136,191) },
        { S(204,131), S(212,194), S(175,194), S(137,204) },
        { S(177,132), S(205,187), S(143,224), S( 94,227) },
        { S(147,118), S(188,178), S(113,199), S( 70,197) },
        { S(116, 72), S(158,121), S( 93,142), S( 48,161) },
        { S( 94, 30), S(120, 76), S( 78,101), S( 31,111) }
    }
};
#undef S

const size_t StateInfo::Size = sizeof (StateInfo);

const size_t Position::Size = sizeof (Position);

u08 Position::DrawClockPly = 100;
// PSQ[color][piece-type][square] contains [color][piece-type][square] scores.
Score Position::PSQ[CLR_NO][NONE][SQ_NO];

void Position::initialize ()
{
    for (auto pt = PAWN; pt <= KING; ++pt)
    {
        auto score = mk_score (PieceValues[MG][pt], PieceValues[EG][pt]);

        for (auto s = SQ_A1; s <= SQ_H8; ++s)
        {
            auto psq_bonus = score + PSQ_Bonus[pt][_rank (s)][_file (s) < F_E ? _file (s) : F_H - _file (s)];
            PSQ[WHITE][pt][ s] = +psq_bonus;
            PSQ[BLACK][pt][~s] = -psq_bonus;
        }
    }
}

// Position::operator=() creates a copy of 'pos' but detaching the state pointer
// from the source to be self-consistent and not depending on any external data.
Position& Position::operator= (const Position &pos)
{
    for (auto s = SQ_A1; s <= SQ_H8; ++s)
    {
        _board[s] = pos._board[s];
        _castle_mask[s] = pos._castle_mask[s];
    }
    for (auto c = WHITE; c <= BLACK; ++c)
    {
        _color_bb[c] = pos._color_bb[c];
    }
    for (auto pt = PAWN; pt <= NONE; ++pt)
    {
        _types_bb[pt] = pos._types_bb[pt];
    }
    for (auto c = WHITE; c <= BLACK; ++c)
    {
        for (auto pt = PAWN; pt <= KING; ++pt)
        {
            _piece_sq[c][pt] = pos._piece_sq[c][pt];
        }
    }
    for (auto r = 0; r <= CR_FULL; ++r)
    {
        _castle_rook[r] = pos._castle_rook[r];
        _castle_path[r] = pos._castle_path[r];
        _king_path[r]   = pos._king_path[r];
    }

    _active     = pos._active;
    _game_ply   = pos._game_ply;
    _game_nodes = 0;
    _chess960   = pos._chess960;
    _thread     = pos._thread;

    _ssi = *pos._psi;
    _psi = &_ssi;

    assert(ok ());
    return *this;
}

// Draw by: 50 Move Rule, Threefold repetition.
// It does not detect draw by Material and Stalemate, this must be done by the search.
bool Position::draw () const
{
    // Draw by Clock Ply Rule?
    // Not in check or in check have legal moves 
    if (    _psi->clock_ply >= DrawClockPly
        && (_psi->checkers == U64(0) || MoveList<LEGAL> (*this).size () != 0)
       )
    {
        return true;
    }
    // Draw by Threefold Repetition?
    const auto *psi = _psi;
    //u08 cnt = 1;
    for (i08 ply = std::min (_psi->clock_ply, _psi->null_ply); ply >= 2; ply -= 2)
    {
        psi = psi->ptr->ptr;
        if (psi->posi_key == _psi->posi_key)
        {
            //if (++cnt >= 3)
            return true; // Draw at first repetition
        }
    }

    /*
    // Draw by Material?
    if (   _types_bb[PAWN] == U64(0)
        && _psi->non_pawn_matl[WHITE] + _psi->non_pawn_matl[BLACK] <= VALUE_MG_BSHP
       )
    {
        return true;
    }
    */
    /*
    // Draw by Stalemate?
    if (   _psi->checkers == U64(0)
        //&& game_phase () < PHASE_MIDGAME - 50
        && count<NONPAWN> (_active) < count<NONPAWN> (~_active)
        && (count<NONPAWN> (_active) < 3 || (count<NONPAWN> (_active) < 5 && pinneds (_active)))
        && MoveList<LEGAL> (*this).size () == 0
       )
    {
        return true;
    }
    */

    return false;
}

// Check whether there has been at least one repetition of positions
// since the last capture or pawn move.
bool Position::repeated () const
{
    auto *si = _psi;
    while (si != nullptr)
    {
        i08 ply = std::min (si->clock_ply, si->null_ply);
        if (4 > ply) return false;
        auto *psi = si->ptr->ptr;
        do
        {
            psi = psi->ptr->ptr;
            if (psi->posi_key == si->posi_key) return true;
            ply -= 2;
        } while (4 <= ply);

        si = si->ptr;
    }
    return false;
}

// Position consistency test, for debugging
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
        if (failed_step != nullptr) *failed_step = step;

        if (step == BASIC)
        {
            if (   (_active != WHITE && _active != BLACK)
                || (_board[_piece_sq[WHITE][KING][0]] != W_KING)
                || (_board[_piece_sq[BLACK][KING][0]] != B_KING)
                || count<NONE> () > 32 || count<NONE> () != pop_count<Full> (_types_bb[NONE])
                || (_psi->en_passant_sq != SQ_NO && (rel_rank (_active, _psi->en_passant_sq) != R_6 || !can_en_passant (_psi->en_passant_sq)))
                || (_psi->clock_ply > 100)
               )
            {
                return false;
            }
        }
        if (step == PIECE)
        {
            if (   std::count (_board, _board + SQ_NO, W_KING) != 1
                || std::count (_board, _board + SQ_NO, B_KING) != 1
                || attackers_to (_piece_sq[~_active][KING][0], _active)
                || pop_count<Max15> (_psi->checkers) > 2
               )
            {
                return false;
            }
        }
        if (step == BITBOARD)
        {
            if (   (_color_bb[WHITE]&_color_bb[BLACK]) != U64(0)
                || (_color_bb[WHITE]|_color_bb[BLACK]) != _types_bb[NONE]
                || (_color_bb[WHITE]^_color_bb[BLACK]) != _types_bb[NONE]
               )
            {
                return false;
            }

            for (auto pt1 = PAWN; pt1 <= KING; ++pt1)
            {
                for (auto pt2 = PAWN; pt2 <= KING; ++pt2)
                {
                    if (pt1 != pt2 && (_types_bb[pt1]&_types_bb[pt2]))
                    {
                        return false;
                    }
                }
            }
            if (   (_types_bb[PAWN]|_types_bb[NIHT]|_types_bb[BSHP]|_types_bb[ROOK]|_types_bb[QUEN]|_types_bb[KING]) != _types_bb[NONE]
                || (_types_bb[PAWN]^_types_bb[NIHT]^_types_bb[BSHP]^_types_bb[ROOK]^_types_bb[QUEN]^_types_bb[KING]) != _types_bb[NONE]
               )
            {
                return false;
            }

            if ((R1_bb|R8_bb) & _types_bb[PAWN])
            {
                return false;
            }

            for (auto c = WHITE; c <= BLACK; ++c)
            {
                auto colors = _color_bb[c];

                // Too many Piece of color
                if (count<NONE> (c) > 16 || count<NONE> (c) != pop_count<Full> (colors))
                {
                    return false;
                }
                // check if the number of Pawns plus the number of
                // extra Queens, Rooks, Bishops, Knights exceeds 8
                // (which can result only by promotion)
                if (           (i32(_piece_sq[c][PAWN].size ())
                    + std::max (i32(_piece_sq[c][NIHT].size () - 2), 0)
                    + std::max (i32(_piece_sq[c][BSHP].size () - 2), 0)
                    + std::max (i32(_piece_sq[c][ROOK].size () - 2), 0)
                    + std::max (i32(_piece_sq[c][QUEN].size () - 1), 0)) > 8
                   )
                {
                    return false; // Too many Promoted Piece of color
                }

                if (_piece_sq[c][BSHP].size () > 1)
                {
                    auto bishops = colors & _types_bb[BSHP];
                    u08 bishop_count[CLR_NO] =
                    {
                        u08(pop_count<Max15> (Liht_bb & bishops)),
                        u08(pop_count<Max15> (Dark_bb & bishops)),
                    };

                    if (           (i32(_piece_sq[c][PAWN].size ())
                        + std::max (bishop_count[WHITE] - 1, 0)
                        + std::max (bishop_count[BLACK] - 1, 0)) > 8
                       )
                    {
                        return false; // Too many Promoted Bishop of color
                    }
                }

                // There should be one and only one KING of color
                auto kings = colors & _types_bb[KING];
                if (kings == U64(0) || more_than_one (kings))
                {
                    return false;
                }
            }
        }
        if (step == STATE)
        {
            if (   _psi->matl_key != Zob.compute_matl_key (*this)
                || _psi->pawn_key != Zob.compute_pawn_key (*this)
                || _psi->posi_key != Zob.compute_posi_key (*this)
                || _psi->psq_score != compute_psq_score ()
                || _psi->non_pawn_matl[WHITE] != compute_non_pawn_material (WHITE)
                || _psi->non_pawn_matl[BLACK] != compute_non_pawn_material (BLACK)
               )
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
                    if (i32(_piece_sq[c][pt].size ()) != pop_count<Max15> (_color_bb[c]&_types_bb[pt]))
                    {
                        return false;
                    }

                    for (auto i = 0; i < i32(_piece_sq[c][pt].size ()); ++i)
                    {
                        if (   !_ok  (_piece_sq[c][pt][i])
                            || _board[_piece_sq[c][pt][i]] != (c|pt)
                           )
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

                    if (!can_castle (cr))
                    {
                        continue;
                    }
                    if (    _board[_castle_rook[cr]] != (c|ROOK)
                        ||  _castle_mask[_castle_rook[cr]] != cr
                        || (_castle_mask[_piece_sq[c][KING][0]] & cr) != cr
                       )
                    {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

// pick_least_val_att() is a helper function used by see()
// to locate the least valuable attacker for the side to move,
// remove the attacker just found from the bitboards and
// scan for new X-ray attacks behind it.
template<PieceType PT>
PieceType Position::pick_least_val_att (Square dst, Bitboard stm_attackers, Bitboard &mocc, Bitboard &attackers) const
{
    auto b = stm_attackers & _types_bb[PT];
    if (b != U64(0))
    {
        mocc ^= b & ~(b - 1);

        switch (PT)
        {
        case PAWN:
        case BSHP:
            attackers |= (attacks_bb<BSHP> (dst, mocc)&(_types_bb[BSHP]|_types_bb[QUEN]));
            break;
        case ROOK:
            attackers |= (attacks_bb<ROOK> (dst, mocc)&(_types_bb[ROOK]|_types_bb[QUEN]));
            break;
        case QUEN:
            attackers |= (attacks_bb<BSHP> (dst, mocc)&(_types_bb[BSHP]|_types_bb[QUEN]))
                      |  (attacks_bb<ROOK> (dst, mocc)&(_types_bb[ROOK]|_types_bb[QUEN]));
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
Value Position::see      (Move m) const
{
    assert(_ok (m));

    auto org = org_sq (m);
    auto dst = dst_sq (m);

    // Side to move
    auto stm = color (_board[org]);

    const i08 MAX_GAINS = 32;
    // Gain list
    Value gain_list[MAX_GAINS];
    i08   depth = 1;

    auto mocc = _types_bb[NONE] - org;

    switch (mtype (m))
    {
    case CASTLE:
        // Castle moves are implemented as king capturing the rook so cannot be
        // handled correctly. Simply return 0 that is always the correct value
        // unless in the rare case the rook ends up under attack.
        return VALUE_ZERO;
        break;

    case ENPASSANT:
        // Remove the captured pawn
        mocc -= dst - pawn_push (stm);
        gain_list[0] = PieceValues[MG][PAWN];
        break;

    default:
        gain_list[0] = PieceValues[MG][ptype (_board[dst])];
        break;
    }

    // Find all attackers to the destination square, with the moving piece
    // removed, but possibly an X-ray attacker added behind it.
    auto attackers = attackers_to (dst, mocc) & mocc;

    // If the opponent has any attackers
    stm = ~stm;
    auto stm_attackers = attackers & _color_bb[stm];

    if (stm_attackers != U64(0))
    {
        // The destination square is defended, which makes things rather more
        // difficult to compute. Proceed by building up a "swap list" containing
        // the material gain or loss at each stop in a sequence of captures to the
        // destination square, where the sides alternately capture, and always
        // capture with the least valuable piece. After each capture, look for
        // new X-ray attacks from behind the capturing piece.
        auto captured = ptype (_board[org]);

        do
        {
            assert(depth < MAX_GAINS);

            // Add the new entry to the swap list
            gain_list[depth] = PieceValues[MG][captured] - gain_list[depth - 1];

            // Locate and remove the next least valuable attacker
            captured = pick_least_val_att<PAWN> (dst, stm_attackers, mocc, attackers);

            stm = ~stm;
            stm_attackers = attackers & _color_bb[stm];

            ++depth;
        } while (stm_attackers != U64(0) && (captured != KING || (--depth, false))); // Stop before a king capture

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

Value Position::see_sign (Move m) const
{
    assert(_ok (m));
    // Early return if SEE cannot be negative because captured piece value
    // is not less then capturing one. Note that king moves always return
    // here because king midgame value is set to 0.
    if (  PieceValues[MG][ptype (_board[org_sq (m)])]
       <= PieceValues[MG][ptype (_board[dst_sq (m)])]
        || mtype (m) == ENPASSANT
       )
    {
        return VALUE_KNOWN_WIN;
    }
    return see (m);
}

Bitboard Position::check_blockers (Color piece_c, Color king_c) const
{
    auto ksq = _piece_sq[king_c][KING][0];
    // Pinners are sliders that give check when a pinned piece is removed
    // Only one real pinner exist other are fake pinner
    auto pinners =
        (  ((_types_bb[QUEN]|_types_bb[BSHP]) & PieceAttacks[BSHP][ksq])
         | ((_types_bb[QUEN]|_types_bb[ROOK]) & PieceAttacks[ROOK][ksq])
        ) &  _color_bb[~king_c];

    auto chk_blockers = U64(0);
    while (pinners != U64(0))
    {
        auto blocker = Between_bb[ksq][pop_lsq (pinners)] & _types_bb[NONE];
        if (blocker != U64(0) && !more_than_one (blocker))
        {
            chk_blockers |= blocker & _color_bb[piece_c];
        }
    }
    return chk_blockers;
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
    if (   empty (org)
        || color (_board[org]) != _active
       )
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
        if ((promote (m) - NIHT) != PAWN)
        {
            return false;
        }
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
              && _psi->checkers == U64(0)
              && _psi->castle_rights & mk_castle_right (_active)
              && !castle_impeded (mk_castle_right (_active, dst > org ? CS_KING : CS_QUEN))
             )
           )
        {
            return false;
        }
        
        // Castle is always encoded as "King captures friendly Rook"
        bool king_side = dst > org;
        assert(dst == castle_rook (mk_castle_right (_active, king_side ? CS_KING : CS_QUEN)));
        dst = rel_sq (_active, king_side ? SQ_WKOO : SQ_WKOOO);
        auto step = king_side ? DEL_E : DEL_W;
        for (auto s = dst; s != org; s -= step)
        {
            if (attackers_to (s, ~_active) != U64(0))
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
              && _psi->en_passant_sq == dst
              && rel_rank (_active, org) == R_5
              && rel_rank (_active, dst) == R_6
              && empty (dst)
             )
           )
        {
            return false;
        }
        cap += pawn_push (~_active);
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
              && (NIHT <= promote (m) && promote (m) <= QUEN)
             )
           )
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
    //// The destination square cannot be occupied by a friendly piece
    //if ((_color_bb[_active] & dst) != U64(0))
    //{
    //    return false;
    //}

    // The captured square cannot be occupied by a friendly piece or kings
    if (((_color_bb[_active]|_types_bb[KING]) & cap) != U64(0))
    {
        return false;
    }
    // Handle the special case of a piece move
    if (mpt == PAWN)
    {
        auto rel_org_rank = rel_rank (_active, org);
        auto rel_dst_rank = rel_rank (_active, dst);
        // In case of any moves origin & destination cannot be on the 1st/8th & 1st/2nd rank.
        if (   rel_org_rank == R_1 || rel_dst_rank == R_2
            || rel_org_rank == R_8 || rel_dst_rank == R_1
           )
        {
            return false;
        }
        // In case of non-promotional moves origin & destination cannot be on the 7th/2nd & 8th/1st rank.
        if (    mtype (m) != PROMOTE
            && (rel_org_rank == R_7 || rel_dst_rank == R_8)
           )
        {
            return false;
        }
        if (   // Not a capture
               !(   (PawnAttacks[_active][org] & _color_bb[~_active] & dst)
                 //&& dist<File> (dst, org) == 1
                 //&& dist<Rank> (dst, org) == 1
                )
               // Not a single push
            && !(   empty (dst)
                 && (org + pawn_push (_active) == dst)
                 //&& dist<File> (dst, org) == 0
                 //&& dist<Rank> (dst, org) == 1
                )
               // Not a double push
            && !(   rel_org_rank == R_2
                 && rel_dst_rank == R_4
                 && empty (dst)
                 && empty (dst - pawn_push (_active))
                 && (org + 2*pawn_push (_active) == dst)
                 //&& dist<File> (dst, org) == 0
                 //&& dist<Rank> (dst, org) == 2
                )
           )
        {
            return false;
        }

    }
    else
    {
        if ((attacks_bb (_board[org], org, _types_bb[NONE]) & dst) == U64(0)) return false;
    }

    // Evasions generator already takes care to avoid some kind of illegal moves
    // and legal() relies on this. So have to take care that the
    // same kind of moves are filtered out here.
    if (_psi->checkers != U64(0))
    {
        // In case of king moves under check, remove king so to catch
        // as invalid moves like B1A1 when opposite queen is on C1.
        if (mpt == KING) return attackers_to (dst, ~_active, _types_bb[NONE] - org) == U64(0); // Remove 'org' but not place 'dst'

        // Double check? In this case a king move is required
        if (more_than_one (_psi->checkers)) return false;

        return mtype (m) == ENPASSANT && mpt == PAWN ?
            // Move must be a capture of the checking en-passant pawn
            // or a blocking evasion of the checking piece
            (_psi->checkers & cap) != U64(0) || (Between_bb[scan_lsq (_psi->checkers)][_piece_sq[_active][KING][0]] & dst) != U64(0) :
            // Move must be a capture or a blocking evasion of the checking piece
            ((_psi->checkers | Between_bb[scan_lsq (_psi->checkers)][_piece_sq[_active][KING][0]]) & dst) != U64(0);
    }
    return true;
}

// legal() tests whether a pseudo-legal move is legal
bool Position::legal        (Move m, Bitboard pinned) const
{
    assert(_ok (m));
    assert(pinned == pinneds (_active));

    auto org = org_sq (m);
    auto dst = dst_sq (m);
    assert(!empty (org)
         && color (_board[org]) == _active);

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
            return attackers_to (dst, ~_active, _types_bb[NONE] - org) == U64(0); // Remove 'org' but not place 'dst'
        }
    }
    // NOTE: no break
    case PROMOTE:
    {
        assert( mtype (m) == NORMAL
            || (mtype (m) == PROMOTE && mpt == PAWN));
        // A non-king move is legal if and only if it is not pinned or
        // it is moving along the ray towards or away from the king or
        // it is a blocking evasion or a capture of the checking piece.
        return  pinned == U64(0)
            || (pinned & org) == U64(0)
            || sqrs_aligned (org, dst, _piece_sq[_active][KING][0]);
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
        auto cap = dst + pawn_push (~_active);

        assert(mpt == PAWN
            &&  empty (dst)
            && !empty (cap)
            && dst == _psi->en_passant_sq
            && _board[cap] == (~_active|PAWN));

        auto mocc = _types_bb[NONE] - org - cap + dst;
        // If any attacker then in check & not legal
        return (attacks_bb<BSHP> (_piece_sq[_active][KING][0], mocc) & (_color_bb[~_active]&(_types_bb[QUEN]|_types_bb[BSHP]))) == U64(0)
            && (attacks_bb<ROOK> (_piece_sq[_active][KING][0], mocc) & (_color_bb[~_active]&(_types_bb[QUEN]|_types_bb[ROOK]))) == U64(0);
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
    assert(ci.discoverers == discoverers (_active));

    auto org = org_sq (m);
    auto dst = dst_sq (m);
    assert(color (_board[org]) == _active);

    // Is there a Direct check ?
    if ((ci.checking_bb[ptype (_board[org])] & dst) != U64(0)) return true;
    // Is there a Discovered check ?
    // For discovery check we need to verify also direction
    if (    ci.discoverers != U64(0)
        && (ci.discoverers & org) != U64(0)
        && !sqrs_aligned (org, dst, ci.king_sq)
       )
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
        dst           = rel_sq (_active, dst > org ? SQ_WKOO : SQ_WKOOO);
        auto rook_dst = rel_sq (_active, dst > org ? SQ_WROO : SQ_WROOO);

        return (PieceAttacks[ROOK][rook_dst] & ci.king_sq) != U64(0) // First x-ray check then full check
            && (attacks_bb<ROOK> (rook_dst, _types_bb[NONE] - org - rook_org + dst + rook_dst) & ci.king_sq) != U64(0);
    }
        break;
    
    case ENPASSANT:
    {
        // En-passant capture with check ?
        // already handled the case of direct checks and ordinary discovered check,
        // the only case need to handle is the unusual case of a discovered check through the captured pawn.
        auto cap = _file (dst)|_rank (org);
        auto mocc = _types_bb[NONE] - org - cap + dst;
        // If any attacker then in check
        return (attacks_bb<BSHP> (ci.king_sq, mocc) & (_color_bb[_active]&(_types_bb[QUEN]|_types_bb[BSHP]))) != U64(0)
            || (attacks_bb<ROOK> (ci.king_sq, mocc) & (_color_bb[_active]&(_types_bb[QUEN]|_types_bb[ROOK]))) != U64(0);
    }
        break;

    case PROMOTE:
    {
        // Promotion with check ?
        return (attacks_bb (Piece(promote (m)), dst, _types_bb[NONE] - org + dst) & ci.king_sq) != U64(0);
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
//bool Position::gives_checkmate (Move m, const CheckInfo &ci)
//{
//    bool checkmate = false;
//    if (gives_check (m, ci))
//    {
//        StateInfo si;
//        do_move (m, si, true);
//        checkmate = MoveList<LEGAL> (*this).size () == 0;
//        undo_move ();
//    }
//    return checkmate;
//}

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
        _color_bb[c] = U64(0);
    }
    for (auto pt = PAWN; pt <= NONE; ++pt)
    {
        _types_bb[pt] = U64(0);
    }
    for (auto c = WHITE; c <= BLACK; ++c)
    {
        for (auto pt = PAWN; pt <= KING; ++pt)
        {
            _piece_sq[c][pt].clear ();
        }
    }
    for (auto r = 0; r <= CR_FULL; ++r)
    {
        _castle_rook[r] = SQ_NO;
        _castle_path[r] = U64(0);
        _king_path[r]   = U64(0);
    }

    _active     = CLR_NO;
    _game_ply   = 0;
    _game_nodes = U64(0);
    _chess960   = false;
    _thread     = nullptr;

    std::fill (_ssi.non_pawn_matl, _ssi.non_pawn_matl + 2, VALUE_ZERO);
    _ssi.psq_score      = SCORE_ZERO;
    _ssi.matl_key       = U64(0);
    _ssi.pawn_key       = U64(0);
    _ssi.castle_rights  = CR_NONE;
    _ssi.en_passant_sq  = SQ_NO;
    _ssi.clock_ply      = 0;
    _ssi.null_ply       = 0;
    _ssi.posi_key       = U64(0);
    _ssi.last_move      = MOVE_NONE;
    _ssi.capture_type   = NONE;
    _ssi.checkers       = U64(0);
    _ssi.ptr            = nullptr;
    _psi = &_ssi;
}

// set_castle() set the castling for the particular color & rook
void Position::set_castle (Color c, Square rook_org)
{
    auto king_org = _piece_sq[c][KING][0];
    assert(king_org != rook_org);

    auto cr = mk_castle_right (c, rook_org > king_org ? CS_KING : CS_QUEN);
    auto rook_dst = rel_sq (c, rook_org > king_org ? SQ_WROO : SQ_WROOO);
    auto king_dst = rel_sq (c, rook_org > king_org ? SQ_WKOO : SQ_WKOOO);

    _psi->castle_rights    |= cr;

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

    auto cap = ep_sq + pawn_push (~_active);
    //if (((_color_bb[~_active]&_types_bb[PAWN]) & cap) == U64(0)) return false;
    if (_board[cap] != (~_active|PAWN)) return false;

    // En-passant attackes
    auto attacks = PawnAttacks[~_active][ep_sq] & _color_bb[_active]&_types_bb[PAWN];
    assert(pop_count<Full> (attacks) <= 2);
    if (attacks == U64(0)) return false;

    Move moves[3], *m = moves;
    while (attacks != U64(0))
    {
        *(m++) = mk_move<ENPASSANT> (pop_lsq (attacks), ep_sq);
    }
    *m = MOVE_NONE;

    // Check en-passant is legal for the position
    auto occ = _types_bb[NONE] + ep_sq - cap;
    for (m = moves; *m != MOVE_NONE; ++m)
    {
        auto mocc = occ - org_sq (*m);
        if (   (attacks_bb<BSHP> (_piece_sq[_active][KING][0], mocc) & (_color_bb[~_active]&(_types_bb[QUEN]|_types_bb[BSHP]))) == U64(0)
            && (attacks_bb<ROOK> (_piece_sq[_active][KING][0], mocc) & (_color_bb[~_active]&(_types_bb[QUEN]|_types_bb[ROOK]))) == U64(0)
           )
        {
            return true;
        }
    }
    return false;
}
// A FEN string defines a particular position using only the ASCII character set.
//
// A FEN string contains six fields separated by a space. The fields are:
//
// 1) Piece placement (from white's perspective).
//    Each rank is described, starting with rank 8 and ending with rank 1;
//    within each rank, the contents of each square are described from file A through file H.
//    Following the Standard Algebraic Notation (SAN),
//    each piece is identified by a single letter taken from the standard English names.
//    White pieces are designated using upper-case letters ("PNBRQK") while Black take lowercase ("pnbrqk").
//    Blank squares are noted using digits 1 through 8 (the number of blank squares),
//    and "/" separates ranks.
//
// 2) Active color. "w" means white, "b" means black - moves next,.
//
// 3) Castling availability. If neither side can castle, this is "-". 
//    Otherwise, this has one or more letters:
//    "K" (White can castle  Kingside),
//    "Q" (White can castle Queenside),
//    "k" (Black can castle  Kingside),
//    "q" (Black can castle Queenside).
//
// 4) En passant target square (in algebraic notation).
//    If there's no en passant target square, this is "-".
//    If a pawn has just made a 2-square move, this is the position "behind" the pawn.
//    This is recorded regardless of whether there is a pawn in position to make an en passant capture.
//
// 5) Halfmove clock. This is the number of halfmoves since the last pawn advance or capture.
//    This is used to determine if a draw can be claimed under the fifty-move rule.
//
// 6) Fullmove number. The number of the full move.
//    It starts at 1, and is incremented after Black's move.
bool Position::setup (const string &f, Thread *const th, bool c960, bool full)
{
    assert(!white_spaces (f));

    istringstream iss (f);
    iss >> noskipws;

    clear ();
    
    u08 ch;
    // 1. Piece placement on Board
    size_t idx;
    auto s = SQ_A8;
    while (iss >> ch && !isspace (ch))
    {
        if (isdigit (ch))
        {
            s += Delta(ch - '0'); // Advance the given number of files
        }
        else
        if (isalpha (ch) && (idx = PieceChar.find (ch)) != string::npos)
        {
            place_piece (s, Piece(idx));
            ++s;
        }
        else
        if (ch == '/')
        {
            s += DEL_SS;
        }
        else
        {
            return false;
        }
    }

    assert(_piece_sq[WHITE][KING][0] != SQ_NO
        && _piece_sq[BLACK][KING][0] != SQ_NO);

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
    while ((iss >> ch) && !isspace (ch))
    {
        Square r_sq;
        auto c = isupper (ch) ? WHITE : BLACK;
        
        if (ch == '-') continue;

        if (c960)
        {
            if (!('a' <= tolower (ch) && tolower (ch) <= 'h')) return false;
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

        if (_board[r_sq] != (c|ROOK)) return false;
        set_castle (c, r_sq);
    }

    // 4. En-passant square. Ignore if no pawn capture is possible
    u08 file, rank;
    if (   (iss >> file && ('a' <= file && file <= 'h'))
        && (iss >> rank && ('3' == rank || rank == '6'))
       )
    {
        if (   (_active == WHITE && rank == '6')
            || (_active == BLACK && rank == '3')
           )
        {
            auto ep_sq = to_square (file, rank);
            if (can_en_passant (ep_sq))
            {
                _psi->en_passant_sq = ep_sq;
            }
        }
    }

    // 5-6. clock ply and game-move count
    i16 clk_ply = 0, g_move = 1;
    if (full)
    {
        iss >> skipws;
        iss >> clk_ply >> g_move;
        // Rule 50 draw case
        //if (clk_ply >100) return false;
        if (g_move <= 0) g_move = 1;
    }

    // Convert from game_move starting from 1 to game_ply starting from 0,
    // handle also common incorrect FEN with game_move = 0.
    _psi->clock_ply = u08(_psi->en_passant_sq != SQ_NO ? 0 : clk_ply);
    _game_ply = i16(2*(g_move - 1) + (_active == BLACK));

    _psi->matl_key = Zob.compute_matl_key (*this);
    _psi->pawn_key = Zob.compute_pawn_key (*this);
    _psi->posi_key = Zob.compute_posi_key (*this);
    _psi->psq_score = compute_psq_score ();
    _psi->non_pawn_matl[WHITE] = compute_non_pawn_material (WHITE);
    _psi->non_pawn_matl[BLACK] = compute_non_pawn_material (BLACK);
    _psi->checkers = checkers (_active);
    _game_nodes   = 0;
    _chess960     = c960;
    _thread       = th;

    return true;
}

// compute_psq_score() computes the incremental scores for the middle
// game and the endgame. These functions are used to initialize the incremental
// scores when a new position is set up, and to verify that the scores are correctly
// updated by do_move and undo_move when the program is running in debug mode.
Score Position::compute_psq_score () const
{
    auto psq_bonus = SCORE_ZERO;
    auto occ = _types_bb[NONE];
    while (occ != U64(0))
    {
        auto s = pop_lsq (occ);
        auto p = _board[s];
        psq_bonus += PSQ[color (p)][ptype (p)][s];
    }
    return psq_bonus;
}

// compute_non_pawn_material() computes the total non-pawn middle
// game material value for the given side. Material values are updated
// incrementally during the search, this function is only used while
// initializing a new Position object.
Value Position::compute_non_pawn_material (Color c) const
{
    auto npm_value = VALUE_ZERO;
    for (auto pt = NIHT; pt <= QUEN; ++pt)
    {
        npm_value += PieceValues[MG][pt] * i32(_piece_sq[c][pt].size ());
    }
    return npm_value;
}

#undef do_capture

#define do_capture() {                                                               \
    remove_piece (cap);                                                              \
    if (cpt == PAWN)                                                                 \
    {                                                                                \
        _psi->pawn_key ^= Zob._.piece_square[~_active][PAWN][cap];                   \
    }                                                                                \
    else                                                                             \
    {                                                                                \
        _psi->non_pawn_matl[~_active] -= PieceValues[MG][cpt];                       \
    }                                                                                \
    _psi->matl_key ^= Zob._.piece_square[~_active][cpt][_piece_sq[~_active][cpt].size ()];\
    key            ^= Zob._.piece_square[~_active][cpt][cap];                        \
    _psi->psq_score -= PSQ[~_active][cpt][cap];                                      \
    _psi->clock_ply = 0;                                                             \
}
// do_move() do the natural-move
void Position::do_move (Move m, StateInfo &nsi, bool give_check)
{
    assert(_ok (m));
    assert(&nsi != _psi);

    Key key = _psi->posi_key ^ Zob._.act_side;
    // Copy some fields of old state info to new state info object except
    // the ones which are going to be recalculated from scratch anyway, 
    std::memcpy (&nsi, _psi, offsetof(StateInfo, posi_key));

    // Switch state pointer to point to the new, ready to be updated, state.
    nsi.ptr = _psi;
    _psi    = &nsi;

    auto pasive = ~_active;

    auto org = org_sq (m);
    auto dst = dst_sq (m);
    auto mpt = ptype (_board[org]);

    assert(!empty (org)
        && color (_board[org]) == _active
        && mpt != NONE);
    assert( empty (dst)
        || color (_board[dst]) == pasive
        || mtype (m) == CASTLE);

    auto cap = dst;
    auto cpt = NONE;

    // Do move according to move type
    switch (mtype (m))
    {
    case NORMAL:
    {
        cpt = ptype (_board[cap]);
        assert((promote (m) - NIHT) == PAWN
            && cpt != KING);

        if (cpt != NONE) // _ok (cpt)
        {
            do_capture ();
        }
        else
        {
            if (mpt == PAWN)
            {
                _psi->clock_ply = 0;
            }
            else
            {
                _psi->clock_ply++;
            }
        }

        move_piece (org, dst);

        if (mpt == PAWN)
        {
            _psi->pawn_key ^=
                 Zob._.piece_square[_active][PAWN][org]
                ^Zob._.piece_square[_active][PAWN][dst];
        }

        key ^=
             Zob._.piece_square[_active][mpt][org]
            ^Zob._.piece_square[_active][mpt][dst];

        _psi->psq_score +=
            -PSQ[_active][mpt][org]
            +PSQ[_active][mpt][dst];
    }
        break;

    case CASTLE:
    {
        assert(_board[org] == (_active|KING)
            && _board[dst] == (_active|ROOK));

        Square rook_org, rook_dst;
        do_castling<true> (org, dst, rook_org, rook_dst);

        key ^=
             Zob._.piece_square[_active][KING][     org]
            ^Zob._.piece_square[_active][KING][     dst]
            ^Zob._.piece_square[_active][ROOK][rook_org]
            ^Zob._.piece_square[_active][ROOK][rook_dst];

        _psi->psq_score +=
            -PSQ[_active][KING][     org]
            +PSQ[_active][KING][     dst]
            -PSQ[_active][ROOK][rook_org]
            +PSQ[_active][ROOK][rook_dst];

        _psi->clock_ply++;
    }
        break;

    case ENPASSANT:
    {
        assert(mpt == PAWN
            && dst == _psi->en_passant_sq
            && empty (dst)
            && rel_rank (_active, org) == R_5
            && rel_rank (_active, dst) == R_6);

        cap += pawn_push (pasive);
        cpt = PAWN;
        assert(!empty (cap)
            && _board[cap] == (pasive|cpt));

        do_capture ();
        _board[cap] = NO_PIECE; // Not done by remove_piece()

        move_piece (org, dst);

        _psi->pawn_key ^=
             Zob._.piece_square[_active][PAWN][org]
            ^Zob._.piece_square[_active][PAWN][dst];

        key ^=
             Zob._.piece_square[_active][PAWN][org]
            ^Zob._.piece_square[_active][PAWN][dst];

        _psi->psq_score +=
            -PSQ[_active][PAWN][org]
            +PSQ[_active][PAWN][dst];

        //_psi->clock50 = 0; // No need as pawn is the last piece moved
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

        if (cpt != NONE) // _ok (cpt)
        {
            do_capture ();
        }
        else
        {
            _psi->clock_ply = 0;
        }

        auto ppt = promote (m);
        assert(NIHT <= ppt && ppt <= QUEN);
        // Replace the PAWN with the Promoted piece
        remove_piece (org);
        _board[org] = NO_PIECE; // Not done by remove_piece()
        place_piece (dst, _active, ppt);

        _psi->matl_key ^=
             Zob._.piece_square[_active][PAWN][_piece_sq[_active][PAWN].size ()]
            ^Zob._.piece_square[_active][ppt ][_piece_sq[_active][ppt].size () - 1];

        _psi->pawn_key ^=
             Zob._.piece_square[_active][PAWN][org];

        key ^=
             Zob._.piece_square[_active][PAWN][org]
            ^Zob._.piece_square[_active][ppt ][dst];

        _psi->psq_score +=
            -PSQ[_active][PAWN][org]
            +PSQ[_active][ppt ][dst];

        _psi->non_pawn_matl[_active] += PieceValues[MG][ppt];
    }
        break;

    default:
    {
        assert(false);
        return;
    }
        break;
    }
    // Update castling rights if needed
    if (_psi->castle_rights != CR_NONE && (_castle_mask[org] | _castle_mask[dst]) != CR_NONE)
    {
        i32 cr = _psi->castle_rights & (_castle_mask[org] | _castle_mask[dst]);
        Bitboard b = cr;
        _psi->castle_rights &= ~cr;
        while (b != U64(0))
        {
            key ^= Zob._.castle_right[0][pop_lsq (b)];
        }
    }
    // Calculate checkers bitboard (if move is check)
    _psi->checkers = give_check ? attackers_to (_piece_sq[pasive][KING][0], _active) : U64(0);
    // Switch sides
    _active = pasive;
    // Reset en-passant square
    if (_psi->en_passant_sq != SQ_NO)
    {
        key ^= Zob._.en_passant[_file (_psi->en_passant_sq)];
        _psi->en_passant_sq = SQ_NO;
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
                _psi->en_passant_sq = ep_sq;
                key ^= Zob._.en_passant[_file (ep_sq)];
            }
        }
    }
    // Update state information
    _psi->posi_key     = key;
    _psi->last_move    = m;
    _psi->capture_type = cpt;
    ++_psi->null_ply;
    ++_game_ply;
    ++_game_nodes;

    assert(ok ());
}
#undef do_capture
// do_move() do the natural-move (CAN)
void Position::do_move (const string &can, StateInfo &nsi)
{
    auto m = move_from_can (can, *this);
    if (m != MOVE_NONE)
    {
        do_move (m, nsi, gives_check (m, CheckInfo (*this)));
    }
}
// undo_move() undo the last natural-move
void Position::undo_move ()
{
    assert(_psi->ptr != nullptr);
    auto m = _psi->last_move;
    assert(_ok (m));

    auto org = org_sq (m);
    auto dst = dst_sq (m);

    _active = ~_active;

    assert(empty (org)
        || mtype (m) == CASTLE);

    assert(_psi->capture_type != KING);

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
        Square rook_org, rook_dst;
        do_castling<false> (org, dst, rook_org, rook_dst);
        //cpt  = NONE;
    }
        break;

    case ENPASSANT:
    {
        cap -= pawn_push (_active);
        assert(_board[dst] == (_active|PAWN)
            && _psi->capture_type == PAWN
            && rel_rank (_active, org) == R_5
            && rel_rank (_active, dst) == R_6
            && dst == _psi->ptr->en_passant_sq
            && empty (cap));
        move_piece (dst, org);
    }
        break;

    case PROMOTE:
    {
        assert(promote (m) == ptype (_board[dst])
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
    if (_psi->capture_type != NONE)
    {
        place_piece (cap, ~_active, _psi->capture_type);
    }

    --_game_ply;
    _psi = _psi->ptr;

    assert(ok ());
}
// do_null_move() do the null-move
void Position::do_null_move (StateInfo &nsi)
{
    assert(&nsi != _psi);
    assert(_psi->checkers == U64(0));

    // Full copy here
    std::memcpy (&nsi, _psi, StateInfo::Size);

    // Switch our state pointer to point to the new, ready to be updated, state.
    nsi.ptr = _psi;
    _psi    = &nsi;

    if (_psi->en_passant_sq != SQ_NO)
    {
        _psi->posi_key ^= Zob._.en_passant[_file (_psi->en_passant_sq)];
        _psi->en_passant_sq = SQ_NO;
    }
    _psi->posi_key ^= Zob._.act_side;
    _psi->clock_ply++;
    _psi->null_ply = 0;

    _active = ~_active;

    assert(ok ());
}
// undo_null_move() undo the last null-move
void Position::undo_null_move ()
{
    assert(_psi->ptr != nullptr);
    assert(_psi->checkers == U64(0));

    _active = ~_active;
    _psi    = _psi->ptr;

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
        flip_fen.insert (0, token + (white_spaces (flip_fen) ? " " : "/"));
    }
    // 2. Active color
    iss >> token;
    flip_fen += (token == "w" ? "B" : "W"); // Will be lowercased later
    flip_fen += " ";
    // 3. Castling availability
    iss >> token;
    flip_fen += token + " ";
    toggle (flip_fen);

    // 4. En-passant square
    iss >> token;
    flip_fen += (token == "-" ? token : token.replace (1, 1, token[1] == '3' ? "6" : token[1] == '6' ? "3" : "-"));
    // 5-6. Half and full moves
    std::getline (iss, token);
    flip_fen += token;

    setup (flip_fen, _thread, _chess960, true);

    assert(ok ());
}

string Position::fen (bool c960, bool full) const
{
    ostringstream oss;

    for (auto r = R_8; r >= R_1; --r)
    {
        for (auto f = F_A; f <= F_H; ++f)
        {
            auto s = f|r;
            auto empty_count = 0;
            while (F_H >= f && empty (s))
            {
                ++empty_count;
                ++f;
                ++s;
            }
            if (empty_count != 0) oss << empty_count;
            if (F_H >= f) oss << _board[s];
        }

        if (R_1 < r) oss << "/";
    }

    oss << " " << _active << " ";

    if (can_castle (CR_FULL))
    {
        if (_chess960 || c960)
        {
            if (can_castle (CR_WHITE))
            {
                if (can_castle (CR_WKING)) oss << to_char (_file (_castle_rook[Castling<WHITE, CS_KING>::Right]), false);
                if (can_castle (CR_WQUEN)) oss << to_char (_file (_castle_rook[Castling<WHITE, CS_QUEN>::Right]), false);
            }
            if (can_castle (CR_BLACK))
            {
                if (can_castle (CR_BKING)) oss << to_char (_file (_castle_rook[Castling<BLACK, CS_KING>::Right]), true);
                if (can_castle (CR_BQUEN)) oss << to_char (_file (_castle_rook[Castling<BLACK, CS_QUEN>::Right]), true);
            }
        }
        else
        {
            if (can_castle (CR_WHITE))
            {
                if (can_castle (CR_WKING)) oss << "K";
                if (can_castle (CR_WQUEN)) oss << "Q";
            }
            if (can_castle (CR_BLACK))
            {
                if (can_castle (CR_BKING)) oss << "k";
                if (can_castle (CR_BQUEN)) oss << "q";
            }
        }
    }
    else
    {
        oss << "-";
    }

    oss << " " << (_psi->en_passant_sq == SQ_NO ? "-" : to_string (_psi->en_passant_sq)) << " ";

    if (full) oss << i16(_psi->clock_ply) << " " << game_move ();

    return oss.str ();
}

// string() returns an ASCII representation of the position to be
// printed to the standard output
Position::operator string () const
{
    const string EDGE  = " +---+---+---+---+---+---+---+---+\n";
    const string ROW_1 = "| . |   | . |   | . |   | . |   |\n" + EDGE;
    const string ROW_2 = "|   | . |   | . |   | . |   | . |\n" + EDGE;

    auto board = EDGE;

    for (auto r = R_8; r >= R_1; --r)
    {
        board += to_char (r) + ((r % 2) != 0 ? ROW_1 : ROW_2);
    }
    for (auto f = F_A; f <= F_H; ++f)
    {
        board += "   ";
        board += to_char (f, false);
    }

    auto occ = _types_bb[NONE];
    while (occ != U64(0))
    {
        auto s = pop_lsq (occ);
        board[3 + i32((ROW_1.length () + 1) * (7.5 - _rank (s)) + 4 * _file (s))] = PieceChar[_board[s]];
    }

    ostringstream oss;

    oss << board << "\n\n";

    oss << "FEN: " << fen () << "\n"
        << "Key: " << std::setfill ('0') << std::hex << std::uppercase << std::setw (16)
        << _psi->posi_key << std::nouppercase << std::dec << std::setfill (' ') << "\n";

    oss << "Checkers: ";
    auto chkrs = _psi->checkers;
    if (chkrs != U64(0))
    {
        while (chkrs != U64(0))
        {
            auto chk_sq = pop_lsq (chkrs);
            oss << (WHITE|ptype (_board[chk_sq])) << chk_sq << " ";
        }
    }
    else
    {
        oss << "<none>";
    }

    return oss.str ();
}
