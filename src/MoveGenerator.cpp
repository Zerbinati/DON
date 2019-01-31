#include "MoveGenerator.h"

#include <iostream>
#include "BitBoard.h"
#include "Notation.h"
#include "Thread.h"

using namespace std;
using namespace BitBoard;

namespace {

    /// Generates piece normal move
    template<GenType GT, PieceType PT>
    void generate_piece_moves (ValMoves &moves, const Position &pos, Bitboard targets)
    {
        static_assert (NIHT == PT
                    || BSHP == PT
                    || ROOK == PT
                    || QUEN == PT, "PT incorrect");
        switch (GT)
        {
        case GenType::CHECK:
        case GenType::QUIET_CHECK:
            targets &= pos.si->checks[PT];
            /* no break */
        case GenType::NATURAL:
        case GenType::EVASION:
        case GenType::CAPTURE:
        case GenType::QUIET:
            for (const auto &s : pos.squares[pos.active][PT])
            {
                if (   (   GenType::CHECK == GT
                        || GenType::QUIET_CHECK == GT)
                    && contains (pos.si->king_blockers[~pos.active], s))
                {
                    continue;
                }
                Bitboard attacks = targets
                                 & attacks_bb<PT> (s, pos.pieces ());
                while (0 != attacks) { moves += mk_move<NORMAL> (s, pop_lsq (attacks)); }
            }
            break;
        default: break;
        }
    }

    /// Generates pawn promotion move
    template<GenType GT>
    void generate_promotion_moves (ValMoves &moves, const Position &pos, Bitboard promotion, Delta del)
    {
        while (0 != promotion)
        {
            auto dst = pop_lsq (promotion);

            switch (GT)
            {
            case GenType::NATURAL:
            case GenType::EVASION:
                moves += mk_move<PROMOTE> (dst - del, dst, QUEN);
                /* no break */
            case GenType::QUIET:
                moves += mk_move<PROMOTE> (dst - del, dst, ROOK);
                moves += mk_move<PROMOTE> (dst - del, dst, BSHP);
                moves += mk_move<PROMOTE> (dst - del, dst, NIHT);
                break;
            case GenType::CAPTURE:
                moves += mk_move<PROMOTE> (dst - del, dst, QUEN);
                break;
            case GenType::CHECK:
                if (contains (attacks_bb<QUEN> (dst, pos.pieces () ^ (dst - del)), pos.square<KING> (~pos.active)))
                {
                    moves += mk_move<PROMOTE> (dst - del, dst, QUEN);
                }
                if (contains (attacks_bb<ROOK> (dst, pos.pieces () ^ (dst - del)), pos.square<KING> (~pos.active)))
                {
                    moves += mk_move<PROMOTE> (dst - del, dst, ROOK);
                }
                if (contains (attacks_bb<BSHP> (dst, pos.pieces () ^ (dst - del)), pos.square<KING> (~pos.active)))
                {
                    moves += mk_move<PROMOTE> (dst - del, dst, BSHP);
                }
                /* no break */
            case GenType::QUIET_CHECK:
                if (contains (PieceAttacks[NIHT][dst], pos.square<KING> (~pos.active)))
                {
                    moves += mk_move<PROMOTE> (dst - del, dst, NIHT);
                }
                break;
            default: assert(false); break;
            }
        }
    }
    /// Generates pawn normal move
    template<GenType GT>
    void generate_pawn_moves (ValMoves &moves, const Position &pos, Bitboard targets)
    {
        Bitboard empties = ~pos.pieces ();
        Bitboard enemies =  pos.pieces (~pos.active) & targets;

        Bitboard pawns = pos.pieces (pos.active, PAWN);
        Bitboard Rank7 = rank_bb (rel_rank (pos.active, R_7));
        // Pawns not on 7th Rank
        Bitboard Rx_pawns = pawns & ~Rank7;
        // Pawns on 7th Rank
        Bitboard R7_pawns = pawns &  Rank7;
        switch (GT)
        {
        case GenType::NATURAL:
        case GenType::EVASION:
        case GenType::CAPTURE:
        case GenType::CHECK:
        {
            // Pawn normal and en-passant captures, no promotions
            Bitboard l_attacks = enemies & pawn_l_attacks_bb (pos.active, Rx_pawns);
            Bitboard r_attacks = enemies & pawn_r_attacks_bb (pos.active, Rx_pawns);
            if (GenType::CHECK == GT)
            {
                l_attacks &= pos.si->checks[PAWN];
                r_attacks &= pos.si->checks[PAWN];
                // Pawns which give discovered check
                // Add pawn captures which give discovered check.
                Bitboard dsc_pawns = Rx_pawns
                                   & pos.si->king_blockers[~pos.active];
                if (0 != dsc_pawns)
                {
                    l_attacks |= enemies & pawn_l_attacks_bb (pos.active, dsc_pawns);
                    r_attacks |= enemies & pawn_r_attacks_bb (pos.active, dsc_pawns);
                }
            }
            while (0 != l_attacks) { auto dst = pop_lsq (l_attacks); moves += mk_move<NORMAL> (dst - pawn_l_att (pos.active), dst); }
            while (0 != r_attacks) { auto dst = pop_lsq (r_attacks); moves += mk_move<NORMAL> (dst - pawn_r_att (pos.active), dst); }

            if (SQ_NO != pos.si->enpassant_sq)
            {
                assert(R_6 == rel_rank (pos.active, pos.si->enpassant_sq));
                Bitboard ep_captures = Rx_pawns
                                     & PawnAttacks[~pos.active][pos.si->enpassant_sq];
                switch (GT)
                {
                case GenType::EVASION:
                    // If the checking piece is the double pushed pawn and also is in the target.
                    // Otherwise this is a discovery check and are forced to do otherwise.
                    if (!contains (enemies & pos.pieces (PAWN), pos.si->enpassant_sq - pawn_push (pos.active)))
                    {
                        ep_captures = 0;
                    }
                    break;
                case GenType::CHECK:
                    if (//!contains (PawnAttacks[pos.active][pos.si->enpassant_sq], pos.square<KING> (~pos.active))
                        !contains (pos.si->checks[PAWN], pos.si->enpassant_sq))
                    {
                        // En-passant pawns which give discovered check
                        ep_captures &= pos.si->king_blockers[~pos.active];
                    }
                    break;
                default: break;
                }
                assert(2 >= pop_count (ep_captures));
                while (0 != ep_captures) { moves += mk_move<ENPASSANT> (pop_lsq (ep_captures), pos.si->enpassant_sq); }
            }
        }
            /* no break */
        case GenType::QUIET:
        case GenType::QUIET_CHECK:
        {
            // Promotions (queening and under-promotions)
            if (0 != R7_pawns)
            {
                Bitboard b;

                b = enemies & pawn_l_attacks_bb (pos.active, R7_pawns);
                generate_promotion_moves<GT> (moves, pos, b, pawn_l_att (pos.active));
                b = enemies & pawn_r_attacks_bb (pos.active, R7_pawns);
                generate_promotion_moves<GT> (moves, pos, b, pawn_r_att (pos.active));
                b = empties & pawn_pushes_bb (pos.active, R7_pawns);
                if (GenType::EVASION == GT)
                {
                    b &= targets;
                }
                generate_promotion_moves<GT> (moves, pos, b, pawn_push (pos.active));
            }
            
            if (GenType::CAPTURE == GT)
            {
                break;
            }

            // Pawn single-push and double-push, no promotions
            Bitboard pushs_1 = empties & pawn_pushes_bb (pos.active, Rx_pawns);
            Bitboard pushs_2 = empties & pawn_pushes_bb (pos.active, pushs_1 & rank_bb (rel_rank (pos.active, R_3)));
            switch (GT)
            {
            case GenType::EVASION:
                pushs_1 &= targets;
                pushs_2 &= targets;
                break;
            case GenType::CHECK:
            case GenType::QUIET_CHECK:
            {
                pushs_1 &= pos.si->checks[PAWN];
                pushs_2 &= pos.si->checks[PAWN];
                // Pawns which give discovered check
                // Add pawn pushes which give discovered check.
                // This is possible only if the pawn is not on the same file as the enemy king, because don't generate captures.
                // Note that a possible discovery check promotion has been already generated among captures.
                Bitboard dsc_pawns = Rx_pawns
                                   & pos.si->king_blockers[~pos.active]
                                   & ~file_bb (pos.square<KING> (~pos.active));
                if (0 != dsc_pawns)
                {
                    Bitboard dc_pushs_1 = empties & pawn_pushes_bb (pos.active, dsc_pawns);
                    Bitboard dc_pushs_2 = empties & pawn_pushes_bb (pos.active, dc_pushs_1 & rank_bb (rel_rank (pos.active, R_3)));
                    pushs_1 |= dc_pushs_1;
                    pushs_2 |= dc_pushs_2;
                }
            }
                break;
            default: break;
            }
            while (0 != pushs_1) { auto dst = pop_lsq (pushs_1); moves += mk_move<NORMAL> (dst - pawn_push (pos.active)  , dst); }
            while (0 != pushs_2) { auto dst = pop_lsq (pushs_2); moves += mk_move<NORMAL> (dst - pawn_push (pos.active)*2, dst); }
        }
            break;
        default: assert(false); break;
        }
    }

    /// Generates king normal move
    template<GenType GT>
    void generate_king_moves (ValMoves &moves, const Position &pos, Bitboard targets)
    {
        Bitboard attacks =  targets
                         &  PieceAttacks[KING][pos.square<KING> ( pos.active)]
                         & ~PieceAttacks[KING][pos.square<KING> (~pos.active)];
        while (0 != attacks) { moves += mk_move<NORMAL> (pos.square<KING> (pos.active), pop_lsq (attacks)); }

        if (   (   GenType::NATURAL == GT
                || GenType::QUIET == GT)
            //&& 0 == pos.si->checkers
            //&& R_1 == rel_rank (pos.active, pos.square<KING> (pos.active))
            && pos.si->can_castle (pos.active))
        {
            if (   pos.expeded_castle (pos.active, CS_KING)
                && pos.si->can_castle (pos.active, CS_KING))
            {
                moves += mk_move<CASTLE> (pos.square<KING> (pos.active), pos.castle_rook_sq[pos.active][CS_KING]);
            }
            if (   pos.expeded_castle (pos.active, CS_QUEN)
                && pos.si->can_castle (pos.active, CS_QUEN))
            {
                moves += mk_move<CASTLE> (pos.square<KING> (pos.active), pos.castle_rook_sq[pos.active][CS_QUEN]);
            }
        }
    }


    /// Generates all pseudo-legal moves of color for targets.
    template<GenType GT>
    void generate_moves (ValMoves &moves, const Position &pos, Bitboard targets)
    {
        generate_pawn_moves<GT> (moves, pos, targets);
        generate_piece_moves<GT, NIHT> (moves, pos, targets);
        generate_piece_moves<GT, BSHP> (moves, pos, targets);
        generate_piece_moves<GT, ROOK> (moves, pos, targets);
        generate_piece_moves<GT, QUEN> (moves, pos, targets);
    }
}

template<GenType GT>
void generate (ValMoves &moves, const Position &pos)
{
    assert(0 == pos.si->checkers);
    static_assert (GenType::NATURAL == GT
                || GenType::CAPTURE == GT
                || GenType::QUIET == GT, "GT incorrect");
    moves.clear ();
    Bitboard targets;
    switch (GT)
    {
    case GenType::NATURAL: targets = ~pos.pieces (pos.active); break;
    case GenType::CAPTURE: targets =  pos.pieces (~pos.active); break;
    case GenType::QUIET:   targets = ~pos.pieces (); break;
    default: assert(false);targets = 0;
    }
    generate_moves<GT> (moves, pos, targets);
    generate_king_moves<GT> (moves, pos, targets);
}

/// Explicit template instantiations
/// --------------------------------
/// generate<NATURAL> generates all pseudo-legal captures and non-captures.
template void generate<GenType::NATURAL> (ValMoves&, const Position&);
/// generate<CAPTURE> generates all pseudo-legal captures and queen promotions.
template void generate<GenType::CAPTURE> (ValMoves&, const Position&);
/// generate<QUIET> generates all pseudo-legal non-captures and underpromotions.
template void generate<GenType::QUIET  > (ValMoves&, const Position&);

/// Generates all pseudo-legal check evasions moves when the side to move is in check.
template<> void generate<GenType::EVASION    > (ValMoves &moves, const Position &pos)
{
    assert(0 != pos.si->checkers);
    moves.clear ();
    auto check_sq = SQ_NO;
    Bitboard check_attacks = 0;

    Bitboard mocc = pos.pieces () ^ pos.square<KING> (pos.active);
    Bitboard checkers = pos.si->checkers & ~pos.pieces (PAWN);
    // Squares attacked by checkers will remove them from the king evasions
    // so to skip known illegal moves avoiding useless legality check later.
    while (0 != checkers)
    {
        check_sq = pop_lsq (checkers);
        assert(color (pos[check_sq]) == ~pos.active);
        switch (ptype (pos[check_sq]))
        {
        case NIHT: check_attacks |= PieceAttacks[NIHT][check_sq]; break;
        case BSHP: check_attacks |= attacks_bb<BSHP> (check_sq, mocc); break;
        case ROOK: check_attacks |= attacks_bb<ROOK> (check_sq, mocc); break;
        case QUEN: check_attacks |= attacks_bb<QUEN> (check_sq, mocc); break;
        default: assert(false); check_attacks |= 0; break;
        }
    }

    Bitboard targets;
    targets = ~check_attacks
            & ~pos.pieces (pos.active);
    // Generate evasions for king, capture and non capture moves
    Bitboard attacks =  targets
                     &  PieceAttacks[KING][pos.square<KING> ( pos.active)]
                     & ~PieceAttacks[KING][pos.square<KING> (~pos.active)];
    while (0 != attacks) { moves += mk_move<NORMAL> (pos.square<KING> (pos.active), pop_lsq (attacks)); }

    // If double-check or only king, then only king move can save the day
    if (   more_than_one (pos.si->checkers)
        || 1 == pos.count (pos.active))
    {
        return;
    }

    // Generates blocking or captures of the checking piece
    targets = SQ_NO == check_sq ?
                square_bb (scan_lsq (pos.si->checkers)) :
                between_bb (check_sq, pos.square<KING> (pos.active)) | check_sq;

    generate_moves<GenType::EVASION> (moves, pos, targets);
}
/// Generates all pseudo-legal check giving moves.
template<> void generate<GenType::CHECK      > (ValMoves &moves, const Position &pos)
{
    assert(0 == pos.si->checkers);
    moves.clear ();
    Bitboard targets = ~pos.pieces (pos.active);
    // Pawns is excluded, will be generated together with direct checks
    Bitboard ex_dsc_blockers =  pos.si->king_blockers[~pos.active]
                             &  pos.pieces (pos.active)
                             & ~pos.pieces (PAWN);
    assert(0 == (ex_dsc_blockers & pos.pieces (pos.active, QUEN)));
    while (0 != ex_dsc_blockers)
    {
        auto org = pop_lsq (ex_dsc_blockers);
        Bitboard attacks;
        switch (ptype (pos[org]))
        {
        case NIHT: attacks = PieceAttacks[NIHT][org]; break;
        case BSHP: attacks = attacks_bb<BSHP> (org, pos.pieces ()); break;
        case ROOK: attacks = attacks_bb<ROOK> (org, pos.pieces ()); break;
        //case QUEN: attacks = attacks_bb<QUEN> (org, pos.pieces ()); break;
        case KING: attacks = PieceAttacks[KING][org] & ~PieceAttacks[QUEN][pos.square<KING> (~pos.active)]; break;
        default: assert(false); attacks = 0; break;
        }
        attacks &= targets;
        while (0 != attacks) { moves += mk_move<NORMAL> (org, pop_lsq (attacks)); }
    }

    generate_moves<GenType::CHECK> (moves, pos, targets);
}
/// Generates all pseudo-legal non-captures and knight under promotions moves that give check.
template<> void generate<GenType::QUIET_CHECK> (ValMoves &moves, const Position &pos)
{
    assert(0 == pos.si->checkers);
    moves.clear ();
    Bitboard targets = ~pos.pieces ();
    // Pawns is excluded, will be generated together with direct checks
    Bitboard ex_dsc_blockers =  pos.si->king_blockers[~pos.active]
                             &  pos.pieces (pos.active)
                             & ~pos.pieces (PAWN);
    assert(0 == (ex_dsc_blockers & pos.pieces (pos.active, QUEN)));
    while (0 != ex_dsc_blockers)
    {
        auto org = pop_lsq (ex_dsc_blockers);
        Bitboard attacks;
        switch (ptype (pos[org]))
        {
        case NIHT: attacks = PieceAttacks[NIHT][org]; break;
        case BSHP: attacks = attacks_bb<BSHP> (org, pos.pieces ()); break;
        case ROOK: attacks = attacks_bb<ROOK> (org, pos.pieces ()); break;
        //case QUEN: attacks = attacks_bb<QUEN> (org, pos.pieces ()); break;
        case KING: attacks = PieceAttacks[KING][org] & ~PieceAttacks[QUEN][pos.square<KING> (~pos.active)]; break;
        default: assert(false); attacks = 0; break;
        }
        attacks &= targets;
        while (0 != attacks) { moves += mk_move<NORMAL> (org, pop_lsq (attacks)); }
    }

    generate_moves<GenType::QUIET_CHECK> (moves, pos, targets);
}

/// Generates all legal moves.
template<> void generate<GenType::LEGAL      > (ValMoves &moves, const Position &pos)
{
    0 == pos.si->checkers ?
        generate<GenType::NATURAL> (moves, pos) :
        generate<GenType::EVASION> (moves, pos);
    filter_illegal (moves, pos);
}

/// Filter illegal moves
void filter_illegal (ValMoves &moves, const Position &pos)
{
    moves.erase (std::remove_if (moves.begin (),
                                 moves.end (),
                                 [&] (const ValMove &vm)
                                 {
                                     return (   pos.enpassant (vm)
                                             || contains (pos.si->king_blockers[pos.active] | pos.pieces (pos.active, KING), org_sq (vm)))
                                         && !pos.legal (vm);
                                 }),
                 moves.end ());
}

/// perft() is utility to verify move generation.
/// All the leaf nodes up to the given depth are generated, and the sum is returned.
template<bool RootNode>
Perft perft (Position &pos, i16 depth)
{
    Perft total;
    if (RootNode)
    {
        sync_cout << std::left
                  << std::setw (3)
                  << "N"
                  << std::setw (16)
                  << "Move"
                  << std::setw (19)
                  << "All"
                  << std::setw (19)
                  << "Capture"
                  << std::setw (19)
                  << "EP"
                  << std::setw (19)
                  << "AnyCheck"
                  << std::setw (19)
                  << "DscCheck"
                  << std::setw (19)
                  //<< "DblCheck"
                  //<< std::setw (19)
                  << "Castle"
                  << std::setw (19)
                  << "Promote"
                  << sync_endl;
    }

    for (const auto &vm : MoveList<GenType::LEGAL> (pos))
    {
        Perft leaf;
        if (   RootNode
            && 1 >= depth)
        {
            leaf.all = 1;
            if (contains (pos.pieces (~pos.active), dst_sq (vm)))
            {
                leaf.capture = 1;
            }
            if (ENPASSANT == mtype (vm))
            {
                leaf.capture = 1;
                leaf.enpassant = 1;
            }
            if (pos.gives_check (vm))
            {
                leaf.any_check = 1;
                if (!contains (pos.si->checks[ptype (pos[org_sq (vm)])], dst_sq (vm)))
                {
                    Bitboard mocc;
                    if (   (   contains (pos.si->king_blockers[~pos.active], org_sq (vm))
                            && !sqrs_aligned (org_sq (vm), dst_sq (vm), pos.square<KING> (~pos.active)))
                        || (   ENPASSANT == mtype (vm)
                            && 0 != (mocc = (pos.pieces () ^ org_sq (vm) ^ (_file (dst_sq (vm)) | _rank (org_sq (vm)))) | dst_sq (vm))
                            && (   (   0 != (pos.pieces (pos.active, BSHP, QUEN) & PieceAttacks[BSHP][pos.square<KING> (~pos.active)])
                                    && 0 != (pos.pieces (pos.active, BSHP, QUEN) & attacks_bb<BSHP> (pos.square<KING> (~pos.active), mocc)))
                                || (   0 != (pos.pieces (pos.active, ROOK, QUEN) & PieceAttacks[ROOK][pos.square<KING> (~pos.active)])
                                    && 0 != (pos.pieces (pos.active, ROOK, QUEN) & attacks_bb<ROOK> (pos.square<KING> (~pos.active), mocc))))))
                    {
                        leaf.dsc_check = 1;
                    }
                }
                //Bitboard mocc = (pos.pieces () ^ org_sq (vm)) | dst_sq (vm);
                //if (more_than_one (pos.attackers_to (pos.square<KING> (~pos.active), pos.active, mocc)))
                //{
                //    leaf.dbl_check = 1;
                //}
            }
            if (CASTLE == mtype (vm))
            {
                leaf.castle = 1;
            }
            if (PROMOTE == mtype (vm))
            {
                leaf.promote = 1;
            }
        }
        else
        {
            StateInfo si;
            pos.do_move (vm, si);

            if (2 >= depth)
            {
                for (const auto &ivm : MoveList<GenType::LEGAL> (pos))
                {
                    ++leaf.all;
                    if (contains (pos.pieces (~pos.active), dst_sq (ivm)))
                    {
                        ++leaf.capture;
                    }
                    if (ENPASSANT == mtype (ivm))
                    {
                        ++leaf.capture;
                        ++leaf.enpassant;
                    }
                    if (pos.gives_check (ivm))
                    {
                        ++leaf.any_check;
                        if (!contains (pos.si->checks[ptype (pos[org_sq (ivm)])], dst_sq (ivm)))
                        {
                            Bitboard mocc;
                            if (   (   contains (pos.si->king_blockers[~pos.active], org_sq (ivm))
                                    && !sqrs_aligned (org_sq (ivm), dst_sq (ivm), pos.square<KING> (~pos.active)))
                                || (   ENPASSANT == mtype (ivm)
                                    && 0 != (mocc = (pos.pieces () ^ org_sq (ivm) ^ (_file (dst_sq (ivm)) | _rank (org_sq (ivm)))) | dst_sq (ivm))
                                    && (   (   0 != (pos.pieces (pos.active, BSHP, QUEN) & PieceAttacks[BSHP][pos.square<KING> (~pos.active)])
                                            && 0 != (pos.pieces (pos.active, BSHP, QUEN) & attacks_bb<BSHP> (pos.square<KING> (~pos.active), mocc)))
                                        || (   0 != (pos.pieces (pos.active, ROOK, QUEN) & PieceAttacks[ROOK][pos.square<KING> (~pos.active)])
                                            && 0 != (pos.pieces (pos.active, ROOK, QUEN) & attacks_bb<ROOK> (pos.square<KING> (~pos.active), mocc))))))
                            {
                                ++leaf.dsc_check;
                            }
                        }
                        //Bitboard mocc = (pos.pieces () ^ org_sq (ivm)) | dst_sq (ivm);
                        //if (more_than_one (pos.attackers_to (pos.square<KING> (~pos.active), pos.active, mocc)))
                        //{
                        //    ++leaf.dbl_check;
                        //}
                    }
                    if (CASTLE == mtype (ivm))
                    {
                        ++leaf.castle;
                    }
                    if (PROMOTE == mtype (ivm))
                    {
                        ++leaf.promote;
                    }
                }
            }
            else
            {
                leaf = perft<false> (pos, depth - 1);
            }

            pos.undo_move (vm);
        }

        if (RootNode)
        {
            sync_cout << std::right
                      << std::setfill ('0')
                      << std::setw (2)
                      << ++total.moves
                      << " "
                      << std::left
                      << std::setfill (' ')
                      << std::setw (7)
                      <<
                         //move_to_can (vm)
                         move_to_san (vm, pos)
                      << std::right
                      << std::setfill ('.')
                      << std::setw (16)
                      << leaf.all
                      << "   "
                      << std::setw (16)
                      << leaf.capture
                      << "   "
                      << std::setw (16)
                      << leaf.enpassant
                      << "   "
                      << std::setw (16)
                      << leaf.any_check
                      << "   "
                      << std::setw (16)
                      << leaf.dsc_check
                      //<< "   "
                      //<< std::setw (16)
                      //<< leaf.dbl_check
                      << "   "
                      << std::setw (16)
                      << leaf.castle
                      << "   "
                      << std::setw (16)
                      << leaf.promote
                      << std::setfill (' ')
                      << std::left << sync_endl;
        }
        total += leaf;
    }

    if (RootNode)
    {
        sync_cout << std::endl
                  << "Total:  "
                  << std::right
                  << std::setfill ('.')
                  << std::setw (18)
                  << total.all
                  << " "
                  << std::setw (18)
                  << total.capture
                  << " "
                  << std::setw (18)
                  << total.enpassant
                  << " "
                  << std::setw (18)
                  << total.any_check
                  << " "
                  << std::setw (18)
                  << total.dsc_check
                  //<< " "
                  //<< std::setw (18)
                  //<< total.dbl_check
                  << " "
                  << std::setw (18)
                  << total.castle
                  << " "
                  << std::setw (18)
                  << total.promote
                  << std::setfill (' ')
                  << std::left
                  << std::endl
                  << sync_endl;
    }
    return total;
}
/// Explicit template instantiations
/// --------------------------------
template Perft perft<true > (Position&, i16);
template Perft perft<false> (Position&, i16);
