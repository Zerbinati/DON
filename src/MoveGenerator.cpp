#include "MoveGenerator.h"

#include <iostream>
#include "BitBoard.h"
#include "Notation.h"
#include "Thread.h"

using namespace std;
using namespace BitBoard;

namespace {

    /// Generates piece normal move
    template<GenType GT, Color Own, PieceType PT>
    void generate_piece_moves (ValMoves &moves, const Position &pos, Bitboard targets)
    {
        static_assert (NIHT == PT
                    || BSHP == PT
                    || ROOK == PT
                    || QUEN == PT, "PT incorrect");

        if (   GenType::CHECK == GT
            || GenType::QUIET_CHECK == GT)
        {
            targets &= pos.si->checks[PT];
        }
        if (0 != targets)
        {
            for (auto s : pos.squares[Own][PT])
            {
                if (   (   GenType::CHECK == GT
                        || GenType::QUIET_CHECK == GT)
                    && contains (pos.si->king_blockers[~pos.active], s))
                {
                    continue;
                }
                Bitboard attacks = targets
                                    & (NIHT == PT ? PieceAttacks[NIHT][s] :
                                    BSHP == PT ? attacks_bb<BSHP> (s, pos.pieces ()) :
                                    ROOK == PT ? attacks_bb<ROOK> (s, pos.pieces ()) :
                                    QUEN == PT ? attacks_bb<QUEN> (s, pos.pieces ()) : (assert(false), 0));
                while (0 != attacks) { moves += mk_move<NORMAL> (s, pop_lsq (attacks)); }
            }
        }
    }

    /// Generates pawn promotion move
    template<GenType GT, Delta Del>
    void generate_promotion_moves (ValMoves &moves, const Position &pos, Square dst)
    {
        static_assert (DEL_N  == Del
                    || DEL_NE == Del
                    || DEL_NW == Del
                    || DEL_S  == Del
                    || DEL_SE == Del
                    || DEL_SW == Del, "Del incorrect");

        if (   GenType::NATURAL == GT
            || GenType::EVASION == GT
            || GenType::CAPTURE == GT
            || (   GenType::CHECK == GT
                && contains (attacks_bb<QUEN> (dst, pos.pieces () ^ (dst - Del)), pos.square<KING> (~pos.active))))
        {
            moves += mk_move (dst - Del, dst, QUEN);
        }
        if (   GenType::NATURAL == GT
            || GenType::EVASION == GT
            || GenType::QUIET == GT
            || (   GenType::CHECK == GT
                && contains (attacks_bb<ROOK> (dst, pos.pieces () ^ (dst - Del)), pos.square<KING> (~pos.active))))
        {
            moves += mk_move (dst - Del, dst, ROOK);
        }
        if (   GenType::NATURAL == GT
            || GenType::EVASION == GT
            || GenType::QUIET == GT
            || (   GenType::CHECK == GT
                && contains (attacks_bb<BSHP> (dst, pos.pieces () ^ (dst - Del)), pos.square<KING> (~pos.active))))
        {
            moves += mk_move (dst - Del, dst, BSHP);
        }
        if (   GenType::NATURAL == GT
            || GenType::EVASION == GT
            || GenType::QUIET == GT
            || (   (   GenType::CHECK == GT
                    || GenType::QUIET_CHECK == GT)
                && contains (PieceAttacks[NIHT][dst], pos.square<KING> (~pos.active))))
        {
            moves += mk_move (dst - Del, dst, NIHT);
        }
    }
    /// Generates pawn normal move
    template<GenType GT, Color Own>
    void generate_pawn_moves (ValMoves &moves, const Position &pos, Bitboard targets)
    {
        constexpr auto Opp = WHITE == Own ? BLACK : WHITE;
        constexpr auto Push = WHITE == Own ? DEL_N : DEL_S;
        constexpr auto LCap = WHITE == Own ? DEL_NW : DEL_SE;
        constexpr auto RCap = WHITE == Own ? DEL_NE : DEL_SW;

        // Pawns on 7th Rank
        Bitboard R7_pawns = pos.pieces (Own, PAWN) &  rank_bb (WHITE == Own ? R_7 : R_2);
        // Pawns not on 7th Rank
        Bitboard Rx_pawns = pos.pieces (Own, PAWN) & ~rank_bb (WHITE == Own ? R_7 : R_2);

        Bitboard empties = ~pos.pieces ();
        Bitboard enemies =  pos.pieces (Opp) & targets;
        // Pawn single-push and double-push, no promotions
        if (   GenType::NATURAL == GT
            || GenType::EVASION == GT
            || GenType::QUIET == GT
            || GenType::CHECK == GT
            || GenType::QUIET_CHECK == GT)
        {
            Bitboard push_1 = empties & shift<Push> (Rx_pawns);
            Bitboard push_2 = empties & shift<Push> (push_1 & rank_bb (WHITE == Own ? R_3 : R_6));
            if (   GenType::CHECK == GT
                || GenType::QUIET_CHECK == GT)
            {
                push_1 &= pos.si->checks[PAWN];
                push_2 &= pos.si->checks[PAWN];
                // Pawns which give discovered check
                // Add pawn pushes which give discovered check.
                // This is possible only if the pawn is not on the same file as the enemy king, because don't generate captures.
                // Note that a possible discovery check promotion has been already generated among captures.
                Bitboard dsc_pawns = Rx_pawns & pos.si->king_blockers[~pos.active] & ~file_bb (pos.square<KING> (Opp));
                if (0 != dsc_pawns)
                {
                    Bitboard dc_push_1 = empties & shift<Push> (dsc_pawns);
                    Bitboard dc_push_2 = empties & shift<Push> (dc_push_1 & rank_bb (WHITE == Own ? R_3 : R_6));
                    push_1 |= dc_push_1;
                    push_2 |= dc_push_2;
                }
            }
            push_1 &= targets;
            push_2 &= targets;
            while (0 != push_1) { auto dst = pop_lsq (push_1); moves += mk_move<NORMAL> (dst - Push  , dst); }
            while (0 != push_2) { auto dst = pop_lsq (push_2); moves += mk_move<NORMAL> (dst - Push*2, dst); }
        }
        // Pawn normal and en-passant captures, no promotions
        if (   GenType::NATURAL == GT
            || GenType::EVASION == GT
            || GenType::CAPTURE == GT
            || GenType::CHECK == GT)
        {
            Bitboard l_attack = enemies & shift<LCap> (Rx_pawns);
            Bitboard r_attack = enemies & shift<RCap> (Rx_pawns);
            if (GenType::CHECK == GT)
            {
                l_attack &= pos.si->checks[PAWN];
                r_attack &= pos.si->checks[PAWN];
                // Pawns which give discovered check
                // Add pawn captures which give discovered check.
                Bitboard dsc_pawns = Rx_pawns & pos.si->king_blockers[~pos.active];
                if (0 != dsc_pawns)
                {
                    l_attack |= enemies & shift<LCap> (dsc_pawns);
                    r_attack |= enemies & shift<RCap> (dsc_pawns);
                }
            }
            while (0 != l_attack) { auto dst = pop_lsq (l_attack); moves += mk_move<NORMAL> (dst - LCap, dst); }
            while (0 != r_attack) { auto dst = pop_lsq (r_attack); moves += mk_move<NORMAL> (dst - RCap, dst); }

            if (SQ_NO != pos.si->en_passant_sq)
            {
                assert(R_6 == rel_rank (Own, pos.si->en_passant_sq));
                Bitboard ep_captures = Rx_pawns & rank_bb (WHITE == Own ? R_5 : R_4);
                if (0 != ep_captures)
                {
                    // If the checking piece is the double pushed pawn and also is in the target.
                    // Otherwise this is a discovery check and are forced to do otherwise.
                    if (GenType::EVASION == GT)
                    {
                        ep_captures &= (  shift<DEL_E> (targets)
                                        | shift<DEL_W> (targets));
                    }
                    ep_captures &= PawnAttacks[Opp][pos.si->en_passant_sq];
                    assert(0 != ep_captures
                        && 2 >= pop_count (ep_captures));
                    while (0 != ep_captures) { moves += mk_move<ENPASSANT> (pop_lsq (ep_captures), pos.si->en_passant_sq); }
                }
            }
        }

        // Promotions (queening and under-promotions)
        if (0 != R7_pawns)
        {
            if (GenType::EVASION == GT)
            {
                empties &= targets;
            }
            // Promoting pawns
            Bitboard proms;
            proms = empties & shift<Push> (R7_pawns);
            while (0 != proms)
            {
                generate_promotion_moves<GT, Push> (moves, pos, pop_lsq (proms));
            }
            proms = enemies & shift<LCap> (R7_pawns);
            while (0 != proms)
            {
                generate_promotion_moves<GT, LCap> (moves, pos, pop_lsq (proms));
            }
            proms = enemies & shift<RCap> (R7_pawns);
            while (0 != proms)
            {
                generate_promotion_moves<GT, RCap> (moves, pos, pop_lsq (proms));
            }
        }
    }

    /// Generates king castling move
    template<GenType GT, Color Own, CastleSide CS>
    void generate_castling_moves (ValMoves &moves, const Position &pos)
    {
        constexpr auto Opp = WHITE == Own ? BLACK : WHITE;

        assert(GenType::EVASION != GT
            && pos.si->can_castle (Own, CS)
            && pos.expeded_castle (Own, CS)
            && 0 == pos.si->checkers);

        auto king_org = pos.square<KING> (Own);
        auto rook_org = pos.castle_rook[Own][CS];
        assert(contains (pos.pieces (Own, ROOK), rook_org));

        Bitboard b = pos.king_path[Own][CS];
        // Check king's path for attackers
        while (0 != b)
        {
            if (0 != pos.attackers_to (pop_lsq (b), Opp))
            {
                return;
            }
        }
        auto king_dst = rel_sq (Own, rook_org > king_org ? SQ_G1 : SQ_C1);
        // Chess960
        // Because generate only legal castling moves needed to verify that
        // when moving the castling rook do not discover some hidden checker.
        // For instance an enemy queen in SQ_A1 when castling rook is in SQ_B1.
        if (   0 != (b = pos.pieces (Opp, ROOK, QUEN) & rank_bb (king_dst))
            && 0 != (b & attacks_bb<ROOK> (king_dst, pos.pieces () ^ rook_org)))
        {
            return;
        }

        auto m = mk_move<CASTLE> (king_org, rook_org);
        if (   GenType::NATURAL == GT
            || GenType::QUIET == GT
            || (   (   GenType::CHECK == GT
                    || GenType::QUIET_CHECK == GT)
                && pos.gives_check (m)))
        {
            moves += m;
        }
    }
    /// Generates king normal move
    template<GenType GT, Color Own>
    void generate_king_moves (ValMoves &moves, const Position &pos, Bitboard targets)
    {
        constexpr auto Opp = WHITE == Own ? BLACK : WHITE;

        assert(GenType::EVASION != GT);

        if (   GenType::NATURAL == GT
            || GenType::CAPTURE == GT
            || GenType::QUIET == GT)
        {
            auto fk_sq = pos.square<KING> (Own);
            Bitboard attacks = targets
                             &  PieceAttacks[KING][fk_sq]
                             & ~PieceAttacks[KING][pos.square<KING> (Opp)];
            while (0 != attacks) { moves += mk_move<NORMAL> (fk_sq, pop_lsq (attacks)); }
        }

        if (   (   GenType::NATURAL == GT
                || GenType::QUIET == GT
                || GenType::CHECK == GT
                || GenType::QUIET_CHECK == GT)
            && 0 == pos.si->checkers
            && pos.si->can_castle (Own))
        {
            if (   pos.expeded_castle (Own, CS_KING)
                && pos.si->can_castle (Own, CS_KING))
            {
                generate_castling_moves<GT, Own, CS_KING> (moves, pos);
            }
            if (   pos.expeded_castle (Own, CS_QUEN)
                && pos.si->can_castle (Own, CS_QUEN))
            {
                generate_castling_moves<GT, Own, CS_QUEN> (moves, pos);
            }
        }
    }


    /// Generates all pseudo-legal moves of color for targets.
    template<GenType GT, Color Own>
    void generate_moves (ValMoves &moves, const Position &pos, Bitboard targets)
    {
        generate_pawn_moves <GT, Own> (moves, pos, targets);
        generate_piece_moves<GT, Own, NIHT> (moves, pos, targets);
        generate_piece_moves<GT, Own, BSHP> (moves, pos, targets);
        generate_piece_moves<GT, Own, ROOK> (moves, pos, targets);
        generate_piece_moves<GT, Own, QUEN> (moves, pos, targets);
        if (   GenType::NATURAL == GT
            || GenType::CAPTURE == GT
            || GenType::QUIET == GT
            || GenType::CHECK == GT
            || GenType::QUIET_CHECK == GT)
        {
            generate_king_moves<GT, Own> (moves, pos, targets);
        }

        assert(std::unique (moves.begin (), moves.end ()) == moves.end ());
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
    Bitboard targets = GenType::NATURAL == GT ? ~pos.pieces ( pos.active) :
                       GenType::CAPTURE == GT ?  pos.pieces (~pos.active) :
                       GenType::QUIET == GT ? ~pos.pieces () : (assert(false), 0);

    WHITE == pos.active ?
        generate_moves<GT, WHITE> (moves, pos, targets) :
        generate_moves<GT, BLACK> (moves, pos, targets);
}
    
/// Explicit template instantiations
/// --------------------------------
/// generate<NATURAL> generates all pseudo-legal captures and non-captures.
template void generate<GenType::NATURAL> (ValMoves&, const Position&);
/// generate<CAPTURE> generates all pseudo-legal captures and queen promotions.
template void generate<GenType::CAPTURE> (ValMoves&, const Position&);
/// generate<QUIET> generates all pseudo-legal non-captures and underpromotions.
template void generate<GenType::QUIET  > (ValMoves&, const Position&);

/// Generates all pseudo-legal non-captures and knight underpromotions moves that give check.
template<> void generate<GenType::QUIET_CHECK> (ValMoves &moves, const Position &pos)
{
    assert(0 == pos.si->checkers);
    moves.clear ();
    Bitboard targets = ~pos.pieces ();
    // Pawns is excluded, will be generated together with direct checks
    Bitboard dsc_blockers = pos.dsc_blockers (pos.active) & ~pos.pieces (PAWN);
    while (0 != dsc_blockers)
    {
        auto org = pop_lsq (dsc_blockers);
        auto pt = ptype (pos[org]);
        Bitboard attacks = NIHT == pt ? targets & PieceAttacks[NIHT][org] :
                           BSHP == pt ? targets & attacks_bb<BSHP> (org, pos.pieces ()) :
                           ROOK == pt ? targets & attacks_bb<ROOK> (org, pos.pieces ()) :
                           QUEN == pt ? targets & attacks_bb<QUEN> (org, pos.pieces ()) :
                           KING == pt ? targets & PieceAttacks[KING][org] & ~PieceAttacks[QUEN][pos.square<KING> (~pos.active)] : (assert(false), 0);
        while (0 != attacks) { moves += mk_move<NORMAL> (org, pop_lsq (attacks)); }
    }

    WHITE == pos.active ?
        generate_moves<GenType::QUIET_CHECK, WHITE> (moves, pos, targets) :
        generate_moves<GenType::QUIET_CHECK, BLACK> (moves, pos, targets);
}
/// Generates all pseudo-legal check giving moves.
template<> void generate<GenType::CHECK      > (ValMoves &moves, const Position &pos)
{
    assert(0 == pos.si->checkers);
    moves.clear ();
    Bitboard targets = ~pos.pieces (pos.active);
    // Pawns is excluded, will be generated together with direct checks
    Bitboard dsc_blockers = pos.dsc_blockers (pos.active) & ~pos.pieces (PAWN);
    while (0 != dsc_blockers)
    {
        auto org = pop_lsq (dsc_blockers);
        auto pt = ptype (pos[org]);
        Bitboard attacks = NIHT == pt ? targets & PieceAttacks[NIHT][org] :
                           BSHP == pt ? targets & attacks_bb<BSHP> (org, pos.pieces ()) :
                           ROOK == pt ? targets & attacks_bb<ROOK> (org, pos.pieces ()) :
                           QUEN == pt ? targets & attacks_bb<QUEN> (org, pos.pieces ()) :
                           KING == pt ? targets & PieceAttacks[KING][org] & ~PieceAttacks[QUEN][pos.square<KING> (~pos.active)] : (assert(false), 0);
        while (0 != attacks) { moves += mk_move<NORMAL> (org, pop_lsq (attacks)); }
    }

    WHITE == pos.active ?
        generate_moves<GenType::CHECK, WHITE> (moves, pos, targets) :
        generate_moves<GenType::CHECK, BLACK> (moves, pos, targets);
}
/// Generates all pseudo-legal check evasions moves when the side to move is in check.
template<> void generate<GenType::EVASION    > (ValMoves &moves, const Position &pos)
{
    assert(0 != pos.si->checkers);
    moves.clear ();
    auto checker_sq = SQ_NO;
    Bitboard checker_attacks = 0;
    Bitboard jumpers = pos.si->checkers & pos.pieces (NIHT);
    if (0 != jumpers)
    {
        checker_sq = scan_lsq (jumpers);
        checker_attacks |= PieceAttacks[NIHT][checker_sq];
    }
    auto fk_sq = pos.square<KING> (pos.active);
    Bitboard mocc = pos.pieces () ^ fk_sq;
    Bitboard sliders = pos.si->checkers & ~(pos.pieces (PAWN) | jumpers);
    // Squares attacked by slider checkers will remove them from the king evasions
    // so to skip known illegal moves avoiding useless legality check later.
    while (0 != sliders)
    {
        checker_sq = pop_lsq (sliders);
        assert(color (pos[checker_sq]) == ~pos.active);
        auto pt = ptype (pos[checker_sq]);
        checker_attacks |= BSHP == pt ? attacks_bb<BSHP> (checker_sq, mocc) :
                           ROOK == pt ? attacks_bb<ROOK> (checker_sq, mocc) :
                           QUEN == pt ? attacks_bb<QUEN> (checker_sq, mocc) : (assert(false), 0);
    }

    // Generate evasions for king, capture and non capture moves
    Bitboard attacks = PieceAttacks[KING][fk_sq]
                        & ~(  checker_attacks
                            | pos.pieces (pos.active)
                            | PieceAttacks[KING][pos.square<KING> (~pos.active)]);
    while (0 != attacks) { moves += mk_move<NORMAL> (fk_sq, pop_lsq (attacks)); }

    // If double-check or only king, then only king move can save the day
    if (   more_than_one (pos.si->checkers)
        || 1 == pos.count (pos.active))
    {
        return;
    }

    // Generates blocking or captures of the checking piece
    Bitboard targets = SQ_NO == checker_sq ?
                        square_bb (scan_lsq (pos.si->checkers)) :
                        between_bb (checker_sq, fk_sq) | checker_sq;

    WHITE == pos.active ?
        generate_moves<GenType::EVASION, WHITE> (moves, pos, targets) :
        generate_moves<GenType::EVASION, BLACK> (moves, pos, targets);
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
                                 [&pos] (const ValMove &vm)
                                 {
                                     return (   0 != pos.abs_blockers (pos.active)
                                             || ENPASSANT == mtype (vm.move)
                                             || pos.square<KING> (pos.active) == org_sq (vm.move))
                                         && !pos.legal (vm.move);
                                 }),
                 moves.end ());
}

/// perft() is utility to verify move generation.
/// All the leaf nodes up to the given depth are generated, and the sum is returned.
template<bool RootNode>
u64 perft (Position &pos, i16 depth)
{
    u64 leaf_nodes = 0;
    i16 move_count = 0;

    const bool LeafNode = 2 >= depth;

    for (const auto &vm : MoveList<GenType::LEGAL> (pos))
    {
        u64 inter_nodes;
        if (   RootNode
            && 1 >= depth)
        {
            inter_nodes = 1;
        }
        else
        {
            StateInfo si;
            pos.do_move (vm.move, si);

            inter_nodes = LeafNode ?
                            MoveList<GenType::LEGAL> (pos).size () :
                            perft<false> (pos, depth - 1);

            pos.undo_move (vm.move);
        }

        if (RootNode)
        {
            sync_cout << std::right
                      << std::setfill ('0')
                      << std::setw (2)
                      << ++move_count
                      << " "
                      << std::left
                      << std::setfill (' ')
                      << std::setw (7)
                      <<
                         //move_to_can (vm.move)
                         move_to_san (vm.move, pos)
                      << std::right
                      << std::setfill ('.')
                      << std::setw (16)
                      << inter_nodes
                      << std::setfill (' ')
                      << std::left << sync_endl;
        }

        leaf_nodes += inter_nodes;
    }
    return leaf_nodes;
}
/// Explicit template instantiations
/// --------------------------------
template u64 perft<true > (Position&, i16);
template u64 perft<false> (Position&, i16);
