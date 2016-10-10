#include "MoveGenerator.h"
#include "BitBoard.h"

namespace MoveGen {

    using namespace std;
    using namespace BitBoard;

    namespace {

        // Generates piece common move
        template<GenType GT, Color Own, PieceType PT>
        void generate_piece_moves (vector<ValMove> &moves, const Position &pos, Bitboard targets)
        {
            assert(PT == NIHT
                || PT == BSHP
                || PT == ROOK
                || PT == QUEN);

            for (auto s : pos.squares[Own][PT])
            {
                if (   GT == CHECK
                    || GT == QUIET_CHECK)
                {
                    if ((pos.dsc_blockers (pos.active) & s) != 0)
                    {
                        continue;
                    }
                    targets &= pos.si->checks[PT];
                }

                if ((PieceAttacks[PT][s] & targets) != 0)
                {
                    Bitboard attacks = targets;
                    switch (PT)
                    {
                    case NIHT: attacks &= PieceAttacks[NIHT][s]; break;
                    case BSHP: attacks &= attacks_bb<BSHP> (s, pos.pieces ()); break;
                    case ROOK: attacks &= attacks_bb<ROOK> (s, pos.pieces ()); break;
                    case QUEN: attacks &= attacks_bb<QUEN> (s, pos.pieces ()); break;
                    }
                    while (attacks != 0) { moves.push_back (ValMove(mk_move<NORMAL> (s, pop_lsq (attacks)))); }
                }
            }
        }

        // Generates PAWN promotion move
        template<GenType GT>
        void generate_promotion_moves (vector<ValMove> &moves, const Position &pos, Square dst, Delta delta)
        {
            assert(delta == DEL_N
                || delta == DEL_NE
                || delta == DEL_NW
                || delta == DEL_S
                || delta == DEL_SE
                || delta == DEL_SW);

            switch (GT)
            {
            case NATURAL:
            case EVASION:
            case CAPTURE:
            case QUIET:
                if (GT != QUIET)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, QUEN)));
                }
                if (GT != CAPTURE)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, ROOK)));
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, BSHP)));
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, NIHT)));
                }
                break;
            case QUIET_CHECK:
                if (   (PieceAttacks[ROOK][dst] & pos.square (~pos.active, KING)) != 0
                    && (attacks_bb<ROOK> (dst, pos.pieces () - (dst - delta)) & pos.square (~pos.active, KING)) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, ROOK)));
                }
                if (   (PieceAttacks[BSHP][dst] & pos.square (~pos.active, KING)) != 0
                    && (attacks_bb<BSHP> (dst, pos.pieces () - (dst - delta)) & pos.square (~pos.active, KING)) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, BSHP)));
                }
                if ((PieceAttacks[NIHT][dst] & pos.square (~pos.active, KING)) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, NIHT)));
                }
                break;
            case CHECK:
                if (   (PieceAttacks[QUEN][dst] & pos.square (~pos.active, KING)) != 0
                    && (attacks_bb<QUEN> (dst, pos.pieces () - (dst - delta)) & pos.square (~pos.active, KING)) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, QUEN)));
                }
                if (   (PieceAttacks[ROOK][dst] & pos.square (~pos.active, KING)) != 0
                    && (attacks_bb<ROOK> (dst, pos.pieces () - (dst - delta)) & pos.square (~pos.active, KING)) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, ROOK)));
                }
                if (   (PieceAttacks[BSHP][dst] & pos.square (~pos.active, KING)) != 0
                    && (attacks_bb<BSHP> (dst, pos.pieces () - (dst - delta)) & pos.square (~pos.active, KING)) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, BSHP)));
                }
                if ((PieceAttacks[NIHT][dst] & pos.square (~pos.active, KING)) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, NIHT)));
                }
                break;
            }
        }
        // Generates PAWN common move
        template<GenType GT, Color Own>
        void generate_pawn_moves (vector<ValMove> &moves, const Position &pos, Bitboard targets)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N  : DEL_S;
            static const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
            static const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;

            static const Bitboard Rank3BB = Own == WHITE ? R3_bb : R6_bb;
            static const Bitboard Rank5BB = Own == WHITE ? R5_bb : R4_bb;
            static const Bitboard Rank7BB = Own == WHITE ? R7_bb : R2_bb;

            // Pawns on 7th Rank
            Bitboard R7_pawns = pos.pieces (Own, PAWN) &  Rank7BB;
            // Pawns not on 7th Rank
            Bitboard Rx_pawns = pos.pieces (Own, PAWN) & ~Rank7BB;

            Bitboard empties = ~pos.pieces ();
            Bitboard enemies = pos.pieces (Opp) & targets;
            // Pawn single-push and double-push, no promotions
            if (GT != CAPTURE)
            {
                Bitboard push_1 = empties & shift<Push> (Rx_pawns);
                Bitboard push_2 = empties & shift<Push> (push_1 & Rank3BB);
                if (   GT == CHECK
                    || GT == QUIET_CHECK)
                {
                    push_1 &= pos.si->checks[PAWN];
                    push_2 &= pos.si->checks[PAWN];
                    // Pawns which give discovered check
                    // Add pawn pushes which give discovered check.
                    // This is possible only if the pawn is not on the same file as the enemy king, because don't generate captures.
                    // Note that a possible discovery check promotion has been already generated among captures.
                    Bitboard dsc_pawns = Rx_pawns & pos.dsc_blockers (pos.active) & ~file_bb (pos.square (Opp, KING));
                    if (dsc_pawns != 0)
                    {
                        Bitboard dc_push_1 = empties & shift<Push> (dsc_pawns);
                        Bitboard dc_push_2 = empties & shift<Push> (dc_push_1 & Rank3BB);
                        push_1 |= dc_push_1;
                        push_2 |= dc_push_2;
                    }
                }
                push_1 &= targets;
                push_2 &= targets;
                while (push_1 != 0) { auto dst = pop_lsq (push_1); moves.push_back (ValMove(mk_move<NORMAL> (dst - Push, dst))); }
                while (push_2 != 0) { auto dst = pop_lsq (push_2); moves.push_back (ValMove(mk_move<NORMAL> (dst - Push*2, dst))); }
            }
            // Pawn normal and en-passant captures, no promotions
            if (   GT != QUIET
                && GT != QUIET_CHECK)
            {
                Bitboard l_attack = enemies & shift<LCap> (Rx_pawns);
                Bitboard r_attack = enemies & shift<RCap> (Rx_pawns);
                if (GT == CHECK)
                {
                    l_attack &= pos.si->checks[PAWN];
                    r_attack &= pos.si->checks[PAWN];
                    // Pawns which give discovered check
                    // Add pawn captures which give discovered check.
                    Bitboard dsc_pawns = Rx_pawns & pos.dsc_blockers (pos.active);
                    if (dsc_pawns != 0)
                    {
                        l_attack |= enemies & shift<LCap> (dsc_pawns);
                        r_attack |= enemies & shift<RCap> (dsc_pawns);
                    }
                }
                while (l_attack != 0) { auto dst = pop_lsq (l_attack); moves.push_back (ValMove(mk_move<NORMAL> (dst - LCap, dst))); }
                while (r_attack != 0) { auto dst = pop_lsq (r_attack); moves.push_back (ValMove(mk_move<NORMAL> (dst - RCap, dst))); }

                auto ep_sq = pos.si->en_passant_sq;
                if (ep_sq != SQ_NO)
                {
                    assert(rel_rank (Own, ep_sq) == R_6);
                    Bitboard ep_captures = Rx_pawns & Rank5BB;
                    if (ep_captures != 0)
                    {
                        // If the checking piece is the double pushed pawn and also is in the target.
                        // Otherwise this is a discovery check and are forced to do otherwise.
                        if (GT == EVASION)
                        {
                            ep_captures &= (  shift<DEL_E> (targets)
                                            | shift<DEL_W> (targets));
                        }
                        ep_captures &= PawnAttacks[Opp][ep_sq];
                        assert(ep_captures != 0);
                        assert(pop_count (ep_captures) <= 2);
                        while (ep_captures != 0) { moves.push_back (ValMove(mk_move<ENPASSANT> (pop_lsq (ep_captures), ep_sq))); }
                    }
                }
            }

            // Promotions (queening and under-promotions)
            if (R7_pawns != 0)
            {
                if (GT == EVASION)
                {
                    empties &= targets;
                }
                // Promoting pawns
                Bitboard proms;
                proms = empties & shift<Push> (R7_pawns);
                while (proms != 0) generate_promotion_moves<GT> (moves, pos, pop_lsq (proms), Push);
                proms = enemies & shift<LCap> (R7_pawns);
                while (proms != 0) generate_promotion_moves<GT> (moves, pos, pop_lsq (proms), LCap);
                proms = enemies & shift<RCap> (R7_pawns);
                while (proms != 0) generate_promotion_moves<GT> (moves, pos, pop_lsq (proms), RCap);
            }
        }

        // Generates KING castling move
        template<GenType GT, Color Own, CastleRight CR>
        void generate_castling_moves (vector<ValMove> &moves, const Position &pos)
        {
            static const auto Opp = Own == WHITE ? BLACK : WHITE;
            static const bool KingSide = (CR & CR_KING) != CR_NONE;
            static const auto king_dst = rel_sq (Own, KingSide ? SQ_G1 : SQ_C1);

            assert(GT != EVASION);
            assert(!pos.impeded_castle (CR)
                 && pos.can_castle (CR) != CR_NONE
                 && pos.si->checkers == 0);

            auto king_org = pos.square (Own, KING);
            auto rook_org = pos.castle_rook[CR];
            assert(pos[rook_org] == (Own|ROOK));

            Bitboard b = pos.king_path[CR];
            // Check king's path for attackers
            while (b != 0)
            {
                if (pos.attackers_to (pop_lsq (b), Opp) != 0)
                {
                    return;
                }
            }

            if (Position::Chess960)
            {
                // Because generate only legal castling moves needed to verify that
                // when moving the castling rook do not discover some hidden checker.
                // For instance an enemy queen in SQ_A1 when castling rook is in SQ_B1.
                if (   (pos.pieces (Opp, ROOK, QUEN) & PieceAttacks[ROOK][king_dst]) != 0
                    && (pos.pieces (Opp, ROOK, QUEN) & attacks_bb<ROOK> (king_dst, pos.pieces () - rook_org)) != 0)
                {
                    return;
                }
            }

            auto m = mk_move<CASTLE> (king_org, rook_org);
            if (   (   GT != CHECK
                    && GT != QUIET_CHECK)
                || pos.gives_check (m))
            {
                moves.push_back (ValMove(m));
            }
        }
        // Generates KING common move
        template<GenType GT, Color Own>
        void generate_king_moves (vector<ValMove> &moves, const Position &pos, Bitboard targets)
        {
            static const auto Opp = Own == WHITE ? BLACK : WHITE;

            assert(GT != EVASION);

            if (   GT != CHECK
                && GT != QUIET_CHECK)
            {
                auto king_sq = pos.square (Own, KING);
                Bitboard attacks =  PieceAttacks[KING][king_sq]
                                 & ~PieceAttacks[KING][pos.square (Opp, KING)]
                                 & targets;
                while (attacks != 0) { moves.push_back (ValMove(mk_move<NORMAL> (king_sq, pop_lsq (attacks)))); }
            }

            if (GT != CAPTURE)
            {
                if (   pos.si->checkers == 0
                    && pos.can_castle (Own) != CR_NONE)
                {
                    if (   pos.can_castle (Castling<Own, CS_KING>::Right) != CR_NONE
                        && !pos.impeded_castle (Castling<Own, CS_KING>::Right))
                    {
                        generate_castling_moves<GT, Own, Castling<Own, CS_KING>::Right> (moves, pos);
                    }
                    if (   pos.can_castle (Castling<Own, CS_QUEN>::Right) != CR_NONE
                        && !pos.impeded_castle (Castling<Own, CS_QUEN>::Right))
                    {
                        generate_castling_moves<GT, Own, Castling<Own, CS_QUEN>::Right> (moves, pos);
                    }
                }
            }
        }


        // Generates all pseudo-legal moves of color for targets.
        template<GenType GT, Color Own>
        void generate_moves (vector<ValMove> &moves, const Position &pos, Bitboard targets)
        {
            generate_pawn_moves<GT, Own> (moves, pos, targets);
            generate_piece_moves<GT, Own, NIHT> (moves, pos, targets);
            generate_piece_moves<GT, Own, BSHP> (moves, pos, targets);
            generate_piece_moves<GT, Own, ROOK> (moves, pos, targets);
            generate_piece_moves<GT, Own, QUEN> (moves, pos, targets);
            if (GT != EVASION)
            {
                generate_king_moves<GT, Own> (moves, pos, targets);
            }
        }
    }

    template<GenType GT> void generate (vector<ValMove> &moves, const Position &pos)
    {
        assert(pos.si->checkers == 0);
        assert(GT == NATURAL
            || GT == CAPTURE
            || GT == QUIET);
        moves.clear ();
        Bitboard targets;
        switch (GT)
        {
        case NATURAL:
            targets = ~pos.pieces ( pos.active);
            break;
        case CAPTURE:
            targets =  pos.pieces (~pos.active);
            break;
        case QUIET:
            targets = ~pos.pieces ();
            break;
        default:
            assert(false);
            targets = 0;
            break;
        }

        pos.active == WHITE ?
            generate_moves<GT, WHITE> (moves, pos, targets) :
            generate_moves<GT, BLACK> (moves, pos, targets);
    }
    // Explicit template instantiations

    // generate<NATURAL> generates all pseudo-legal captures and non-captures.
    template void generate<NATURAL> (vector<ValMove>&, const Position&);
    // generate<CAPTURES> generates all pseudo-legal captures and queen promotions.
    template void generate<CAPTURE> (vector<ValMove>&, const Position&);
    // generate<QUIETS> generates all pseudo-legal non-captures and underpromotions.
    template void generate<QUIET  > (vector<ValMove>&, const Position&);

    // Generates all pseudo-legal non-captures and knight underpromotions moves that give check.
    template<> void generate<QUIET_CHECK> (vector<ValMove> &moves, const Position &pos)
    {
        assert(pos.si->checkers == 0);
        moves.clear ();
        Bitboard targets = ~pos.pieces ();
        // Pawns is excluded, will be generated together with direct checks
        Bitboard dsc_blockers = pos.dsc_blockers (pos.active) & ~pos.pieces (PAWN);
        while (dsc_blockers != 0)
        {
            auto org = pop_lsq (dsc_blockers);
            Bitboard attacks = 0;
            switch (ptype (pos[org]))
            {
            case NIHT: attacks = targets & PieceAttacks[NIHT][org];               break;
            case BSHP: attacks = targets & attacks_bb<BSHP> (org, pos.pieces ()); break;
            case ROOK: attacks = targets & attacks_bb<ROOK> (org, pos.pieces ()); break;
            case QUEN: attacks = targets & attacks_bb<QUEN> (org, pos.pieces ()); break;
            case KING: attacks = targets & PieceAttacks[KING][org] & ~PieceAttacks[QUEN][pos.square (~pos.active, KING)]; break;
            default: assert(false); break;
            }
            while (attacks != 0) { moves.push_back (ValMove(mk_move<NORMAL> (org, pop_lsq (attacks)))); }
        }

        pos.active == WHITE ?
            generate_moves<QUIET_CHECK, WHITE> (moves, pos, targets) :
            generate_moves<QUIET_CHECK, BLACK> (moves, pos, targets);
    }
    // Generates all pseudo-legal check giving moves.
    template<> void generate<CHECK      > (vector<ValMove> &moves, const Position &pos)
    {
        assert(pos.si->checkers == 0);
        moves.clear ();
        Bitboard targets = ~pos.pieces (pos.active);
        // Pawns is excluded, will be generated together with direct checks
        Bitboard dsc_blockers = pos.dsc_blockers (pos.active) & ~pos.pieces (PAWN);
        while (dsc_blockers != 0)
        {
            auto org = pop_lsq (dsc_blockers);
            Bitboard attacks = 0;
            switch (ptype (pos[org]))
            {
            case NIHT: attacks = targets & PieceAttacks[NIHT][org];               break;
            case BSHP: attacks = targets & attacks_bb<BSHP> (org, pos.pieces ()); break;
            case ROOK: attacks = targets & attacks_bb<ROOK> (org, pos.pieces ()); break;
            case QUEN: attacks = targets & attacks_bb<QUEN> (org, pos.pieces ()); break;
            case KING: attacks = targets & PieceAttacks[KING][org] & ~PieceAttacks[QUEN][pos.square (~pos.active, KING)]; break;
            default: assert(false); break;
            }
            while (attacks != 0) { moves.push_back (ValMove(mk_move<NORMAL> (org, pop_lsq (attacks)))); }
        }

        pos.active == WHITE ?
            generate_moves<CHECK, WHITE> (moves, pos, targets) :
            generate_moves<CHECK, BLACK> (moves, pos, targets);
    }
    // Generates all pseudo-legal check evasions moves when the side to move is in check.
    template<> void generate<EVASION    > (vector<ValMove> &moves, const Position &pos)
    {
        assert(pos.si->checkers != 0);
        moves.clear ();
        auto checker_sq = SQ_NO;
        Bitboard checker_attacks = 0;
        Bitboard jumpers = pos.si->checkers & pos.pieces (NIHT);
        if (jumpers != 0)
        {
            checker_sq = scan_lsq (jumpers);
            checker_attacks |= PieceAttacks[NIHT][checker_sq];
        }
        auto fk_sq = pos.square (pos.active, KING);
        Bitboard mocc = pos.pieces () - fk_sq;
        Bitboard sliders = pos.si->checkers & ~(pos.pieces (PAWN) | jumpers);
        // Squares attacked by slider checkers will remove them from the king evasions
        // so to skip known illegal moves avoiding useless legality check later.
        while (sliders != 0)
        {
            checker_sq = pop_lsq (sliders);
            assert(color (pos[checker_sq]) == ~pos.active);
            switch (ptype (pos[checker_sq]))
            {
            case BSHP: checker_attacks |= attacks_bb<BSHP> (checker_sq, mocc); break;
            case ROOK: checker_attacks |= attacks_bb<ROOK> (checker_sq, mocc); break;
            case QUEN: checker_attacks |= attacks_bb<QUEN> (checker_sq, mocc); break;
            default: assert(false); break;
            }
        }

        // Generate evasions for king, capture and non capture moves
        Bitboard attacks =
              PieceAttacks[KING][fk_sq]
            & ~(  checker_attacks
                | pos.pieces (pos.active)
                | PieceAttacks[KING][pos.square (~pos.active, KING)]);
        while (attacks != 0) { moves.push_back (ValMove(mk_move<NORMAL> (fk_sq, pop_lsq (attacks)))); }

        // If double-check or only king, then only king move can save the day
        if (   more_than_one (pos.si->checkers)
            || pos.count<NONE> (pos.active) == 1)
        {
            return;
        }

        // Generates blocking or captures of the checking piece

        Bitboard targets = 
            checker_sq == SQ_NO ?
                square_bb (scan_lsq (pos.si->checkers)) : // Pawn checker
                between_bb (checker_sq, fk_sq) + checker_sq;

        pos.active == WHITE ?
            generate_moves<EVASION, WHITE> (moves, pos, targets) :
            generate_moves<EVASION, BLACK> (moves, pos, targets);
    }
    // Generates all legal moves.
    template<> void generate<LEGAL      > (vector<ValMove> &moves, const Position &pos)
    {
        pos.si->checkers == 0 ?
            generate<NATURAL> (moves, pos) :
            generate<EVASION> (moves, pos);
        filter_illegal (moves, pos);
    }

    void filter_illegal (vector<ValMove> &moves, const Position &pos)
    {
        moves.erase (std::remove_if (moves.begin (),
                                     moves.end (),
                                     [&pos] (const ValMove &vm)
                                     {
                                         return
                                            (   pos.abs_blockers (pos.active) != 0
                                             || mtype (vm.move) == ENPASSANT
                                             || org_sq (vm.move) == pos.square (pos.active, KING))
                                         && !pos.legal (vm.move);
                                     }),
                     moves.end ());
    }

}
