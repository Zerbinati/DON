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

            for (Square s : pos.squares<PT> (Own))
            {
                if (   GT == CHECK
                    || GT == QUIET_CHECK)
                {
                    if ((pos.dsc_checkers (pos.active ()) & s) != 0)
                    {
                        continue;
                    }
                    targets &= pos.checks (PT);
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
            case RELAX:
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
                if (   (PieceAttacks[ROOK][dst] & pos.square<KING> (~pos.active ())) != 0
                    && (attacks_bb<ROOK> (dst, pos.pieces () - (dst - delta)) & pos.square<KING> (~pos.active ())) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, ROOK)));
                }
                if (   (PieceAttacks[BSHP][dst] & pos.square<KING> (~pos.active ())) != 0
                    && (attacks_bb<BSHP> (dst, pos.pieces () - (dst - delta)) & pos.square<KING> (~pos.active ())) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, BSHP)));
                }
                if ((PieceAttacks[NIHT][dst] & pos.square<KING> (~pos.active ())) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, NIHT)));
                }
                break;
            case CHECK:
                if (   (PieceAttacks[QUEN][dst] & pos.square<KING> (~pos.active ())) != 0
                    && (attacks_bb<QUEN> (dst, pos.pieces () - (dst - delta)) & pos.square<KING> (~pos.active ())) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, QUEN)));
                }
                if (   (PieceAttacks[ROOK][dst] & pos.square<KING> (~pos.active ())) != 0
                    && (attacks_bb<ROOK> (dst, pos.pieces () - (dst - delta)) & pos.square<KING> (~pos.active ())) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, ROOK)));
                }
                if (   (PieceAttacks[BSHP][dst] & pos.square<KING> (~pos.active ())) != 0
                    && (attacks_bb<BSHP> (dst, pos.pieces () - (dst - delta)) & pos.square<KING> (~pos.active ())) != 0)
                {
                    moves.push_back (ValMove(mk_move<PROMOTE> (dst - delta, dst, BSHP)));
                }
                if ((PieceAttacks[NIHT][dst] & pos.square<KING> (~pos.active ())) != 0)
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
                Bitboard push_1 = empties & shift_bb<Push> (Rx_pawns);
                Bitboard push_2 = empties & shift_bb<Push> (push_1 & Rank3BB);
                if (   GT == CHECK
                    || GT == QUIET_CHECK)
                {
                    push_1 &= pos.checks (PAWN);
                    push_2 &= pos.checks (PAWN);
                    // Pawns which give discovered check
                    // Add pawn pushes which give discovered check.
                    // This is possible only if the pawn is not on the same file as the enemy king, because don't generate captures.
                    // Note that a possible discovery check promotion has been already generated among captures.
                    Bitboard dsc_pawns = Rx_pawns & pos.dsc_checkers (pos.active ()) & ~file_bb (pos.square<KING> (Opp));
                    if (dsc_pawns != 0)
                    {
                        Bitboard push_cd_1 = empties & shift_bb<Push> (dsc_pawns);
                        Bitboard push_cd_2 = empties & shift_bb<Push> (push_cd_1 & Rank3BB);
                        push_1 |= push_cd_1;
                        push_2 |= push_cd_2;
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
                Bitboard attack_l = enemies & shift_bb<LCap> (Rx_pawns);
                Bitboard attack_r = enemies & shift_bb<RCap> (Rx_pawns);
                if (GT == CHECK)
                {
                    attack_l &= pos.checks (PAWN);
                    attack_r &= pos.checks (PAWN);
                    // Pawns which give discovered check
                    // Add pawn captures which give discovered check.
                    Bitboard dsc_pawns = Rx_pawns & pos.dsc_checkers (pos.active ());
                    if (dsc_pawns != 0)
                    {
                        Bitboard attack_cd_l = enemies & shift_bb<LCap> (dsc_pawns);
                        Bitboard attack_cd_r = enemies & shift_bb<RCap> (dsc_pawns);
                        attack_l |= attack_cd_l;
                        attack_r |= attack_cd_r;
                    }
                }
                while (attack_l != 0) { auto dst = pop_lsq (attack_l); moves.push_back (ValMove(mk_move<NORMAL> (dst - LCap, dst))); }
                while (attack_r != 0) { auto dst = pop_lsq (attack_r); moves.push_back (ValMove(mk_move<NORMAL> (dst - RCap, dst))); }

                auto ep_sq = pos.en_passant_sq ();
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
                            ep_captures &= (  shift_bb<DEL_E> (targets)
                                            | shift_bb<DEL_W> (targets));
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
                proms = empties & shift_bb<Push> (R7_pawns);
                while (proms != 0) generate_promotion_moves<GT> (moves, pos, pop_lsq (proms), Push);
                proms = enemies & shift_bb<LCap> (R7_pawns);
                while (proms != 0) generate_promotion_moves<GT> (moves, pos, pop_lsq (proms), LCap);
                proms = enemies & shift_bb<RCap> (R7_pawns);
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
                 && pos.checkers () == 0);

            auto king_org = pos.square<KING> (Own);
            auto rook_org = pos.castle_rook (CR);
            assert(ptype (pos[rook_org]) == ROOK);

            auto step = king_dst > king_org ? DEL_E : DEL_W;
            for (auto s = king_dst; s != king_org; s -= step)
            {
                if (pos.attackers_to (s, Opp) != 0)
                {
                    return;
                }
            }

            if (Position::Chess960)
            {
                // Because generate only legal castling moves needed to verify that
                // when moving the castling rook do not discover some hidden checker.
                // For instance an enemy queen in SQ_A1 when castling rook is in SQ_B1.
                if ((attacks_bb<ROOK> (king_dst, pos.pieces () - rook_org) & pos.pieces (Opp, ROOK, QUEN)) != 0)
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
                auto king_sq = pos.square<KING> (Own);
                Bitboard attacks =  PieceAttacks[KING][king_sq]
                                 & ~PieceAttacks[KING][pos.square<KING> (Opp)]
                                 & targets;
                while (attacks != 0) { moves.push_back (ValMove(mk_move<NORMAL> (king_sq, pop_lsq (attacks)))); }
            }

            if (GT != CAPTURE)
            {
                if (   pos.checkers () == 0
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

    template<GenType GT>
    void generate (vector<ValMove> &moves, const Position &pos)
    {
        assert(pos.checkers () == 0);
        assert(GT == RELAX
            || GT == CAPTURE
            || GT == QUIET);
        moves.clear ();
        Bitboard targets;
        switch (GT)
        {
        case RELAX:
            targets = ~pos.pieces (pos.active ());
            break;
        case CAPTURE:
            targets =  pos.pieces (~pos.active ());
            break;
        case QUIET:
            targets = ~pos.pieces ();
            break;
        default:
            assert(false);
            targets = 0;
            break;
        }

        pos.active () == WHITE ?
            generate_moves<GT, WHITE> (moves, pos, targets) :
            generate_moves<GT, BLACK> (moves, pos, targets);
    }

    // Explicit template instantiations

    // generate<RELAX> generates all pseudo-legal captures and non-captures.
    // Returns a pointer to the end of the move list.
    template void generate<RELAX  > (vector<ValMove>&, const Position&);
    // generate<CAPTURES> generates all pseudo-legal captures and queen promotions.
    // Returns a pointer to the end of the move list.
    template void generate<CAPTURE> (vector<ValMove>&, const Position&);
    // generate<QUIETS> generates all pseudo-legal non-captures and underpromotions.
    // Returns a pointer to the end of the move list.
    template void generate<QUIET  > (vector<ValMove>&, const Position&);

    // Generates all pseudo-legal non-captures and knight underpromotions moves that give check.
    // Returns a pointer to the end of the move list.
    template<>
    void generate<QUIET_CHECK> (vector<ValMove> &moves, const Position &pos)
    {
        assert(pos.checkers () == 0);
        moves.clear ();
        Bitboard targets = ~pos.pieces ();
        // Pawns is excluded, will be generated together with direct checks
        Bitboard dsc_checkers = pos.dsc_checkers (pos.active ()) & ~pos.pieces (pos.active (), PAWN);
        while (dsc_checkers != 0)
        {
            auto org = pop_lsq (dsc_checkers);
            auto pt  = ptype (pos[org]);
            Bitboard attacks = attacks_bb (Piece(pt), org, pos.pieces ()) & targets;
            if (pt == KING)
            {
                attacks &= ~PieceAttacks[QUEN][pos.square<KING> (~pos.active ())];
            }
            while (attacks != 0) { moves.push_back (ValMove(mk_move<NORMAL> (org, pop_lsq (attacks)))); }
        }

        pos.active () == WHITE ?
            generate_moves<QUIET_CHECK, WHITE> (moves, pos, targets) :
            generate_moves<QUIET_CHECK, BLACK> (moves, pos, targets);
    }

    // Generates all pseudo-legal check giving moves.
    // Returns a pointer to the end of the move list.
    template<>
    void generate<CHECK      > (vector<ValMove> &moves, const Position &pos)
    {
        assert(pos.checkers () == 0);
        moves.clear ();
        Bitboard targets = ~pos.pieces (pos.active ());

        // Pawns is excluded, will be generated together with direct checks
        Bitboard dsc_checkers = pos.dsc_checkers (pos.active ()) & ~pos.pieces (pos.active (), PAWN);
        while (dsc_checkers != 0)
        {
            auto org = pop_lsq (dsc_checkers);
            auto pt  = ptype (pos[org]);
            Bitboard attacks = attacks_bb (Piece(pt), org, pos.pieces ()) & targets;
            if (pt == KING)
            {
                attacks &= ~PieceAttacks[QUEN][pos.square<KING> (~pos.active ())];
            }
            while (attacks != 0) { moves.push_back (ValMove(mk_move<NORMAL> (org, pop_lsq (attacks)))); }
        }

        pos.active () == WHITE ?
            generate_moves<CHECK, WHITE> (moves, pos, targets) :
            generate_moves<CHECK, BLACK> (moves, pos, targets);
    }

    // Generates all pseudo-legal check evasions moves when the side to move is in check.
    // Returns a pointer to the end of the move list.
    template<>
    void generate<EVASION    > (vector<ValMove> &moves, const Position &pos)
    {
        assert(pos.checkers () != 0); // If any checker exists
        moves.clear ();
        auto check_sq = SQ_NO;
        Bitboard checker_attacks = 0;
        Bitboard sliders = pos.checkers () & ~pos.pieces (PAWN, NIHT);
        // Squares attacked by slider checkers will remove them from the king evasions
        // so to skip known illegal moves avoiding useless legality check later.
        while (sliders != 0)
        {
            check_sq = pop_lsq (sliders);
            assert(color (pos[check_sq]) == ~pos.active ());
            checker_attacks |= (  attacks_bb (pos[check_sq], check_sq, pos.pieces ())
                                | strline_bb (check_sq, pos.square<KING> (pos.active ()))) - check_sq;
        }
        if (check_sq == SQ_NO)
        {
            check_sq = scan_lsq (pos.checkers ());
            checker_attacks = attacks_bb (pos[check_sq], check_sq, pos.pieces ());
        }

        // Generate evasions for king, capture and non capture moves
        Bitboard attacks =
              PieceAttacks[KING][pos.square<KING> (pos.active ())]
            & ~(  pos.pieces (pos.active ())
                | checker_attacks
                | PieceAttacks[KING][pos.square<KING> (~pos.active ())]);
        while (attacks != 0) { moves.push_back (ValMove(mk_move<NORMAL> (pos.square<KING> (pos.active ()), pop_lsq (attacks)))); }

        // If double-check, then only a king move can save the day, triple+ check not possible
        if (   more_than_one (pos.checkers ())
            || pos.count<NONE> (pos.active ()) == 1)
        {
            return;
        }

        // Generates blocking evasions or captures of the checking piece
        Bitboard targets = between_bb (check_sq, pos.square<KING> (pos.active ())) + check_sq;

        pos.active () == WHITE ?
            generate_moves<EVASION, WHITE> (moves, pos, targets) :
            generate_moves<EVASION, BLACK> (moves, pos, targets);
    }

    // Generates all legal moves.
    template<>
    void generate<LEGAL      > (vector<ValMove> &moves, const Position &pos)
    {
        pos.checkers () == 0 ?
            generate<RELAX  > (moves, pos) :
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
                                            (   org_sq (vm.move) == pos.square<KING> (pos.active ())
                                             || pos.abs_pinneds (pos.active ()) != 0
                                             || mtype (vm.move) == ENPASSANT)
                                         && !pos.legal (vm.move);
                                     }),
                     moves.end ());
    }

}
