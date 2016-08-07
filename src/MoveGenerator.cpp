#include "MoveGenerator.h"
#include "BitBoard.h"

namespace MoveGen {

    using namespace std;
    using namespace BitBoard;

    namespace {

        // Generates piece common move
        template<GenType GT, Color Own, PieceType PT>
        void generate_piece_moves (ValMove *&moves, const Position &pos, Bitboard targets, const CheckInfo *ci = nullptr)
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
                    // Already generated
                    if ((ci->dsc_checkers & s) != 0)
                    {
                        continue;
                    }
                    targets &= ci->checking_bb[PT];
                }
                
                if ((PieceAttacks[PT][s] & targets) == 0)
                {
                    continue;
                }

                Bitboard attacks;
                switch (PT)
                {
                case NIHT: attacks = PieceAttacks[NIHT][s]               & targets; break;
                case BSHP: attacks = attacks_bb<BSHP> (s, pos.pieces ()) & targets; break;
                case ROOK: attacks = attacks_bb<ROOK> (s, pos.pieces ()) & targets; break;
                case QUEN: attacks = attacks_bb<QUEN> (s, pos.pieces ()) & targets; break;
                default:   attacks = 0;                                             break;
                }

                while (attacks != 0) { (*moves++).move = mk_move<NORMAL> (s, pop_lsq (attacks)); }
            }
        }

        // Generates KING castling move
        template<GenType GT, Color Own, CastleRight CR>
        void generate_castling_moves (ValMove *&moves, const Position &pos, const CheckInfo *ci)
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

            if (   GT == CHECK
                || GT == QUIET_CHECK)
            {
                if (!pos.gives_check (m, *ci))
                {
                    return;
                }
            }

            (*moves++).move = m;
        }
        // Generates KING common move
        template<GenType GT, Color Own>
        void generate_king_moves (ValMove *&moves, const Position &pos, Bitboard targets, const CheckInfo *ci = nullptr)
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
                while (attacks != 0) { (*moves++).move = mk_move<NORMAL> (king_sq, pop_lsq (attacks)); }
            }

            if (GT != CAPTURE)
            {
                if (   pos.checkers () == 0
                    && pos.can_castle (Own) != CR_NONE)
                {
                    if (   pos.can_castle (Castling<Own, CS_KING>::Right) != CR_NONE
                        && !pos.impeded_castle (Castling<Own, CS_KING>::Right))
                    {
                        generate_castling_moves<GT, Own, Castling<Own, CS_KING>::Right> (moves, pos, ci);
                    }
                    if (   pos.can_castle (Castling<Own, CS_QUEN>::Right) != CR_NONE
                        && !pos.impeded_castle (Castling<Own, CS_QUEN>::Right))
                    {
                        generate_castling_moves<GT, Own, Castling<Own, CS_QUEN>::Right> (moves, pos, ci);
                    }
                }
            }
        }

        // Generates PAWN promotion move
        template<GenType GT>
        void generate_promotion_moves (ValMove *&moves, Square dst, Delta delta, const CheckInfo *ci)
        {
            assert(delta == DEL_N
                || delta == DEL_NE
                || delta == DEL_NW
                || delta == DEL_S
                || delta == DEL_SE
                || delta == DEL_SW);

            if (   GT == RELAX
                || GT == EVASION
                || GT == CAPTURE)
            {
                (*moves++).move = mk_move<PROMOTE> (dst - delta, dst, QUEN);
            }
            if (   GT == RELAX
                || GT == EVASION
                //|| GT == CAPTURE
                || GT == QUIET)
            {
                (*moves++).move = mk_move<PROMOTE> (dst - delta, dst, ROOK);
                (*moves++).move = mk_move<PROMOTE> (dst - delta, dst, BSHP);
                (*moves++).move = mk_move<PROMOTE> (dst - delta, dst, NIHT);
            }
            // Knight-promotion is the only one that can give a direct check
            // not already included in the queen-promotion (queening).
            if (   GT == CHECK
                || GT == QUIET_CHECK)
            {
                //if ((attacks_bb<QUEN> (dst, targets) & ci->king_sq) != 0) (*moves++).move = mk_move<PROMOTE> (dst - delta, dst, QUEN);
                //if ((attacks_bb<ROOK> (dst, targets) & ci->king_sq) != 0) (*moves++).move = mk_move<PROMOTE> (dst - delta, dst, ROOK);
                //if ((attacks_bb<BSHP> (dst, targets) & ci->king_sq) != 0) (*moves++).move = mk_move<PROMOTE> (dst - delta, dst, BSHP);
                if ((PieceAttacks[NIHT][dst] & ci->king_sq) != 0) (*moves++).move = mk_move<PROMOTE> (dst - delta, dst, NIHT);
            }
        }
        // Generates PAWN common move
        template<GenType GT, Color Own>
        void generate_pawn_moves (ValMove *&moves, const Position &pos, Bitboard targets, const CheckInfo *ci = nullptr)
        {
            static const auto Opp  = Own == WHITE ? BLACK : WHITE;
            static const auto Push = Own == WHITE ? DEL_N  : DEL_S;
            static const auto LCap = Own == WHITE ? DEL_NW : DEL_SE;
            static const auto RCap = Own == WHITE ? DEL_NE : DEL_SW;

            static const Bitboard Rank3BB = Own == WHITE ? R3_bb : R6_bb;
            static const Bitboard Rank5BB = Own == WHITE ? R5_bb : R4_bb;
            static const Bitboard Rank7BB = Own == WHITE ? R7_bb : R2_bb;
            static const Bitboard Rank8BB = Own == WHITE ? R8_bb : R1_bb;

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
                    push_1 &= PawnAttacks[Opp][ci->king_sq];
                    push_2 &= PawnAttacks[Opp][ci->king_sq];
                    // Pawns which give discovered check
                    // Add pawn pushes which give discovered check.
                    // This is possible only if the pawn is not on the same file as the enemy king, because don't generate captures.
                    // Note that a possible discovery check promotion has been already generated among captures.
                    Bitboard dsc_pawns = Rx_pawns & ci->dsc_checkers & ~file_bb (ci->king_sq);
                    if (dsc_pawns != 0)
                    {
                        Bitboard push_cd_1 = empties & shift_bb<Push> (dsc_pawns);
                        Bitboard push_cd_2 = empties & shift_bb<Push> (push_cd_1 & Rank3BB);
                        push_1 |= push_cd_1;
                        push_2 |= push_cd_2;
                    }
                }
                else
                {
                    push_1 &= targets;
                    push_2 &= targets;
                }

                while (push_1 != 0) { auto dst = pop_lsq (push_1); (*moves++).move = mk_move<NORMAL> (dst - Push, dst); }
                while (push_2 != 0) { auto dst = pop_lsq (push_2); (*moves++).move = mk_move<NORMAL> (dst - Push-Push, dst); }
            }
            // Pawn normal and en-passant captures, no promotions
            if (   GT == RELAX
                || GT == EVASION
                || GT == CAPTURE)
            {
                Bitboard l_attacks = enemies & shift_bb<LCap> (Rx_pawns);
                Bitboard r_attacks = enemies & shift_bb<RCap> (Rx_pawns);

                while (l_attacks != 0) { auto dst = pop_lsq (l_attacks); (*moves++).move = mk_move<NORMAL> (dst - LCap, dst); }
                while (r_attacks != 0) { auto dst = pop_lsq (r_attacks); (*moves++).move = mk_move<NORMAL> (dst - RCap, dst); }

                auto ep_sq = pos.en_passant_sq ();
                if (ep_sq != SQ_NO)
                {
                    assert(rel_rank (Own, ep_sq) == R_6);
                    Bitboard ep_captures = Rx_pawns & Rank5BB;
                    if (ep_captures != 0)
                    {
                        // An en-passant capture can be an evasion only if the checking piece
                        // is the double pushed pawn and so is in the target.
                        // Otherwise this is a discovery check and are forced to do otherwise.
                        if (   GT != EVASION
                            || (targets & (ep_sq - Push)) != 0)
                        {
                            Bitboard ep_attacks = ep_captures & PawnAttacks[Opp][ep_sq];
                            assert(ep_attacks != 0);
                            assert(pop_count (ep_attacks) <= 2);

                            while (ep_attacks != 0) { (*moves++).move = mk_move<ENPASSANT> (pop_lsq (ep_attacks), ep_sq); }
                        }
                    }
                }
            }

            // Promotions (queening and under-promotions)
            if (R7_pawns != 0)
            {
                empties &= Rank8BB;
                enemies &= Rank8BB;
                if (GT == EVASION)
                {
                    empties &= targets;
                }
                // Promoting pawns
                Bitboard proms;
                proms = empties & shift_bb<Push> (R7_pawns);
                while (proms != 0) generate_promotion_moves<GT> (moves, pop_lsq (proms), Push, ci);

                proms = enemies & shift_bb<LCap> (R7_pawns);
                while (proms != 0) generate_promotion_moves<GT> (moves, pop_lsq (proms), LCap, ci);

                proms = enemies & shift_bb<RCap> (R7_pawns);
                while (proms != 0) generate_promotion_moves<GT> (moves, pop_lsq (proms), RCap, ci);
            }
        }


        // Generates all pseudo-legal moves of color for targets.
        template<GenType GT, Color Own>
        ValMove* generate_moves (ValMove *&moves, const Position &pos, Bitboard targets, const CheckInfo *ci = nullptr)
        {
            generate_pawn_moves<GT, Own> (moves, pos, targets, ci);
            generate_piece_moves<GT, Own, NIHT> (moves, pos, targets, ci);
            generate_piece_moves<GT, Own, BSHP> (moves, pos, targets, ci);
            generate_piece_moves<GT, Own, ROOK> (moves, pos, targets, ci);
            generate_piece_moves<GT, Own, QUEN> (moves, pos, targets, ci);
            if (GT != EVASION) generate_king_moves<GT, Own> (moves, pos, targets, ci);
            return moves;
        }

    }

    // Generates all pseudo-legal moves.
    template<GenType GT>
    ValMove* generate (ValMove *moves, const Position &pos)
    {
        assert(GT == RELAX
            || GT == CAPTURE
            || GT == QUIET);
        assert(pos.checkers () == 0);

        auto active  = pos.active ();
        Bitboard targets =
            GT == RELAX   ? ~pos.pieces (active) :
            GT == CAPTURE ? pos.pieces (~active) :
            GT == QUIET   ? ~pos.pieces () : 0;

        return active == WHITE ? generate_moves<GT, WHITE> (moves, pos, targets) :
               active == BLACK ? generate_moves<GT, BLACK> (moves, pos, targets) :
               moves;
    }
    // Explicit template instantiations

    // generate<RELAX> generates all pseudo-legal captures and non-captures.
    // Returns a pointer to the end of the move list.
    template ValMove* generate<RELAX  > (ValMove*, const Position&);
    // generate<CAPTURES> generates all pseudo-legal captures and queen promotions.
    // Returns a pointer to the end of the move list.
    template ValMove* generate<CAPTURE> (ValMove*, const Position&);
    // generate<QUIETS> generates all pseudo-legal non-captures and underpromotions.
    // Returns a pointer to the end of the move list.
    template ValMove* generate<QUIET  > (ValMove*, const Position&);

    // Generates all pseudo-legal non-captures and knight underpromotions moves that give check.
    // Returns a pointer to the end of the move list.
    template<>
    ValMove* generate<QUIET_CHECK> (ValMove *moves, const Position &pos)
    {
        assert(pos.checkers () == 0);

        auto active = pos.active ();
        Bitboard targets = ~pos.pieces ();
        CheckInfo ci (pos);
        // Pawns is excluded, will be generated together with direct checks
        Bitboard dsc_checkers = ci.dsc_checkers & ~pos.pieces (active, PAWN);
        while (dsc_checkers != 0)
        {
            auto org = pop_lsq (dsc_checkers);
            auto pt  = ptype (pos[org]);
            Bitboard attacks = attacks_bb (Piece(pt), org, pos.pieces ()) & targets;
            if (pt == KING)
            {
                // Clear path for checker
                attacks &= ~PieceAttacks[QUEN][ci.king_sq];
            }

            while (attacks != 0) { (*moves++).move = mk_move<NORMAL> (org, pop_lsq (attacks)); }
        }

        return active == WHITE ? generate_moves<QUIET_CHECK, WHITE> (moves, pos, targets, &ci) :
               active == BLACK ? generate_moves<QUIET_CHECK, BLACK> (moves, pos, targets, &ci) :
               moves;
    }

    // Generates all pseudo-legal check giving moves.
    // Returns a pointer to the end of the move list.
    template<>
    ValMove* generate<CHECK      > (ValMove *moves, const Position &pos)
    {
        auto active = pos.active ();
        Bitboard targets = ~pos.pieces (active);
        CheckInfo ci (pos);
        // Pawns is excluded, will be generated together with direct checks
        Bitboard dsc_checkers = ci.dsc_checkers & ~pos.pieces (active, PAWN);
        while (dsc_checkers != 0)
        {
            auto org = pop_lsq (dsc_checkers);
            auto pt  = ptype (pos[org]);
            Bitboard attacks = attacks_bb (Piece(pt), org, pos.pieces ()) & targets;
            if (pt == KING)
            {
                attacks &= ~PieceAttacks[QUEN][ci.king_sq]; // Clear path for checker
            }

            while (attacks != 0) { (*moves++).move = mk_move<NORMAL> (org, pop_lsq (attacks)); }
        }

        return active == WHITE ? generate_moves<CHECK, WHITE> (moves, pos, targets, &ci) :
               active == BLACK ? generate_moves<CHECK, BLACK> (moves, pos, targets, &ci) :
               moves;
    }

    // Generates all pseudo-legal check evasions moves when the side to move is in check.
    // Returns a pointer to the end of the move list.
    template<>
    ValMove* generate<EVASION    > (ValMove *moves, const Position &pos)
    {
        Bitboard checkers = pos.checkers ();
        assert(checkers != 0); // If any checker exists

        auto active  = pos.active ();
        auto king_sq = pos.square<KING> (active);

        auto check_sq = SQ_NO;

        Bitboard checker_attacks = 0;
        Bitboard sliders = checkers & ~pos.pieces (PAWN, NIHT);
        // Find squares attacked by slider checkers, will remove them from the king
        // evasions so to skip known illegal moves avoiding useless legality check later.
        while (sliders != 0)
        {
            check_sq = pop_lsq (sliders);
            assert(color (pos[check_sq]) == ~active);
            checker_attacks |= (  attacks_bb (pos[check_sq], check_sq, pos.pieces ())
                                | strline_bb (check_sq, king_sq)) - check_sq;
        }
        if (check_sq == SQ_NO)
        {
            check_sq = scan_lsq (checkers);
            checker_attacks = attacks_bb (pos[check_sq], check_sq, pos.pieces ());
        }

        // Generate evasions for king, capture and non capture moves
        Bitboard attacks =
              PieceAttacks[KING][king_sq]
            & ~(  pos.pieces (active)
                | checker_attacks
                | PieceAttacks[KING][pos.square<KING> (~active)]);

        while (attacks != 0) { (*moves++).move = mk_move<NORMAL> (king_sq, pop_lsq (attacks)); }

        // If double-check, then only a king move can save the day, triple+ check not possible
        if (   more_than_one (checkers)
            || pos.count<NONE> (active) == 1)
        {
            return moves;
        }

        // Generates blocking evasions or captures of the checking piece
        Bitboard targets = between_bb (check_sq, king_sq) + check_sq;

        return active == WHITE ? generate_moves<EVASION, WHITE> (moves, pos, targets) :
               active == BLACK ? generate_moves<EVASION, BLACK> (moves, pos, targets) :
               moves;
    }

    // Generates all legal moves.
    template<>
    ValMove* generate<LEGAL      > (ValMove *moves, const Position &pos)
    {
        return filter_illegal (pos, moves, pos.checkers () == 0 ?
                                               generate<RELAX  > (moves, pos) :
                                               generate<EVASION> (moves, pos));
    }

    ValMove* filter_illegal (const Position &pos, ValMove *beg_move, ValMove *end_move)
    {
        auto king_sq = pos.square<KING> (pos.active ());
        Bitboard abs_pinneds = pos.abs_pinneds (pos.active ());
        while (beg_move < end_move)
        {
            if (   (   abs_pinneds != 0
                    || org_sq (beg_move->move) == king_sq
                    || mtype (beg_move->move) == ENPASSANT)
                && !pos.legal (beg_move->move, abs_pinneds))
            {
                *beg_move = *(--end_move);
                continue;
            }
            assert(pos.pseudo_legal (beg_move->move));
            ++beg_move;
        }
        return end_move;
    }
}
