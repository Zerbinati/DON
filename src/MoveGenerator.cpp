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
            assert(NIHT == PT
                || BSHP == PT
                || ROOK == PT
                || QUEN == PT);

            for (auto s : pos.squares[Own][PT])
            {
                Bitboard attacks = targets & PieceAttacks[PT][s];
                if (   CHECK == GT
                    || QUIET_CHECK == GT)
                {
                    if (contains (pos.dsc_blockers (pos.active), s))
                    {
                        continue;
                    }
                    attacks &= pos.si->checks[PT];
                }
                if (0 != attacks)
                {
                    if (NIHT != PT)
                    {
                        attacks &= attacks_bb<PT> (s, pos.pieces ());
                    }
                    while (0 != attacks) { moves.push_back (ValMove(mk_move<NORMAL> (s, pop_lsq (attacks)))); }
                }
            }
        }

        // Generates PAWN promotion move
        template<GenType GT, Delta Del>
        void generate_promotion_moves (vector<ValMove> &moves, const Position &pos, Square dst)
        {
            assert(DEL_N == Del
                || DEL_NE == Del
                || DEL_NW == Del
                || DEL_S == Del
                || DEL_SE == Del
                || DEL_SW == Del);

            switch (GT)
            {
            case NATURAL:
            case EVASION:
            case CAPTURE:
            case QUIET:
                if (QUIET != GT)
                {
                    moves.push_back (ValMove(mk_move (dst - Del, dst, QUEN)));
                }
                if (CAPTURE != GT)
                {
                    moves.push_back (ValMove(mk_move (dst - Del, dst, ROOK)));
                    moves.push_back (ValMove(mk_move (dst - Del, dst, BSHP)));
                    moves.push_back (ValMove(mk_move (dst - Del, dst, NIHT)));
                }
                break;
            case QUIET_CHECK:
            {
                auto ek_sq = pos.square (~pos.active, KING);
                if (   contains (PieceAttacks[ROOK][dst]                            , ek_sq)
                    && contains (attacks_bb<ROOK> (dst, pos.pieces () ^ (dst - Del)), ek_sq))
                {
                    moves.push_back (ValMove(mk_move (dst - Del, dst, ROOK)));
                }
                if (   contains (PieceAttacks[BSHP][dst]                            , ek_sq)
                    && contains (attacks_bb<BSHP> (dst, pos.pieces () ^ (dst - Del)), ek_sq))
                {
                    moves.push_back (ValMove(mk_move (dst - Del, dst, BSHP)));
                }
                if (   contains (PieceAttacks[NIHT][dst]                            , ek_sq))
                {
                    moves.push_back (ValMove(mk_move (dst - Del, dst, NIHT)));
                }
            }
                break;
            case CHECK:
            {
                auto ek_sq = pos.square (~pos.active, KING);
                if (   contains (PieceAttacks[QUEN][dst]                            , ek_sq)
                    && contains (attacks_bb<QUEN> (dst, pos.pieces () ^ (dst - Del)), ek_sq))
                {
                    moves.push_back (ValMove(mk_move (dst - Del, dst, QUEN)));
                }
                if (   contains (PieceAttacks[ROOK][dst]                            , ek_sq)
                    && contains (attacks_bb<ROOK> (dst, pos.pieces () ^ (dst - Del)), ek_sq))
                {
                    moves.push_back (ValMove(mk_move (dst - Del, dst, ROOK)));
                }
                if (   contains (PieceAttacks[BSHP][dst]                            , ek_sq)
                    && contains (attacks_bb<BSHP> (dst, pos.pieces () ^ (dst - Del)), ek_sq))
                {
                    moves.push_back (ValMove(mk_move (dst - Del, dst, BSHP)));
                }
                if (   contains (PieceAttacks[NIHT][dst]                            , ek_sq))
                {
                    moves.push_back (ValMove(mk_move (dst - Del, dst, NIHT)));
                }
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
            if (CAPTURE != GT)
            {
                Bitboard push_1 = empties & shift<Push> (Rx_pawns);
                Bitboard push_2 = empties & shift<Push> (push_1 & Rank3BB);
                if (   CHECK == GT
                    || QUIET_CHECK == GT)
                {
                    push_1 &= pos.si->checks[PAWN];
                    push_2 &= pos.si->checks[PAWN];
                    // Pawns which give discovered check
                    // Add pawn pushes which give discovered check.
                    // This is possible only if the pawn is not on the same file as the enemy king, because don't generate captures.
                    // Note that a possible discovery check promotion has been already generated among captures.
                    Bitboard dsc_pawns = Rx_pawns & pos.dsc_blockers (pos.active) & ~file_bb (pos.square (Opp, KING));
                    if (0 != dsc_pawns)
                    {
                        Bitboard dc_push_1 = empties & shift<Push> (dsc_pawns);
                        Bitboard dc_push_2 = empties & shift<Push> (dc_push_1 & Rank3BB);
                        push_1 |= dc_push_1;
                        push_2 |= dc_push_2;
                    }
                }
                push_1 &= targets;
                push_2 &= targets;
                while (0 != push_1) { auto dst = pop_lsq (push_1); moves.push_back (ValMove(mk_move<NORMAL> (dst - Push, dst))); }
                while (0 != push_2) { auto dst = pop_lsq (push_2); moves.push_back (ValMove(mk_move<NORMAL> (dst - Push*2, dst))); }
            }
            // Pawn normal and en-passant captures, no promotions
            if (   QUIET != GT
                && QUIET_CHECK != GT)
            {
                Bitboard l_attack = enemies & shift<LCap> (Rx_pawns);
                Bitboard r_attack = enemies & shift<RCap> (Rx_pawns);
                if (CHECK == GT)
                {
                    l_attack &= pos.si->checks[PAWN];
                    r_attack &= pos.si->checks[PAWN];
                    // Pawns which give discovered check
                    // Add pawn captures which give discovered check.
                    Bitboard dsc_pawns = Rx_pawns & pos.dsc_blockers (pos.active);
                    if (0 != dsc_pawns)
                    {
                        l_attack |= enemies & shift<LCap> (dsc_pawns);
                        r_attack |= enemies & shift<RCap> (dsc_pawns);
                    }
                }
                while (0 != l_attack) { auto dst = pop_lsq (l_attack); moves.push_back (ValMove(mk_move<NORMAL> (dst - LCap, dst))); }
                while (0 != r_attack) { auto dst = pop_lsq (r_attack); moves.push_back (ValMove(mk_move<NORMAL> (dst - RCap, dst))); }

                if (SQ_NO != pos.si->en_passant_sq)
                {
                    assert(rel_rank (Own, pos.si->en_passant_sq) == R_6);
                    Bitboard ep_captures = Rx_pawns & Rank5BB;
                    if (0 != ep_captures)
                    {
                        // If the checking piece is the double pushed pawn and also is in the target.
                        // Otherwise this is a discovery check and are forced to do otherwise.
                        if (EVASION == GT)
                        {
                            ep_captures &= (  shift<DEL_E> (targets)
                                            | shift<DEL_W> (targets));
                        }
                        ep_captures &= PawnAttacks[Opp][pos.si->en_passant_sq];
                        assert(0 != ep_captures);
                        assert(pop_count (ep_captures) <= 2);
                        while (0 != ep_captures) { moves.push_back (ValMove(mk_move<ENPASSANT> (pop_lsq (ep_captures), pos.si->en_passant_sq))); }
                    }
                }
            }

            // Promotions (queening and under-promotions)
            if (0 != R7_pawns)
            {
                if (EVASION == GT)
                {
                    empties &= targets;
                }
                // Promoting pawns
                Bitboard proms;
                proms = empties & shift<Push> (R7_pawns);
                while (0 != proms) generate_promotion_moves<GT, Push> (moves, pos, pop_lsq (proms));
                proms = enemies & shift<LCap> (R7_pawns);
                while (0 != proms) generate_promotion_moves<GT, LCap> (moves, pos, pop_lsq (proms));
                proms = enemies & shift<RCap> (R7_pawns);
                while (0 != proms) generate_promotion_moves<GT, RCap> (moves, pos, pop_lsq (proms));
            }
        }

        // Generates KING castling move
        template<GenType GT, Color Own, CastleSide CS>
        void generate_castling_moves (vector<ValMove> &moves, const Position &pos)
        {
            static const auto Opp = Own == WHITE ? BLACK : WHITE;

            assert(EVASION != GT
                && 0 == pos.si->checkers
                && pos.can_castle (Own, CS)
                && !pos.impeded_castle (Own, CS));

            auto king_org = pos.square (Own, KING);
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
            auto king_dst = rel_sq (Own, CS == CS_KING ? SQ_G1 : SQ_C1);
            // Chess960
            // Because generate only legal castling moves needed to verify that
            // when moving the castling rook do not discover some hidden checker.
            // For instance an enemy queen in SQ_A1 when castling rook is in SQ_B1.
            if (   0 != (b = pos.pieces (Opp, ROOK, QUEN) & FA_bb & rank_bb (king_dst))
                && 0 != (b & PieceAttacks[ROOK][king_dst])
                && 0 != (b & attacks_bb<ROOK> (king_dst, pos.pieces () ^ rook_org)))
            {
                return;
            }

            auto m = mk_move<CASTLE> (king_org, rook_org);
            if (   (   CHECK != GT
                    && QUIET_CHECK != GT)
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

            assert(EVASION != GT);

            if (   CHECK != GT
                && QUIET_CHECK != GT)
            {
                auto king_sq = pos.square (Own, KING);
                Bitboard attacks = targets
                                 &  PieceAttacks[KING][king_sq]
                                 & ~PieceAttacks[KING][pos.square (Opp, KING)];
                while (0 != attacks) { moves.push_back (ValMove(mk_move<NORMAL> (king_sq, pop_lsq (attacks)))); }
            }

            if (CAPTURE != GT)
            {
                if (   0 == pos.si->checkers
                    && pos.can_castle (Own))
                {
                    if (   pos.can_castle (Own, CS_KING)
                        && !pos.impeded_castle (Own, CS_KING))
                    {
                        generate_castling_moves<GT, Own, CS_KING> (moves, pos);
                    }
                    if (   pos.can_castle (Own, CS_QUEN)
                        && !pos.impeded_castle (Own, CS_QUEN))
                    {
                        generate_castling_moves<GT, Own, CS_QUEN> (moves, pos);
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
            if (EVASION != GT)
            {
                generate_king_moves<GT, Own> (moves, pos, targets);
            }
        }
    }

    template<GenType GT> void generate (vector<ValMove> &moves, const Position &pos)
    {
        assert(0 == pos.si->checkers);
        assert(NATURAL == GT
            || CAPTURE == GT
            || QUIET == GT);
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

        WHITE == pos.active ?
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
        assert(0 == pos.si->checkers);
        moves.clear ();
        Bitboard targets = ~pos.pieces ();
        // Pawns is excluded, will be generated together with direct checks
        Bitboard dsc_blockers = pos.dsc_blockers (pos.active) & ~pos.pieces (PAWN);
        while (0 != dsc_blockers)
        {
            auto org = pop_lsq (dsc_blockers);
            Bitboard attacks = targets;
            switch (ptype (pos[org]))
            {
            case NIHT: attacks &= PieceAttacks[NIHT][org];                            break;
            case BSHP: attacks &= PieceAttacks[BSHP][org];
                if (0 != attacks) attacks &= attacks_bb<BSHP> (org, pos.pieces ());   break;
            case ROOK: attacks &= PieceAttacks[ROOK][org];
                if (0 != attacks) attacks &= attacks_bb<ROOK> (org, pos.pieces ());   break;
            case QUEN: attacks &= PieceAttacks[QUEN][org];
                if (0 != attacks) attacks &= attacks_bb<QUEN> (org, pos.pieces ());   break;
            case KING: attacks &= PieceAttacks[KING][org]
                               & ~PieceAttacks[QUEN][pos.square (~pos.active, KING)]; break;
            default: assert(false); break;
            }
            while (0 != attacks) { moves.push_back (ValMove(mk_move<NORMAL> (org, pop_lsq (attacks)))); }
        }

        WHITE == pos.active ?
            generate_moves<QUIET_CHECK, WHITE> (moves, pos, targets) :
            generate_moves<QUIET_CHECK, BLACK> (moves, pos, targets);
    }
    // Generates all pseudo-legal check giving moves.
    template<> void generate<CHECK      > (vector<ValMove> &moves, const Position &pos)
    {
        assert(0 == pos.si->checkers);
        moves.clear ();
        Bitboard targets = ~pos.pieces (pos.active);
        // Pawns is excluded, will be generated together with direct checks
        Bitboard dsc_blockers = pos.dsc_blockers (pos.active) & ~pos.pieces (PAWN);
        while (0 != dsc_blockers)
        {
            auto org = pop_lsq (dsc_blockers);
            Bitboard attacks = targets;
            switch (ptype (pos[org]))
            {
            case NIHT: attacks &= PieceAttacks[NIHT][org];                            break;
            case BSHP: attacks &= PieceAttacks[BSHP][org];
                if (0 != attacks) attacks &= attacks_bb<BSHP> (org, pos.pieces ());   break;
            case ROOK: attacks &= PieceAttacks[ROOK][org];
                if (0 != attacks) attacks &= attacks_bb<ROOK> (org, pos.pieces ());   break;
            case QUEN: attacks &= PieceAttacks[QUEN][org];
                if (0 != attacks) attacks &= attacks_bb<QUEN> (org, pos.pieces ());   break;
            case KING: attacks &= PieceAttacks[KING][org]
                               & ~PieceAttacks[QUEN][pos.square (~pos.active, KING)]; break;
            default: assert(false); break;
            }
            while (0 != attacks) { moves.push_back (ValMove(mk_move<NORMAL> (org, pop_lsq (attacks)))); }
        }

        WHITE == pos.active ?
            generate_moves<CHECK, WHITE> (moves, pos, targets) :
            generate_moves<CHECK, BLACK> (moves, pos, targets);
    }
    // Generates all pseudo-legal check evasions moves when the side to move is in check.
    template<> void generate<EVASION    > (vector<ValMove> &moves, const Position &pos)
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
        auto fk_sq = pos.square (pos.active, KING);
        Bitboard mocc = pos.pieces () ^ fk_sq;
        Bitboard sliders = pos.si->checkers & ~(pos.pieces (PAWN) | jumpers);
        // Squares attacked by slider checkers will remove them from the king evasions
        // so to skip known illegal moves avoiding useless legality check later.
        while (0 != sliders)
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
        while (0 != attacks) { moves.push_back (ValMove(mk_move<NORMAL> (fk_sq, pop_lsq (attacks)))); }

        // If double-check or only king, then only king move can save the day
        if (   more_than_one (pos.si->checkers)
            || 1 == pos.count<NONE> (pos.active))
        {
            return;
        }

        // Generates blocking or captures of the checking piece
        Bitboard targets = 
            SQ_NO == checker_sq ?
                square_bb (scan_lsq (pos.si->checkers)) : // Pawn checker
                between_bb (checker_sq, fk_sq) | checker_sq;

        WHITE == pos.active ?
            generate_moves<EVASION, WHITE> (moves, pos, targets) :
            generate_moves<EVASION, BLACK> (moves, pos, targets);
    }
    // Generates all legal moves.
    template<> void generate<LEGAL      > (vector<ValMove> &moves, const Position &pos)
    {
        0 == pos.si->checkers ?
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
                                         //bool b =
                                         return
                                            (   0 != pos.abs_blockers (pos.active)
                                             || ENPASSANT == mtype (vm.move)
                                             || pos.square (pos.active, KING) == org_sq (vm.move))
                                         && !pos.legal (vm.move);
                                         //if (!b) { assert(pos.pseudo_legal (vm.move)); }
                                         //return b;
                                     }),
                     moves.end ());
    }

}
