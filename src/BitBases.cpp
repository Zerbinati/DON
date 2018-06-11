#include "BitBases.h"

#include <vector>
#include "BitBoard.h"

namespace BitBases {

    using namespace std;
    using namespace BitBoard;

    namespace {

        // There are 24 possible pawn squares: the first 4 files and ranks from 2 to 7
        constexpr u32 MaxIndex = 2*24*SQ_NO*SQ_NO; // stm * p_sq * wk_sq * bk_sq = 196608

        // Each u32 entity stores results of 32 positions, one per bit
        u32 KPK_Bitbase[MaxIndex / 32];

        // A KPK bitbase index is an integer in [0, MaxIndex] range
        //
        // Information is mapped in a way that minimizes the number of iterations:
        //
        // bit  0- 5: white king square (from SQ_A1 to SQ_H8)
        // bit  6-11: black king square (from SQ_A1 to SQ_H8)
        // bit    12: color (WHITE or BLACK)
        // bit 13-14: white pawn file (from F_A to F_D)
        // bit 15-17: white pawn R_7 - rank (from R_7 to R_2)
        u32 index (Color c, Square wk_sq, Square bk_sq, Square wp_sq)
        {
            return u32(wk_sq)
                | (u32(bk_sq) << 6)
                | (u32(c) << 12)
                | (u32(_file (wp_sq)) << 13)
                | (u32(R_7 - _rank (wp_sq)) << 15);
        }

        enum Result : u08
        {
            NONE    = 0,
            UNKNOWN = 1,
            DRAW    = 2,
            WIN     = 4,
            LOSE    = 8,
        };

        Result& operator|= (Result &r1, Result r2) { return r1 = Result(r1|r2); }
        //Result& operator&= (Result &r1, Result r2) { return r1 = Result(r1&r2); }

        struct KPK_Position
        {
        private:
            Color  active;
            Square k_sq[CLR_NO]
                ,  p_sq;

            template<Color Own>
            Result classify (const vector<KPK_Position> &db)
            {
                // White to Move:
                // If one move leads to a position classified as WIN, the result of the current position is WIN.
                // If all moves lead to positions classified as DRAW, the result of the current position is DRAW
                // otherwise the current position is classified as UNKNOWN.
                //
                // Black to Move:
                // If one move leads to a position classified as DRAW, the result of the current position is DRAW.
                // If all moves lead to positions classified as WIN, the result of the current position is WIN
                // otherwise the current position is classified as UNKNOWN.

                constexpr auto Opp  = WHITE == Own ? BLACK : WHITE;
                constexpr auto Good = WHITE == Own ? Result::WIN : Result::DRAW;
                constexpr auto Bad  = WHITE == Own ? Result::DRAW : Result::WIN;

                Result r = Result::NONE;
                Bitboard b = PieceAttacks[KING][k_sq[Own]];
                while (0 != b)
                {
                    r |= WHITE == Own ?
                            db[index (Opp, pop_lsq (b), k_sq[Opp], p_sq)].result :
                            db[index (Opp, k_sq[Opp], pop_lsq (b), p_sq)].result;
                }

                if (WHITE == Own)
                {
                    // Single push
                    if (_rank (p_sq) < R_7)
                    {
                        r |= db[index (Opp, k_sq[Own], k_sq[Opp], p_sq + DEL_N)].result;
                    }
                    // Double push
                    if (   _rank (p_sq) == R_2
                        // Front is not own king
                        && k_sq[Own] != (p_sq + DEL_N)
                        // Front is not opp king
                        && k_sq[Opp] != (p_sq + DEL_N))
                    {
                        r |= db[index (Opp, k_sq[Own], k_sq[Opp], p_sq + DEL_N + DEL_N)].result;
                    }
                }

                return result = r & Good  ?
                                    Good  :
                                    r & Result::UNKNOWN ?
                                        Result::UNKNOWN :
                                        Bad;
            }

        public:

            Result result;

            KPK_Position () = default;
            explicit KPK_Position (u32 idx)
            {
                k_sq[WHITE] = Square(        (idx >>  0) & i08(SQ_H8));
                k_sq[BLACK] = Square(        (idx >>  6) & i08(SQ_H8));
                active      = Color(         (idx >> 12) & i08(BLACK));
                p_sq        = File(          (idx >> 13) & 3)
                            | Rank(i08(R_7)-((idx >> 15) & i08(R_8)));

                // Check if two pieces are on the same square or if a king can be captured
                if (   1 >= dist (k_sq[WHITE], k_sq[BLACK])
                    || k_sq[WHITE] == p_sq
                    || k_sq[BLACK] == p_sq
                    || (   WHITE == active
                        && contains (PawnAttacks[WHITE][p_sq], k_sq[BLACK])))
                {
                    result = Result::NONE;
                }
                else
                // Immediate win if a pawn can be promoted without getting captured
                if (   WHITE == active
                    && _rank (p_sq) == R_7
                    && k_sq[WHITE] != (p_sq + DEL_N)
                    && (   1 < dist (k_sq[BLACK], p_sq + DEL_N)
                        || contains (PieceAttacks[KING][k_sq[WHITE]], p_sq + DEL_N)))
                {
                    result = Result::WIN;
                }
                else
                // Immediate draw if is a stalemate or king captures undefended pawn
                if (   BLACK == active
                    && (   0 == (PieceAttacks[KING][k_sq[BLACK]] & ~(PieceAttacks[KING][k_sq[WHITE]] | PawnAttacks[WHITE][p_sq]))
                        || contains (PieceAttacks[KING][k_sq[BLACK]] & ~PieceAttacks[KING][k_sq[WHITE]], p_sq)))
                {
                    result = Result::DRAW;
                }
                else
                // Position will be classified later
                {
                    result = Result::UNKNOWN;
                }
            }

            Result classify (const vector<KPK_Position> &db)
            {
                return WHITE == active ?
                        classify<WHITE> (db) :
                        classify<BLACK> (db);
            }
        };
    }

    void initialize ()
    {
        vector<KPK_Position> db;
        db.reserve (MaxIndex);
        // Initialize db with known win / draw positions
        for (u32 idx = 0; idx < MaxIndex; ++idx)
        {
            db.push_back (KPK_Position (idx));
        }

        bool repeat;
        // Iterate through the positions until none of the unknown positions can be
        // changed to either wins or draws (15 cycles needed).
        do
        {
            repeat = false;
            for (u32 idx = 0; idx < MaxIndex; ++idx)
            {
                repeat |= (   Result::UNKNOWN == db[idx].result
                           && Result::UNKNOWN != db[idx].classify (db));
            }
        }
        while (repeat);

        // Map 32 results into one KPK_Bitbase[] entry
        for (u32 idx = 0; idx < MaxIndex; ++idx)
        {
            if (Result::WIN == db[idx].result)
            {
                KPK_Bitbase[idx / 32] |= 1 << (idx & 0x1F);
            }
        }
    }

    bool probe (Color c, Square wk_sq, Square wp_sq, Square bk_sq)
    {
        assert(_file (wp_sq) <= F_D);

        auto idx = index (c, wk_sq, bk_sq, wp_sq);
        return KPK_Bitbase[idx / 32] & (1 << (idx & 0x1F));
    }

}