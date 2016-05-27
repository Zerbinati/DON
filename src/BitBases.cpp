#include "BitBases.h"

#include "BitBoard.h"

namespace BitBases {

    using namespace std;
    using namespace BitBoard;

    namespace {

        // There are 24 possible pawn squares: the first 4 files and ranks from 2 to 7
        const u32 MaxIndex = 2*24*i08(SQ_NO)*i08(SQ_NO); // stm * p_sq * wk_sq * bk_sq = 196608

        // Each u32 entity stores results of 32 positions, one per bit
        u32 KPK_Bitbase[MaxIndex / 32];

        // A KPK bitbase index is an integer in [0, MaxIndex] range
        //
        // Information is mapped in a way that minimizes the number of iterations:
        //
        // bit  0- 5: white king square (from SQ_A1 to SQ_H8)
        // bit  6-11: black king square (from SQ_A1 to SQ_H8)
        // bit    12: side to move color (WHITE or BLACK)
        // bit 13-14: white pawn file (from F_A to F_D)
        // bit 15-17: white pawn R_7 - rank (from R_7 to R_2)
        u32 index (Color c, Square wk_sq, Square bk_sq, Square wp_sq)
        {
            return wk_sq | (bk_sq << 6) | (c << 12) | (_file (wp_sq) << 13) | ((R_7 - _rank (wp_sq)) << 15);
        }

        enum Result : u08
        {
            INVALID = 0,
            UNKNOWN = 1,
            DRAW    = 2,
            WIN     = 4,
            LOSE    = 8,
        };

        Result& operator|= (Result &r1, Result r2) { return r1 = Result (r1|r2); }
        //Result& operator&= (Result &r1, Result r2) { return r1 = Result (r1&r2); }

        struct KPK_Position
        {
        private:

            Color  _active;
            Square _k_sq[CLR_NO]
                ,  _p_sq;
            Result _result;

            template<Color Own>
            Result classify (const vector<KPK_Position> &kpk_db)
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

                const auto Opp  = Own == WHITE ? BLACK : WHITE;
                const auto Good = Own == WHITE ? WIN  : DRAW;
                const auto Bad  = Own == WHITE ? DRAW : WIN;

                Result result = INVALID;
                Bitboard b = PieceAttacks[KING][_k_sq[Own]];
                while (b != 0)
                {
                    result |= Own == WHITE ?
                            kpk_db[index (Opp, pop_lsq (b), _k_sq[Opp], _p_sq)] :
                            kpk_db[index (Opp, _k_sq[Opp], pop_lsq (b), _p_sq)];
                }

                if (Own == WHITE)
                {
                    // Single push
                    if (_rank (_p_sq) < R_7)
                    {
                        result |= kpk_db[index (Opp, _k_sq[Own], _k_sq[Opp], _p_sq + DEL_N)];
                    }
                    // Double push
                    if (   _rank (_p_sq) == R_2
                        // Front is not own king
                        && _k_sq[Own] != (_p_sq + DEL_N)
                        // Front is not opp king
                        && _k_sq[Opp] != (_p_sq + DEL_N))
                    {
                        result |= kpk_db[index (Opp, _k_sq[Own], _k_sq[Opp], _p_sq + DEL_N + DEL_N)];
                    }
                }

                _result =
                    result & Good  ?
                        Good  :
                        result & UNKNOWN ?
                            UNKNOWN :
                            Bad;
                return _result;
            }

        public:

            KPK_Position () = default;

            explicit KPK_Position (u32 idx)
            {
                _k_sq[WHITE] = Square(         (idx >>  0) & 0x3F);
                _k_sq[BLACK] = Square(         (idx >>  6) & 0x3F);
                _active      = Color (         (idx >> 12) & 0x01);
                _p_sq        = File  (         (idx >> 13) & 0x03)
                             | Rank  (R_7-Rank((idx >> 15) & 0x07));

                // Check if two pieces are on the same square or if a king can be captured
                if (   dist (_k_sq[WHITE], _k_sq[BLACK]) <= 1
                    || _k_sq[WHITE] == _p_sq
                    || _k_sq[BLACK] == _p_sq
                    || (_active == WHITE && (PawnAttacks[WHITE][_p_sq] & _k_sq[BLACK]) != 0))
                {
                    _result = INVALID;
                }
                else
                // Immediate win if a pawn can be promoted without getting captured
                if (   _active == WHITE
                    && _rank (_p_sq) == R_7
                    && _k_sq[WHITE] != (_p_sq + DEL_N)
                    && (   dist (_k_sq[BLACK], _p_sq + DEL_N) > 1
                        || (PieceAttacks[KING][_k_sq[WHITE]] & (_p_sq + DEL_N)) != 0))
                {
                    _result = WIN;
                }
                else
                // Immediate draw if is a stalemate or king captures undefended pawn
                if (   _active == BLACK
                    && (   (PieceAttacks[KING][_k_sq[BLACK]] & ~(PieceAttacks[KING][_k_sq[WHITE]] | PawnAttacks[WHITE][_p_sq])) == 0
                        || ((PieceAttacks[KING][_k_sq[BLACK]] & ~PieceAttacks[KING][_k_sq[WHITE]]) & _p_sq) != 0))
                {
                    _result = DRAW;
                }
                else
                // Position will be classified later
                {
                    _result  = UNKNOWN;
                }
            }

            operator Result () const { return _result; }

            Result classify (const vector<KPK_Position>& kpk_db)
            {
                return _active == WHITE ? classify<WHITE> (kpk_db) : classify<BLACK> (kpk_db);
            }

        };

    }

    void initialize ()
    {
        vector<KPK_Position> kpk_db;
        kpk_db.reserve (MaxIndex);
        // Initialize db with known win / draw positions
        for (u32 idx = 0; idx < MaxIndex; ++idx)
        {
            kpk_db.push_back (KPK_Position (idx));
        }
        kpk_db.shrink_to_fit ();

        bool repeat;
        // Iterate through the positions until none of the unknown positions can be
        // changed to either wins or draws (15 cycles needed).
        do {
            repeat = false;
            for (u32 idx = 0; idx < MaxIndex; ++idx)
            {
                repeat |= kpk_db[idx] == Result::UNKNOWN
                       && kpk_db[idx].classify (kpk_db) != Result::UNKNOWN;
            }
        } while (repeat);

        // Map 32 results into one KPK_Bitbase[] entry
        for (u32 idx = 0; idx < MaxIndex; ++idx)
        {
            if (kpk_db[idx] == Result::WIN)
            {
                KPK_Bitbase[idx / 32] |= 1 << (idx & 0x1F);
            }
        }
    }

    bool probe (Color c, Square wk_sq, Square wp_sq, Square bk_sq)
    {
        assert(_file (wp_sq) <= F_D);

        u32 idx = index (c, wk_sq, bk_sq, wp_sq);
        return KPK_Bitbase[idx / 32] & (1 << (idx & 0x1F));
    }

}