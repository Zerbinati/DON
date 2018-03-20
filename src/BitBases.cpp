#include "BitBases.h"

#include <vector>
#include "BitBoard.h"

namespace BitBases {

    using namespace std;
    using namespace BitBoard;

    namespace {

        // There are 24 possible pawn squares: the first 4 files and ranks from 2 to 7
        constexpr u32 MaxIndex = 2*24*+Square::NO*+Square::NO; // stm * p_sq * wk_sq * bk_sq = 196608

        // Each u32 entity stores results of 32 positions, one per bit
        u32 KPK_Bitbase[MaxIndex / 32];

        // A KPK bitbase index is an integer in [0, MaxIndex] range
        //
        // Information is mapped in a way that minimizes the number of iterations:
        //
        // bit  0- 5: white king square (from Square::A1 to Square::H8)
        // bit  6-11: black king square (from Square::A1 to Square::H8)
        // bit    12: color (Color::WHITE or Color::BLACK)
        // bit 13-14: white pawn file (from fA to fD)
        // bit 15-17: white pawn Rank::r7 - rank (from Rank::r7 to Rank::r2)
        u32 index (Color c, Square wk_sq, Square bk_sq, Square wp_sq)
        {
            return +wk_sq | (+bk_sq << 6) | (+c << 12) | (+_file (wp_sq) << 13) | ((+Rank::r7 - +_rank (wp_sq)) << 15);
        }

        enum class Result : u08
        {
            INVALID = 0,
            UNKNOWN = 1,
            DRAW    = 2,
            WIN     = 4,
            LOSE    = 8,
        };

        Result& operator|= (Result &r1, Result r2) { return r1 = Result(+r1|+r2); }
        //Result& operator&= (Result &r1, Result r2) { return r1 = Result(+r1&+r2); }

        struct KPK_Position
        {
        private:
            Color  active;
            Square k_sq[+Color::NO]
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

                constexpr auto Opp  = Color::WHITE == Own ? Color::BLACK : Color::WHITE;
                constexpr auto Good = Color::WHITE == Own ? Result::WIN : Result::DRAW;
                constexpr auto Bad  = Color::WHITE == Own ? Result::DRAW : Result::WIN;

                Result r = Result::INVALID;
                Bitboard b = PieceAttacks[+PieceType::KING][+k_sq[+Own]];
                while (0 != b)
                {
                    r |= Color::WHITE == Own ?
                            db[index (Opp, pop_lsq (b), k_sq[+Opp], p_sq)].result :
                            db[index (Opp, k_sq[+Opp], pop_lsq (b), p_sq)].result;
                }

                if (Color::WHITE == Own)
                {
                    // Single push
                    if (_rank (p_sq) < Rank::r7)
                    {
                        r |= db[index (Opp, k_sq[+Own], k_sq[+Opp], p_sq + Delta::NORTH)].result;
                    }
                    // Double push
                    if (   _rank (p_sq) == Rank::r2
                        // Front is not own king
                        && k_sq[+Own] != (p_sq + Delta::NORTH)
                        // Front is not opp king
                        && k_sq[+Opp] != (p_sq + Delta::NORTH))
                    {
                        r |= db[index (Opp, k_sq[+Own], k_sq[+Opp], p_sq + Delta::NORTH + Delta::NORTH)].result;
                    }
                }

                return result = +r & +Good  ?
                                      Good  :
                                      +r & +Result::UNKNOWN ?
                                            Result::UNKNOWN :
                                            Bad;
            }

        public:

            Result result;

            KPK_Position () = default;
            explicit KPK_Position (u32 idx)
            {
                k_sq[+Color::WHITE] = Square(         (idx >>  0) & +Square::H8);
                k_sq[+Color::BLACK] = Square(         (idx >>  6) & +Square::H8);
                active      = Color (         (idx >> 12) & +Color::BLACK);
                p_sq        = File  (         (idx >> 13) & 0x03)
                            | Rank  (Rank::r7-Rank((idx >> 15) & 0x07));

                // Check if two pieces are on the same square or if a king can be captured
                if (   dist (k_sq[+Color::WHITE], k_sq[+Color::BLACK]) <= 1
                    || k_sq[+Color::WHITE] == p_sq
                    || k_sq[+Color::BLACK] == p_sq
                    || (   Color::WHITE == active
                        && contains (PawnAttacks[+Color::WHITE][+p_sq], k_sq[+Color::BLACK])))
                {
                    result = Result::INVALID;
                }
                else
                // Immediate win if a pawn can be promoted without getting captured
                if (   Color::WHITE == active
                    && _rank (p_sq) == Rank::r7
                    && k_sq[+Color::WHITE] != (p_sq + Delta::NORTH)
                    && (   dist (k_sq[+Color::BLACK], p_sq + Delta::NORTH) > 1
                        || contains (PieceAttacks[+PieceType::KING][+k_sq[+Color::WHITE]], p_sq + Delta::NORTH)))
                {
                    result = Result::WIN;
                }
                else
                // Immediate draw if is a stalemate or king captures undefended pawn
                if (   Color::BLACK == active
                    && (   0 == (PieceAttacks[+PieceType::KING][+k_sq[+Color::BLACK]] & ~(PieceAttacks[+PieceType::KING][+k_sq[+Color::WHITE]] | PawnAttacks[+Color::WHITE][+p_sq]))
                        || contains (PieceAttacks[+PieceType::KING][+k_sq[+Color::BLACK]] & ~PieceAttacks[+PieceType::KING][+k_sq[+Color::WHITE]], p_sq)))
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
                return Color::WHITE == active ?
                        classify<Color::WHITE> (db) :
                        classify<Color::BLACK> (db);
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
        assert(_file (wp_sq) <= File::fD);

        u32 idx = index (c, wk_sq, bk_sq, wp_sq);
        return KPK_Bitbase[idx / 32] & (1 << (idx & 0x1F));
    }

}