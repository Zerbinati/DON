#ifndef _TYPE_H_INC_
#define _TYPE_H_INC_

#include <algorithm>
#include <array>
#include <cassert>
#include <type_traits>
#include <cctype>
#include <chrono>
#include <climits>
#include <cstring>
#include <functional>
#include <iomanip>
#include <iosfwd>
#include <sstream>
#include <string>
#include <vector>

/// Compiling:
/// With Makefile (e.g. for Linux and OSX), configuration is done automatically, to get started type 'make help'.
/// Without Makefile (e.g. with Microsoft Visual Studio) some switches need to be set manually:
///
/// -DNDEBUG    | Disable debugging mode. Always use this.
/// -DPREFETCH  | Enable use of prefetch asm-instruction.
///             | Don't enable it if want the executable to run on some very old machines.
/// -DABM       | Add runtime support for use of ABM asm-instruction. Works only in 64-bit mode.
///             | For compiling requires hardware with ABM support.
/// -DBM2       | Add runtime support for use of BM2 asm-instruction. Works only in 64-bit mode.
///             | For compiling requires hardware with BM2 support.
/// -DLPAGES    | Add runtime support for large pages.

/// Predefined macros hell:
///
/// __GNUC__           Compiler is gcc, Clang or Intel on Linux
/// __INTEL_COMPILER   Compiler is Intel
/// _MSC_VER           Compiler is MSVC or Intel on Windows
/// _WIN32             Building on Windows (any)
/// _WIN64             Building on Windows 64 bit

// Windows or MinGW
#if defined(_WIN32)

// Auto make 64-bit compiles
#   if defined(_WIN64) && defined(_MSC_VER) // No Makefile used
#       if !defined(BIT64)
#           define BIT64
#       endif
#   endif

#endif

#if defined(BM2)
#   include <immintrin.h>   // Header for BMI2 instructions
// BEXTR = Bit field extract (with register)
// PDEP  = Parallel bits deposit
// PEXT  = Parallel bits extract
// BLSR  = Reset lowest set bit
#   if defined(BIT64)
#       define BEXTR(b, m, l)   _bextr_u64 (b, m, l)
#       define PDEP(b, m)       _pdep_u64 (b, m)
#       define PEXT(b, m)       _pext_u64 (b, m)
#       define BLSR(b)          _blsr_u64 (b)
#   else
//#       define BEXTR(b, m, l)   _bextr_u32 (b, m, l)
//#       define PDEP(b, m)       _pdep_u32 (b, m)
//#       define PEXT(b, m)       _pext_u32 (b, m)
//#       define BLSR(b)          _blsr_u32 (b)
#   endif
#endif

#if defined(_MSC_VER)
// Disable some silly and noisy warning from MSVC compiler
#   pragma warning (disable: 4127) // Conditional expression is constant
#   pragma warning (disable: 4146) // Unary minus operator applied to unsigned type
#   pragma warning (disable: 4800) // Forcing value to bool 'true' or 'false'

typedef   signed __int8     i08;
typedef unsigned __int8     u08;
typedef   signed __int16    i16;
typedef unsigned __int16    u16;
typedef   signed __int32    i32;
typedef unsigned __int32    u32;
typedef   signed __int64    i64;
typedef unsigned __int64    u64;

#   define S32(X) (X ##  i32)
#   define U32(X) (X ## ui32)
#   define S64(X) (X ##  i64)
#   define U64(X) (X ## ui64)

#else

#   include <inttypes.h>

typedef          int8_t    i08;
typedef         uint8_t    u08;
typedef         int16_t    i16;
typedef        uint16_t    u16;
typedef         int32_t    i32;
typedef        uint32_t    u32;
typedef         int64_t    i64;
typedef        uint64_t    u64;

#   define S32(X) (X ##   L)
#   define U32(X) (X ##  UL)
#   define S64(X) (X ##  LL)
#   define U64(X) (X ## ULL)

#endif

/// Preloads the given address in L1/L2 cache.
/// This is a non-blocking function that doesn't stall the CPU
/// waiting for data to be loaded from memory, which can be quite slow.
#if defined(PREFETCH)
#   if defined(_MSC_VER) || defined(__INTEL_COMPILER)

#   include <xmmintrin.h> // Intel and Microsoft header for _mm_prefetch()

inline void prefetch (const void *addr)
{
#   if defined(__INTEL_COMPILER)
    // This hack prevents prefetches from being optimized away by
    // Intel compiler. Both MSVC and gcc seem not be affected by this.
    __asm__ ("");
#   endif
    _mm_prefetch (reinterpret_cast<const char*> (addr), _MM_HINT_T0);
}

#   else

inline void prefetch (const void *addr)
{
    __builtin_prefetch (addr);
}

#   endif
#else

inline void prefetch (const void *)
{}

#endif

typedef u64 Key;
typedef u64 Bitboard;

const u16 MaxPlies = 128; // Maximum Plies

const i16 DepthZero         =  0;
const i16 DepthQSCheck      =  0;
const i16 DepthQSNoCheck    = -1;
const i16 DepthQSRecapture  = -5;
const i16 DepthNone         = -6;
const i16 DepthEmpty        = -7;

template <typename T>
constexpr auto operator+(T t) noexcept
    -> std::enable_if_t<std::is_enum<T>::value, std::underlying_type_t<T>>
{
    return static_cast<std::underlying_type_t<T>>(t);
}

enum class File : i08
{
    fA,
    fB,
    fC,
    fD,
    fE,
    fF,
    fG,
    fH,
    NO,
};

enum class Rank : i08
{
    r1,
    r2,
    r3,
    r4,
    r5,
    r6,
    r7,
    r8,
    NO,
};

enum class Color : i08
{
    WHITE,
    BLACK,
    NO,
};

/// Square needs 6-bits to be stored
/// bit 0-2: File
/// bit 3-5: Rank
enum class Square : i08
{
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
    NO,
};

enum Delta : i08
{
    DEL_O =  000,

    DEL_E =  001,
    DEL_N =  010,
    
    DEL_W = -i08(DEL_E),
    DEL_S = -i08(DEL_N),

    DEL_NN = i08(DEL_N) + i08(DEL_N),
    DEL_EE = i08(DEL_E) + i08(DEL_E),
    DEL_SS = i08(DEL_S) + i08(DEL_S),
    DEL_WW = i08(DEL_W) + i08(DEL_W),

    DEL_NE = i08(DEL_N) + i08(DEL_E),
    DEL_SE = i08(DEL_S) + i08(DEL_E),
    DEL_SW = i08(DEL_S) + i08(DEL_W),
    DEL_NW = i08(DEL_N) + i08(DEL_W),

    DEL_NNE = i08(DEL_NN) + i08(DEL_E),
    DEL_NNW = i08(DEL_NN) + i08(DEL_W),

    DEL_EEN = i08(DEL_EE) + i08(DEL_N),
    DEL_EES = i08(DEL_EE) + i08(DEL_S),

    DEL_SSE = i08(DEL_SS) + i08(DEL_E),
    DEL_SSW = i08(DEL_SS) + i08(DEL_W),

    DEL_WWN = i08(DEL_WW) + i08(DEL_N),
    DEL_WWS = i08(DEL_WW) + i08(DEL_S),
};

enum class CastleSide : i08
{
    KING, // King  Side (Short Castle)
    QUEN, // Queen Side (Long  Castle)
    NO,
};
/// Castle Right defined as in Polyglot book hash key
enum class CastleRight : u08
{
    NONE  = 0,              // 0000
    WKING = 1,              // 0001
    WQUEN = +WKING << 1,    // 0010
    BKING = +WKING << 2,    // 0100
    BQUEN = +WKING << 3,    // 1000

    WHITE = +WKING + +WQUEN,// 0011
    BLACK = +BKING + +BQUEN,// 1100
    KING  = +WKING + +BKING,// 0101
    QUEN  = +WQUEN + +BQUEN,// 1010
    ANY   = +WHITE + +BLACK,// 1111
    NO,
};

enum PieceType : i08
{
    PAWN     , // 000
    NIHT     , // 001
    BSHP     , // 010
    ROOK     , // 011
    QUEN     , // 100
    KING     , // 101
    NONE     , // 110
    MAX_PTYPE, // 111
};
/// Piece needs 4-bits to be stored
/// bit 0-2: Type of piece
/// bit   3: Color of piece { White = 0..., Black = 1... }
enum class Piece : u08
{
    W_PAWN   = 0, //  0000
    W_NIHT      , //  0001
    W_BSHP      , //  0010
    W_ROOK      , //  0011
    W_QUEN      , //  0100
    W_KING      , //  0101

    NONE = 6, //  0110

    B_PAWN   = 8, //  1000
    B_NIHT      , //  1001
    B_BSHP      , //  1010
    B_ROOK      , //  1011
    B_QUEN      , //  1100
    B_KING      , //  1101

    NO            //  1110
};

enum MoveType : u16
{
    NORMAL    = 0x0000, // 0000
    CASTLE    = 0x4000, // 0100
    ENPASSANT = 0x8000, // 1000
    PROMOTE   = 0xC000, // 11xx
};
/// Move needs 16-bits to be stored
///
/// bit 00-05: Destiny square: (0...63)
/// bit 06-11: Origin square: (0...63)
/// bit 12-13: Promotion piece: (Knight...Queen) - 1
/// bit 14-15: Move flag: (0) Normal (1) Castle (2) En-Passant (3) Promotion
///
/// Special cases are Move::NONE and Move::NULL_.
enum class Move : u16
{
    NONE  = 0x00,
    NULL_ = 0x41,
};

enum class Value : i32
{
    ZERO      = 0,
    DRAW      = 0,

    NONE      = SHRT_MAX,
    INFINITE_ = +NONE - 1,
    MATE      = +INFINITE_ - 1,
    
    MATE_MAX_PLY = +MATE - 2*MaxPlies,
    
    KNOWN_WIN = 10000,

    MG_PAWN =  171,  EG_PAWN =  240,
    MG_NIHT =  764,  EG_NIHT =  848,
    MG_BSHP =  826,  EG_BSHP =  891,
    MG_ROOK = 1282,  EG_ROOK = 1373,
    MG_QUEN = 2526,  EG_QUEN = 2646,

    MIDGAME = 15258, ENDGAME = 3915,

    //MG_FULL = MG_NIHT * 4 + MG_BSHP * 4 + MG_ROOK * 4 + MG_QUEN * 2,
    //EG_FULL = EG_NIHT * 4 + EG_BSHP * 4 + EG_ROOK * 4 + EG_QUEN * 2,
};
/// Score needs 32-bits to be stored
/// the lower 16-bits are used to store the midgame value
/// the upper 16-bits are used to store the endgame value
/// Take some care to avoid left-shifting a signed int to avoid undefined behavior.
enum class Score : u32
{
    ZERO = 0,
};

enum class Bound : u08
{
    NONE  = 0,
    UPPER = 1,
    LOWER = 2,
    EXACT = 3,
};

enum Phase : u08
{
    MG    = 0,
    EG    = 1,
};

enum Scale : u08
{
    SCALE_DRAW    =   0,
    SCALE_ONEPAWN =  48,
    SCALE_NORMAL  =  64,
    SCALE_MAX     = 128,
    SCALE_NONE    = 255,
};

#define BASIC_OPERATORS(T)                                          \
    constexpr T operator- (T t) { return T(- +t); }                 \
    constexpr T operator+ (T t1, T t2) { return T(+t1 + +t2); }     \
    constexpr T operator- (T t1, T t2) { return T(+t1 - +t2); }     \
    inline T& operator+= (T &t1, T t2) { t1 = t1 + t2; return t1; } \
    inline T& operator-= (T &t1, T t2) { t1 = t1 - t2; return t1; }

#define ARTHMAT_OPERATORS(T)                                    \
    constexpr T operator+ (T t, i32 i) { return T(+t + i); }    \
    constexpr T operator- (T t, i32 i) { return T(+t - i); }    \
    constexpr T operator* (T t, i32 i) { return T(+t * i); }    \
    constexpr T operator/ (T t, i32 i) { return T(+t / i); }    \
    inline T& operator+= (T &t, i32 i) { t = t + i; return t; } \
    inline T& operator-= (T &t, i32 i) { t = t - i; return t; } \
    inline T& operator*= (T &t, i32 i) { t = t * i; return t; } \
    inline T& operator/= (T &t, i32 i) { t = t / i; return t; }

#define INC_DEC_OPERATORS(T)                                 \
    inline T& operator++ (T &t) { t = T(+t + 1); return t; } \
    inline T& operator-- (T &t) { t = T(+t - 1); return t; }

#define BINARY_OPERATORS(T)                                       \
    constexpr T operator~ (T t) { return T(~+t); }                \
    constexpr T operator| (T t1, T t2) { return T(+t1 | +t2); }   \
    constexpr T operator& (T t1, T t2) { return T(+t1 & +t2); }   \
    constexpr T operator^ (T t1, T t2) { return T(+t1 ^ +t2); }   \
    inline T operator|= (T t1, T t2) { t1 = t1 | t2; return t1; } \
    inline T operator&= (T t1, T t2) { t1 = t1 & t2; return t1; } \
    inline T operator^= (T t1, T t2) { t1 = t1 ^ t2; return t1; } \

BASIC_OPERATORS(File)
//ARTHMAT_OPERATORS(File)
INC_DEC_OPERATORS(File)

BASIC_OPERATORS(Rank)
//ARTHMAT_OPERATORS(Rank)
INC_DEC_OPERATORS(Rank)

INC_DEC_OPERATORS(Color)

BASIC_OPERATORS(Delta)
ARTHMAT_OPERATORS(Delta)

inline Square operator+ (Square s, Delta d) { return Square(+s + +d); }
inline Square operator- (Square s, Delta d) { return Square(+s - +d); }

inline Square& operator+= (Square &s, Delta d) { s = s + d; return s; }
inline Square& operator-= (Square &s, Delta d) { s = s - d; return s; }

inline Delta operator- (Square s1, Square s2) { return Delta(+s1 - +s2); }
INC_DEC_OPERATORS(Square)

INC_DEC_OPERATORS(CastleSide)

BINARY_OPERATORS(CastleRight)

INC_DEC_OPERATORS(PieceType)

BASIC_OPERATORS(Value)
ARTHMAT_OPERATORS(Value)
INC_DEC_OPERATORS(Value)

constexpr Score mk_score (i32 mg, i32 eg)
{
    return Score(i32((u32(eg) << 0x10)) + mg);
}

/// Extracting the signed lower and upper 16 bits is not so trivial because
/// according to the standard a simple cast to short is implementation defined
/// and so is a right shift of a signed integer.

inline Value mg_value (u32 s)
{
    union { u16 u; i16 s; } mg = { u16(u32(s + 0x0000) >> 0x00) };
    return Value(mg.s);
}
inline Value eg_value (u32 s)
{
    union { u16 u; i16 s; } eg = { u16(u32(s + 0x8000) >> 0x10) };
    return Value(eg.s);
}

BASIC_OPERATORS(Score)
/// Multiplication & Division of a Score must be handled separately for each term
inline Score operator* (Score s, i32 i) { return mk_score (+mg_value (+s) * i, +eg_value (+s) * i); }
inline Score operator/ (Score s, i32 i) { return mk_score (+mg_value (+s) / i, +eg_value (+s) / i); }

inline Score& operator*= (Score &s, i32 i) { s = mk_score (+mg_value (+s) * i, +eg_value (+s) * i); return s; }
inline Score& operator/= (Score &s, i32 i) { s = mk_score (+mg_value (+s) / i, +eg_value (+s) / i); return s; }

/// Don't want to multiply two scores due to a very high risk of overflow.
/// So user should explicitly convert to integer.
Score operator* (Score, Score) = delete;
Score operator/ (Score, Score) = delete;

BINARY_OPERATORS(Bound)

#undef BINARY_OPERATORS
#undef INC_DEC_OPERATORS
#undef ARTHMAT_OPERATORS
#undef BASIC_OPERATORS

constexpr Color operator~ (Color c) { return Color(+c ^ +Color::BLACK); }

constexpr File operator~ (File f) { return File(+f ^ +File::fH); }
constexpr File to_file   (char f) { return File(f - 'a'); }

constexpr Rank operator~ (Rank r) { return Rank(+r ^ +Rank::r8); }
constexpr Rank to_rank   (char r) { return Rank(r - '1'); }

constexpr Square operator| (File f, Rank r) { return Square((+ r << 3) + +f); }
constexpr Square operator| (Rank r, File f) { return Square((+~r << 3) + +f); }
constexpr Square to_square (char f, char r) { return to_file (f) | to_rank (r); }

constexpr bool _ok    (Square s) { return (+s & ~+Square::H8) == 0; }
constexpr File _file  (Square s) { return File(+s & +File::fH); }
constexpr Rank _rank  (Square s) { return Rank(+s >> 3); }
constexpr Color color (Square s) { return Color(((+s ^ (+s >> 3)) & 1) != 1); }

// Flip   -> Square::A1 -> Square::A8
constexpr Square operator~ (Square s) { return Square(+s ^ +Square::A8); }
// Mirror -> Square::A1 -> Square::H1
constexpr Square operator! (Square s) { return Square(+s ^ +Square::H1); }

constexpr Rank rel_rank (Color c, Square s) { return Rank(+_rank (s) ^ (+c*+Rank::r8)); }
constexpr Square rel_sq (Color c, Square s) { return Square(+s ^ (+c*+Square::A8)); }

inline bool opposite_colors (Square s1, Square s2)
{
    i08 s = +s1 ^ +s2;
    return 0 != (((s >> 3) ^ s) & +Color::BLACK);
}

constexpr Delta pawn_push (Color c)
{
    return Color::WHITE == c ? DEL_N : DEL_S;
}

constexpr CastleRight castle_right (Color c)
{
    //return CastleRight(CastleRight::WHITE << ((+c << +Color::BLACK)));
    return Color::WHITE == c ? CastleRight::WHITE : CastleRight::BLACK;
}
constexpr CastleRight castle_right (Color c, CastleSide cs)
{
    //return CastleRight(CastleRight::WKING << ((+c << +Color::BLACK) + cs));
    return Color::WHITE == c ? 
               CastleSide::KING == cs ? CastleRight::WKING : CastleRight::WQUEN :
               CastleSide::KING == cs ? CastleRight::BKING : CastleRight::BQUEN;
}

constexpr Piece operator| (Color c, PieceType pt) { return Piece((+c << 3) + pt); }

constexpr bool _ok (Piece p)
{
    return (Piece::W_PAWN <= p && p <= Piece::W_KING)
        || (Piece::B_PAWN <= p && p <= Piece::B_KING);
}
constexpr PieceType ptype (Piece p) { return PieceType(+p & MAX_PTYPE); }
constexpr Color     color (Piece p) { return Color(+p >> 3); }
constexpr Piece operator~ (Piece p) { return Piece(+p ^ 8); }

constexpr Square    org_sq  (Move m) { return Square((+m >> 6) & +Square::H8); }
constexpr Square    dst_sq  (Move m) { return Square((+m >> 0) & +Square::H8); }
constexpr bool      _ok     (Move m) { return org_sq (m) != dst_sq (m); }
constexpr PieceType promote (Move m) { return PieceType(((+m >> 12) & 3) + NIHT); }
constexpr MoveType  mtype   (Move m) { return MoveType(+m & PROMOTE); }
constexpr i16       move_pp (Move m) { return +m & 0x0FFF; }
inline    void      promote (Move &m, PieceType pt) { m = Move(/*PROMOTE +*/ ((pt - 1) << 12) + (+m & 0x0FFF)); }
constexpr Square fix_dst_sq (Move m, bool chess960 = false)
{
    return mtype (m) != CASTLE
        || chess960 ?
        dst_sq (m) :
        (dst_sq (m) > org_sq (m) ? File::fG : File::fC) | _rank (dst_sq (m));
}

template<MoveType MT>
constexpr Move mk_move (Square org, Square dst)               { return Move(MT + (+org << 6) + +dst); }
constexpr Move mk_move (Square org, Square dst, PieceType pt) { return Move(PROMOTE + ((pt - NIHT) << 12) + (+org << 6) + +dst); }

constexpr i16   value_to_cp (Value v) { return i16(v*100/i32(Value::EG_PAWN)); }
constexpr Value cp_to_value (i16  cp) { return Value(cp*i32(Value::EG_PAWN)/100); }

constexpr Value mates_in (i32 ply) { return  Value::MATE - ply; }
constexpr Value mated_in (i32 ply) { return -Value::MATE + ply; }

typedef std::chrono::milliseconds::rep TimePoint; // Time in milli-seconds

inline TimePoint now ()
{
    return std::chrono::duration_cast<std::chrono::milliseconds>
               (std::chrono::steady_clock::now ().time_since_epoch ()).count ();
}

struct ValMove
{
public:
    Move move;
    i32  value;
    
    explicit ValMove (Move m = Move::NONE)
        : move (m)
        , value (0)
    {}
    ValMove (Move m, i32 v)
        : move (m)
        , value (v)
    {}
    
    operator Move () const { return move; }
    void operator= (Move m) { move = m; }

    // Inhibit unwanted implicit conversions to Move
    // with an ambiguity that yields to a compile error.
    operator float () const = delete;
    operator double () const = delete;

    bool operator<  (const ValMove &vm) const { return value <  vm.value; }
    bool operator>  (const ValMove &vm) const { return value >  vm.value; }
    //bool operator<= (const ValMove &vm) const { return value <= vm.value; }
    //bool operator>= (const ValMove &vm) const { return value >= vm.value; }
    bool operator== (const ValMove &vm) const { return move == vm.move; }
    bool operator!= (const ValMove &vm) const { return move != vm.move; }
};

class ValMoves
    : public std::vector<ValMove>
{
public:
    void operator+= (Move move) { emplace_back (move); }
    //void operator-= (Move move) { erase (std::remove (begin (), end (), move), end ()); }
};

template<typename T, u32 Size>
struct HashTable
    : std::array<T, Size>
{
public:
    T* get (Key key)
    {
        return &(*this)[u32(key) & (Size - 1)];
    }
};

// Return the sign of a number (-1, 0, 1)
template<typename T> i32 sign (T val)
{
    return (T(0) < val) - (val < T(0));
}

const std::string Empty = "<empty>";

inline bool white_spaces (const std::string &str)
{
    return str.empty ()
        || str.find_first_not_of (" \t\n") == std::string::npos
        || str == Empty;
}

inline void to_lower (std::string &str)
{
    std::transform (str.begin (), str.end (), str.begin (), ::tolower);
}
inline void to_upper (std::string &str)
{
    std::transform (str.begin (), str.end (), str.begin (), ::toupper);
}
inline void toggle (std::string &str)
{
    std::transform (str.begin (), str.end (), str.begin (),
        [](i32 c) { return char (islower (c) ? ::toupper (c) : ::tolower (c)); });
}

inline std::string& trim_beg (std::string &str)
{
    str.erase (str.begin (), 
                std::find_if (str.begin (), str.end (), 
                    std::not1 (std::ptr_fun<i32, i32> (std::isspace))));
    return str;
}
inline std::string& trim_end (std::string &str)
{
    str.erase (std::find_if (str.rbegin (), str.rend (), 
                std::not1 (std::ptr_fun<i32, i32> (std::isspace))).base (),
                    str.end ());
    return str;
}
inline std::string& trim (std::string &str)
{
    return trim_beg (trim_end (str));
}

inline std::vector<std::string> split (const std::string str, char delimiter = ' ', bool keep_empty = true, bool do_trim = false)
{
    std::vector<std::string> tokens;
    std::istringstream iss (str);
    do
    {
        std::string token;
        bool fail = !std::getline (iss, token, delimiter);
        if (do_trim)
        {
            token = trim (token);
        }
        if (   keep_empty
            || !token.empty ())
        {
            tokens.push_back (token);
        }
        if (fail)
        {
            break;
        }
    }
    while (iss.good ());

    return tokens;
}

inline void remove_substring (std::string &str, const std::string &sub)
{
    size_t pos = str.find (sub);
    while (pos != std::string::npos)
    {
        str.erase (pos, sub.length ());
        pos = str.find (sub, pos);
    }
}

inline void remove_extension (std::string &filename)
{
    size_t pos = filename.find_last_of ('.');
    if (pos != std::string::npos)
    {
        filename = filename.substr (0, pos);
    }
}

inline std::string append_path (const std::string &base_path, const std::string &file_path)
{
    return base_path[base_path.length () - 1] != '/' ?
            base_path + '/' + file_path :
            base_path + file_path;
}

inline void convert_path (std::string &path)
{
    std::replace (path.begin (), path.end (), '\\', '/'); // Replace all '\\' to '/'
}

constexpr Square SQ[+Square::NO] =
{
    Square::A1, Square::B1, Square::C1, Square::D1, Square::E1, Square::F1, Square::G1, Square::H1,
    Square::A2, Square::B2, Square::C2, Square::D2, Square::E2, Square::F2, Square::G2, Square::H2,
    Square::A3, Square::B3, Square::C3, Square::D3, Square::E3, Square::F3, Square::G3, Square::H3,
    Square::A4, Square::B4, Square::C4, Square::D4, Square::E4, Square::F4, Square::G4, Square::H4,
    Square::A5, Square::B5, Square::C5, Square::D5, Square::E5, Square::F5, Square::G5, Square::H5,
    Square::A6, Square::B6, Square::C6, Square::D6, Square::E6, Square::F6, Square::G6, Square::H6,
    Square::A7, Square::B7, Square::C7, Square::D7, Square::E7, Square::F7, Square::G7, Square::H7,
    Square::A8, Square::B8, Square::C8, Square::D8, Square::E8, Square::F8, Square::G8, Square::H8,
};

constexpr Value PieceValues[][MAX_PTYPE] =
{
    { Value::MG_PAWN, Value::MG_NIHT, Value::MG_BSHP, Value::MG_ROOK, Value::MG_QUEN, Value::ZERO, Value::ZERO },
    { Value::EG_PAWN, Value::EG_NIHT, Value::EG_BSHP, Value::EG_ROOK, Value::EG_QUEN, Value::ZERO, Value::ZERO }
};

#endif // _TYPE_H_INC_
