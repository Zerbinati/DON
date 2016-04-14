#ifndef _BITSCAN_H_INC_
#define _BITSCAN_H_INC_

#include "Type.h"

namespace BitBoard {

#if defined(_MSC_VER)

#   include <intrin.h> // MSVC popcnt and bsfq instrinsics _BitScanForward64() & _BitScanReverse64()

inline Square scan_lsq (Bitboard bb)
{
    assert(bb != 0);

    unsigned long index;
#if defined(BIT64)
    _BitScanForward64 (&index, bb);
#else
    if (u32(bb >> 0) != 0)
    {
        _BitScanForward (&index, u32(bb >> 0));
    }
    else
    {
        _BitScanForward (&index, u32(bb >> 32));
        index += 32;
    }
#endif

    return Square(index);
}

inline Square scan_msq (Bitboard bb)
{
    assert(bb != 0);

    unsigned long index;
#if defined(BIT64)
    _BitScanReverse64 (&index, bb);
#else
    if (u32(bb >> 32) != 0)
    {
        _BitScanReverse (&index, u32(bb >> 32));
        index += 32;
    }
    else
    {
        _BitScanReverse (&index, u32(bb >> 0));
    }
#endif

    return Square(index);
}

#elif defined(__GNUC__) //|| defined(__arm__)

inline Square scan_lsq (Bitboard bb)
{
    assert(bb != 0);

#if defined(BIT64)
    return Square(__builtin_ctzll (bb));
#else
    return Square(u32(bb >> 0) != 0 ? __builtin_ctz (bb >> 0) : __builtin_ctz (bb >> 32) + 32);
#endif
}
inline Square scan_msq (Bitboard bb)
{
    assert(bb != 0);

#if defined(BIT64)
    return Square(i08(SQ_H8) - __builtin_clzll (bb));
#else
    return Square(i08(SQ_H8) - (u32(bb >> 32) != 0 ? __builtin_clz (bb >> 32) : __builtin_clz (bb >> 0) + 32));
#endif
}

//#else
//
//// Assembly code by Heinz van Saanen
//inline Square scan_lsq (Bitboard bb)
//{
//    assert(bb != 0);
//    Bitboard index;
//    __asm__ ("bsfq %1, %0": "=r" (index) : "rm" (bb));
//    return Square(index);
//}
//
//inline Square scan_msq (Bitboard bb)
//{
//    assert(bb != 0);
//    Bitboard index;
//    __asm__ ("bsrq %1, %0": "=r" (index) : "rm" (bb));
//    return Square(index);
//}

#else

#   define NO_BSFQ

#endif

#if defined(NO_BSFQ)

#   if defined(BIT64)

    const u64 DeBruijn_64 = U64(0x03F79D71B4CB0A89);
    // * @author Kim Walisch (2012)
    // * DeBruijn(U32(0x4000000)) = U64(0x03F79D71B4CB0A89)
    const u08 BSF_Table[SQ_NO] =
    {
        00, 47, 01, 56, 48, 27, 02, 60,
        57, 49, 41, 37, 28, 16, 03, 61,
        54, 58, 35, 52, 50, 42, 21, 44,
        38, 32, 29, 23, 17, 11, 04, 62,
        46, 55, 26, 59, 40, 36, 15, 53,
        34, 51, 20, 43, 31, 22, 10, 45,
        25, 39, 14, 33, 19, 30,  9, 24,
        13, 18,  8, 12, 07, 06, 05, 63
    };

#   else

    const u32 DeBruijn_32 = U32(0x783A9B23);

    const u08 BSF_Table[SQ_NO] =
    {
        63, 30, 03, 32, 25, 41, 22, 33,
        15, 50, 42, 13, 11, 53, 19, 34,
        61, 29, 02, 51, 21, 43, 45, 10,
        18, 47, 01, 54,  9, 57, 00, 35,
        62, 31, 40, 04, 49, 05, 52, 26,
        60, 06, 23, 44, 46, 27, 56, 16,
        07, 39, 48, 24, 59, 14, 12, 55,
        38, 28, 58, 20, 37, 17, 36,  8
    };

    const u08 MSB_Table[256] =
    {
        0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
        5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    };

#   endif

inline Square scan_lsq (Bitboard bb)
{
    assert(bb != 0);
    bb ^= (bb - 1); // Set all bits including the LS1B and below
    u08 index =
#   if defined(BIT64)
    // Use Kim Walisch extending trick for 64-bit
        (bb * DeBruijn_64) >> 58;
#   else
    // Use Matt Taylor's folding trick for 32-bit
        (u32((bb >> 0) ^ (bb >> 32)) * DeBruijn_32) >> 26;
#   endif
    return Square(BSF_Table[index]);
}

inline Square scan_msq (Bitboard bb)
{
    assert(bb != 0);

#   if defined(BIT64)
    // Set all bits including the MS1B and below
    bb |= bb >> 0x01;
    bb |= bb >> 0x02;
    bb |= bb >> 0x04;
    bb |= bb >> 0x08;
    bb |= bb >> 0x10;
    bb |= bb >> 0x20;
    u08 index = (bb * DeBruijn_64) >> 58;
    return Square(BSF_Table[index]);
#   else
    u08 msb = 0;
    if (bb > 0xFFFFFFFF)
    {
        bb >>= 32;
        msb = 32;
    }
    if (bb > 0xFFFF)
    {
        bb >>= 16;
        msb += 16;
    }
    if (bb > 0xFF)
    {
        bb >>= 8;
        msb += 8;
    }
    return Square(msb + MSB_Table[bb]);
#   endif

}

#endif

// scan_frntmost_sq() and scan_backmost_sq() find the square
// corresponding to the most/least advanced bit relative to the given color.
inline Square scan_frntmost_sq (Color c, Bitboard bb) { return c == WHITE ? scan_msq (bb) : scan_lsq (bb); }
inline Square scan_backmost_sq (Color c, Bitboard bb) { return c == WHITE ? scan_lsq (bb) : scan_msq (bb); }

inline Square pop_lsq (Bitboard &bb)
{
    Square sq = scan_lsq (bb);
#if defined(BM2)
    bb = BLSR(bb);
#else
    bb &= (bb - 1);
#endif
    return sq;
}

}

#endif // _BITSCAN_H_INC_
