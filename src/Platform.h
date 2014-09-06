#ifndef _PLATFORM_H_INC_
#define _PLATFORM_H_INC_

/// To get started type 'make help'.
///
/// For Linux and OSX configuration is done automatically using Makefile.
///
/// For Windows, part of the configuration is detected automatically, but some
/// switches need to be set manually:
///
/// -DNDEBUG    | Disable debugging mode. Always use this.
/// -DPREFETCH  | Enable use of prefetch asm-instruction.
///             | Don't enable it if want the executable to run on some very old machines.
/// -DBSFQ      | Add runtime support for use of Bitscans asm-instruction.
/// -DABM       | Add runtime support for use of ABM asm-instruction. Works only in 64-bit mode.
///             | For compiling requires hardware with ABM support.
/// -DBM2       | Add runtime support for use of BM2 asm-instruction. Works only in 64-bit mode.
///             | For compiling requires hardware with BM2 support.
/// -DLPAGES    | Add runtime support for large pages.

#include <cstdint>

typedef         int8_t     i08;
typedef        uint8_t     u08;
typedef         int16_t    i16;
typedef        uint16_t    u16;
typedef         int32_t    i32;
typedef        uint32_t    u32;
typedef         int64_t    i64;
typedef        uint64_t    u64;


#ifdef _MSC_VER
// Disable some silly and noisy warning from MSVC compiler
#   pragma warning (disable: 4127) // Conditional expression is constant
#   pragma warning (disable: 4146) // Unary minus operator applied to unsigned type
#   pragma warning (disable: 4267) // 'argument' : conversion from '-' to '-', possible loss of data
#   pragma warning (disable: 4800) // Forcing value to bool 'true' or 'false'
#   pragma warning (disable: 6326) // Constant comparison

#   define  S32(X) (X##i32)
#   define  U32(X) (X##ui32)
#   define  S64(X) (X##i64)
#   define  U64(X) (X##ui64)

#else

#   define S32(X) (X##L)
#   define U32(X) (X##UL)
#   define S64(X) (X##LL)
#   define U64(X) (X##ULL)

#endif

// Windows or MinGW
#if defined(_WIN32) || defined(_MSC_VER) || defined(__CYGWIN__) || defined(__MINGW32__) || defined(__MINGW64__) || defined(__BORLANDC__)

#   ifdef _WIN64
#       ifndef _64BIT
#           define _64BIT
#       endif
#       ifndef BSFQ
#           define BSFQ
#       endif
#   endif

#endif

#undef INLINE

#ifdef _MSC_VER

#   define INLINE     __forceinline

#elif __GNUC__

#   define INLINE     inline __attribute__((always_inline))

#else

#   define INLINE     inline

#endif

#if defined(_MSC_VER) || defined(__INTEL_COMPILER)

#   define CACHE_ALIGN(x)     __declspec(align(x))
//#   define CACHE_ALIGN(x)     alignas(x)

#else

#   define CACHE_ALIGN(x)     __attribute__((aligned(x)))

#endif

// ---

#undef ASSERT
#undef ASSERT_MSG

#ifdef NDEBUG

#   define ASSERT(condition)          ((void) 0)
#   define ASSERT_MSG(condition, msg) ((void) 0)

#else

#   include <cassert>

#   define ASSERT(condition)          (void)( (!!(condition)) || (_wassert(_CRT_WIDE(#condition), _CRT_WIDE(__FILE__), __LINE__), 0) )
#   define ASSERT_MSG(condition, msg) (void)( (!!(condition)) || (_wassert(_CRT_WIDE(msg),        _CRT_WIDE(__FILE__), __LINE__), 0) )

#endif

#endif // _PLATFORM_H_INC_
