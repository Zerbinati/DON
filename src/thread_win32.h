#ifndef _THREAD_WIN32_H_INC_
#define _THREAD_WIN32_H_INC_

/// STL thread library used by mingw and gcc when cross compiling for Windows
/// relies on libwinpthread. Currently libwinpthread implements mutexes directly
/// on top of Windows semaphores. Semaphores, being kernel objects, require kernel
/// mode transition in order to lock or unlock, which is very slow compared to
/// interlocked operations (about 30% slower on bench test). To workaround this
/// issue, we define our wrappers to the low level Win32 calls. We use critical
/// sections to support Windows XP and older versions. Unfortunately, cond_wait()
/// is racy between unlock() and WaitForSingleObject() but they have the same
/// speed performance of SRW locks.

#include <condition_variable>
#include <mutex>
#include <vector>

#if defined(_WIN32)

#   if !defined(NOMINMAX)
#       define NOMINMAX // Disable macros min() and max()
#   endif
#   if !defined(WIN32_LEAN_AND_MEAN)
#       define WIN32_LEAN_AND_MEAN
#   endif
#   if _WIN32_WINNT < 0x0601
#       undef  _WIN32_WINNT
#       define _WIN32_WINNT 0x0601 // Force to include needed API prototypes
#   endif
#   include <windows.h>

#endif

#if defined(_WIN32) && !defined(_MSC_VER)
/// Mutex and ConditionVariable struct are wrappers of the low level locking
/// machinery and are modeled after the corresponding C++11 classes.
class Mutex
{
private:
    CRITICAL_SECTION cs;

public:
    Mutex () { InitializeCriticalSection (&cs); }
    Mutex (const Mutex&) = delete;
    Mutex& operator= (const Mutex&) = delete;
    ~Mutex () { DeleteCriticalSection (&cs); }

    void lock ()   { EnterCriticalSection (&cs); }
    void unlock () { LeaveCriticalSection (&cs); }
};

typedef std::condition_variable_any ConditionVariable;

#else // Default case

// STL classes are used
typedef std::mutex              Mutex;
typedef std::condition_variable ConditionVariable;

#endif

#endif // _THREAD_WIN32_H_INC_