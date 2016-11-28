#ifndef _THREAD_WIN32_H_INC_
#define _THREAD_WIN32_H_INC_

// STL thread library used by mingw and gcc when cross compiling for Windows
// relies on libwinpthread. Currently libwinpthread implements mutexes directly
// on top of Windows semaphores. Semaphores, being kernel objects, require kernel
// mode transition in order to lock or unlock, which is very slow compared to
// interlocked operations (about 30% slower on bench test). To workaround this
// issue, we define our wrappers to the low level Win32 calls. We use critical
// sections to support Windows XP and older versions. Unfortunately, cond_wait()
// is racy between unlock() and WaitForSingleObject() but they have the same
// speed performance of SRW locks.

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
#   undef  _WIN32_WINNT
#   define _WIN32_WINNT 0x0601 // Force to include needed API prototypes
#   endif
#   include <windows.h>

// The needed Windows API for processor groups could be missed from old Windows
// versions, so instead of calling them directly (forcing the linker to resolve
// the calls at compile time), try to load them at runtime. To do this we need
// first to define the corresponding function pointers.
extern "C" {
    typedef bool (*fun1_t)(LOGICAL_PROCESSOR_RELATIONSHIP,
                           PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX, PDWORD);
    typedef bool (*fun2_t)(USHORT, PGROUP_AFFINITY);
    typedef bool (*fun3_t)(HANDLE, CONST GROUP_AFFINITY*, PGROUP_AFFINITY);
}

#endif

#if defined(_WIN32) && !defined(_MSC_VER)
// Mutex and ConditionVariable struct are wrappers of the low level locking
// machinery and are modeled after the corresponding C++11 classes.
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