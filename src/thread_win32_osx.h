#ifndef _THREAD_WIN32_OSX_H_INC_
#define _THREAD_WIN32_OSX_H_INC_

/// STL thread library used by mingw and gcc when cross compiling for Windows
/// relies on libwinpthread. Currently libwinpthread implements mutexes directly
/// on top of Windows semaphores. Semaphores, being kernel objects, require kernel
/// mode transition in order to lock or unlock, which is very slow compared to
/// interlocked operations (about 30% slower on bench test). To workaround this
/// issue, we define our wrappers to the low level Win32 calls. We use critical
/// sections to support Windows XP and older versions. Unfortunately, cond_wait()
/// is racy between unlock() and WaitForSingleObject() but they have the same
/// speed performance of SRW locks.

#if defined(_WIN32) && !defined(_MSC_VER)

#   if !defined(NOMINMAX)
#       define NOMINMAX // Disable macros min() and max()
#   endif
#   if !defined(WIN32_LEAN_AND_MEAN)
#       define WIN32_LEAN_AND_MEAN
#   endif
#   include <windows.h>
#   undef WIN32_LEAN_AND_MEAN
#   undef NOMINMAX

#include <condition_variable>

/// Mutex and ConditionVariable struct are wrappers of the low level locking
/// machinery and are modeled after the corresponding C++11 classes.
class Mutex
{
private:
    CRITICAL_SECTION cs;

public:
    Mutex() { InitializeCriticalSection(&cs); }
    Mutex(Mutex const&) = delete;
    Mutex& operator=(Mutex const&) = delete;
   ~Mutex() { DeleteCriticalSection(&cs); }

    void lock()   { EnterCriticalSection(&cs); }
    void unlock() { LeaveCriticalSection(&cs); }
};

typedef std::condition_variable_any ConditionVariable;

#else // Default case: use STL classes

#include <condition_variable>
#include <mutex>

typedef std::condition_variable ConditionVariable;
typedef std::mutex              Mutex;

#endif

/// On OSX threads other than the main thread are created with a reduced stack
/// size of 512KB by default, this is dangerously low for deep searches, so
/// adjust it to TH_STACK_SIZE. The implementation calls pthread_create() with
/// proper stack size parameter.
#if defined(__APPLE__)

#include <pthread.h>

class NativeThread
{
private:
    static constexpr size_t TH_STACK_SIZE = 2 * 1024 * 1024;

    template <class T, class P = std::pair<T*, void(T::*)()>>
    static void* start_routine(void *arg)
    {
        P *p = reinterpret_cast<P*>(arg);
        (p->first->*(p->second))(); // Call member function pointer
        delete p;
        return NULL;
    }

    pthread_t thread;

public:
    template<class T, class P = std::pair<T*, void (T::*)()>>
    explicit NativeThread(void (T::*fun)(), T *obj)
    {
        pthread_attr_t attribute;
        pthread_attr_init(&attribute);
        pthread_attr_setstacksize(&attribute, TH_STACK_SIZE);
        pthread_create(&thread, &attribute, start_routine<T>, new P(obj, fun));
    }

    void join() { pthread_join(thread, NULL); }
};

#else // Default case: use STL classes

#include <thread>

typedef std::thread NativeThread;

#endif

#endif // _THREAD_WIN32_OSX_H_INC_
