#ifndef _LOGGER_H_INC_
#define _LOGGER_H_INC_

#include <fstream>
#include <iostream>
#include "tiebuffer.h"
#include "Type.h"

#if defined(_WIN32)
#   include <ctime>
#endif

inline std::string time_to_string (const std::chrono::system_clock::time_point &tp)
{

#   if defined(_WIN32)

    auto time = std::chrono::system_clock::to_time_t (tp);
    auto tp_sec = std::chrono::system_clock::from_time_t (time);
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds> (tp - tp_sec).count ();
    auto *ttm = localtime (&time);
    const char date_time_format[] = "%Y.%m.%d-%H.%M.%S";
    char time_str[] = "yyyy.mm.dd.HH-MM.SS.fff";
    strftime (time_str, strlen (time_str), date_time_format, ttm);
    std::string stime (time_str);
    stime.append (".");
    stime.append (std::to_string (ms));
    return stime;

#   else

    return Empty;

#   endif

}

template<typename CharT, typename Traits>
inline std::basic_ostream<CharT, Traits>&
    operator<< (std::basic_ostream<CharT, Traits> &os, const std::chrono::system_clock::time_point &tp)
{
    os << time_to_string (tp);
    return os;
}

// Singleton I/O Logger class
class Logger
{
private:
    std::ofstream _ofs;
    std::tie_buf  _inb; // Input
    std::tie_buf  _otb; // Output

public:

    std::string filename;

    Logger ()
        : _inb (std::cin.rdbuf (), _ofs.rdbuf ())
        , _otb (std::cout.rdbuf (), _ofs.rdbuf ())
        , filename (Empty)
    {}
    Logger (const Logger&) = delete;
    Logger& operator= (const Logger&) = delete;
        
   ~Logger ()
    {
        set (Empty);
    }

    void set (const std::string &fn)
    {
        if (_ofs.is_open ())
        {
            std::cout.rdbuf (_otb.streambuf ());
            std::cin.rdbuf (_inb.streambuf ());

            _ofs << "[" << std::chrono::system_clock::now () << "] <-" << std::endl;
            _ofs.close ();
        }
        filename = fn;
        if (!white_spaces (filename))
        {
            _ofs.open (filename, std::ios_base::out|std::ios_base::app);
            _ofs << "[" << std::chrono::system_clock::now () << "] ->" << std::endl;

            std::cin.rdbuf (&_inb);
            std::cout.rdbuf (&_otb);
        }
    }

};

// Global Logger
extern Logger Log;
extern TimePoint DebugTime;

// Debug functions used mainly to collect run-time statistics
extern void debug_init ();
extern void debug_hit_on (bool);
extern void debug_hit_on (bool, bool);
extern void debug_mean_of (i64);
extern void debug_print ();

#endif // _LOGGER_H_INC_
