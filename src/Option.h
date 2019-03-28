#ifndef _UCI_H_INC_
#define _UCI_H_INC_

#include <map>
#include "functor.h"
#include "Type.h"

namespace UCI {

    /// Option class implements an option as defined by UCI protocol
    class Option
    {
    private:
        typedef void (*OnChange) (const Option&);

        std::string type
            ,       default_value
            ,       current_value;
        i32         minimum
            ,       maximum;

        OnChange on_change = nullptr;

    public:
        size_t index;

        explicit Option (OnChange = nullptr);
        Option (const char*, OnChange = nullptr);
        Option (const bool, OnChange = nullptr);
        Option (const i32, i32, i32, OnChange = nullptr);
        Option (const char*, const char*, OnChange = nullptr);
        Option (const Option&) = delete;

        explicit operator std::string () const;
        explicit operator bool () const;
        explicit operator i32  () const;
        bool operator==(const char*) const;

        Option& operator=  (const char*);
        Option& operator=  (const std::string&);

        void    operator<< (const Option&);

        std::string operator() ()  const;

    };

    /// Options container is actually a std::map of Option
    typedef std::map<std::string, Option, std::no_case_less_comparer> NoCaseMap;

    extern void initialize ();
    extern void deinitialize ();

    template<typename CharT, typename Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, const Option &opt)
    {
        os << opt.operator() ();
        return os;
    }

    /// operator<<() is used to print all the options default values in chronological
    /// insertion order and in the format defined by the UCI protocol.
    template<typename CharT, typename Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, const NoCaseMap &optmap)
    {
        for (size_t idx = 0; idx < optmap.size (); ++idx)
        {
            for (const auto &pair : optmap)
            {
                const auto &option = pair.second;
                if (idx != option.index)
                {
                    continue;
                }
                os  << "option name "
                    << pair.first
                    << option
                    << std::endl;
            }
        }
        return os;
    }
}

// Global nocase mapping of Options
extern UCI::NoCaseMap Options;

extern u32 threads_option ();

#endif // _UCI_H_INC_
