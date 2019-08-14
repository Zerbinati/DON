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
        typedef void (*OnChange)(Option const&);

        std::string type
            ,       default_value
            ,       current_value;
        i32         minimum
            ,       maximum;

        OnChange on_change = nullptr;

    public:
        size_t index;

        explicit Option(OnChange = nullptr);
        Option(char const*, OnChange = nullptr);
        Option(bool const, OnChange = nullptr);
        Option(i32 const, i32, i32, OnChange = nullptr);
        Option(char const*, char const*, OnChange = nullptr);
        Option(Option const&) = delete;

        explicit operator std::string() const;
        explicit operator bool () const;
        explicit operator i32  () const;
        bool operator==(char const*) const;

        Option& operator=(char const*);
        Option& operator=(std::string const&);

        void    operator<<(Option const&);

        std::string operator()()  const;

    };

    /// Options container is actually a std::map of Option
    typedef std::map<std::string, Option, no_case_less_comparer> NoCaseLessMap;

    extern void initialize();
    extern void deinitialize();

    template<typename CharT, typename Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<<(std::basic_ostream<CharT, Traits> &os, Option const &opt)
    {
        os << opt.operator()();
        return os;
    }

    /// operator<<() is used to print all the options default values in chronological
    /// insertion order and in the format defined by the UCI protocol.
    template<typename CharT, typename Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<<(std::basic_ostream<CharT, Traits> &os, const NoCaseLessMap &optmap)
    {
        for (size_t idx = 0; idx < optmap.size(); ++idx)
        {
            for (auto const &pair : optmap)
            {
                auto const &option = pair.second;
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
extern UCI::NoCaseLessMap Options;

extern u32 option_threads();

#endif // _UCI_H_INC_
