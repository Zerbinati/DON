#ifndef _UCI_H_INC_
#define _UCI_H_INC_

#include <map>
#include <memory>
#include <iostream>

#include "Type.h"
#include "functor.h"

namespace UCI {

    class Option;

    typedef std::unique_ptr<Option> OptionPtr;

    // Our options container is actually a std::map
    typedef std::map<std::string, OptionPtr, std::no_case_less_comparer> OptionMap;

    // Option class implements an option as defined by UCI protocol
    class       Option
    {

    private:

        u08 _idx;

        template<class CharT, class Traits>
        friend std::basic_ostream<CharT, Traits>&
            operator<< (std::basic_ostream<CharT, Traits> &os, const Option &opt);

        template<class CharT, class Traits>
        friend std::basic_ostream<CharT, Traits>&
            operator<< (std::basic_ostream<CharT, Traits> &os, const OptionMap &optmap);

    protected:

        typedef void (*OnChange) (const Option &);
        OnChange _on_change;

    public:

        Option (const OnChange on_change = nullptr);
        virtual ~Option ();

        virtual std::string operator() ()  const   = 0;

        virtual operator bool ()        const { return bool (); }
        virtual operator i32 ()         const { return i32 (); }
        virtual operator std::string () const { return std::string (); }
        virtual Option& operator= (std::string &value) = 0;

    };

    class ButtonOption : public Option
    {
    public:
        ButtonOption (const OnChange on_change = nullptr);

        std::string operator() ()  const;

        Option& operator= (std::string &value);

    };

    class  CheckOption : public Option
    {
    public:
        bool _default
            , _value;

        CheckOption (const bool val, const OnChange on_change = nullptr);

        std::string operator() ()  const;
        virtual operator bool () const;

        Option& operator= (std::string &value);

    };

    class StringOption : public Option
    {
    public:
        std::string _default
            , _value;

        StringOption (const char *val, const OnChange on_change = nullptr);

        std::string operator() ()  const;
        operator std::string () const;

        Option& operator= (std::string &value);

    };

    class   SpinOption : public Option
    {
    public:
        i32 _default
            , _value
            , _minimum
            , _maximum;

        SpinOption (const i32 val, i32 minimum, i32 maximum, const OnChange on_change = nullptr);

        std::string operator() ()  const;
        operator i32 () const;

        Option& operator= (std::string &value);

    };

    template<class CharT, class Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, const Option &opt)
    {
        os << opt.operator() ();
        return os;
    }

    // operator<<() is used to print all the options default values in chronological
    // insertion order (the idx field) and in the format defined by the UCI protocol.
    template<class CharT, class Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, const OptionMap &optmap)
    {
        for (u08 idx = 0; idx <= optmap.size (); ++idx)
        {
            //for (const auto &pair : optmap)
            for (const std::pair<const std::string, OptionPtr> &pair : optmap)
            {
                const Option &option = *(pair.second.get ());
                if (idx == option._idx)
                {
                    os << "option name " << pair.first << option << std::endl;
                    break;
                }
            }
        }
        return os;
    }


    extern void   initialize ();
    extern void deinitialize ();

    // ---------------------------------------------

    extern void start (const std::string &arg = "");

    extern void stop ();
}

extern UCI::OptionMap Options;  // Global string mapping of Options

#endif // _UCI_H_INC_
