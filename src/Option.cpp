#include "Option.h"

#include <fstream>
#include "Polyglot.h"
#include "Logger.h"
#include "Thread.h"
#include "Transposition.h"
#include "Searcher.h"
#include "TBsyzygy.h"

UCI::NoCaseLessMap Options;

using namespace std;

namespace UCI {

    using namespace Searcher;
    using namespace TBSyzygy;

    Option::Option(OnChange on_cng)
        : type ("button")
        , default_value ("")
        , current_value ("")
        , minimum (0)
        , maximum (0)
        , on_change (on_cng)
    {}
    Option::Option(char const *val, OnChange on_cng)
        : type ("string")
        , minimum (0)
        , maximum (0)
        , on_change (on_cng)
    {
        default_value = current_value = val;
    }
    Option::Option(bool const val, OnChange on_cng)
        : type ("check")
        , minimum (0)
        , maximum (0)
        , on_change (on_cng)
    {
        default_value = current_value = (val ? "true" : "false");
    }
    Option::Option(i32 const val, i32 min, i32 max, OnChange on_cng)
        : type ("spin")
        , minimum (min)
        , maximum (max)
        , on_change (on_cng)
    {
        default_value = current_value = std::to_string(val);
    }
    Option::Option(char const* v, char const* cur, OnChange on_cng)
        : type ("combo")
        , minimum (0)
        , maximum (0)
        , on_change (on_cng)
    {
        default_value = v;
        current_value = cur;
    }

    Option::operator string() const
    {
        assert(type == "string");
        return current_value;
    }
    Option::operator bool () const
    {
        assert(type == "check");
        return current_value == "true";
    }
    Option::operator i32 () const
    {
        assert(type == "spin");
        return stoi(current_value);
    }
    bool Option::operator==(char const *val) const
    {
        assert(type == "combo");
        return !no_case_less_comparer()(current_value, val)
            && !no_case_less_comparer()(val, current_value);
    }

    /// Option::operator=() updates value and triggers on_change() action.
    Option& Option::operator=(char const *value)
    {
        return *this = {value};
    }
    /// Option::operator=() updates value and triggers on_change() action.
    Option& Option::operator=(string const &value)
    {
        assert(!type.empty());

        if (type != "button")
        {
            auto val = value;

            if (type == "check")
            {
                to_lower (val);
                if (   val != "true"
                    && val != "false")
                {
                    return *this;
                }
            }
            else
            if (type == "spin")
            {
                val = std::to_string(clamp(stoi(val), minimum, maximum));
            }
            else
            if (type == "string")
            {
                if (white_spaces(val))
                {
                    val = "<empty>";
                }
            }
            else
            if (type == "combo")
            {
                NoCaseLessMap comboMap; // To have case insensitive compare
                std::istringstream ss (default_value);
                string token;
                while (ss >> token)
                {
                    if (token == "var")
                    {
                        continue;
                    }
                    comboMap[token] << Option();
                }
                if (comboMap.find(val) == comboMap.end())
                {
                    return *this;
                }
            }
            current_value = val;
        }

        if (nullptr != on_change)
        {
            on_change (*this);
        }

        return *this;
    }

    /// Option::operator<<() inits options and assigns idx in the correct printing order
    void Option::operator<<(Option const &opt)
    {
        static size_t insert_order = 0;
        *this = opt;
        index = insert_order++;
    }
    /// Option::operator()() is to string method of option
    string Option::operator()()  const
    {
        ostringstream oss;
        oss << " type " << type;

        if (type != "button")
        {
            if (   type == "string"
                || type == "check"
                || type == "combo")
            {
                oss << " default " << default_value;
            }
            else
            if (type == "spin")
            {
                oss << " default " << default_value
                    << " min " << minimum
                    << " max " << maximum;
            }
            //oss << " current " << current_value;
        }
        return oss.str ();
    }

    /// 'On change' actions, triggered by an option's value change

    namespace {

        void on_hash(Option const &o)
        {
            TT.auto_resize(i32(o));
        }

#   if defined(LPAGES)
        void on_large_pages(Option const&)
        {
            TT.auto_resize(i32(Options["Hash"]));
        }
#   endif

        void on_clear_hash(Option const&)
        {
            clear();
        }

        void on_save_hash(Option const&)
        {
            TT.save(string(Options["Hash File"]));
        }
        void on_load_hash(Option const&)
        {
            TT.load(string(Options["Hash File"]));
        }

        void on_threads(Option const&)
        {
            auto threads = option_threads();
            if (threads != Threadpool.size())
            {
                Threadpool.configure(threads);
            }
        }

        void on_book_fn(Option const &o)
        {
            Book.initialize(string(o));
        }

        void on_debug_fn(Option const &o)
        {
            Log.set(string(o));
        }

        void on_syzygy_path(Option const &o)
        {
            TBSyzygy::initialize(string(o));
        }

    }

    void initialize()
    {

        Options["Hash"]               << Option(16, 0, TTable::MaxHashSize, on_hash);

#if defined(LPAGES)
        Options["Large Pages"]        << Option(true, on_large_pages);
#endif

        Options["Clear Hash"]         << Option(on_clear_hash);
        Options["Retain Hash"]        << Option(false);

        Options["Hash File"]          << Option("Hash.dat");
        Options["Save Hash"]          << Option(on_save_hash);
        Options["Load Hash"]          << Option(on_load_hash);

        Options["Use Book"]           << Option(false);
        Options["Book File"]          << Option("Book.bin", on_book_fn);
        Options["Book Pick Best"]     << Option(true);
        Options["Book Move Num"]      << Option(20, 0, 100);

        Options["Threads"]            << Option(1, 0, 512, on_threads);

        Options["Skill Level"]        << Option(MaxLevel,  0, MaxLevel);

        Options["MultiPV"]            << Option(1, 1, 500);
        
        Options["Fixed Contempt"]     << Option(0, -100, 100);
        Options["Contempt Time"]      << Option(60, 0, 1000);
        Options["Contempt Value"]     << Option(10, 0, 1000);
        Options["Analysis Contempt"]  << Option("Both var Off var White var Black var Both", "Both");

        Options["Draw MoveCount"]     << Option(50, 5, 50);

        Options["Overhead Move Time"] << Option(30, 0, 5000);
        Options["Minimum Move Time"]  << Option(20, 0, 5000);
        Options["Move Slowness"]      << Option(84, 10, 1000);
        Options["Time Nodes"]         << Option(0, 0, 10000);
        Options["Ponder"]             << Option(true);

        Options["SyzygyPath"]         << Option("<empty>", on_syzygy_path);
        Options["SyzygyProbeDepth"]   << Option(TBProbeDepth, 1, 100);
        Options["SyzygyLimitPiece"]   << Option(TBLimitPiece, 0, 6);
        Options["SyzygyUseRule50"]    << Option(TBUseRule50);

        Options["Debug File"]         << Option("<empty>", on_debug_fn);
        Options["Output File"]        << Option("<empty>");

        Options["UCI_Chess960"]       << Option(false);
        Options["UCI_AnalyseMode"]    << Option(false);
        Options["UCI_LimitStrength"]  << Option(false);
        Options["UCI_Elo"]            << Option(1350, 1350, 3100);

    }

    void deinitialize()
    {
        Options.clear();
    }

}

u32 option_threads()
{
    auto threads = i32(Options["Threads"]);
    if (0 == threads)
    {
        threads = NativeThread::hardware_concurrency();
    }
    return threads;
}
