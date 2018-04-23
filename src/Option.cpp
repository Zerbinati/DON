#include "Option.h"

#include <fstream>
#include "Polyglot.h"
#include "Logger.h"
#include "Thread.h"
#include "Transposition.h"
#include "Searcher.h"
#include "TBsyzygy.h"
#include "MemoryHandler.h"

UCI::OptionMap Options;

namespace UCI {

    using namespace std;
    using namespace Searcher;
    using namespace TBSyzygy;

    Option::Option (OnChange on_cng)
        : type ("button")
        , default_value ("")
        , current_value ("")
        , minimum (0)
        , maximum (0)
        , on_change (on_cng)
    {}
    Option::Option (const char *val, OnChange on_cng)
        : Option (string(val), on_cng)
    {}
    Option::Option (const string &val, OnChange on_cng)
        : type ("string")
        , minimum (0)
        , maximum (0)
        , on_change (on_cng)
    {
        default_value = current_value = val;
    }
    Option::Option (const bool val, OnChange on_cng)
        : type ("check")
        , minimum (0)
        , maximum (0)
        , on_change (on_cng)
    {
        default_value = current_value = (val ? "true" : "false");
    }
    Option::Option (const i32 val, i32 min, i32 max, OnChange on_cng)
        : type ("spin")
        , minimum (min)
        , maximum (max)
        , on_change (on_cng)
    {
        default_value = current_value = std::to_string (val);
    }
    Option::Option (const char* v, const char* cur, OnChange on_cng)
        : type ("combo")
        , minimum (0)
        , maximum (0)
        , on_change (on_cng)
    {
        default_value = v;
        current_value = cur;
    }

    Option::operator string () const
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
        return stoi (current_value);
    }
    bool Option::operator== (const char *val) const
    {
        assert(type == "combo");
        return !no_case_less_comparer ()(current_value, val)
            && !no_case_less_comparer ()(val, current_value);
    }

    /// Option::operator=() updates value and triggers on_change() action.
    Option& Option::operator= (const char   *value)
    {
        return *this = string(value);
    }
    /// Option::operator=() updates value and triggers on_change() action.
    Option& Option::operator= (const string &value)
    {
        assert(!type.empty ());

        if (type != "button")
        {
            //if (value.empty ())
            //{
            //    return *this;
            //}

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
                val = std::to_string (std::min (std::max (stoi (val), minimum), maximum));
            }
            else
            if (type == "string")
            {
                if (white_spaces (val))
                {
                    val = Empty;
                }
            }

            current_value = val;
        }

        if (nullptr != on_change)
        {
            on_change ();
        }

        return *this;
    }

    /// Option::operator<<() inits options and assigns idx in the correct printing order
    void Option::operator<< (const Option &opt)
    {
        static size_t insert_order = 0;
        *this = opt;
        index = insert_order++;
    }
    /// Option::operator()() is to string method of option
    string Option::operator() ()  const
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

        void on_hash_size ()
        {
            TT.auto_resize (i32(Options["Hash"]), false);
        }

#   if defined(LPAGES)
        void on_memory_type ()
        {
            Memory::LargePages = bool(Options["Large Pages"]);
            TT.resize ();
        }
#   endif

        void on_clear_hash ()
        {
            clear ();
        }

        void on_retain_hash ()
        {
            TT.retain_hash = bool(Options["Retain Hash"]);
        }

        void on_hash_fn ()
        {
            TT.set_hash_fn (string(Options["Hash File"]));
        }

        void on_save_hash ()
        {
            on_hash_fn ();
            TT.save ();
        }

        void on_load_hash ()
        {
            on_hash_fn ();
            TT.load ();
        }

        void on_threads ()
        {
            auto threads = i32(Options["Threads"]);
            if (0 == threads)
            {
                threads = thread::hardware_concurrency ();
            }
            Threadpool.configure (threads);
        }

        void on_draw_movecount ()
        {
            Position::DrawClockPly = 2 * u08(i32(Options["Draw MoveCount"]));
        }

        void on_contempt_opt ()
        {
            FixedContempt = i16(i32(Options["Fixed Contempt"]));
            ContemptTime = i16(i32(Options["Timed Contempt"]));
            ContemptValue = i16(i32(Options["Valued Contempt"]));
        }

        void on_multipv ()
        {
            MultiPV = i32(Options["MultiPV"]);
            //MultiPV_cp = i32(Options["MultiPV_cp"]);
        }

        void on_book_opt ()
        {
            Book.use = bool(Options["Use Book"]);
            Book.initialize (string(Options["Book File"]));
            Book.pick_best = bool(Options["Book Pick Best"]);
            Book.move_num = i16(i32(Options["Book Move Num"]));
        }

        void on_skill_level ()
        {
            Threadpool.main_thread ()->skill_mgr.level = u08(i32(Options["Skill Level"]));
        }

        void on_time_opt ()
        {
            OverheadMoveTime = TimePoint(i32(Options["Overhead Move Time"]));
            MinimumMoveTime = TimePoint(i32(Options["Minimum Move Time"]));
            //OverheadClockTime = TimePoint(i32(Options["Overhead Clock Time"]));
            //MaximumMoveHorizon = u08(i32(Options["Maximum Move Horizon"]));
            //ReadyMoveHorizon = u08(i32(Options["Ready Move Horizon"]));
            MoveSlowness = i32(Options["Move Slowness"]);
            NodesTime = u16(i32(Options["Nodes Time"]));
            Ponder = bool(Options["Ponder"]);
        }

        void on_debug_file ()
        {
            auto filename = string(Options["Debug File"]);
            trim (filename);
            convert_path (filename);
            Loger.log (filename);
        }

        void on_output_file ()
        {
            auto filename = string(Options["Output File"]);
            trim (filename);
            convert_path (filename);
            OutputFile = filename;
        }

        void on_syzygy_path ()
        {
            auto filename = string(Options["SyzygyPath"]);
            trim (filename);
            PathString = filename;
            TBSyzygy::initialize ();
        }

        void on_uci_chess960 ()
        {
            Position::Chess960 = bool(Options["UCI_Chess960"]);
        }

        void on_uci_elo_limit ()
        {
            u08 skill_level;
            if (bool(Options["UCI_LimitStrength"]))
            {
                // ELO values corresponded to every Skill Levels
                const i32 LevelELO[SkillManager::MaxLevel + 1] =
                {
                    1250, 1436, 1622, 1808, 1994, 2180, 2366, 2552, 2738, 2924, 3110, 3296, 3482
                };

                skill_level = SkillManager::MaxLevel;
                i32 elo = i32(Options["UCI_ELO"]);
                for (u08 level = 0; level < SkillManager::MaxLevel; ++level)
                {
                    if (elo < LevelELO[level + 1])
                    {
                        skill_level = level;
                        break;
                    }
                }
            }
            else
            {
                skill_level = u08(i32(Options["Skill Level"]));
            }
            Threadpool.main_thread ()->skill_mgr.level = skill_level;
        }
    }

    void initialize ()
    {

        Options["Hash"]               << Option (16, 0, TTable::MaxHashSize, on_hash_size);

#if defined(LPAGES)
        Options["Large Pages"]        << Option (Memory::LargePages, on_memory_type);
#endif

        Options["Clear Hash"]         << Option (on_clear_hash);
        Options["Retain Hash"]        << Option (TT.retain_hash, on_retain_hash);

        Options["Hash File"]          << Option (TT.hash_fn, on_hash_fn);
        Options["Save Hash"]          << Option (on_save_hash);
        Options["Load Hash"]          << Option (on_load_hash);

        Options["Use Book"]           << Option (Book.use, on_book_opt);
        Options["Book File"]          << Option (Book.filename, on_book_opt);
        Options["Book Pick Best"]     << Option (Book.pick_best, on_book_opt);
        Options["Book Move Num"]      << Option (Book.move_num, 0, 100, on_book_opt);

        Options["Threads"]            << Option ( 1, 0, 512, on_threads);

        Options["Skill Level"]        << Option (SkillManager::MaxLevel,  0, SkillManager::MaxLevel, on_skill_level);

        Options["MultiPV"]            << Option (MultiPV, 1, 255, on_multipv);
        //Options["MultiPV_cp"]         << Option (MultiPV_cp, 0, 1000, on_multipv);

        Options["Fixed Contempt"]     << Option (FixedContempt, -100, 100, on_contempt_opt);
        Options["Timed Contempt"]     << Option (ContemptTime , 0, 1000, on_contempt_opt);
        Options["Valued Contempt"]    << Option (ContemptValue, 0, 1000, on_contempt_opt);

        Options["UCI_AnalyseMode"]    << Option (false);
        Options["Analysis Contempt"]  << Option ("Both var Off var White var Black var Both", "Both");

        Options["Draw MoveCount"]     << Option (Position::DrawClockPly/2, 5, 50, on_draw_movecount);

        Options["Overhead Move Time"] << Option (OverheadMoveTime, 0, 5000, on_time_opt);
        Options["Minimum Move Time"]  << Option (MinimumMoveTime, 0, 5000, on_time_opt);
        //Options["Maximum Move Horizon"]<< Option (MaximumMoveHorizon, 0, 100, on_time_opt);
        //Options["Ready Move Horizon"]  << Option (ReadyMoveHorizon, 0, 100, on_time_opt);
        //Options["Overhead Clock Time"] << Option (OverheadClockTime, 0, 30000, on_time_opt);
        Options["Move Slowness"]      << Option (MoveSlowness, 10, 1000, on_time_opt);
        Options["Nodes Time"]         << Option (NodesTime, 0, 10000, on_time_opt);
        Options["Ponder"]             << Option (Ponder, on_time_opt);

        Options["SyzygyPath"]         << Option (PathString, on_syzygy_path);
        Options["SyzygyProbeDepth"]   << Option (TBProbeDepth, 1, 100);
        Options["SyzygyLimitPiece"]   << Option (TBLimitPiece, 0, 6);
        Options["SyzygyUseRule50"]    << Option (TBUseRule50);

        Options["Debug File"]         << Option (Loger.filename, on_debug_file);
        Options["Output File"]        << Option (OutputFile, on_output_file);

        Options["UCI_Chess960"]       << Option (Position::Chess960, on_uci_chess960);
        Options["UCI_LimitStrength"]  << Option (false, on_uci_elo_limit);
        Options["UCI_ELO"]            << Option (3490, 1250, 3490, on_uci_elo_limit);

    }

    void deinitialize ()
    {
        Options.clear ();
    }

}
