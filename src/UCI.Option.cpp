#include "UCI.h"

#include <fstream>

#include "Thread.h"
#include "Transposition.h"
#include "Searcher.h"
#include "TBsyzygy.h"
#include "Debugger.h"
#include "MemoryHandler.h"

UCI::OptionMap Options;

namespace UCI {

    using namespace std;
    using namespace Transposition;
    using namespace Searcher;
    using namespace TBSyzygy;
    using namespace Debugger;
#if defined(LPAGES)
    using namespace Memory;
#endif

    Option::Option (OnChange on_change)
        : _type ("button")
        , _default_value ("")
        , _current_value ("")
        , _minimum (0)
        , _maximum (0)
        , _on_change (on_change)
    {}
    Option::Option (const bool  val, OnChange on_change)
        : _type ("check")
        , _minimum (0)
        , _maximum (0)
        , _on_change (on_change)
    {
        _default_value = _current_value = (val ? "true" : "false");
    }
    Option::Option (const char *val, OnChange on_change)
        : Option (string(val), on_change)
    {}
    Option::Option (const string &val, OnChange on_change)
        : _type ("string")
        , _minimum (0)
        , _maximum (0)
        , _on_change (on_change)
    {
        _default_value = _current_value = val;
    }
    Option::Option (const i32 val, i32 minimum, i32 maximum, OnChange on_change)
        : _type ("spin")
        , _minimum (minimum)
        , _maximum (maximum)
        , _on_change (on_change)
    {
        _default_value = _current_value = std::to_string (val);
    }

    Option::operator bool () const
    {
        assert(_type == "check");
        return _current_value == "true";
    }
    Option::operator i32 () const
    {
        assert(_type == "spin");
        return stoi (_current_value);
    }
    Option::operator string () const
    {
        assert(_type == "string");
        return _current_value;
    }

    // operator=() updates value and triggers on_change() action.
    // It's up to the GUI to check for option's limits,
    Option& Option::operator= (const char   *value)
    {
        return *this = string(value);
    }
    Option& Option::operator= (const string &value)
    {
        assert(!_type.empty ());

        if (_type != "button")
        {
            auto val = value;

            if (val.empty ())
            {
                return *this;
            }

            if (_type == "check")
            {
                to_lower (val);
                if (   val != "true"
                    && val != "false")
                {
                    return *this;
                }
            }
            else
            if (_type == "spin")
            {
                val = std::to_string (std::min (std::max (stoi (val), _minimum), _maximum));
            }

            if (_current_value != val)
            {
                _current_value = val;
            }
        }

        if (_on_change != nullptr)
        {
            _on_change ();
        }

        return *this;
    }

    // operator<<() inits options and assigns idx in the correct printing order
    void Option::operator<< (const Option &opt)
    {
        static u08 insert_order = 0;
        *this = opt;
        _index = insert_order++;
    }
    // operator()() is to string method of option
    string Option::operator() ()  const
    {
        ostringstream oss;
        oss << " type " << _type;
        if (_type != "button")
        {
            oss << " default " << _default_value;
            if (_type == "spin")
            {
                oss << " min " << _minimum << " max " << _maximum;
            }
            //oss << " current " << _current_value;
        }
        return oss.str ();
    }
    
    namespace {
        
        // 'On change' actions, triggered by an option's value change

        void on_hash_size ()
        {
            TT.auto_resize (i32(Options["Hash"]), false);
        }

#   if defined(LPAGES)
        void on_memory_type ()
        {
            LargePages = bool(Options["Large Pages"]);
            TT.resize ();
        }
#   endif

        void on_clear_hash ()
        {
            ForceStop = true;
            Threadpool.wait_while_thinking ();
            Searcher::clear ();
        }

        void on_retain_hash ()
        {
            TT.retain_hash = bool(Options["Retain Hash"]);
        }

        void on_save_hash ()
        {
            string hash_fn = string(Options["Hash File"]);
            trim (hash_fn);
            if (!hash_fn.empty ())
            {
                convert_path (hash_fn);
                TT.save (hash_fn);
            }
        }

        void on_load_hash ()
        {
            string hash_fn = string(Options["Hash File"]);
            trim (hash_fn);
            if (!hash_fn.empty ())
            {
                convert_path (hash_fn);
                TT.load (hash_fn);
            }
        }

        void on_thread_count ()
        {
            Threadpool.configure (i32(Options["Threads"]));
        }

        void on_draw_move_dist ()
        {
            Position::DrawClockPly = u08(2 * i32(Options["Draw Move Dist"]));
        }

        void on_hash_file ()
        {
            HashFile = string(Options["Hash File"]);
            trim (HashFile);
            if (!HashFile.empty ())
            {
                convert_path (HashFile);
            }
        }

        void on_contempt_opt ()
        {
            FixedContempt = i16(i32(Options["Fixed Contempt"]));
            ContemptTime  = i16(i32(Options["Timed Contempt"]));
            ContemptValue = i16(i32(Options["Valued Contempt"]));
        }

        void on_multipv ()
        {
            MultiPV = u08(i32(Options["MultiPV"]));
            //MultiPV_cp  = i32(Options["MultiPV_cp"]);
        }

        void on_book_opt ()
        {
            OwnBook = bool(Options["OwnBook"]);
            BookFile = string(Options["Book File"]);
            BookMoveBest = bool(Options["Book Move Best"]);
            BookUptoMove = i16(i32(Options["Book Upto Move"]));

            trim (BookFile);
            if (!BookFile.empty ())
            {
                convert_path (BookFile);
            }
        }

        void on_skill_level ()
        {
            Threadpool.skill_mgr.change_skill_level (u08(i32(Options["Skill Level"])));
        }

        void on_time_opt ()
        {
            //MaximumMoveHorizon  = i32(Options["Maximum Move Horizon"]);
            //ReadyMoveHorizon    = i32(Options["Ready Move Horizon"]);
            //OverheadClockTime   = TimePoint(i32(Options["Overhead Clock Time"]));
            //OverheadMoveTime    = TimePoint(i32(Options["Overhead Move Time"]));
            //MinimumMoveTime     = TimePoint(i32(Options["Minimum Move Time"]));
            MoveSlowness        = i32(Options["Move Slowness"])/100.0;
            NodesTime           = i32(Options["Nodes Time"]);
            Ponder              = bool(Options["Ponder"]);
        }

        void on_debug_file ()
        {
            string filename = string(Options["Debug File"]);
            trim (filename);
            if (!filename.empty ())
            {
                convert_path (filename);
                Logger::log (filename);
            }
        }

        void on_output_file ()
        {
            OutputFile = string(Options["Output File"]);
            trim (OutputFile);
            if (!OutputFile.empty ())
            {
                convert_path (OutputFile);
            }
        }

        void on_syzygy_path ()
        {
            PathString = string(Options["SyzygyPath"]);
            trim (PathString);
            if (!PathString.empty ())
            {
                TBSyzygy::initialize ();
            }
        }

        void on_uci_chess960 ()
        {
            Position::Chess960 = bool(Options["UCI_Chess960"]);
        }

        void on_uci_elo ()
        {
            // ELO values corresponded to every Skill Levels
            static const i32 LevelELO[SkillManager::MaxSkillLevel + 1] =
            {
                1250, 1436, 1622, 1808, 1994, 2180, 2366, 2552, 2738, 2924, 3110, 3296, 3482
            };

            u08 skill_level = SkillManager::MaxSkillLevel;
            if (bool(Options["UCI_LimitStrength"]))
            {
                i32 elo = i32(Options["UCI_ELO"]);
                for (u08 level = 0; level < SkillManager::MaxSkillLevel; ++level)
                {
                    if (elo < LevelELO[level + 1])
                    {
                        skill_level = level;
                        break;
                    }
                }
            }
            Threadpool.skill_mgr.change_skill_level (skill_level);
        }
    }

    void initialize ()
    {

        Options["Hash"]                         << Option (16, 0, Table::MaxHashSize, on_hash_size);

#if defined(LPAGES)
        Options["Large Pages"]                  << Option (LargePages, on_memory_type);
#endif

        Options["Clear Hash"]                   << Option (on_clear_hash);
        Options["Retain Hash"]                  << Option (TT.retain_hash, on_retain_hash);

        Options["Hash File"]                    << Option (HashFile, on_hash_file);
        Options["Save Hash"]                    << Option (on_save_hash);
        Options["Load Hash"]                    << Option (on_load_hash);

        Options["OwnBook"]                      << Option (OwnBook, on_book_opt);
        Options["Book File"]                    << Option (BookFile, on_book_opt);
        Options["Book Move Best"]               << Option (BookMoveBest, on_book_opt);
        Options["Book Upto Move"]               << Option (BookUptoMove, 0, 50, on_book_opt);

        Options["Threads"]                      << Option ( 1, 0, 256, on_thread_count);

        Options["Skill Level"]                  << Option (SkillManager::MaxSkillLevel,  0, SkillManager::MaxSkillLevel, on_skill_level);

        Options["MultiPV"]                      << Option (MultiPV, 1, 64, on_multipv);
        //Options["MultiPV_cp"]                   << Option (MultiPV_cp, 0, 1000, on_multipv);

        Options["Fixed Contempt"]               << Option (FixedContempt, -100, 100, on_contempt_opt);
        Options["Timed Contempt"]               << Option (ContemptTime , 0, 1000, on_contempt_opt);
        Options["Valued Contempt"]              << Option (ContemptValue, 0, 1000, on_contempt_opt);

        Options["Draw Move Dist"]               << Option (Position::DrawClockPly/2, 5, 50, on_draw_move_dist);

        //Options["Maximum Move Horizon"]         << Option (MaximumMoveHorizon, 0, 100, on_time_opt);
        //Options["Ready Move Horizon"]           << Option (ReadyMoveHorizon, 0, 100, on_time_opt);
        //Options["Overhead Clock Time"]          << Option (i32(OverheadClockTime), 0, 30000, on_time_opt);
        //Options["Overhead Move Time"]           << Option (i32(OverheadMoveTime), 0, 5000, on_time_opt);
        //Options["Minimum Move Time"]            << Option (i32(MinimumMoveTime), 0, 5000, on_time_opt);
        Options["Move Slowness"]                << Option (i32(std::round (MoveSlowness*100)), 10, 1000, on_time_opt);
        Options["Nodes Time"]                   << Option (NodesTime, 0, 10000, on_time_opt);
        Options["Ponder"]                       << Option (Ponder, on_time_opt);

        Options["SyzygyPath"]                   << Option (PathString, on_syzygy_path);
        Options["SyzygyProbeDepth"]             << Option (TBProbeDepth, 1, 100);
        Options["SyzygyLimitPiece"]             << Option (TBLimitPiece, 0, 6);
        Options["SyzygyUseRule50"]              << Option (TBUseRule50);

        Options["Debug File"]                   << Option (Empty, on_debug_file);
        Options["Output File"]                  << Option (OutputFile, on_output_file);

        Options["UCI_Chess960"]                 << Option (Position::Chess960, on_uci_chess960);
        Options["UCI_LimitStrength"]            << Option (false, on_uci_elo);
        Options["UCI_ELO"]                      << Option (1250, 1250, 3490, on_uci_elo);
    }

    void deinitialize ()
    {
        Options.clear ();
    }

}
