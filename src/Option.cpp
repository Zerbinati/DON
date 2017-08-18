#include "Option.h"

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

    Option::Option (OnChange on_cng)
        : type ("button")
        , default_value ("")
        , current_value ("")
        , minimum (0)
        , maximum (0)
        , on_change (on_cng)
    {}
    Option::Option (const bool val, OnChange on_cng)
        : type ("check")
        , minimum (0)
        , maximum (0)
        , on_change (on_cng)
    {
        default_value = current_value = (val ? "true" : "false");
    }
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
    Option::Option (const i32 val, i32 min, i32 max, OnChange on_cng)
        : type ("spin")
        , minimum (min)
        , maximum (max)
        , on_change (on_cng)
    {
        default_value = current_value = std::to_string (val);
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
    Option::operator string () const
    {
        assert(type == "string");
        return current_value;
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
            auto val = value;

            if (val.empty ())
            {
                return *this;
            }

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

            if (current_value != val)
            {
                current_value = val;
            }
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
        static u08 insert_order = 0;
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
            oss << " default " << default_value;
            if (type == "spin")
            {
                oss << " min " << minimum << " max " << maximum;
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
            RetainHash = bool(Options["Retain Hash"]);
        }

        void on_save_hash ()
        {
            string hash_fn = string(Options["Hash File"]);
            trim (hash_fn);
            if (!white_spaces (hash_fn))
            {
                convert_path (hash_fn);
                TT.save (hash_fn);
            }
        }

        void on_load_hash ()
        {
            string hash_fn = string(Options["Hash File"]);
            trim (hash_fn);
            if (!white_spaces (hash_fn))
            {
                convert_path (hash_fn);
                TT.load (hash_fn);
            }
        }

        void on_thread_count ()
        {
            auto threads = i32(Options["Threads"]);
            if (0 == threads)
            {
                threads = thread::hardware_concurrency ();
            }
            Threadpool.configure (threads);
        }

        void on_draw_move_dist ()
        {
            Position::DrawClockPly = u08(2 * i32(Options["Draw Move Dist"]));
        }

        void on_hash_file ()
        {
            auto filename = string(Options["Hash File"]);
            trim (filename);
            if (!white_spaces (filename))
            {
                convert_path (filename);
                HashFile = filename;
            }
        }

        void on_contempt_opt ()
        {
            FixedContempt = i16(i32(Options["Fixed Contempt"]));
            ContemptTime =  i16(i32(Options["Timed Contempt"]));
            ContemptValue = i16(i32(Options["Valued Contempt"]));
        }

        void on_multipv ()
        {
            MultiPV = u08(i32(Options["MultiPV"]));
            //MultiPV_cp =  i32(Options["MultiPV_cp"]);
        }

        void on_book_opt ()
        {
            OwnBook =         bool(Options["OwnBook"]);
            auto filename = string(Options["Book File"]);
            BookMoveBest =    bool(Options["Book Move Best"]);
            BookUptoMove = i16(i32(Options["Book Upto Move"]));

            trim (filename);
            if (!white_spaces (filename))
            {
                convert_path (filename);
                OutputFile = filename;
            }
        }

        void on_skill_level ()
        {
            Threadpool.main_thread ()->skill_mgr.level = (u08(i32(Options["Skill Level"])));
        }

        void on_time_opt ()
        {
            //MaximumMoveHorizon = i32(Options["Maximum Move Horizon"]);
            //ReadyMoveHorizon =   i32(Options["Ready Move Horizon"]);
            //OverheadClockTime =  i32(Options["Overhead Clock Time"]);
            OverheadMoveTime =   i32(Options["Overhead Move Time"]);
            NodesTime =          i32(Options["Nodes Time"]);
            Ponder =             bool(Options["Ponder"]);
        }

        void on_debug_file ()
        {
            auto filename = string(Options["Debug File"]);
            trim (filename);
            if (!white_spaces (filename))
            {
                convert_path (filename);
                Logger::log (filename);
            }
        }

        void on_output_file ()
        {
            auto filename = string(Options["Output File"]);
            trim (filename);
            if (!white_spaces (filename))
            {
                convert_path (filename);
                OutputFile = filename;
            }
        }

        void on_syzygy_path ()
        {
            auto filename = string(Options["SyzygyPath"]);
            trim (filename);
            if (!white_spaces (filename))
            {
                PathString = filename;
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
            const i32 LevelELO[SkillManager::MaxLevel + 1] =
            {
                1250, 1436, 1622, 1808, 1994, 2180, 2366, 2552, 2738, 2924, 3110, 3296, 3482
            };

            u08 skill_level = SkillManager::MaxLevel;
            if (bool(Options["UCI_LimitStrength"]))
            {
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
            Threadpool.main_thread ()->skill_mgr.level = skill_level;
        }
    }

    void initialize ()
    {

        Options["Hash"]                         << Option (16, 0, Table::MaxHashSize, on_hash_size);

#if defined(LPAGES)
        Options["Large Pages"]                  << Option (Memory::LargePages, on_memory_type);
#endif

        Options["Clear Hash"]                   << Option (on_clear_hash);
        Options["Retain Hash"]                  << Option (RetainHash, on_retain_hash);

        Options["Hash File"]                    << Option (HashFile, on_hash_file);
        Options["Save Hash"]                    << Option (on_save_hash);
        Options["Load Hash"]                    << Option (on_load_hash);

        Options["OwnBook"]                      << Option (OwnBook, on_book_opt);
        Options["Book File"]                    << Option (BookFile, on_book_opt);
        Options["Book Move Best"]               << Option (BookMoveBest, on_book_opt);
        Options["Book Upto Move"]               << Option (BookUptoMove, 0, 50, on_book_opt);

        Options["Threads"]                      << Option ( 1, 0, 512, on_thread_count);

        Options["Skill Level"]                  << Option (SkillManager::MaxLevel,  0, SkillManager::MaxLevel, on_skill_level);

        Options["MultiPV"]                      << Option (MultiPV, 1, 255, on_multipv);
        //Options["MultiPV_cp"]                   << Option (MultiPV_cp, 0, 1000, on_multipv);

        Options["Fixed Contempt"]               << Option (FixedContempt, -100, 100, on_contempt_opt);
        Options["Timed Contempt"]               << Option (ContemptTime , 0, 1000, on_contempt_opt);
        Options["Valued Contempt"]              << Option (ContemptValue, 0, 1000, on_contempt_opt);

        Options["Draw Move Dist"]               << Option (Position::DrawClockPly/2, 5, 50, on_draw_move_dist);

        //Options["Maximum Move Horizon"]         << Option (MaximumMoveHorizon, 0, 100, on_time_opt);
        //Options["Ready Move Horizon"]           << Option (ReadyMoveHorizon, 0, 100, on_time_opt);
        //Options["Overhead Clock Time"]          << Option (i32(OverheadClockTime), 0, 30000, on_time_opt);
        Options["Overhead Move Time"]           << Option (i32(OverheadMoveTime), 0, 5000, on_time_opt);
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
        Options["UCI_ELO"]                      << Option (3490, 1250, 3490, on_uci_elo);
    }

    void deinitialize ()
    {
        Options.clear ();
    }

}
