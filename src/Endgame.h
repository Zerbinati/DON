#ifndef _ENDGAME_H_INC_
#define _ENDGAME_H_INC_

#include <cstring>   // For std::memset
#include <map>
#include <memory>
#include <type_traits>
#include <utility>
#include "Position.h"
#include "Type.h"

namespace EndGame {

    /// EndgameCode lists all supported endgame functions by corresponding codes
    enum EndgameCode : u08
    {
        EVALUATION_FUNCTIONS,
        KXK,   // Generic "mate lone king" eval
        KPK,   // KP vs K
        KBNK,  // KBN vs K
        KNNK,  // KNN vs K
        KRKP,  // KR vs KP
        KRKB,  // KR vs KB
        KRKN,  // KR vs KN
        KQKP,  // KQ vs KP
        KQKR,  // KQ vs KR

        SCALING_FUNCTIONS,
        KRPKR,   // KRP vs KR
        KRPKB,   // KRP vs KB
        KRPPKRP, // KRPP vs KRP
        KNPK,    // KNP vs K
        KBPKB,   // KBP vs KB
        KBPPKB,  // KBPP vs KB
        KBPKN,   // KBP vs KN
        KNPKB,   // KNP vs KB

        // Generic Scale functions
        KPKP,    // KP vs KP
        KPsK,    // KPs vs K
        KBPsKP,  // KBPs vs KP
        KQKRPs,  // KQ vs KRPs
    };

    /// Endgame functions can be of two category depending on whether they return Value or Scale.
    template<EndgameCode EC>
    using EndgameType = typename std::conditional<EC < SCALING_FUNCTIONS, Value, Scale>::type;

    /// Base and derived functors for endgame evaluation and scaling functions
    template<typename ET>
    class EndgameBase
    {
    public:
        const Color strong_color
            ,         weak_color;

        explicit EndgameBase (Color c)
            : strong_color ( c)
            ,   weak_color (~c)
        {}
        virtual ~EndgameBase () = default;
        EndgameBase& operator= (const EndgameBase&) = delete;

        virtual ET operator() (const Position &pos) const = 0;
    };

    template<EndgameCode EC, typename ET = EndgameType<EC>>
    class Endgame
        : public EndgameBase<ET>
    {
    public:
        explicit Endgame (Color c)
            : EndgameBase<ET> (c)
        {}
        virtual ~Endgame () = default;
        Endgame& operator= (const Endgame&) = delete;

        ET operator() (const Position &pos) const override;
    };

    /// The Endgames class stores the pointers to endgame evaluation and scaling base objects in two std::map. 
    /// Uses polymorphism to invoke the actual endgame function by calling its virtual operator().
    class Endgames
    {
    private:
        template<typename T> using Ptr = std::unique_ptr<EndgameBase<T>>;
        template<typename T> using Map = std::map<Key, Ptr<T>>;

        std::pair<Map<Value>, Map<Scale>> maps;

        template<typename T>
        Map<T>& map ()
        {
            return std::get<std::is_same<T, Scale>::value> (maps);
        }

        template<EndgameCode EC, typename ET = EndgameType<EC>>
        void add (const std::string &code)
        {
            StateInfo si[CLR_NO];
            std::memset (si, 0, sizeof (si));
            map<ET> ()[Position ().setup (code, WHITE, si[WHITE]).si->matl_key] = Ptr<ET> (new Endgame<EC> (WHITE));
            map<ET> ()[Position ().setup (code, BLACK, si[BLACK]).si->matl_key] = Ptr<ET> (new Endgame<EC> (BLACK));
        }

    public:
        Endgames ()
        {
            // EVALUATION_FUNCTIONS
            add<KPK>     ("KPK");
            add<KNNK>    ("KNNK");
            add<KBNK>    ("KBNK");
            add<KRKP>    ("KRKP");
            add<KRKB>    ("KRKB");
            add<KRKN>    ("KRKN");
            add<KQKP>    ("KQKP");
            add<KQKR>    ("KQKR");

            // SCALING_FUNCTIONS
            add<KRPKR>   ("KRPKR");
            add<KRPKB>   ("KRPKB");
            add<KRPPKRP> ("KRPPKRP");
            add<KNPK>    ("KNPK");
            add<KBPKB>   ("KBPKB");
            add<KBPPKB>  ("KBPPKB");
            add<KBPKN>   ("KBPKN");
            add<KNPKB>   ("KNPKB");
        }

        Endgames (const Endgames&) = delete;
        Endgames& operator= (const Endgames&) = delete;

        template<typename T>
        const EndgameBase<T>* probe (Key matl_key)
        {
            return map<T> ().find (matl_key) != map<T> ().end () ?
                    map<T> ()[matl_key].get () :
                    nullptr;
        }
    };

    extern void initialize ();

    extern void deinitialize ();
}

// Global Endgames
extern EndGame::Endgames *EndGames;

#endif // _ENDGAME_H_INC_
