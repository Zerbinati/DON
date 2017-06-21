#ifndef _ENDGAME_H_INC_
#define _ENDGAME_H_INC_

#include <map>
#include <memory>
#include <type_traits>
#include <utility>

#include "Type.h"

#include "Position.h"

namespace EndGame {

    // EndgameCode lists all supported endgame functions by corresponding codes
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
        KPsK,    // KPs vs Ks
        KPKP,    // KP vs KP
        KNPK,    // KNP vs K
        KBPKB,   // KBP vs KB
        KBPPKB,  // KBPP vs KB
        KBPKN,   // KBP vs KN
        KNPKB,   // KNP vs KB

        // Generic Scale functions
        KBPsKPs, // KBPs vs KPs
        KQKRPs,  // KQ vs KRPs
    };

    // Endgame functions can be of two category depending on whether they return Value or Scale.
    template<EndgameCode EC>
    using EndgameType = typename std::conditional<EC < SCALING_FUNCTIONS, Value, Scale>::type;

    // Base and derived functors for endgame evaluation and scaling functions
    template<typename T>
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

        virtual T operator() (const Position &pos) const = 0;
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

    // The Endgames class stores the pointers to endgame evaluation and scaling base objects in two std::map. 
    // Uses polymorphism to invoke the actual endgame function by calling its virtual operator().
    class Endgames
    {
    private:
        template<typename T> using Ptr = std::unique_ptr<EndgameBase<T>>;
        template<typename T> using Map = std::map<Key, Ptr<T>>;

        std::pair<Map<Value>, Map<Scale>> _maps;

        template<typename T> Map<T>& map ()
        {
            return std::get<std::is_same<T, Scale>::value> (_maps);
        }

        template<EndgameCode EC, typename ET = EndgameType<EC>, typename EP = Ptr<ET>>
        void add (const std::string &code)
        {
            StateInfo si;
            map<ET> ()[Position ().setup (code, si, WHITE).si->matl_key] = EP (new Endgame<EC> (WHITE));
            map<ET> ()[Position ().setup (code, si, BLACK).si->matl_key] = EP (new Endgame<EC> (BLACK));
        }

    public:
        Endgames ();
        Endgames (const Endgames&) = delete;
        Endgames& operator= (const Endgames&) = delete;

        template<typename T> EndgameBase<T>* probe (Key key)
        {
            return map<T> ().find (key) != map<T> ().end () ? map<T> ()[key].get () : nullptr;
        }
    };

    extern void initialize ();

    extern void deinitialize ();
}

// Global Endgames
extern EndGame::Endgames *EndGames;

#endif // _ENDGAME_H_INC_
