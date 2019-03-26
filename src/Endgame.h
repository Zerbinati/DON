#ifndef _ENDGAME_H_INC_
#define _ENDGAME_H_INC_

#include <cstring>   // For std::memset
#include <map>
#include <memory>
#include <type_traits>
#include <utility>
#include "Position.h"
#include "Type.h"

namespace Endgames {

    /// EndgameCode lists all supported endgame functions by corresponding codes
    enum EndgameCode : u08
    {
        EVALUATION_FUNCTIONS,
        KXK,   // Generic "mate lone king" eval
        KPK,   // KP vs K
        KBNK,  // KBN vs K
        KNNK,  // KNN vs K
        KNNKP, // KNN vs KP
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

    /// Base functors for endgame evaluation and scaling functions
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

        virtual ET operator() (const Position&) const = 0;
    };

    /// Derived functors for endgame evaluation and scaling functions
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

        ET operator() (const Position&) const override;
    };


    template<typename T> using Ptr = std::unique_ptr<EndgameBase<T>>;
    template<typename T> using Map = std::map<Key, Ptr<T>>;

    // Stores the pointers to endgame evaluation and scaling base objects in two std::map
    extern std::pair<Map<Value>, Map<Scale>> pair;

    template<typename T>
    Map<T>& map ()
    {
        return std::get<std::is_same<T, Scale>::value> (pair);
    }

    template<typename ET>
    const EndgameBase<ET>* probe (Key matl_key)
    {
        return map<ET> ().count (matl_key) != 0 ?
                map<ET> ()[matl_key].get () :
                nullptr;
    }

    extern void initialize ();
};

#endif // _ENDGAME_H_INC_
