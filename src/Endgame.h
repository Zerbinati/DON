#pragma once

#include <unordered_map>
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
    template<EndgameCode C>
    using EndgameType = typename std::conditional<C < SCALING_FUNCTIONS, Value, Scale>::type;

    /// Base functors for endgame evaluation and scaling functions
    template<typename T>
    class EndgameBase
    {
    public:
        const Color strong_color
            ,         weak_color;

        explicit EndgameBase(Color c)
            : strong_color{ c}
            ,   weak_color{~c}
        {}
        virtual ~EndgameBase() = default;
        EndgameBase& operator=(EndgameBase const&) = delete;

        virtual T operator()(Position const&) const = 0;
    };

    /// Derived functors for endgame evaluation and scaling functions
    template<EndgameCode C, typename T = EndgameType<C>>
    class Endgame
        : public EndgameBase<T>
    {
    public:
        explicit Endgame(Color c)
            : EndgameBase<T>{c}
        {}
        virtual ~Endgame() = default;
        Endgame& operator=(Endgame const&) = delete;

        T operator()(Position const&) const override;
    };


    template<typename T> using EG_Ptr = std::unique_ptr<EndgameBase<T>>;
    template<typename T> using EG_Map = std::unordered_map<Key, EG_Ptr<T>>;
    template<typename T1, typename T2> using EG_MapPair = std::pair<EG_Map<T1>, EG_Map<T2>>;

    // Stores the pointers to endgame evaluation and scaling base objects in two std::map
    extern EG_MapPair<Value, Scale> EndgameMapPair;

    template<typename T>
    EG_Map<T>& map()
    {
        return std::get<std::is_same<T, Scale>::value>(EndgameMapPair);
    }

    template<typename T>
    const EndgameBase<T>* probe(Key matl_key)
    {
        auto egItr = map<T>().find(matl_key);
        return egItr != map<T>().end() ?
                egItr->second.get() :
                nullptr;
    }

    extern void initialize();
}
