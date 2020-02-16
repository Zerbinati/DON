#pragma once

#include <memory>
#include <unordered_map>

#include "Position.h"
#include "Type.h"

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
    KBPsK,   // KBPs vs K
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
    const Color stngColor
        ,       weakColor;

    explicit EndgameBase(Color c)
        : stngColor{ c}
        , weakColor{~c}
    {}
    virtual ~EndgameBase() = default;
    EndgameBase& operator=(const EndgameBase&) = delete;

    virtual T operator()(const Position&) const = 0;
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
    Endgame& operator=(const Endgame&) = delete;

    T operator()(const Position&) const override;
};


namespace Endgames
{
    template<typename T>    using EGPtr = std::unique_ptr<EndgameBase<T>>;
    template<typename T>    using EGMap = std::unordered_map<Key, EGPtr<T>>;
    template<typename T1
           , typename T2>   using EGMapPair = std::pair<EGMap<T1>, EGMap<T2>>;

    extern EGMapPair<Value, Scale> EndGames;

    template<typename T>
    EGMap<T>& mapEG()
    {
        return std::get<std::is_same<T, Scale>::value>(EndGames);
    }

    template<typename T>
    const EndgameBase<T>* probe(Key matlKey)
    {
        auto itr{mapEG<T>().find(matlKey)};
        return itr != mapEG<T>().end() ?
                itr->second.get() :
                nullptr;
    }

    extern void initialize();
}
