#ifndef _ENDGAME_H_INC_
#define _ENDGAME_H_INC_

#include <map>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>

#include "Type.h"

class Position;

namespace EndGame {

    // Endgame Type lists all supported endgames
    enum EndgameT
    {
        // Evaluation functions
        KXK,   // Generic "mate lone king" eval
        KPK,   // KP vs K
        KBNK,  // KBN vs K
        KNNK,  // KNN vs K
        KRKP,  // KR vs KP
        KRKB,  // KR vs KB
        KRKN,  // KR vs KN
        KQKP,  // KQ vs KP
        KQKR,  // KQ vs KR
        KBBKN, // KBB vs KN

        // Scaling functions
        SCALE_FUNS,

        // Generic Scaling functions
        KBPsKs,  // KBPs vs K+s
        KQKRPs,  // KQ vs KR+Ps

        KRPKR,   // KRP vs KR
        KRPKB,   // KRP vs KB
        KRPPKRP, // KRPP vs KRP
        KPsK,    // KPs vs Ks
        KPKP,    // KP vs KP
        KNPK,    // KNP vs K
        KBPKB,   // KBP vs KB
        KBPPKB,  // KBPP vs KB
        KBPKN,   // KBP vs KN
        KNPKB    // KNP vs KB

    };

    // Endgame functions can be of two types according if return a Value or a ScaleFactor.
    // Type eg_fun<i32>::type equals to either ScaleFactor or Value depending if the template parameter is 0 or 1.
    template<EndgameT ET>
    using eg_fun = std::conditional<(ET < SCALE_FUNS), Value, ScaleFactor>;

    // Base and derived templates for endgame evaluation and scaling functions
    template<typename T>
    class EndgameBase
    {
    public:

        virtual ~EndgameBase () {}

        virtual Color color () const = 0;

        virtual T operator() (const Position &pos) const = 0;

    };

    template<EndgameT ET, typename T = typename eg_fun<ET>::type>
    class Endgame
        : public EndgameBase<T>
    {

    private:
        const Color _stong_side
                  , _weak_side;

    public:

        explicit Endgame (Color c)
            : _stong_side (c)
            , _weak_side (~c)
        {}

        inline Color color () const { return _stong_side; }

        T operator() (const Position &pos) const;
    };

    // Endgames class stores in two std::map the pointers to endgame evaluation
    // and scaling base objects. Then use polymorphism to invoke the actual
    // endgame function calling its operator() that is virtual.
    class Endgames
    {

    private:

        template<typename T> using Map = std::map<Key, std::unique_ptr<T> >;

        std::pair<Map<EndgameBase<Value> >, Map<EndgameBase<ScaleFactor> > > maps;
        
        template<EndgameT ET, typename T = EndgameBase<typename eg_fun<ET>::type> >
        void add (const std::string &code);

        template<typename T, i32 I = std::is_same< T, EndgameBase<ScaleFactor> >::value>
        inline Map<T>& map ()
        {
            return std::get<I> (maps);
        }

    public:

        Endgames ();

        template<typename T>
        inline T* probe (Key key, T *&eg)
        {
            return eg = (map<T> ().count (key) ? map<T> ()[key].get () : nullptr);
        }
    };

    extern void   initialize ();
    extern void deinitialize ();

}

extern EndGame::Endgames *EndGames; // Global Endgames

#endif // _ENDGAME_H_INC_
