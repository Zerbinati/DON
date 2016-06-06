#ifndef _ENDGAME_H_INC_
#define _ENDGAME_H_INC_

#include <map>
#include <memory>
#include <type_traits>
#include <utility>

#include "Type.h"

#include "Position.h"

namespace EndGame {

    // Endgame Type lists all supported endgames
    enum EndgameType : u08
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

    // Endgame functions can be of two category depending on whether they return Value or ScaleFactor.
    template<EndgameType ET>
    using EndgameCategory = typename std::conditional<ET < SCALE_FUNS, Value, ScaleFactor>::type;

    // Base and derived templates for endgame evaluation and scaling functions
    template<class T>
    class EndgameBase
    {
    protected:
        
        Color _strong_side;
    public:

        explicit EndgameBase (Color c)
            : _strong_side (c)
        {}
        //EndgameBase (const EndgameBase&) = delete;
        EndgameBase& operator= (const EndgameBase&) = delete;
        virtual ~EndgameBase () = default;

        Color strong_side () const { return _strong_side; }

        virtual T operator() (const Position &pos) const = 0;
    };

    template<EndgameType ET, class T = EndgameCategory<ET>>
    class Endgame
        : public EndgameBase<T>
    {

    public:

        explicit Endgame (Color c)
            : EndgameBase<T> (c)
        {}
        //Endgame (const Endgame&) = delete;
        Endgame& operator= (const Endgame&) = delete;
        //virtual ~Endgame () = default;

        T operator() (const Position &pos) const override;
    };

    // The Endgames class stores the pointers to endgame evaluation and scaling base objects in two std::map. 
    // Uses polymorphism to invoke the actual endgame function by calling its virtual operator().
    class Endgames
    {

    private:
        template<class T>
        using Map = std::map<Key, std::unique_ptr<EndgameBase<T>>>;

        std::pair<Map<Value>, Map<ScaleFactor>> maps;

        template<class T>
        Map<T>& map ()
        {
            return std::get<std::is_same<T, ScaleFactor>::value> (maps);
        }

        template<EndgameType ET, class T = EndgameCategory<ET>>
        void add (const std::string &code);

    public:

        Endgames ();
        Endgames (const Endgames&) = delete;
        Endgames& operator= (const Endgames&) = delete;

        template<class T>
        EndgameBase<T>* probe (Key matl_key)
        {
            return map<T> ().find (matl_key) != map<T> ().end () ? map<T> ()[matl_key].get () : nullptr;
        }
    };

    extern void initialize ();

    extern void deinitialize ();

}

// Global Endgames
extern EndGame::Endgames *EndGames;

#endif // _ENDGAME_H_INC_
