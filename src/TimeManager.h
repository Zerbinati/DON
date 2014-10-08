#ifndef _TIME_MANAGER_H_INC_
#define _TIME_MANAGER_H_INC_

#include "Type.h"
#include "UCI.h"

namespace Time {

    // TimeManager class computes the optimal time to think depending on the
    // maximum available time, the move game number and other parameters.
    // Support four different kind of time controls:
    //
    // moves_to_go = 0, increment = 0 means: x basetime  [sudden death!]
    // moves_to_go = 0, increment > 0 means: x basetime + z increment
    // moves_to_go > 0, increment = 0 means: x moves in y minutes
    // moves_to_go > 0, increment > 0 means: x moves in y minutes + z increment
    class TimeManager
    {
    private:

        u32   _optimum_time;
        u32   _maximum_time;

        float _instability_factor;

    public:

        inline u32 available_time () const { return u32(_optimum_time * _instability_factor * 0.71f); }
    
        inline u32 maximum_time   () const { return _maximum_time; }

        inline void instability (float best_move_change) { _instability_factor = 1.0f + best_move_change; }

        void initialize (const GameClock &gameclock, u08 movestogo, i32 game_ply);
    
    };

    extern void configure (const UCI::Option &);

}

#endif // _TIME_MANAGER_H_INC_
