#ifndef _TB_SYZYGY_H_INC_
#define _TB_SYZYGY_H_INC_

#include "Type.h"
#include "Searcher.h"

namespace TBSyzygy {

    /// WDL Score
    enum WDLScore
    {
        LOSS         = -2, // Loss
        BLESSED_LOSS = -1, // Loss, but draw under 50-move rule
        DRAW         =  0, // Draw
        CURSED_WIN   = +1, // Win, but draw under 50-move rule
        WIN          = +2, // Win
    };
    inline WDLScore operator-(WDLScore d) { return WDLScore(-i32(d)); }

    /// Possible states after a probing operation
    enum ProbeState
    {
        CHANGE_STM        = -1, // DTZ should check the other side
        FAILURE           =  0, // Probe failure (missing file table)
        SUCCESS           = +1, // Probe success
        ZEROING_BEST_MOVE = +2  // Best move zeroes DTZ (capture or pawn move)
    };

    extern std::string  PathString;
    extern i32          MaxLimitPiece;

    extern i32      probe_dtz (Position&, ProbeState&);
    extern WDLScore probe_wdl (Position&, ProbeState&);

    extern bool root_probe_dtz (Position&, RootMoves&, Value&);
    extern bool root_probe_wdl (Position&, RootMoves&, Value&);

    extern void initialize ();

    template<typename CharT, typename Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, WDLScore wdl)
    {
        switch (wdl)
        {
        case WDLScore::LOSS:
            os << "Loss";
            break;
        case WDLScore::BLESSED_LOSS:
            os << "Blessed Loss";
            break;
        case WDLScore::DRAW:
            os << "Draw";
            break;
        case WDLScore::CURSED_WIN:
            os << "Cursed win";
            break;
        case WDLScore::WIN:
            os << "Win";
            break;
        default:
            os << "None";
            break;
        }
        return os;
    }

    template<typename CharT, typename Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, ProbeState ps)
    {
        switch (ps)
        {
        case ProbeState::CHANGE_STM:
            os << "Probed opponent side";
            break;
        case ProbeState::FAILURE:
            os << "Failure";
            break;
        case ProbeState::SUCCESS:
            os << "Success";
            break;
        case ProbeState::ZEROING_BEST_MOVE:
            os << "Best move zeroes DTZ";
            break;
        default:
            os << "None";
            break;
        }
        return os;
    }
}

#endif // _TB_SYZYGY_H_INC_
