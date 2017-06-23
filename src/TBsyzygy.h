#ifndef _TB_SYZYGY_H_INC_
#define _TB_SYZYGY_H_INC_

#include "Type.h"
#include "Searcher.h"

namespace TBSyzygy {

    // WDL Score
    enum WDLScore
    {
        WDL_LOSS         = -2, // Loss
        WDL_BLESSED_LOSS = -1, // Loss, but draw under 50-move rule
        WDL_DRAW         =  0, // Draw
        WDL_CURSED_WIN   =  1, // Win, but draw under 50-move rule
        WDL_WIN          =  2, // Win
    };
    inline WDLScore operator-(WDLScore d) { return WDLScore(-i32(d)); }

    // Possible states after a probing operation
    enum ProbeState
    {
        PB_CHANGE_STM        = -1, // DTZ should check the other side
        PB_FAILURE           =  0, // Probe failure (missing file table)
        PB_SUCCESS           =  1, // Probe success
        PB_ZEROING_BEST_MOVE =  2  // Best move zeroes DTZ (capture or pawn move)
    };

    extern std::string  PathString;
    extern i32          MaxLimitPiece;

    extern i32      probe_dtz (Position &pos, ProbeState &state);
    extern WDLScore probe_wdl (Position &pos, ProbeState &state);

    extern bool root_probe_dtz (Position &root_pos, RootMoves &root_moves, Value &value);
    extern bool root_probe_wdl (Position &root_pos, RootMoves &root_moves, Value &value);

    extern void initialize ();

    template<typename CharT, typename Traits>
    inline std::basic_ostream<CharT, Traits>&
        operator<< (std::basic_ostream<CharT, Traits> &os, WDLScore wdl)
    {
        switch (wdl)
        {
        case WDL_LOSS:
            os << "Loss";
            break;
        case WDL_BLESSED_LOSS:
            os << "Blessed Loss";
            break;
        case WDL_DRAW:
            os << "Draw";
            break;
        case WDL_CURSED_WIN:
            os << "Cursed win";
            break;
        case WDL_WIN:
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
        case PB_CHANGE_STM:
            os << "Probed opponent side";
            break;
        case PB_FAILURE:
            os << "Failure";
            break;
        case PB_SUCCESS:
            os << "Success";
            break;
        case PB_ZEROING_BEST_MOVE:
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
