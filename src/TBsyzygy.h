#ifndef _TB_SYZYGY_H_INC_
#define _TB_SYZYGY_H_INC_

#include "Type.h"
#include "Searcher.h"

namespace TBSyzygy {

    // WDL Score
    enum WDLScore
    {
        WDLLoss       = -2, // Loss
        WDLCursedLoss = -1, // Loss, but draw under 50-move rule
        WDLDraw       =  0, // Draw
        WDLCursedWin  =  1, // Win, but draw under 50-move rule
        WDLWin        =  2, // Win

        WDLScoreNone  = -1000
    };
    inline WDLScore operator-(WDLScore d) { return WDLScore(-i32(d)); }

    // Possible states after a probing operation
    enum ProbeState
    {
        FAIL              =  0, // Probe failed (missing file table)
        OK                =  1, // Probe succesful
        CHANGE_STM        = -1, // DTZ should check the other side
        ZEROING_BEST_MOVE =  2  // Best move zeroes DTZ (capture or pawn move)
    };

    extern std::string  PathString;
    extern i32          MaxLimitPiece;

    extern i32      probe_dtz (Position &pos, ProbeState &state);
    extern WDLScore probe_wdl (Position &pos, ProbeState &state);

    extern bool root_probe_dtz (Position &root_pos, RootMoveVector &root_moves, Value &value);
    extern bool root_probe_wdl (Position &root_pos, RootMoveVector &root_moves, Value &value);

    extern void initialize ();

    inline std::ostream& operator<< (std::ostream &os, const WDLScore wdl)
    {
        os << (wdl == WDLLoss       ? "Loss" :
               wdl == WDLCursedLoss ? "Cursed loss" :
               wdl == WDLDraw       ? "Draw" :
               wdl == WDLCursedWin  ? "Cursed win" :
               wdl == WDLWin        ? "Win" : "None");
        return os;
    }

    inline std::ostream& operator<< (std::ostream &os, const ProbeState ps)
    {
        os << (ps == FAIL              ? "Failed" :
               ps == OK                ? "Success" :
               ps == CHANGE_STM        ? "Probed opponent side" :
               ps == ZEROING_BEST_MOVE ? "Best move zeroes DTZ" : "None");
        return os;
    }
}

#endif // _TB_SYZYGY_H_INC_
