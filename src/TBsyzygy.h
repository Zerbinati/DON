#ifndef _TB_SYZYGY_H_INC_
#define _TB_SYZYGY_H_INC_

#include "Type.h"
#include "Searcher.h"

namespace TBSyzygy {

    extern std::string  PathString;
    extern i32          MaxPieceLimit;
    extern Value        ProbeValue;

    Value probe_dtz (Position &pos, i32 &success);
    Value probe_wdl (Position &pos, i32 &success);

    bool root_probe_dtz (Position &root_pos, Searcher::RootMoveVector &root_moves);
    bool root_probe_wdl (Position &root_pos, Searcher::RootMoveVector &root_moves);

    void initialize ();
}

#endif // _TB_SYZYGY_H_INC_
