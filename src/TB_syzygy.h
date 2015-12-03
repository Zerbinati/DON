#ifndef _TB_SYZYGY_H_INC_
#define _TB_SYZYGY_H_INC_

#include "Type.h"
#include "Searcher.h"

namespace Tablebases {

    extern i32 MaxCardinality;

    void initialize (const std::string& path);

    i32 probe_wdl (Position &pos, i32 *success);
    i32 probe_dtz (Position &pos, i32 *success);

    bool root_probe (Position &pos, Searcher::RootMoveVector &rootMoves, Value &value);
    bool root_probe_wdl (Position &pos, Searcher::RootMoveVector &rootMoves, Value &value);

}

#endif // _TB_SYZYGY_H_INC_
