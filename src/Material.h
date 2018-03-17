#ifndef _MATERIAL_H_INC_
#define _MATERIAL_H_INC_

#include "Endgame.h"
#include "Position.h"
#include "Type.h"

namespace Material {

    const i32 PhaseResolution = 128;

    /// Material::Entry contains various information about a material configuration.
    struct Entry
    {
    public:
        Key   key;
        i32   phase;
        Score imbalance;
        Scale scale[+Color::NO];

        EndGame::EndgameBase<Value> *value_func;
        EndGame::EndgameBase<Scale> *scale_func[+Color::NO];
    };

    typedef HashTable<Entry, 0x2000> Table;

    extern Entry* probe (const Position&);
}

#endif // _MATERIAL_H_INC_
