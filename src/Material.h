#ifndef _MATERIAL_H_INC_
#define _MATERIAL_H_INC_

#include "Type.h"
#include "Endgame.h"

#include "Position.h"

namespace Material {

    // Material::Entry contains various information about a material configuration.
    struct Entry
    {
    public:
        Key     key;
        Score   imbalance;
        Phase   phase;
        Scale   scale[CLR_NO];

        EndGame::EndgameBase<Value> *value_func;
        EndGame::EndgameBase<Scale> *scale_func[CLR_NO];
    };

    typedef HashTable<Entry, 0x2000> Table;

    extern Entry* probe (const Position &pos);
}

#endif // _MATERIAL_H_INC_
