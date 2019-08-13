#ifndef _MEMORY_HANDLER_H_INC_
#define _MEMORY_HANDLER_H_INC_

#if defined(LPAGES)

#include "Type.h"

namespace Memory {

    extern void alloc_memory(void*&, size_t, u32);

    extern void free_memory(void*);

    extern void initialize();
    extern void deinitialize();

}

#endif // LPAGES

#endif // _MEMORY_HANDLER_H_INC_
