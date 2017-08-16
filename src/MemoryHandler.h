#if defined(LPAGES)

#ifndef _MEMORY_HANDLER_H_INC_
#define _MEMORY_HANDLER_H_INC_

#include "Type.h"

namespace Memory {

    extern bool LargePages;

    extern void alloc_memory (void*&, size_t, u32);

    extern void free_memory (void*);

    extern void initialize ();
    extern void deinitialize ();

}

#endif // _MEMORY_HANDLER_H_INC_

#endif // LPAGES
