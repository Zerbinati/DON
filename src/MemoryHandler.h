#if defined(LPAGES)

#ifndef _MEMORY_HANDLER_H_INC_
#define _MEMORY_HANDLER_H_INC_

#   include "Type.h"

namespace Memory {

    extern bool LargePages;

    extern void alloc_memory (void *&mem_ref, u64 mem_size, u32 alignment);

    extern void  free_memory (void *mem);

    extern void initialize   ();
    extern void deinitialize ();

}

#endif // _MEMORY_HANDLER_H_INC_

#endif // LPAGES
