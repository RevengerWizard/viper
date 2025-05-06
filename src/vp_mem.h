/*
** vp_mem.h
** Memory management
*/

#ifndef _VP_MEM_H
#define _VP_MEM_H

typedef struct Arena
{
    char* p;
    char* end;
    char** blocks;
} Arena;

/* Arena allocator */
void vp_arena_grow(Arena* arena, size_t minsize);
void* vp_arena_alloc(Arena* arena, size_t size);
void vp_arena_free(Arena* arena);

/* Memory allocation */
void* vp_mem_calloc(size_t num, size_t size);
void* vp_mem_realloc(void* p, size_t size);
void* vp_mem_alloc(size_t size);
void* vp_mem_dup(void* src, size_t size);

#endif