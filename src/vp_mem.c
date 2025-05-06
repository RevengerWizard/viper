/*
** vp_mem.c
** Memory management
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vp_mem.h"
#include "vp_vec.h"

void* vp_mem_calloc(size_t num, size_t size)
{
    void* p = calloc(num, size);
    if(VP_UNLIKELY(!p))
    {
        fputs("calloc failed", stderr);
        exit(EXIT_FAILURE);
    }
    return p;
}

void* vp_mem_realloc(void* p, size_t size)
{
    p = realloc(p, size);
    if(VP_UNLIKELY(!p))
    {
        fputs("realloc failed", stderr);
        exit(EXIT_FAILURE);
    }
    return p;
}

void* vp_mem_alloc(size_t size)
{
    void* p = malloc(size);
    if(VP_UNLIKELY(!p))
    {
        fputs("malloc failed", stderr);
        exit(EXIT_FAILURE);
    }
    return p;
}

void* vp_mem_dup(void* src, size_t size)
{
    void* dest = vp_mem_alloc(size);
    memcpy(dest, src, size);
    return dest;
}

#define ARENA_ALIGNMENT 8
#define ARENA_BLOCK_SIZE (1024 * 1024)

void vp_arena_grow(Arena* arena, size_t minsize)
{
    size_t size = ALIGN_UP(CLAMP_MIN(minsize, ARENA_BLOCK_SIZE), ARENA_ALIGNMENT);
    arena->p = vp_mem_alloc(size);
    vp_assertX(arena->p == ALIGN_DOWN_PTR(arena->p, ARENA_ALIGNMENT), "unaligned arena pointer after grow");
    arena->end = arena->p + size;
    vec_push(arena->blocks, arena->p);
}

void* vp_arena_alloc(Arena* arena, size_t size)
{
    if(size > (size_t)(arena->end - arena->p))
    {
        vp_arena_grow(arena, size);
        vp_assertX(size <= (size_t)(arena->end - arena->p), "allocation exceeds grown block");
    }
    void* p = arena->p;
    arena->p = ALIGN_UP_PTR(arena->p + size, ARENA_ALIGNMENT);
    vp_assertX(arena->p <= arena->end, "arena pointer overflow");
    vp_assertX(p == ALIGN_DOWN_PTR(p, ARENA_ALIGNMENT), "unaligned allocated pointer");
    return p;
}

void vp_arena_free(Arena* arena)
{
    for(char** b = arena->blocks; b != vec_end(arena->blocks); b++)
    {
        free(*b);
    }
    vec_free(arena->blocks);
}