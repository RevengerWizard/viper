/*
** vp_vec.h
** Dynamic array handling
*/

#ifndef _VP_VEC_H
#define _VP_VEC_H

#include <stdlib.h>

#include "vp_def.h"

typedef struct VecHeader
{
    uint32_t len;
    uint32_t size;
    char data[];
} VecHeader;

#define vec_t(type) type*
#define vec_hdr(v) ((VecHeader*)((char*)(v) - offsetof(VecHeader, data)))

#define vec_len(v) ((v) ? vec_hdr(v)->len : 0)
#define vec_size(v) ((v) ? vec_hdr(v)->size : 0)
#define vec_end(v) ((v) + vec_len(v))
#define vec_sizeof(v) ((v) ? vec_len(v)*sizeof(*(v)) : 0)

#define vec_free(v) ((v) ? (free(vec_hdr(v)), (v) = NULL) : 0)
#define vec_fit(v, n) \
    ((n) <= vec_size(v) ? 0 : ((v) = vp_vec_grow((v), (n), sizeof(*(v)))))
#define vec_push(v, ...) \
    (vec_fit((v), 1 + vec_len(v)), (v)[vec_hdr(v)->len++] = (__VA_ARGS__))
#define vec_clear(v) ((v) ? vec_hdr(v)->len = 0 : 0)

#define vec_insert(v, idx, ...) \
    (vp_assertX((idx) <= vec_len(v), "insert index out of bounds"), \
        vec_fit((v), 1 + vec_len(v)), \
        vp_vec_insert((v), (idx), sizeof(*(v))), \
        (v)[idx] = (__VA_ARGS__))

#define vec_remove_at(v, idx) \
    (vp_assertX((v) && (idx) < vec_len(v), "remove index out of bounds"), \
     vp_vec_remove_at((v), (idx), sizeof(*(v))))

#define vec_contains(v, elem) \
    vp_vec_contains((v), &(elem), sizeof(*(v)))

#define vec_concat(dst, src) \
    (vp_vec_concat((void**)&(dst), (src), sizeof(*(dst))))

#define vec_pop(v) \
    (vp_assertX((v) && vec_len(v) > 0, "pop from empty vector"), \
     vec_hdr(v)->len--, (v)[vec_hdr(v)->len])

void* vp_vec_grow(const void* vec, size_t len, size_t size);
void vp_vec_insert(const void* vec, size_t idx, size_t elemsize);
void vp_vec_remove_at(const void* vec, size_t idx, size_t elemsize);
bool vp_vec_contains(const void* vec, const void* elem, size_t elemsize);
void vp_vec_concat(void** dstp, const void* src, size_t elemsize);

#endif