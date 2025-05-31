/*
** vp_vec.h
** Dynamic array handling
*/

#ifndef _VP_VEC_H
#define _VP_VEC_H

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

void* vp_vec_grow(const void* vec, size_t len, size_t size);

#endif