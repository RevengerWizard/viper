/*
** vp_vec.h
** Dynamic array handling
*/

#ifndef _VP_VEC_H
#define _VP_VEC_H

#include "vp_def.h"

#define vec_unpack_(v) \
    (char**)&(v)->data, &(v)->len, &(v)->size, sizeof(*(v)->data)

#define vec_t(T) \
    struct \
    { \
        T* data; \
        uint32_t len, size; \
    }

#define vec_init(v) memset((v), 0, sizeof(*(v)))

#define vec_deinit(v) (free((v)->data), vec_init(v))

#define vec_push(v, val) \
    (vec_expand_(vec_unpack_(v)) ? -1 : ((v)->data[(v)->len++] = (val), 0), 0)

#define vec_pop(v) (v)->data[--(v)->len]

#define vec_splice(v, start, count) \
    (vec_splice_(vec_unpack_(v), start, count), (v)->len -= (count))

#define vec_swapsplice(v, start, count) \
    (vec_swapsplice_(vec_unpack_(v), start, count), (v)->len -= (count))

#define vec_insert(v, idx, val) \
    (vec_insert_(vec_unpack_(v), idx) ? -1 : ((v)->data[idx] = (val), 0), \
    (v)->len++, 0)

#define vec_swap(v, idx1, idx2) vec_swap_(vec_unpack_(v), idx1, idx2)

#define vec_truncate(v, l) \
  ((v)->len = (len) < (v)->len ? (l) : (v)->len)

#define vec_clear(v) ((v)->len = 0)

#define vec_first(v) (v)->data[0]

#define vec_last(v) (v)->data[(v)->len - 1]

#define vec_reserve(v, n) vec_reserve_(vec_unpack_(v), n)

#define vec_compact(v) vec_compact_(vec_unpack_(v))

#define vec_pusharr(v, arr, count) \
    do { \
        int i__, n__ = (count); \
        if(vec_reserve_po2_(vec_unpack_(v), (v)->len + n__) != 0) \
            break; \
        for (i__ = 0; i__ < n__; i__++) { \
        (v)->data[(v)->len++] = (arr)[i__]; \
        } \
    } while(0)

#define vec_extend(v, v2) vec_pusharr((v), (v2)->data, (v2)->len)

#define vec_find(v, val, idx) \
    do { \
        for((idx) = 0; (idx) < (v)->len; (idx)++) { \
            if((v)->data[(idx)] == (val)) \
                break; \
        } \
        if((idx) == (v)->len) \
            (idx) = -1; \
    } while (0)

#define vec_remove(v, val) \
    do { \
        int idx__; \
        vec_find(v, val, idx__); \
        if (idx__ != -1) \
            vec_splice(v, idx__, 1); \
    } while(0)

#define vec_reverse(v) \
    do { \
        int i__ = (v)->len / 2; \
        while(i__--) { \
            vec_swap((v), i__, (v)->len - (i__ + 1)); \
        } \
    } while(0)

int vec_expand_(char** data, uint32_t* len, uint32_t* size, int memsz);
int vec_reserve_(char** data, uint32_t* len, uint32_t* size, int memsz, int n);
int vec_reserve_po2_(char** data, uint32_t* len, uint32_t* size, int memsz, int n);
int vec_compact_(char** data, uint32_t* len, uint32_t* size, int memsz);
int vec_insert_(char** data, uint32_t* len, uint32_t* size, int memsz, int idx);
void vec_splice_(char** data, uint32_t* len, uint32_t* size, int memsz, int start,
                int count);
void vec_swapsplice_(char** data, uint32_t* len, uint32_t* size, int memsz,
                     int start, int count);
void vec_swap_(char** data, uint32_t* len, uint32_t* size, int memsz, int idx1,
               int idx2);

#endif