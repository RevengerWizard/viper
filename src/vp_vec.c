/*
** vp_vec.c
** Dynamic array handling
*/

#include <stdlib.h>
#include <string.h>

#include "vp_vec.h"
#include "vp_mem.h"

void* vp_vec_init(uint32_t elemsize)
{
    size_t size = 16;
    vp_assertX(size <= (UINT32_MAX - offsetof(VecHeader, data)) / elemsize, "allocation overflow");
    size_t memsize = offsetof(VecHeader, data) + size * elemsize;
    VecHeader* hdr = vp_mem_alloc(memsize);
    hdr->len = 0;
    hdr->size = size;
    return hdr->data;
}

void* vp_vec_grow(const void* vec, uint32_t len, uint32_t elemsize)
{
    vp_assertX(((uint64_t)vec_size(vec)) <= (UINT32_MAX - 1) / 2, "vec size too large");
    size_t newsize = CLAMP_MIN(2 * vec_size(vec), MAX(len, 16));
    vp_assertX(len <= newsize, "len exceeds new size");
    vp_assertX(newsize <= (UINT32_MAX - offsetof(VecHeader, data)) / elemsize, "allocation overflow");
    size_t memsize = offsetof(VecHeader, data) + (size_t)newsize * elemsize;
    VecHeader* hdr = vp_mem_realloc(vec_hdr(vec), memsize);
    hdr->size = newsize;
    return hdr->data;
}

void vp_vec_insert(const void* vec, uint32_t idx, uint32_t elemsize)
{
    VecHeader* hdr = vec_hdr(vec);
    uint32_t len = hdr->len;

    /* Shift elements to the right to make space */
    if(idx < len)
    {
        char* data = (char*)vec;
        memmove(data + (idx + 1) * elemsize,
                data + idx * elemsize,
                (len - idx) * elemsize);
    }
    hdr->len++;
}

void vp_vec_remove_at(const void* vec, uint32_t idx, uint32_t elemsize)
{
    VecHeader* hdr = vec_hdr(vec);
    uint32_t len = hdr->len;

    /* Shift elements to the left to fill the gap */
    if(idx < len - 1)
    {
        char* data = (char*)vec;
        memmove(data + idx * elemsize,
                data + (idx + 1) * elemsize,
                (len - idx - 1) * elemsize);
    }
    hdr->len--;
}

bool vp_vec_contains(const void* vec, const void* elem, uint32_t elemsize)
{
    uint32_t len = vec_len(vec);
    const char* data = (const char*)vec;

    for(uint32_t i = 0; i < len; i++)
    {
        if(memcmp(data + i * elemsize, elem, elemsize) == 0)
            return true;
    }
    return false;
}

void vp_vec_find(const void* vec, void* elem, uint32_t elemsize)
{
    size_t len = vec_len(vec);
    const char* data = (const char*)vec;

    for(size_t i = 0; i < len; i++)
    {
        if(memcmp(data + i * elemsize, elem, elemsize) == 0)
        {
            memcpy(elem, data + i * elemsize, elemsize);
            break;
        }
    }
}

void vp_vec_concat(void** dst_ptr, const void* src, uint32_t elemsize)
{
    void* dst = *dst_ptr;
    uint32_t srclen = vec_len(src);
    uint32_t dstlen = vec_len(dst);
    uint32_t newlen = dstlen + srclen;

    /* Ensure dst has enough capacity */
    if(newlen > vec_size(dst))
    {
        dst = vp_vec_grow(dst, newlen, elemsize);
        *dst_ptr = dst;
    }

    memcpy((char*)dst + dstlen * elemsize, src, srclen * elemsize);
    vec_hdr(dst)->len = newlen;
}