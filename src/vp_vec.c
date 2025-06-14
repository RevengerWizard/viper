/*
** vp_vec.c
** Dynamic array handling
*/

#include <stdlib.h>
#include <string.h>

#include "vp_vec.h"
#include "vp_mem.h"

void* vp_vec_grow(const void* vec, size_t len, size_t size)
{
    vp_assertX(((size_t)vec_size(vec)) <= (SIZE_MAX - 1)/2, "vec size too large");
    size_t newsize = CLAMP_MIN(2*vec_size(vec), MAX(len, 16));
    vp_assertX(len <= newsize, "len exceeds new size");
    vp_assertX(newsize <= (SIZE_MAX - offsetof(VecHeader, data))/size, "allocation overflow");
    size_t memsize = offsetof(VecHeader, data) + newsize*size;
    VecHeader* hdr;
    if(vec)
    {
        hdr = vp_mem_realloc(vec_hdr(vec), memsize);
    }
    else
    {
        hdr = vp_mem_alloc(memsize);
        hdr->len = 0;
    }
    hdr->size = newsize;
    return hdr->data;
}

void vp_vec_insert(const void* vec, size_t idx, size_t elemsize)
{
    VecHeader* hdr = vec_hdr(vec);
    size_t len = hdr->len;

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

void vp_vec_remove_at(const void* vec, size_t idx, size_t elemsize)
{
    VecHeader* hdr = vec_hdr(vec);
    size_t len = hdr->len;
    
    /* Shift elements to the left to fill the gap */
    if (idx < len - 1) {
        char* data = (char*)vec;
        memmove(data + idx * elemsize,
                data + (idx + 1) * elemsize,
                (len - idx - 1) * elemsize);
    }
    hdr->len--;
}