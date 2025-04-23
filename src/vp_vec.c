/*
** vp_vec.c
** Dynamic array handling
*/

#include <stdlib.h>
#include <string.h>

#include "vp_vec.h"

int vec_expand_(char** data, uint32_t* len, uint32_t* size, int memsz)
{
    if(*len + 1 > *size)
    {
        void* p;
        uint32_t n = (*size == 0) ? 1 : *size << 1;
        p = realloc(*data, n * memsz);
        if(p == NULL)
            return -1;
        *data = p;
        *size = n;
    }
    return 0;
}

int vec_reserve_(char** data, uint32_t* len, uint32_t* size, int memsz, int n)
{
    (void)len;
    if(n > *size)
    {
        void* ptr = realloc(*data, n * memsz);
        if(ptr == NULL)
            return -1;
        *data = ptr;
        *size = n;
    }
    return 0;
}

int vec_reserve_po2_(char** data, uint32_t* len, uint32_t* size, int memsz,
                     int n)
{
    int n2 = 1;
    if(n == 0)
        return 0;
    while(n2 < n)
        n2 <<= 1;
    return vec_reserve_(data, len, size, memsz, n2);
}

int vec_compact_(char** data, uint32_t* len, uint32_t* size, int memsz)
{
    if(*len == 0)
    {
        free(*data);
        *data = NULL;
        *size = 0;
        return 0;
    }
    else
    {
        void* p;
        uint32_t n = *len;
        p = realloc(*data, n * memsz);
        if(p == NULL)
            return -1;
        *size = n;
        *data = p;
    }
    return 0;
}

int vec_insert_(char** data, uint32_t* len, uint32_t* size, int memsz, int idx)
{
    int err = vec_expand_(data, len, size, memsz);
    if(err)
        return err;
    memmove(*data + (idx + 1) * memsz, *data + idx * memsz,
            (*len - idx) * memsz);
    return 0;
}

void vec_splice_(char** data, uint32_t* len, uint32_t* size, int memsz, int start,
                 int count)
{
    (void)size;
    memmove(*data + start * memsz, *data + (start + count) * memsz,
          (*len - start - count) * memsz);
}

void vec_swapsplice_(char** data, uint32_t* len, uint32_t* size, int memsz,
                     int start, int count)
{
  (void)size;
    memmove(*data + start * memsz, *data + (*len - count) * memsz,
            count * memsz);
}

void vec_swap_(char** data, uint32_t* len, uint32_t* size, int memsz, int idx1,
               int idx2)
{
    unsigned char* a, *b, tmp;
    uint32_t count;
    (void)len;
    (void)size;
    if(idx1 == idx2)
        return;
    a = (unsigned char*)*data + idx1 * memsz;
    b = (unsigned char*)*data + idx2 * memsz;
    count = memsz;
    while(count--)
    {
        tmp = *a;
        *a = *b;
        *b = tmp;
        a++, b++;
    }
}