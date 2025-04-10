/*
** vp_buf.c
** Buffer handling
*/

#include <stdlib.h>

#include "vp_buf.h"

static void buf_grow(SBuf* sb, size_t size)
{
    size_t oldsize = sbuf_size(sb), len = sbuf_len(sb), newsize = oldsize;

    if(newsize < VP_MIN_SBUF) newsize = VP_MIN_SBUF;
    while(newsize < size) newsize += newsize;

    char* b = (char*)realloc(sb->b, newsize);

    /* Adjust buffer pointers */
    sb->b = b;
    sb->w = b + len;
    sb->e = b + newsize;
}

char* vp_buf_need2(SBuf* sb, size_t size)
{
    size_t len = sbuf_len(sb);
    buf_grow(sb, len + size);
    return sb->b;
}

char* vp_buf_more2(SBuf* sb, size_t size)
{
    size_t len = sbuf_len(sb);
    buf_grow(sb, len + size);
    return sb->w;
}