/*
** vp_buf.c
** Buffer handling
*/

#include <stdlib.h>

#include "vp_buf.h"
#include "vp_mem.h"
#include "vp_state.h"

static void buf_grow(SBuf* sb, size_t size)
{
    size_t oldsize = sbuf_size(sb), len = sbuf_len(sb), newsize = oldsize;

    if(newsize < VP_MIN_SBUF) newsize = VP_MIN_SBUF;
    while(newsize < size) newsize += newsize;

    char* b = (char*)vp_mem_realloc(sb->b, newsize);

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

SBuf* vp_buf_putmem(SBuf* sb, const void* q, size_t len)
{
    char* w = vp_buf_more(sb, len);
    w = vp_buf_wmem(w, q, len);
    sb->w = w;
    return sb;
}

SBuf* vp_buf_putstr(SBuf* sb, Str* s)
{
    size_t len = s->len;
    char* w = vp_buf_more(sb, len);
    w = vp_buf_wmem(w, str_data(s), len);
    sb->w = w;
    return sb;
}

static char* buf_tmp(uint32_t size)
{
    SBuf* sb = &V->tmpbuf;
    return vp_buf_need(sb, size);
}

Str* vp_buf_cat2str(Str* s1, Str* s2)
{
    uint32_t len1 = s1->len, len2 = s2->len;
    char* buf = buf_tmp(len1 + len2);
    memcpy(buf, str_data(s1), len1);
    memcpy(buf + len1, str_data(s2), len2);
    return vp_str_new(buf, len1 + len2);
}

void vp_buf_fmt(SBuf* sb, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    int n = vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    if(n <= 0) return;

    /* Reserve space directly in the buffer and write into it */
    char* w = vp_buf_more(sb, (size_t)n + 1);
    va_start(args, fmt);
    vsnprintf(w, (size_t)n + 1, fmt, args);
    va_end(args);

    sb->w = w + n;
}