/*
** vp_buf.h
** Buffer handling
*/

#ifndef _VP_BUF_H
#define _VP_BUF_H

#include <string.h>

#include "vp_def.h"

typedef struct SBuf
{
    char* w, *e, *b;
} SBuf;

#define sbuf_size(sb)  ((size_t)((sb)->e - (sb)->b))
#define sbuf_len(sb)   ((size_t)((sb)->w - (sb)->b))
#define sbuf_left(sb)   ((size_t)((sb)->e - (sb)->w))

char* vp_buf_need2(SBuf* sb, size_t size);
char* vp_buf_more2(SBuf* sb, size_t size);

static VP_AINLINE void vp_buf_init(SBuf* sb)
{
    sb->w = sb->e = sb->b = NULL;
}

static VP_AINLINE void vp_buf_reset(SBuf* sb)
{
    sb->w = sb->b;
}

static VP_AINLINE char* vp_buf_need(SBuf* sb, size_t size)
{
    if(size > sbuf_left(sb))
        return vp_buf_need2(sb, size);
    return sb->b;
}

static VP_AINLINE char* vp_buf_more(SBuf* sb, size_t size)
{
    if(size > sbuf_left(sb))
        return vp_buf_more2(sb, size);
    return sb->w;
}

static VP_AINLINE char* vp_buf_wmem(char* p, const void* q, size_t len)
{
    return (char*)memcpy(p, q, len) + len;
}

static VP_AINLINE void vp_buf_putb(SBuf* sb, int c)
{
    char* w = vp_buf_more(sb, 1);
    *w++ = (char)c;
    sb->w = w;
}

#endif