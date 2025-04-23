/*
** vp_load.c
** File loader
*/

/* I HATE YOU MSVC, I HATE YOU */
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdio.h>
#include <stdlib.h>

#include "vp_lex.h"
#include "vp_parse.h"
#include "vp_state.h"

typedef struct FileReaderCtx
{
    FILE* fp;
    char buf[VP_BUFFER_SIZE];
} FileReaderCtx;

static const char* reader(void* ud, size_t* size)
{
    FileReaderCtx* ctx = (FileReaderCtx*)ud;
    if(feof(ctx->fp))
        return NULL;
    *size = fread(ctx->buf, 1, sizeof(ctx->buf), ctx->fp);
    return (*size > 0) ? ctx->buf : NULL;
}

void vp_load(VpState* V, const char* filename)
{
    FileReaderCtx ctx;
    ctx.fp = fopen(filename, "rb");
    if(ctx.fp == NULL)
    {
        fprintf(stderr, "Cannot open %s", filename);
        exit(EXIT_FAILURE);
    }
    LexState ls;
    ls.reader = reader;
    ls.rdata = &ctx;
    vp_buf_init(&ls.sb);
    vp_lex_setup(&ls);
    vp_parse(V, &ls);
    fclose(ctx.fp);    
}