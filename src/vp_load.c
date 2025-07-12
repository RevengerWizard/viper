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
#include "vp_ast.h"
#include "vp_sema.h"
#include "vp_codegen.h"
#include "vp_sel.h"

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
    V->txtfile = fopen(filename, "r");
    if(!ctx.fp || !V->txtfile)
    {
        fprintf(stderr, "Cannot open %s\n", filename);
        exit(EXIT_FAILURE);
    }
    LexState ls;
    ls.reader = reader;
    ls.rdata = &ctx;
    ls.name = filename;
    vp_buf_init(&ls.sb);
    vp_lex_setup(&ls);
    Decl** decls = vp_parse(V, &ls);
    decls = vp_sema(decls);
    Code** codes = vp_codegen(decls);
    vp_sel(codes);
    fclose(ctx.fp);
}