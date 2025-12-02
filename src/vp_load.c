/*
** vp_load.c
** Frontend driver: parse + compile
*/

/* You are not MSVC, clang... */

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
#include "vp_low.h"
#include "vp_link.h"
#include "vp_dump.h"

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
        fprintf(stderr, "cannot open '%s'\n", filename);
        exit(EXIT_FAILURE);
    }
    LexState ls;
    ls.reader = reader;
    ls.rdata = &ctx;
    ls.name = filename;
    vp_buf_init(&ls.sb);
    vp_lex_setup(&ls);
    vec_t(Decl*) decls = vp_parse(V, &ls);
    decls = vp_sema(decls);
    vec_t(Code*) codes = vp_codegen(decls);
    V->codes = codes;
    vp_low(codes);
    vp_link();
    vp_layout_init(&V->L);
    //vp_dump_code(codes);
    fclose(ctx.fp);
}