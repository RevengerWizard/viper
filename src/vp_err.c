/*
** vp_err.c
** Error and warning handling
*/

#include <stdio.h>
#include <stdlib.h>

#include "vp_err.h"
#include "vp_mod.h"

/* Show line of error with an offset indicator */
static void err_line(LexLine line, LexOffset ofs)
{
    int c;
    uint32_t fline = 1;
    vp_buf_reset(&V->tmpbuf);
    vp_assertX(V->mod, "no module");
    /* Not really efficient, but the file handle is already opened */
    FILE* ftxt = fopen(str_data(V->mod->path), "r");
    /*rewind(V->txtfile);*/ /* Rewind file at the start */

    /* Find the error line */
    while(fline < line && (c = fgetc(ftxt)) != EOF)
    {
        if(c == '\n')
            fline++;
    }

    vp_assertX(fline == line, "no error line");
    while((c = fgetc(ftxt)) != EOF && c != '\n')
    {
        vp_buf_putb(&V->tmpbuf, c);
    }
    vp_buf_putb(&V->tmpbuf, '\0');  /* Terminate the line */

    /* Print the actual line */
    fputs(V->tmpbuf.b, stderr);
    fputc('\n', stderr);

    /* Print the error indicator */
    for(uint64_t i = 0; i < ofs - 2; i++)
    {
        fputc(V->tmpbuf.b[i] == '\t' ? '\t' : ' ', stderr);
    }
    fputs("^\n", stderr);
    fclose(ftxt);
}

/* Warning */
void vp_err_warn(SrcLoc loc, const char* msg, ...)
{
    va_list args;
    va_start(args, msg);
    fprintf(stderr, "%s:%d warn: ", loc.name, loc.line);
    vfprintf(stderr, msg, args);
    fputc('\n', stderr);
    va_end(args);
    err_line(loc.line, loc.ofs);
}

/* Error */
void vp_err_error(SrcLoc loc, const char* msg, ...)
{
    va_list args;
    va_start(args, msg);
    fprintf(stderr, "%s:%d error: ", loc.name, loc.line);
    vfprintf(stderr, msg, args);
    fputc('\n', stderr);
    va_end(args);
    err_line(loc.line, loc.ofs);
    exit(EXIT_FAILURE);
}

/* Lexer error */
void vp_err_lex(LexLine line, LexOffset ofs, const char* name, const char* msg, va_list argp)
{
    fprintf(stderr, "%s:%d error: ", name, line);
    vfprintf(stderr, msg, argp);
    fputc('\n', stderr);
    err_line(line, ofs);
    exit(EXIT_FAILURE);
}