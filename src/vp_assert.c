/*
** vp_assert.c
** Internal assertions
*/

#if defined(VIPER_USE_ASSERT)

#include <stdio.h>
#include <stdlib.h>

#include "vp_def.h"

void vp_assert_fail(const char* file, int line, const char* func, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);
    fputc('\n', stderr);
    fprintf(stderr, "Viper ASSERT %s:%d: %s: ", file, line, func);
    vfprintf(stderr, fmt, argp);
    fputc('\n', stderr);
    va_end(argp);
    abort();
}

#endif