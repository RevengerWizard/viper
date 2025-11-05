/*
** viper.c
** Viper compiler
*/

#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdio.h>
#include <stdlib.h>

#include "vp_state.h"

#include "vp_pe.c"

static void print_usage()
{
    fputs(
        "usage: vxc file\n",
        stderr
    );
    fflush(stderr);
}

int main(int argc, char** argv)
{
    if(argc != 2)
    {
        print_usage();
        return EXIT_SUCCESS;
    }

    V = vp_state_open();

    SBuf* code = &V->code;
    vp_buf_init(code);
    vp_buf_need(code, 1024);

    vp_load(V, argv[1]);

    FILE* f = fopen("out.bin", "wb");
    if(f)
    {
        fwrite(code->b, 1, sbuf_len(code), f);
        fclose(f);
    }
    else
    {
        fprintf(stderr, "unable to open out.bin\n");
        return EXIT_FAILURE;
    }

    SBuf sb;
    vp_buf_init(&sb);
    vp_buf_need(&sb, 1024);
    vp_emit_exe(V, &sb);
    f = fopen("out.exe", "wb");
    if(f)
    {
        fwrite(sb.b, 1, sbuf_len(&sb), f);
        fclose(f);
    }
    else
    {
        fprintf(stderr, "unable to open out.exe\n");
        return EXIT_FAILURE;
    }

    vp_state_close(V);

    return EXIT_SUCCESS;
}