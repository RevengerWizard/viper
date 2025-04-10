/*
** viper.c
** Viper compiler
*/

#include <stdio.h>
#include <stdlib.h>

#include "vp_buf.h"
#include "vp_state.h"
#include "vp_emit.h"

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

    VpState* V = vp_state_open();

    vp_load(V, argv[1]);

    SBuf sb;
    vp_buf_init(&sb);
    vp_buf_need(&sb, 1024);
    vp_emit_exe(V, &sb);
    FILE* f = fopen("output.exe", "wb");
    if(f)
    {
        fwrite(sb.b, 1, sbuf_len(&sb), f);
        fclose(f);
    }

    vp_state_close(V);

    return EXIT_SUCCESS;
}