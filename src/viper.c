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
#include "vp_buf.h"
#include "vp_dump.h"
#include "vp_pe.h"

#define VXC_VERSION "0.0.0"

enum
{
    MODE_CHECK, /* Check mode */
    MODE_COMP,  /* Compilation mode */
};

/* Dump flags */
#define DUMP_AST  (1 << 0)
#define DUMP_IR   (1 << 1)
#define DUMP_DOT  (1 << 2)

/* Stats flags */
#define STATS_MEM (1 << 0)

static void ext_replace(const char* path, const char* ext, char* buf, size_t size)
{
    const char* dot = strrchr(path, '.');
    const char* sep = strrchr(path, '/');
#ifdef _WIN32
    const char* sep2 = strrchr(path, '\\');
    if(!sep || (sep2 && sep2 > sep)) sep = sep2;
#endif
    size_t base = dot && (!sep || dot > sep) ? (size_t)(dot - path) : strlen(path);
    size_t extlen = strlen(ext);
    if(base + extlen + 1 >= size)
    {
        fprintf(stderr, "vxc: path too long\n");
        exit(EXIT_FAILURE);
    }
    memcpy(buf, path, base);
    memcpy(buf + base, ext, extlen + 1);
}

static int parse_dump(const char* s, uint32_t* flags)
{
    char tmp[64];
    while(*s)
    {
        const char* comma = strchr(s, ',');
        size_t len = comma ? (size_t)(comma - s) : strlen(s);
        if(len == 0 || len >= sizeof(tmp))
            return 0;
        memcpy(tmp, s, len);
        tmp[len] = '\0';

        if(strcmp(tmp, "ast") == 0) *flags |= DUMP_AST;
        else if(strcmp(tmp, "ir")  == 0) *flags |= DUMP_IR;
        else if(strcmp(tmp, "dot") == 0) *flags |= DUMP_DOT;
        else
        {
            fprintf(stderr, "vxc: unknown emit kind '%s'\n", tmp);
            return 0;
        }
        s += len + (comma ? 1 : 0);
    }
    return 1;
}

static int parse_stats(const char* s, uint32_t* flags)
{
    char tmp[64];
    while(*s)
    {
        const char* comma = strchr(s, ',');
        size_t len = comma ? (size_t)(comma - s) : strlen(s);
        if(len == 0 || len >= sizeof(tmp))
            return 0;
        memcpy(tmp, s, len);
        tmp[len] = '\0';

        if(strcmp(tmp, "mem") == 0) *flags |= STATS_MEM;
        else
        {
            fprintf(stderr, "vxc: unknown stats kind '%s'\n", tmp);
            return 0;
        }
        s += len + (comma ? 1 : 0);
    }
    return 1;
}

static void fmt_mem(uint64_t bytes, char* buf, size_t size)
{
    if(bytes >= 1024ULL * 1024 * 1024)
        snprintf(buf, size, "%.1f GB", (double)bytes / (1024.0 * 1024 * 1024));
    else if(bytes >= 1024ULL * 1024)
        snprintf(buf, size, "%.1f MB", (double)bytes / (1024.0 * 1024));
    else if(bytes >= 1024)
        snprintf(buf, size, "%.0f KB", (double)bytes / 1024.0);
    else
        snprintf(buf, size, "%u B", (unsigned)bytes);
}

static void print_stats(VpState* V, uint32_t stats)
{
    if(!(stats & STATS_MEM))
        return;

    char buf[32];
    fputs("== stats ==\n", stderr);
    fmt_mem(V->astmem, buf, sizeof(buf));
    fprintf(stderr, "ast:     %s\n", buf);
    fputc('\n', stderr);
}

static void print_usage(void)
{
    fputs(
        "usage: vxc <mode> [options] <file>\n"
        "modes:\n"
        "    check <file.vp>         semantically check source\n"
        "    comp <file.vp>          compile source to object file\n"
        "options:\n"
        "    --help                  print this message and exit\n"
        "    --version               print version and exit\n"
        "    -o <file|->             output file (- for stdout)\n"
        "    --dump=<kind>[,kind]    dump: ast, ir, dot\n"
        "    --stats[=<kind>[,kind]] print stats: mem\n",
        stderr
    );
    fflush(stderr);
}

/* Open output file; "-" maps to stdout */
static FILE* open_out(const char* path, const char* fmode)
{
    if(strcmp(path, "-") == 0)
        return stdout;
    FILE* f = fopen(path, fmode);
    if(!f)
        fprintf(stderr, "vxc: cannot open '%s'\n", path);
    return f;
}

static void close_out(FILE* f)
{
    if(f && f != stdout)
        fclose(f);
}

static int sbuf_flush(SBuf* sb, FILE* f)
{
    size_t n = sbuf_len(sb);
    if(n > 0 && fwrite(sb->b, 1, n, f) != n)
        return 0;
    vp_buf_reset(sb);
    return 1;
}

static int do_dump(VpState* V, uint32_t flags)
{
    SBuf sb;
    vp_buf_init(&sb);

    if(flags & DUMP_AST)
    {
        fputs("== ast ==\n", stdout);
        vp_dump_ast(&sb, V->decls);
        if(!sbuf_flush(&sb, stdout)) return EXIT_FAILURE;
        fputc('\n', stdout);
    }
    if(flags & DUMP_IR)
    {
        fputs("== ir ==\n", stdout);
        vp_dump_ir(&sb, V->codes);
        if(!sbuf_flush(&sb, stdout)) return EXIT_FAILURE;
        fputc('\n', stdout);
    }
    if(flags & DUMP_DOT)
    {
        fputs("== dot ==\n", stdout);
        vp_dump_dot(&sb, V->codes);
        if(!sbuf_flush(&sb, stdout)) return EXIT_FAILURE;
        fputc('\n', stdout);
    }

    return EXIT_SUCCESS;
}

static int mode_check(int argc, char** argv)
{
    const char* infile = NULL;
    uint32_t stats = 0;

    for(int i = 0; i < argc; i++)
    {
        const char* a = argv[i];
        if(strcmp(a, "--help") == 0) { print_usage(); return EXIT_SUCCESS; }
        else if(strcmp(a, "--stats") == 0)
            stats = STATS_MEM;  /* bare --stats defaults to all */
        else if(strncmp(a, "--stats=", 8) == 0)
        {
            if(!parse_stats(a + 8, &stats))
                return EXIT_FAILURE;
        }
        else if(a[0] == '-') { fprintf(stderr, "vxc check: unknown option '%s'\n", a); return EXIT_FAILURE; }
        else
        {
            if(infile) { fputs("vxc check: multiple input files not supported\n", stderr); return EXIT_FAILURE; }
            infile = a;
        }
    }

    if(!infile) { fputs("vxc check: no input file\n", stderr); return EXIT_FAILURE; }

    V = vp_state_open();
    vp_check(V, infile);
    print_stats(V, stats);
    vp_state_close(V);
    return EXIT_SUCCESS;
}

static int mode_comp(int argc, char** argv)
{
    const char* infile = NULL;
    const char* out_path = NULL;
    uint32_t flags = 0;
    uint32_t stats = 0;

    for(int i = 0; i < argc; i++)
    {
        const char* a = argv[i];

        if(strcmp(a, "--help") == 0) { print_usage(); return EXIT_SUCCESS; }
        else if(strcmp(a, "-o") == 0)
        {
            if(++i >= argc) { fputs("vxc: -o requires an argument\n", stderr); return EXIT_FAILURE; }
            out_path = argv[i];
        }
        else if(strncmp(a, "--dump=", 7) == 0)
        {
            if(!parse_dump(a + 7, &flags))
                return EXIT_FAILURE;
        }
        else if(strcmp(a, "--stats") == 0)
            stats = STATS_MEM;  /* bare --stats defaults to all */
        else if(strncmp(a, "--stats=", 8) == 0)
        {
            if(!parse_stats(a + 8, &stats))
                return EXIT_FAILURE;
        }
        else if(a[0] == '-')
        {
            fprintf(stderr, "vxc comp: unknown option '%s'\n", a);
            return EXIT_FAILURE;
        }
        else
        {
            if(infile) { fputs("vxc comp: multiple input files not supported\n", stderr); return EXIT_FAILURE; }
            infile = a;
        }
    }

    if(!infile) { fputs("vxc comp: no input file\n", stderr); return EXIT_FAILURE; }

    V = vp_state_open();
    vp_load(V, infile);

    print_stats(V, stats);

    if(flags && do_dump(V, flags) != EXIT_SUCCESS)
    {
        vp_state_close(V);
        return EXIT_FAILURE;
    }

    char derived[4096];
    SBuf sb;
    vp_buf_init(&sb);
    vp_emit_coff(V, &sb);
    FILE* f = open_out(out_path ? out_path : (ext_replace(infile, ".obj", derived, sizeof(derived)), derived), "wb");
    int ret = EXIT_SUCCESS;
    if(!f || !sbuf_flush(&sb, f)) ret = EXIT_FAILURE;
    close_out(f);

    vp_state_close(V);
    return ret;
}

int main(int argc, char** argv)
{
    if(argc >= 2 && strcmp(argv[1], "--version") == 0)
    {
        fputs("vxc " VXC_VERSION "\n", stdout);
        return EXIT_SUCCESS;
    }

    if(argc >= 2 && strcmp(argv[1], "--help") == 0)
    {
        print_usage();
        return EXIT_SUCCESS;
    }

    if(argc < 2)
    {
        print_usage();
        return EXIT_FAILURE;
    }

    const char* mode = argv[1];

    if(strcmp(mode, "check") == 0)
        return mode_check(argc - 2, argv + 2);

    if(strcmp(mode, "comp") == 0)
        return mode_comp(argc - 2, argv + 2);

    fprintf(stderr, "vxc: unknown mode '%s'\n", mode);
    print_usage();
    return EXIT_FAILURE;
}