/*
** vp_pe.c
** Windows PE emitter
*/

#include <stdio.h>
#include <string.h>

#include "vp_codegen.h"
#include "vp_buf.h"
#include "vp_state.h"
#include "vp_str.h"
#include "vp_tab.h"

/*
** WORD = uint16_t
** DWORD = uint32_t
** ULONGLONG = uint64_t
*/

#define PE32_PLUS_MAGIC         0x20b
#define MAJOR_SUBSYSTEM_VERSION 4
#define SUBSYSTEM_CONSOLE       3

/* Machine types */
#define IMAGE_FILE_MACHINE_AMD64    0x8664

/* Characteristics */
#define IMAGE_FILE_EXECUTABLE_IMAGE 0x0002
#define IMAGE_FILE_LARGE_ADDRESS_AWARE 0x0020
#define IMAGE_FILE_DEBUG_STRIPPED   0x0200

/*  Section flags */
#define IMAGE_SCN_CNT_INITIALIZED_DATA  0x00000040
#define IMAGE_SCN_CNT_CODE              0x00000020
#define IMAGE_SCN_MEM_EXECUTE           0x20000000
#define IMAGE_SCN_MEM_READ              0x40000000
#define IMAGE_SCN_MEM_WRITE             0x80000000

#define PE_CHARACTERISTICS \
    (IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE | IMAGE_FILE_DEBUG_STRIPPED)
#define TEXT_CHARACTERISTICS \
    (IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ)
#define IDATA_CHARACTERISTICS \
    (IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ)
#define DATA_CHARACTERISTICS \
    (IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE)

#define SECTIONS_NUM 3
#define RVA_TEXT 0x1000
#define RVA_IDATA 0x2000
#define RVA_DATA 0x3000

#define RVA_GetStdHandle (RVA_IDATA + 0x24)
#define RVA_WriteConsoleA (RVA_IDATA + 0x2C)

static void pe_sec_data(VpState* V, SBuf* sb)
{
    char* p = vp_buf_more(sb, 512);
    memset(p, 0, 512);
    char* w = p;

    for(uint32_t i = 0; i < vec_len(V->strs); i++)
    {
        Str* s = V->strs[i];

        memcpy(w, str_data(s), s->len);
        /* \0 */
        w += s->len + 1;
    }

    sb->w = p + 512;
}

static void pe_sec_idata(VpState* V, SBuf* sb)
{
    const uint32_t RVA_INT = RVA_IDATA + 0x28;
    const uint32_t RVA_IAT = RVA_IDATA + 0x40;
    const uint32_t RVA_DLL_NAME = RVA_IDATA + 0x58;

    const uint32_t RVA_NAME_GETSTDHANDLE = RVA_IDATA + 0x65;
    const uint32_t RVA_NAME_WRITECONSOLEA = RVA_IDATA + 0x74;

    char* p = vp_buf_more(sb, 512);
    memset(p, 0, 512);
    char* w = p;

    /* Import Directory Table */
    *(uint32_t*)(&w[0]) = RVA_INT;  /* OriginalFirstThunk */
    *(uint32_t*)(&w[12]) = RVA_DLL_NAME;    /* Name */
    *(uint32_t*)(&w[16]) = RVA_IAT;    /* FirstThunk */
    /* NULL entry */
    w += 40;

    /* Import Lookup Table (INT) */
    *(uint64_t*)(&w[0]) = RVA_NAME_GETSTDHANDLE;
    *(uint64_t*)(&w[8]) = RVA_NAME_WRITECONSOLEA;
    /* NULL entry */
    w += 24;

    /* Import Address Table (IAT) */
    *(uint64_t*)(&w[0]) = RVA_NAME_GETSTDHANDLE;
    *(uint64_t*)(&w[8]) = RVA_NAME_WRITECONSOLEA;
    /* NULL entry */
    w += 24;

    /* DLL Name String */
    strcpy(w, "kernel32.dll");
    w += 13;

    /* Hint/Name Table */
    *(uint16_t*)(&w[0]) = 0; // Hint
    strcpy(w + 2, "GetStdHandle");
    w += 2 + 13;

    *(uint16_t*)(&w[0]) = 0; // Hint
    strcpy(w + 2, "WriteConsoleA");
    w += 2 + 14;

    sb->w = p + 512;
}

static void pe_build(VpState* V, SBuf* sb)
{
    size_t codesize = sbuf_len(&V->code);

    Code* code = vp_tab_get(&V->funcs, vp_str_newlen("main"));
    vp_assertX(code, "?");
    int32_t entryofs = code->ofs;

    /* DOS header */
    char* p = vp_buf_need(sb, 64);
    p[0] = 'M'; p[1] = 'Z'; /* e_magic = 'MZ' */
    memset(p + 2, 0,60 - 2);
    *(uint32_t*)(&p[60]) = 64;   /* e_lfanew */
    sb->w = p + 64;

    /* PE header */
    p = vp_buf_more(sb, 24);
    p[0] = 'P'; p[1] = 'E'; p[2] = '\0'; p[3] = '\0';   /* Signature = 'PE\0\0' */
    *(uint16_t*)(&p[4]) = IMAGE_FILE_MACHINE_AMD64;    /* Machine */
    *(uint16_t*)(&p[6]) = SECTIONS_NUM;     /* NumberOfSections */
    memset(&p[8], 0, 20);   /* TimeDateStamp, PointerToSymbolTable, NumberOfSymbols */
    *(uint16_t*)(&p[20]) = 240;    /* SizeOfOptionalHeader */
    *(uint16_t*)(&p[22]) = PE_CHARACTERISTICS;    /* Characteristics */
    sb->w = p + 24;

    /* Optional header */
    p = vp_buf_more(sb, 240);
    memset(p, 0, 240);
    *(uint16_t*)(&p[0]) = PE32_PLUS_MAGIC;    /* Magic (PE32+) */
    *(uint32_t*)(&p[16]) = RVA_TEXT + entryofs;    /* AddressOfEntryPoint */
    *(uint64_t*)(&p[24]) = 0x00400000ULL;    /* ImageBase (64-bit) */
    *(uint32_t*)(&p[32]) = 4096;    /* SectionAlignment */
    *(uint32_t*)(&p[36]) = 512;    /* FileAlignment (standard) */
    *(uint16_t*)(&p[48]) = MAJOR_SUBSYSTEM_VERSION;    /* MajorSubsystemVersion */
    *(uint32_t*)(&p[56]) = RVA_DATA + 0x200;    /* SizeOfImage (total runtime memory size) */
    *(uint32_t*)(&p[60]) = 512;    /* SizeOfHeaders */
    *(uint16_t*)(&p[68]) = SUBSYSTEM_CONSOLE;    /* Subsystem */
    *(uint32_t*)(&p[108]) = 16;    /* NumberOfRvaAndSizes */

    /* Data directories */
    /* ImportTable  */
    *(uint32_t*)(&p[120]) = RVA_IDATA; /* VirtualAddress */
    *(uint32_t*)(&p[124]) = 512;    /* Size */
    sb->w = p + 240;

    /* Section tables */
    /* .text */
    p = vp_buf_more(sb, 40);
    memset(p, 0, 40);
    p[0] = '.';
    p[1] = 't';
    p[2] = 'e';
    p[3] = 'x';
    p[4] = 't';
    *(uint32_t*)(&p[8]) = (uint32_t)codesize;   /* VirtualSize */
    *(uint32_t*)(&p[12]) = RVA_TEXT;   /* VirtualAddress */
    *(uint32_t*)(&p[16]) = (uint32_t)codesize;   /* SizeOfRawData */
    *(uint32_t*)(&p[20]) = 0x200;   /* PointerToRawData */
    *(uint32_t*)(&p[36]) = TEXT_CHARACTERISTICS;   /* Characteristics */
    sb->w = p + 40;

    /* .idata */
    p = vp_buf_more(sb, 40);
    memset(p, 0, 40);
    p[0] = '.';
    p[1] = 'i';
    p[2] = 'd';
    p[3] = 'a';
    p[4] = 't';
    p[5] = 'a';
    *(uint32_t*)(&p[8]) = 512;   /* VirtualSize */
    *(uint32_t*)(&p[12]) = RVA_IDATA;   /* VirtualAddress */
    *(uint32_t*)(&p[16]) = 512;   /* SizeOfRawData */
    *(uint32_t*)(&p[20]) = 0x400;   /* PointerToRawData */
    *(uint32_t*)(&p[36]) = IDATA_CHARACTERISTICS;   /* Characteristics */
    sb->w = p + 40;

    /* .data */
    p = vp_buf_more(sb, 40);
    memset(p, 0, 40);
    p[0] = '.';
    p[1] = 'd';
    p[2] = 'a';
    p[3] = 't';
    p[4] = 'a';
    *(uint32_t*)(&p[8]) = 512;   /* VirtualSize */
    *(uint32_t*)(&p[12]) = RVA_DATA;   /* VirtualAddress */
    *(uint32_t*)(&p[16]) = 512;   /* SizeOfRawData */
    *(uint32_t*)(&p[20]) = 0x600;   /* PointerToRawData */
    *(uint32_t*)(&p[36]) = DATA_CHARACTERISTICS;   /* Characteristics */
    sb->w = p + 40;

    /* Ensure alignment */
    uint32_t off = sbuf_len(sb);
    uint32_t end = 512;
    if(off < end)
    {
        char* pad = vp_buf_more(sb, end - off);
        memset(pad, 0, end - off);
        sb->w = pad + (end - off);
    }

    /* .text section */
    p = vp_buf_more(sb, codesize);
    memcpy(p, V->code.b, codesize);
    sb->w = p + codesize;

    /* Ensure alignment */
    off = sbuf_len(sb);
    end = 512 + 512;
    if(off < end)
    {
        char* pad = vp_buf_more(sb, end - off);
        memset(pad, 0, end - off);
        sb->w = pad + (end - off);
    }

    /* .idata section */
    pe_sec_idata(V, sb);

    /* Ensure alignment */
    off = sbuf_len(sb);
    end = 512 + 512 + 512;
    if(off < end)
    {
        char* pad = vp_buf_more(sb, end - off);
        memset(pad, 0, end - off);
        sb->w = pad + (end - off);
    }

    /* .data section */
    pe_sec_data(V, sb);
}

void vp_emit_exe(VpState* V, SBuf* sb)
{
    pe_build(V, sb);
}