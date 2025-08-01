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

static void pe_build(VpState* V, SBuf* sb)
{
    size_t codesize = sbuf_len(&V->code);

    /* DOS header */
    char* p = vp_buf_need(sb, 64);
    p[0] = 'M'; p[1] = 'Z'; /* e_magic = 'MZ' */
    memset(p + 2, 0,60 - 2);
    *(uint32_t*)(&p[60]) = 64;   /* e_lfanew */
    sb->w = p + 64;

    /* PE header */
    p = vp_buf_more(sb, 24);
    p[0] = 'P'; /* Signature = 'PE\0\0' */
    p[1] = 'E';
    p[2] = '\0';
    p[3] = '\0';
    *(uint16_t*)(&p[4]) = 0x8664;    /* Machine = IMAGE_FILE_MACHINE_AMD64 */
    *(uint16_t*)(&p[6]) = 1;    /* NumberOfSections */
    memset(&p[8], 0, 20);
    *(uint16_t*)(&p[20]) = 240;    /* SizeOfOptionalHeader */
    *(uint16_t*)(&p[22]) = 0x103;    /* Characteristics */
    sb->w = p + 24;

    Code* c = vp_tab_get(&V->funcs, vp_str_newlen("main"));
    vp_assertX(c, "?");
    int32_t ofs = c->ofs;

    /* Optional header */
    p = vp_buf_more(sb, 240);
    memset(p, 0, 240);
    *(uint16_t*)(&p[0]) = 0x20b;    /* Magic (PE32+) */
    *(uint32_t*)(&p[16]) = 4096 + ofs;    /* AddressOfEntryPoint */
    *(uint64_t*)(&p[24]) = 0x00400000ULL;    /* ImageBase (64-bit) */
    *(uint32_t*)(&p[32]) = 4096;    /* SectionAlignment */
    *(uint32_t*)(&p[36]) = 512;    /* FileAlignment (standard) */
    *(uint16_t*)(&p[48]) = 4;    /* MajorSubsystemVersion */
    *(uint32_t*)(&p[56]) = 4096 * 2;    /* SizeOfImage (total runtime memory size) */
    *(uint32_t*)(&p[60]) = 512;    /* SizeOfHeaders */
    *(uint16_t*)(&p[68]) = 3;    /* Subsystem (IMAGE_SUBSYSTEM_WINDOWS_CUI) */
    *(uint32_t*)(&p[108]) = 16;    /* NumberOfRvaAndSizes */
    sb->w = p + 240;

    /* Data directories */
    /* Already zeroed out */

    /* Section table */
    p = vp_buf_more(sb, 40);
    memset(p, 0, 40);
    p[0] = '.'; /* Name = '.text' */
    p[1] = 't';
    p[2] = 'e';
    p[3] = 'x';
    p[4] = 't';
    *(uint32_t*)(&p[8]) = (uint32_t)codesize;   /* VirtualSize */
    *(uint32_t*)(&p[12]) = 4096;   /* VirtualAddress */
    *(uint32_t*)(&p[16]) = (uint32_t)codesize;   /* SizeOfRawData */
    *(uint32_t*)(&p[20]) = 512;   /* PointerToRawData */
    *(uint32_t*)(&p[36]) = 0x60000020;   /* Characteristics */
    sb->w = p + 40;

    /* Ensure alignment */
    size_t off = sbuf_len(sb);
    if(off < 512)
    {
        char* pad = vp_buf_more(sb, 512 - off);
        memset(pad, 0, 512 - off);
        sb->w = pad + (512 - off);
    }

    /* .text section */
    p = vp_buf_more(sb, codesize);
    memcpy(p, V->code.b, codesize);
    sb->w = p + codesize;
}

void vp_emit_exe(VpState* V, SBuf* sb)
{
    pe_build(V, sb);
}