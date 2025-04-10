/*
** vp_pe.c
** Windows PE emitter
*/

#include <IntSafe.h>

#include <stdio.h>
#include <string.h>

#include "vp_buf.h"
#include "vp_emit.h"
#include "vp_state.h"

static void pe_build(VpState* V, SBuf* sb)
{
    size_t codesize = sbuf_len(&V->code);

    /* DOS header */
    char* p = vp_buf_need(sb, 64);
    p[0] = 'M'; p[1] = 'Z'; /* e_magic = 'MZ' */
    memset(p + 2, 0,60 - 2);
    *(DWORD*)(&p[60]) = 64;   /* e_lfanew */
    sb->w = p + 64;

    /* PE header */
    p = vp_buf_more(sb, 24);
    p[0] = 'P'; /* Signature = 'PE\0\0' */
    p[1] = 'E';
    p[2] = '\0';
    p[3] = '\0';
    *(WORD*)(&p[4]) = 0x8664;    /* Machine = IMAGE_FILE_MACHINE_AMD64 */
    *(WORD*)(&p[6]) = 1;    /* NumberOfSections */
    memset(&p[8], 0, 20);
    *(WORD*)(&p[20]) = 240;    /* SizeOfOptionalHeader */
    *(WORD*)(&p[22]) = 0x103;    /* Characteristics */
    sb->w = p + 24;
    
    /* Optional header */
    p = vp_buf_more(sb, 240);
    memset(p, 0, 240);
    *(WORD*)(&p[0]) = 0x20b;    /* Magic (PE32+) */
    *(DWORD*)(&p[16]) = 4096;    /* AddressOfEntryPoint */
    *(ULONGLONG*)(&p[24]) = 0x00400000ULL;    /* ImageBase (64-bit) */
    *(DWORD*)(&p[32]) = 4096;    /* SectionAlignment */
    *(DWORD*)(&p[36]) = 512;    /* FileAlignment (standard) */
    *(WORD*)(&p[48]) = 4;    /* MajorSubsystemVersion */
    *(DWORD*)(&p[56]) = 4096 * 2;    /* SizeOfImage (total runtime memory size) */
    *(DWORD*)(&p[60]) = 512;    /* SizeOfHeaders */
    *(WORD*)(&p[68]) = 3;    /* Subsystem (IMAGE_SUBSYSTEM_WINDOWS_CUI) */
    *(DWORD*)(&p[108]) = 16;    /* NumberOfRvaAndSizes */
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
    *(DWORD*)(&p[8]) = (DWORD)codesize;   /* VirtualSize */
    *(DWORD*)(&p[12]) = 4096;   /* VirtualAddress */
    *(DWORD*)(&p[16]) = (DWORD)codesize;   /* SizeOfRawData */
    *(DWORD*)(&p[20]) = 512;   /* PointerToRawData */
    *(DWORD*)(&p[36]) = 0x60000020;   /* Characteristics */
    sb->w = p + 40;

    /* Ensure alignment */
    size_t off = sbuf_len(sb);
    if(off < 512)
    {
        char* pad = vp_buf_more(sb, 512 - off);
        memset(pad, 0, 512 - off);
        sb->w = pad + (512 - off);
    }

    printf("codesize = %llu\n", codesize);
    /* Copy code section */
    p = vp_buf_more(sb, codesize);
    memcpy(p, V->code.b, codesize);
    sb->w = p + codesize;

#if 0
    /* .text section */
    p = vp_buf_more(sb, 8);
    p[0] = 0x48;    /* REX.W prefix for 64-bit operand */
    p[1] = 0xc7;    /* mov r/m64, imm32 */
    p[2] = 0xc0;    /* mod=11, reg=000 (rax), r/m=000 (rax) */
    p[3] = 0x2a;    /* imm32 (value 42) - little endian */
    p[4] = 0x00;
    p[5] = 0x00;
    p[6] = 0x00;
    p[7] = 0xc3;    /* ret */
    sb->w = p + 8;
#endif
}

void vp_emit_exe(VpState* V, SBuf* sb)
{
    pe_build(V, sb);
}