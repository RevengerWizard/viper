/*
** vp_pe.c
** Windows PE emitter
*/

#include <string.h>

#include "vp_buf.h"
#include "vp_state.h"
#include "vp_str.h"
#include "vp_link.h"

#define PE32_PLUS_MAGIC         0x20b
#define MAJOR_SUBSYSTEM_VERSION 4

/* Subsystems */
#define IMAGE_SUBSYSTEM_WINDOWS_GUI 2
#define IMAGE_SUBSYSTEM_CONSOLE 3

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

static void pe_sec_text(VpState* V, SBuf* sb, Layout* L)
{
    char *p = vp_buf_more(sb, L->text.secsize);
    memcpy(p, V->code.b, L->text.virtsize);
    memset(p + L->text.virtsize, 0, L->text.secsize - L->text.virtsize);
    sb->w = p + L->text.secsize;
}

static void pe_sec_idata(VpState* V, SBuf* sb, uint32_t base, uint32_t size)
{
    char* p = vp_buf_more(sb, size);
    memset(p, 0, size);
    char* w = p;

    uint32_t dllsnum = vec_len(V->imports);

    /* Calculate offsets */
    uint32_t idt_size = (dllsnum + 1) * 20;
    uint32_t offset = idt_size;

    /* Calculate INT offsets and sizes */
    uint32_t* int_offsets = vp_mem_alloc(sizeof(uint32_t) * dllsnum);
    for(uint32_t i = 0; i < dllsnum; i++)
    {
        int_offsets[i] = offset;
        uint32_t num_funcs = vec_len(V->imports[i].funcs);
        offset += (num_funcs + 1) * 8;
    }

    /* Calculate IAT offsets */
    uint32_t* iat_offsets = vp_mem_alloc(sizeof(uint32_t) * dllsnum);
    for(uint32_t i = 0; i < dllsnum; i++)
    {
        iat_offsets[i] = offset;
        uint32_t num_funcs = vec_len(V->imports[i].funcs);
        offset += (num_funcs + 1) * 8;
    }

    /* Calculate DLL name offsets */
    uint32_t* dll_name_offsets = vp_mem_alloc(sizeof(uint32_t) * dllsnum);
    for(uint32_t i = 0; i < dllsnum; i++)
    {
        dll_name_offsets[i] = offset;
        offset += V->imports[i].name->len + 1;
    }

    /* Calculate Hint/Name offsets */
    uint32_t** hint_name_offsets = vp_mem_alloc(sizeof(uint32_t*) * dllsnum);
    for(uint32_t i = 0; i < dllsnum; i++)
    {
        ImportDLL* dll = &V->imports[i];
        uint32_t num_funcs = vec_len(dll->funcs);
        hint_name_offsets[i] = vp_mem_alloc(sizeof(uint32_t) * num_funcs);

        for(uint32_t j = 0; j < num_funcs; j++)
        {
            hint_name_offsets[i][j] = offset;
            uint32_t name_len = dll->funcs[j].name->len;
            offset += 2 + name_len + 1; /* hint + name + \0 */
            if(offset & 1) offset++; /* Align to 2-byte boundary */
        }
    }

    /* Import Directory Table */
    for(uint32_t i = 0; i < dllsnum; i++)
    {
        *(uint32_t*)(&w[0]) = base + int_offsets[i];  /* OriginalFirstThunk */
        *(uint32_t*)(&w[4]) = 0;  /* TimeDateStamp */
        *(uint32_t*)(&w[8]) = 0;  /* ForwarderChain */
        *(uint32_t*)(&w[12]) = base + dll_name_offsets[i];  /* Name */
        *(uint32_t*)(&w[16]) = base + iat_offsets[i];  /* FirstThunk */
        w += 20;
    }
    /* NULL entry */
    memset(w, 0, 20);
    w += 20;

    /* Import Lookup Table (INT) */
    for(uint32_t i = 0; i < dllsnum; i++)
    {
        ImportDLL* dll = &V->imports[i];
        w = p + int_offsets[i];

        for(uint32_t j = 0; j < vec_len(dll->funcs); j++)
        {
            *(uint64_t*)w = base + hint_name_offsets[i][j];
            w += 8;
        }
        /* NULL entry */
        *(uint64_t*)w = 0;
        w += 8;
    }

    /* Import Address Table (IAT) */
    for(uint32_t i = 0; i < dllsnum; i++)
    {
        ImportDLL* dll = &V->imports[i];
        w = p + iat_offsets[i];

        for(uint32_t j = 0; j < vec_len(dll->funcs); j++)
        {
            *(uint64_t*)w = base + hint_name_offsets[i][j];
            w += 8;
        }
        /* NULL entry */
        *(uint64_t*)w = 0;
        w += 8;
    }

    /* DLL Name Strings */
    for(uint32_t i = 0; i < dllsnum; i++)
    {
        ImportDLL* dll = &V->imports[i];
        w = p + dll_name_offsets[i];
        memcpy(w, str_data(dll->name), dll->name->len);
        w[dll->name->len] = '\0';
    }

    /* Hint/Name Table */
    for(uint32_t i = 0; i < dllsnum; i++)
    {
        ImportDLL* dll = &V->imports[i];

        for(uint32_t j = 0; j < vec_len(dll->funcs); j++)
        {
            ImportFunc* func = &dll->funcs[j];
            w = p + hint_name_offsets[i][j];

            *(uint16_t*)w = 0; /* Hint */
            w += 2;
            memcpy(w, str_data(func->name), func->name->len);
            w[func->name->len] = '\0';
        }
    }

    sb->w = p + size;
}

static void pe_sec_data(VpState* V, SBuf* sb, uint32_t size)
{
    char* p = vp_buf_more(sb, size);
    memset(p, 0, size);
    char* w = p;

    for(uint32_t i = 0; i < vec_len(V->strs); i++)
    {
        Str* s = V->strs[i];
        memcpy(w, str_data(s), s->len);
        w += s->len + 1;    /* + \0 */
    }

    sb->w = p + size;
}

static void pe_build(VpState* V, SBuf* sb, Layout* L)
{
    /* DOS header */
    char* p = vp_buf_need(sb, 64);
    memcpy(p, "MZ", 2); /* e_magic = 'MZ' */
    memset(p + 2, 0, 60 - 2);
    *(uint32_t*)(&p[60]) = 64;   /* e_lfanew */
    sb->w = p + 64;

    /* PE header */
    p = vp_buf_more(sb, 24);
    memcpy(p, "PE\0\0", 4); /* Signature = 'PE\0\0' */
    *(uint16_t*)(&p[4]) = IMAGE_FILE_MACHINE_AMD64;    /* Machine */
    *(uint16_t*)(&p[6]) = L->nsecs;     /* NumberOfSections */
    memset(&p[8], 0, 20);   /* TimeDateStamp, PointerToSymbolTable, NumberOfSymbols */
    *(uint16_t*)(&p[20]) = 240;    /* SizeOfOptionalHeader */
    *(uint16_t*)(&p[22]) = PE_CHARACTERISTICS;    /* Characteristics */
    sb->w = p + 24;

    /* Optional header */
    p = vp_buf_more(sb, 240);
    memset(p, 0, 240);
    *(uint16_t*)(&p[0]) = PE32_PLUS_MAGIC;    /* Magic (PE32+) */
    *(uint32_t*)(&p[16]) = L->entry;    /* AddressOfEntryPoint */
    *(uint64_t*)(&p[24]) = 0x00400000ULL;    /* ImageBase (64-bit) */
    *(uint32_t*)(&p[32]) = SECTION_ALIGNMENT;    /* SectionAlignment */
    *(uint32_t*)(&p[36]) = FILE_ALIGNMENT;    /* FileAlignment (standard) */
    *(uint16_t*)(&p[48]) = MAJOR_SUBSYSTEM_VERSION;    /* MajorSubsystemVersion */
    *(uint32_t*)(&p[56]) = L->imgsize;    /* SizeOfImage (total runtime memory size) */
    *(uint32_t*)(&p[60]) = 512;    /* SizeOfHeaders */
    *(uint16_t*)(&p[68]) = IMAGE_SUBSYSTEM_CONSOLE;    /* Subsystem */
    *(uint32_t*)(&p[108]) = 16;    /* NumberOfRvaAndSizes */

    /* Data directories */
    if(L->idata.secsize)
    {
        /* ImportTable  */
        *(uint32_t*)(&p[120]) = L->idata.virtaddr; /* VirtualAddress */
        *(uint32_t*)(&p[124]) = 512;    /* Size */
    }
    sb->w = p + 240;

    /* Section tables */
    /* .text */
    p = vp_buf_more(sb, 40);
    memset(p, 0, 40);
    memcpy(p, ".text", 5);
    *(uint32_t*)(&p[8]) = L->text.virtsize;   /* VirtualSize */
    *(uint32_t*)(&p[12]) = L->text.virtaddr;   /* VirtualAddress */
    *(uint32_t*)(&p[16]) = L->text.secsize;   /* SizeOfRawData */
    *(uint32_t*)(&p[20]) = L->text.secofs;   /* PointerToRawData */
    *(uint32_t*)(&p[36]) = TEXT_CHARACTERISTICS;   /* Characteristics */
    sb->w = p + 40;

    if(L->idata.secsize)
    {
        /* .idata */
        p = vp_buf_more(sb, 40);
        memset(p, 0, 40);
        memcpy(p, ".idata", 6);
        *(uint32_t*)(&p[8]) = L->idata.virtsize;   /* VirtualSize */
        *(uint32_t*)(&p[12]) = L->idata.virtaddr;   /* VirtualAddress */
        *(uint32_t*)(&p[16]) = L->idata.secsize;   /* SizeOfRawData */
        *(uint32_t*)(&p[20]) = L->idata.secofs;   /* PointerToRawData */
        *(uint32_t*)(&p[36]) = IDATA_CHARACTERISTICS;   /* Characteristics */
        sb->w = p + 40;
    }

    if(L->data.secsize)
    {
        /* .data */
        p = vp_buf_more(sb, 40);
        memset(p, 0, 40);
        memcpy(p, ".data", 5);
        *(uint32_t*)(&p[8]) = L->data.virtsize;   /* VirtualSize */
        *(uint32_t*)(&p[12]) = L->data.virtaddr;   /* VirtualAddress */
        *(uint32_t*)(&p[16]) = L->data.secsize;   /* SizeOfRawData */
        *(uint32_t*)(&p[20]) = L->data.secofs;   /* PointerToRawData */
        *(uint32_t*)(&p[36]) = DATA_CHARACTERISTICS;   /* Characteristics */
        sb->w = p + 40;
    }

    /* Pad to 512 */
    uint32_t off = sbuf_len(sb);
    if(off < 512)
    {
        char* pad = vp_buf_more(sb, 512 - off);
        memset(pad, 0, 512 - off);
        sb->w = pad + (512 - off);
    }
}

void vp_emit_exe(VpState* V, SBuf* sb)
{
    Layout L = V->L;

    pe_build(V, sb, &L);

    /* .text */
    pe_sec_text(V, sb, &L);

    /* .idata */
    if(L.idata.secsize)
    {
        pe_sec_idata(V, sb, L.idata.virtaddr, L.idata.secsize);
    }

    /* .data */
    if(L.data.secsize)
    {
        pe_sec_data(V, sb, L.data.secsize);
    }
}