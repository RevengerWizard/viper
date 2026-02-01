/*
** vp_pe.c
** Windows PE emitter
*/

#include <string.h>

#include "vp_pe.h"
#include "vp_buf.h"
#include "vp_state.h"
#include "vp_str.h"
#include "vp_link.h"

#if 0
static uint32_t pe_sec_flags(SectionKind kind)
{
    switch(kind)
    {
        case SEC_TEXT:
            return IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ;
        case SEC_DATA:
            return IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
    }
}

static Str* pe_sec_names(SectionKind kind)
{
    switch(kind)
    {
        case SEC_TEXT:
            return vp_str_newlit(".text");
        case SEC_DATA:
            return vp_str_newlit(".data");
    }
}

static void pe_sec_text(VpState* V, SBuf* sb, Section* sec)
{
    char *p = vp_buf_more(sb, sec->secsize);
    memcpy(p, V->code.b, sec->virtsize);
    memset(p + sec->virtsize, 0, sec->secsize - sec->virtsize);
    sb->w = p + sec->secsize;
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

static void pe_build(VpState* V, SBuf* sb, Layout* L, uint32_t nsecs)
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
    *(uint16_t*)(&p[22]) = IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE | IMAGE_FILE_DEBUG_STRIPPED;    /* Characteristics */
    sb->w = p + 24;

    /* Optional header */
    p = vp_buf_more(sb, 240);
    memset(p, 0, 240);
    *(uint16_t*)(&p[0]) = 0x20b;    /* Magic (PE32+) */
    *(uint32_t*)(&p[16]) = L->entry;    /* AddressOfEntryPoint */
    *(uint64_t*)(&p[24]) = 0x00400000ULL;    /* ImageBase (64-bit) */
    *(uint32_t*)(&p[32]) = SECTION_ALIGNMENT;    /* SectionAlignment */
    *(uint32_t*)(&p[36]) = FILE_ALIGNMENT;    /* FileAlignment (standard) */
    *(uint16_t*)(&p[48]) = 4;    /* MajorSubsystemVersion */
    *(uint32_t*)(&p[56]) = L->imgsize;    /* SizeOfImage (total runtime memory size) */
    *(uint32_t*)(&p[60]) = 512;    /* SizeOfHeaders */
    *(uint16_t*)(&p[68]) = IMAGE_SUBSYSTEM_CONSOLE;    /* Subsystem */
    *(uint32_t*)(&p[108]) = 16;    /* NumberOfRvaAndSizes */

    /* Data directories */
    for(uint32_t i = 0; i < nsecs; i++)
    {
        if(L->secs[i].kind == SEC_IDATA)
        {
            *(uint32_t*)(&p[120]) = L->secs[i].virtaddr;
            *(uint32_t*)(&p[124]) = 512;
            break;
        }
    }
    sb->w = p + 240;

    /* Section headers */
    for(uint32_t i = 0; i < nsecs; i++)
    {
        Section* s = &L->secs[i];
        Str* name = pe_sec_names(s->kind);
        p = vp_buf_more(sb, 40);
        memset(p, 0, 40);
        memcpy(p, str_data(name), name->len);
        *(uint32_t*)(&p[8]) = s->virtsize;
        *(uint32_t*)(&p[12]) = s->virtaddr;
        *(uint32_t*)(&p[16]) = s->secsize;
        *(uint32_t*)(&p[20]) = s->secofs;
        *(uint32_t*)(&p[36]) = pe_sec_flags(s->kind);
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
    Layout* L = &V->L;
    uint32_t nsecs = vec_len(L->secs);

    /* Entry point */
    for(uint32_t i = 0; i < nsecs; i++)
    {
        Section* sec = &L->secs[i];
        if(L->secs[i].kind == SEC_TEXT)
        {
            Code *maincode = vp_tab_get(&V->funcs, vp_str_newlen("main"));
            vp_assertX(maincode, "main not found");
            V->L.entry = sec->virtaddr + maincode->ofs;
            break;
        }
    }

    pe_build(V, sb, L, nsecs);

    /* Section data */
    for(uint32_t i = 0; i < nsecs; i++)
    {
        Section* sec = &L->secs[i];
        switch(sec->kind)
        {
            case SEC_TEXT:
            {
                pe_sec_text(V, sb, sec);
                break;
            }
            case SEC_IDATA:
            {
                pe_sec_idata(V, sb, sec->virtaddr, sec->secsize);
                break;
            }
            case SEC_DATA:
            {
                pe_sec_data(V, sb, sec->secsize);
                break;
            }
        }
    }
}
#endif