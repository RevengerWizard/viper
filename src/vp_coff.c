/*
** vp_coff.c
** COFF object file emitter
*/

#include "vp_buf.h"
#include "vp_link.h"
#include "vp_pe.h"
#include "vp_state.h"
#include "vp_codegen.h"
#include "vp_map.h"
#include "vp_str.h"

/* COFF relocation */
typedef struct
{
    uint32_t vaddr; /* Virtual address (offset in section) */
    uint32_t symidx;    /* Symbol table index */
    uint16_t type;  /* Relocation type */
} COFFReloc;

/* COFF symbol */
typedef struct
{
    Str* name;
    uint32_t value; /* Offset in section or absolute value */
    int16_t sec;    /* Section number (1-based, 0=external) */
    uint16_t type;  /* Symbol type */
    uint8_t scl;    /* Storage class */
} COFFSym;

static vec_t(COFFReloc) relocs;
static vec_t(COFFSym) syms;
static Map strsyms;

static uint32_t coff_sec_flags(SectionKind kind)
{
    switch(kind)
    {
        case SEC_TEXT:
            return IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ;
        case SEC_IDATA:
            return IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ;
        case SEC_DATA:
            return IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
    }
}

static Str* coff_sec_names(SectionKind kind)
{
    switch(kind)
    {
        case SEC_TEXT:
            return vp_str_newlit(".text");
        case SEC_IDATA:
            return vp_str_newlit(".idata");
        case SEC_DATA:
            return vp_str_newlit(".data");
    }
}

static void coff_build()
{
    /* Add section symbols */
    for(uint32_t i = 0; i < vec_len(V->L.secs); i++)
    {
        Section* sec = &V->L.secs[i];
        Str* name = coff_sec_names(sec->kind);
        COFFSym sym = {
            .name = name,
            .value = 0,
            .sec = i + 1,  /* 1-based */
            .type = 0,
            .scl = IMAGE_SYM_CLASS_STATIC
        };
        vec_push(syms, sym);
    }

    /* Add string symbols */
    for(uint32_t i = 0; i < vec_len(V->strs); i++)
    {
        char buf[36];
        uint32_t buflen = (uint32_t)snprintf(buf, sizeof(buf), ".str%u", i);
        Str* symname = vp_str_new(buf, buflen);
        COFFSym sym = {
            .name = symname,
            .value = V->strofs[i],
            .sec = 2,  /* 1-based */
            .type = 0,
            .scl = IMAGE_SYM_CLASS_STATIC
        };
        uint32_t idx = vec_len(syms);
        vec_push(syms, sym);
        vp_map_put(&strsyms, V->strs[i], (void*)(uintptr_t)idx);
    }

    /* Add externel function symbols */
    for(uint32_t i = 0; i < V->ifuncs.size; i++)
    {
        TabEntry* entry = &V->ifuncs.entries[i];
        Str* name = entry->key;
        if(entry->key != NULL && entry->val == NULL)
        {
            name = vp_buf_cat2str(vp_str_newlit("__imp_"), name);
            COFFSym sym = {
                .name = name,
                .value = 0,
                .sec = 0 /* undefined */,
                .type = 0,
                .scl = IMAGE_SYM_CLASS_EXTERNAL
            };
            vec_push(syms, sym);
        }
    }

    /* Add function symbols */
    for(uint32_t i = 0; i < vec_len(V->codes); i++)
    {
        Code* code = V->codes[i];
        Str* name = code->name;
        if(code->flags & FN_EXPORT)
        {
            name = vp_buf_cat2str(vp_str_newlit("__imp_"), name);
        }
        COFFSym sym = {
            .name = name,
            .value = code->ofs,
            .sec = (code->flags & FN_EXPORT) ? 0 /* undefined */ : 1 /* .text */,
            .type = (code->flags & FN_EXPORT) ? 0 : IMAGE_SYM_DTYPE_FUNCTION,
            .scl = IMAGE_SYM_CLASS_EXTERNAL
        };
        vec_push(syms, sym);
    }

    /* Add relocations */
    for(uint32_t i = 0; i < vec_len(V->patches); i++)
    {
        PatchInfo* p = &V->patches[i];
        COFFReloc rel = {
            .vaddr = (uint32_t)p->ofs,
            .symidx = 0,
            .type = IMAGE_REL_AMD64_REL32
        };
        switch(p->kind)
        {
            case PATCH_JMP_REL:
            case PATCH_LEA_REL:
                continue;
            case PATCH_CALL_REL:
            {
                /* Find symbol index for target function */
                Str* fname = p->code->name;
                for(uint32_t j = 0; j < vec_len(syms); j++)
                {
                    if(syms[j].name == fname)
                    {
                        rel.symidx = j;
                        break;
                    }
                }
                break;
            }
            case PATCH_LEA_ABS:
            {
                /* String literal, use numerated symbol */
                void* idx = vp_map_get(&strsyms, p->label);
                if(idx)
                {
                    rel.symidx = (uint32_t)(uintptr_t)idx;
                }
                else
                {
                    vp_assertX(0, "string symbol not found");
                }
                break;
            }
            case PATCH_CALL_ABS:
            {
                /* External symbol, add if not exists */
                uint32_t symidx = 0;
                Str* fname = vp_buf_cat2str(vp_str_newlit("__imp_"), p->label);
                for(uint32_t j = 0; j < vec_len(syms); j++)
                {
                    if(syms[j].name == fname)
                    {
                        symidx = j;
                        break;
                    }
                }

                rel.symidx = symidx;
                rel.type = IMAGE_REL_AMD64_REL32;
                break;
            }
        }
        vec_push(relocs, rel);
    }
}

static void coff_strtab(SBuf* sb, uint32_t* ofs, uint32_t start)
{
    uint32_t size = 4;

    char* p = vp_buf_more(sb, 4);
    sb->w = p + 4;

    /* Write strings */
    for(uint32_t i = 0; i < vec_len(syms); i++)
    {
        Str* name = syms[i].name;
        if(syms[i].name->len > 8)
        {
            ofs[i] = size;
            size += name->len + 1;
            p = vp_buf_more(sb, name->len + 1);
            memcpy(p, str_data(name), name->len);
            p[name->len] = '\0';
            sb->w = p + name->len + 1;
        }
    }

    *(uint32_t*)(sb->b + start) = size; /* Size */
}

void vp_emit_coff(VpState* V, SBuf* sb)
{
    relocs = vec_init(COFFReloc);
    syms = vec_init(COFFSym);
    Layout* L = &V->L;
    uint32_t nsecs = vec_len(L->secs);

    coff_build();

    uint32_t nsyms = vec_len(syms);

    /* COFF header */
    char* p = vp_buf_need(sb, 20);
    *(uint16_t*)(&p[0]) = IMAGE_FILE_MACHINE_AMD64; /* Machine */
    *(uint16_t*)(&p[2]) = nsecs; /* NumberOfSections */
    *(uint32_t*)(&p[4]) = 0; /* TimeDateStamp */
    *(uint32_t*)(&p[8]) = 0; /* PointerToSymbolTable */
    *(uint32_t*)(&p[12]) = nsyms; /* NumberOfSymbols */
    *(uint16_t*)(&p[16]) = 0; /* SizeOfOptionalHeader */
    *(uint16_t*)(&p[18]) = 0; /* Characteristics */
    sb->w = p + 20;

    uint32_t hpos = 8;  /* Position of symbol table pointer */

    /* Section headers */
    uint32_t start = 20 + nsecs * 40;
    uint32_t rofs = start;
    for(uint32_t i = 0; i < nsecs; i++)
    {
        Section* sec = &L->secs[i];
        uint32_t nrelocs = (i == 0) ? vec_len(relocs) : 0;

        p = vp_buf_more(sb, 40);
        memset(p, 0, 40);

        /* Section name */
        Str* name = coff_sec_names(sec->kind);
        uint32_t namelen = name->len;
        if(namelen <= 8)
        {
            memcpy(p, str_data(name), namelen);
        }
        else
        {
            p[0] = '/';
        }

        *(uint32_t*)(&p[8]) = 0;    /* VirtualSize */
        *(uint32_t*)(&p[12]) = 0;    /* VirtualAddress */
        *(uint32_t*)(&p[16]) = sbuf_len(&sec->sb);    /* SizeOfRawData */
        *(uint32_t*)(&p[20]) = rofs;    /* PointerToRawData */
        *(uint32_t*)(&p[24]) = (i == 0) ? rofs + sbuf_len(&sec->sb) : 0;    /* PointerToRelocations */
        *(uint32_t*)(&p[28]) = 0;    /* PointerToLineNumbers */
        *(uint16_t*)(&p[32]) = (i == 0) ? nrelocs : 0;    /* NumberOfRelocations */
        *(uint16_t*)(&p[34]) = 0;    /* NumberOfLinenumbers */
        *(uint16_t*)(&p[36]) = coff_sec_flags(sec->kind);    /* Characteristics */
        sb->w = p + 40;

        rofs += sbuf_len(&sec->sb) + nrelocs * 10;
    }

    /* Section data */
    for(uint32_t i = 0; i < nsecs; i++)
    {
        Section* sec = &L->secs[i];
        uint32_t len = sbuf_len(&sec->sb);
        p = vp_buf_more(sb, len);
        memcpy(p, sec->sb.b, len);
        sb->w = p + len;

        /* .text relocations */
        if(i == 0)
        {
            for(uint32_t j = 0; j < vec_len(relocs); j++)
            {
                COFFReloc* r = &relocs[j];
                p = vp_buf_more(sb, 10);
                *(uint32_t*)(&p[0]) = r->vaddr; /* VirtualAddress */
                *(uint32_t*)(&p[4]) = r->symidx;    /* SymbolTableIndex */
                *(uint16_t*)(&p[8]) = r->type;  /* Type */
                sb->w = p + 10;
            }
        }
    }

    /* Symbol table */
    uint32_t symtab = sbuf_len(sb); /* Offset of symbol table */
    uint32_t* symstrs = vp_mem_calloc(nsyms, sizeof(uint32_t));
    for(uint32_t i = 0; i < nsyms; i++)
    {
        COFFSym* sym = &syms[i];
        p = vp_buf_more(sb, 18);

        /* Name */
        if(sym->name->len <= 8)
        {
            memcpy(p, str_data(sym->name), sym->name->len);
            memset(p + sym->name->len, 0, 8 - sym->name->len);
        }
        else
        {
            /* Long name, string table offset */
            *(uint32_t*)(&p[0]) = 0;
            *(uint32_t*)(&p[4]) = 0;  /* Offset written later */
            symstrs[i] = 1;  /* Mark as needing string table */
        }

        *(uint32_t*)(&p[8]) = sym->value;   /* Value */
        *(int16_t*)(&p[12]) = sym->sec; /* SectionNumber */
        *(uint16_t*)(&p[14]) = sym->type;   /* Type */
        p[16] = sym->scl;   /* StorageClass */
        p[17] = 0;  /* NumberOfAuxSymbols */
        sb->w = p + 18;
    }

    /* String table */
    uint32_t strtab = sbuf_len(sb); /* Offset of string table */
    coff_strtab(sb, symstrs, strtab);
    /* Fix string table offsets in symbols */
    for(uint32_t i = 0; i < nsyms; i++)
    {
        if(symstrs[i])
        {
            uint32_t pos = symtab + i * 18 + 4;
            *(uint32_t*)(sb->b + pos) = symstrs[i];
        }
    }

    /* Fix symbol table pointer */
    *(uint32_t*)(sb->b + hpos) = symtab;
}