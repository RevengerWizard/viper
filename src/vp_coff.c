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

static vec_t(COFFReloc) textrelocs;
static vec_t(COFFReloc) datarelocs;
static vec_t(COFFReloc) rdatarelocs;
static vec_t(COFFSym) syms;
static Map symsmap;

static uint32_t coff_secflags(SectionKind kind)
{
    switch(kind)
    {
        case SEC_TEXT:
            return IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ;
        case SEC_RDATA:
            return IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ;
        case SEC_DATA:
            return IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
    }
}

static Str* coff_secname(SectionKind kind)
{
    switch(kind)
    {
        case SEC_TEXT:
            return vp_str_newlit(".text");
        case SEC_RDATA:
            return vp_str_newlit(".rdata");
        case SEC_DATA:
            return vp_str_newlit(".data");
    }
}

static uint32_t coff_symadd(COFFSym sym)
{
    uint32_t idx = vec_len(syms);
    vec_push(syms, sym);
    vp_map_put(&symsmap, sym.name, (void*)(uintptr_t)idx);
    return idx;
}

static uint32_t coff_symfind(Str* name)
{
    void* idx = vp_map_get(&symsmap, name);
    if(idx)
        return (uint32_t)(uintptr_t)idx;
    return (uint32_t)-1;
}

static void coff_build()
{
    /* Add section symbols */
    for(uint32_t i = 0; i < vec_len(V->L.secs); i++)
    {
        Section* sec = &V->L.secs[i];
        Str* name = coff_secname(sec->kind);
        COFFSym sym = {
            .name = name,
            .value = 0,
            .sec = i + 1,  /* 1-based */
            .type = 0,
            .scl = IMAGE_SYM_CLASS_STATIC
        };
        coff_symadd(sym);
    }

    /* Add data symbols */
    for(uint32_t i = 0; i < vec_len(V->globdata); i++)
    {
        DataEntry* de = V->globdata[i];
        int16_t secnum = 0;
        switch(de->kind)
        {
            case DATA_VAR:
            case DATA_ANON:
                for(uint32_t j = 0; j < vec_len(V->L.secs); j++)
                {
                    if(V->L.secs[j].kind == SEC_DATA)
                    {
                        secnum = j + 1;  /* 1-based */
                        break;
                    }
                }
                break;
            case DATA_STR:
                for(uint32_t j = 0; j < vec_len(V->L.secs); j++)
                {
                    if(V->L.secs[j].kind == SEC_RDATA)
                    {
                        secnum = j + 1;  /* 1-based */
                        break;
                    }
                }
                break;
        }
        vp_assertX(secnum > 0, "section not found for data entry");

        COFFSym sym = {
            .name = de->name,
            .value = de->ofs,
            .sec = secnum,  /* (1-based) */
            .type = 0,
            .scl = IMAGE_SYM_CLASS_STATIC
        };
        coff_symadd(sym);
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
            coff_symadd(sym);
        }
    }

    /* Add function symbols */
    for(uint32_t i = 0; i < vec_len(V->codes); i++)
    {
        Code* code = V->codes[i];
        Str* name = code->name;
        if(code->flags & FN_EXTERN)
        {
            name = vp_buf_cat2str(vp_str_newlit("__imp_"), name);
        }
        COFFSym sym = {
            .name = name,
            .value = code->ofs,
            .sec = (code->flags & FN_EXTERN) ? 0 /* undefined */ : 1 /* .text */,
            .type = (code->flags & FN_EXTERN) ? 0 : IMAGE_SYM_DTYPE_FUNCTION,
            .scl = IMAGE_SYM_CLASS_EXTERNAL
        };
        coff_symadd(sym);
    }

    /* Add text relocations */
    for(uint32_t i = 0; i < vec_len(V->patches); i++)
    {
        PatchInfo* p = &V->patches[i];
        COFFReloc rel = {
            .vaddr = (uint32_t)p->ofs,
            .symidx = 0,
            .type = IMAGE_REL_AMD64_REL32
        };
        Str* sym = NULL;
        switch(p->kind)
        {
            case PATCH_JMP_REL:
            case PATCH_LEA_REL:
                continue;
            case PATCH_CALL_REL:
                sym = p->code->name;
                break;
            case PATCH_LEA_ABS:
                sym = p->label;
                break;
            case PATCH_CALL_ABS:
                sym = vp_buf_cat2str(vp_str_newlit("__imp_"), p->label);
                break;
        }
        rel.symidx = coff_symfind(sym);
        vp_assertX(rel.symidx != (uint32_t)-1, "symbol %s not found in patch %d", str_data(sym), p->kind);

        printf("Data relocation: at offset %u\n",
                       rel.vaddr);

        vec_push(textrelocs, rel);
    }

    /* Add data relocations */
    for(uint32_t i = 0; i < vec_len(V->relocs); i++)
    {
        Reloc* r = &V->relocs[i];
        COFFReloc rel = {
            .vaddr = r->entry->ofs + r->ofs,
            .symidx = coff_symfind(r->sym),
            .type = IMAGE_REL_AMD64_ADDR64
        };
        vp_assertX(rel.symidx != (uint32_t)-1, "reloc symbol not found");
        if(r->entry->kind == DATA_STR)
        {
            vec_push(rdatarelocs, rel);
        }
        else
        {
            vec_push(datarelocs, rel);
        }

        printf("Data relocation: %s at offset %u (entry kind=%d)\n",
            str_data(r->sym), rel.vaddr, r->entry->kind);
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
    textrelocs = vec_init(COFFReloc);
    datarelocs = vec_init(COFFReloc);
    rdatarelocs = vec_init(COFFReloc);
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
    uint32_t rofs = 20 + nsecs * 40;
    for(uint32_t i = 0; i < nsecs; i++)
    {
        Section* sec = &L->secs[i];
        uint32_t nrelocs = 0;
        if(sec->kind == SEC_TEXT)
            nrelocs = vec_len(textrelocs);
        else if(sec->kind == SEC_DATA)
            nrelocs = vec_len(datarelocs);
        else if(sec->kind == SEC_RDATA)
            nrelocs = vec_len(rdatarelocs);

        p = vp_buf_more(sb, 40);
        memset(p, 0, 40);

        /* Section name */
        Str* name = coff_secname(sec->kind);
        if(name->len <= 8)
        {
            memcpy(p, str_data(name), name->len);
        }
        else
        {
            p[0] = '/';
        }

        *(uint32_t*)(&p[8]) = 0;    /* VirtualSize */
        *(uint32_t*)(&p[12]) = 0;    /* VirtualAddress */
        *(uint32_t*)(&p[16]) = sbuf_len(&sec->sb);    /* SizeOfRawData */
        *(uint32_t*)(&p[20]) = rofs;    /* PointerToRawData */
        *(uint32_t*)(&p[24]) = (nrelocs > 0) ? rofs + sbuf_len(&sec->sb) : 0;    /* PointerToRelocations */
        *(uint32_t*)(&p[28]) = 0;    /* PointerToLineNumbers */
        *(uint16_t*)(&p[32]) = nrelocs;    /* NumberOfRelocations */
        *(uint16_t*)(&p[34]) = 0;    /* NumberOfLinenumbers */
        *(uint32_t*)(&p[36]) = coff_secflags(sec->kind);    /* Characteristics */
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

        vec_t(COFFReloc) relocvec = NULL;
        if(sec->kind == SEC_TEXT)
            relocvec = textrelocs;
        else if(sec->kind == SEC_DATA)
            relocvec = datarelocs;
        else if(sec->kind == SEC_RDATA)
            relocvec = rdatarelocs;

        /* Section relocations */
        if(relocvec)
        {
            for(uint32_t j = 0; j < vec_len(relocvec); j++)
            {
                COFFReloc* r = &relocvec[j];
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

        /* Name (8 bytes) */
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