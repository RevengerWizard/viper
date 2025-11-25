/*
** vp_link_x64.c
** Address patching
*/

#include "vp_link.h"

static uint32_t calc_idata_size()
{
    uint32_t num_dlls = vec_len(V->imports);
    if(num_dlls == 0) return 0;
    uint32_t size = 0;

    /* Import Directory Table */
    size += (num_dlls + 1) * 20;

    /* INT + IAT (two copies) */
    for(uint32_t i = 0; i < num_dlls; i++)
    {
        uint32_t num_funcs = vec_len(V->imports[i].funcs);
        size += 2 * (num_funcs + 1) * 8; /* INT and IAT */
    }

    /* DLL names */
    for(uint32_t i = 0; i < num_dlls; i++)
    {
        size += V->imports[i].name->len + 1;
    }

    /* Hint/Name entries */
    for(uint32_t i = 0; i < num_dlls; i++)
    {
        ImportDLL* dll = &V->imports[i];
        for(uint32_t j = 0; j < vec_len(dll->funcs); j++)
        {
            ImportFunc* func = &dll->funcs[j];
            size += 2 + func->name->len + 1; /* 2-byte hint + name + null */
            /* Align to 2-byte boundary */
            if(size & 1) size++;
        }
    }

    return size;
}

static uint32_t calc_data_size()
{
    uint32_t sz = 0;
    for(uint32_t i = 0; i < vec_len(V->strs); i++)
        sz += V->strs[i]->len + 1;
    return sz;
}

static void calc_import_rvas(VpState *V)
{
    uint32_t ndlls = vec_len(V->imports);
    uint32_t ofs = (ndlls + 1) * 20;

    /* INT offsets */
    uint32_t* int_ofs = vp_mem_alloc(sizeof(uint32_t) * ndlls);
    for(uint32_t i = 0; i < ndlls; i++)
    {
        int_ofs[i] = ofs;
        ofs += (vec_len(V->imports[i].funcs) + 1) * 8;
    }

    /* IAT offsets */
    uint32_t* iat_ofs = vp_mem_alloc(sizeof(uint32_t) * ndlls);
    for(uint32_t i = 0; i < ndlls; i++)
    {
        iat_ofs[i] = ofs;
        ofs += (vec_len(V->imports[i].funcs) + 1) * 8;
    }

    /* Store IAT RVAs in funcs */
    for(uint32_t i = 0; i < ndlls; i++)
    {
        ImportDLL *dll = &V->imports[i];
        for(uint32_t j = 0; j < vec_len(dll->funcs); j++)
            dll->funcs[j].rva = iat_ofs[i] + j * 8;
    }

    vp_mem_free(int_ofs);
    vp_mem_free(iat_ofs);
}

void vp_layout_init(Layout *L)
{
    uint32_t csize = sbuf_len(&V->code);
    uint32_t isize = calc_idata_size();
    uint32_t dsize = calc_data_size();

    L->nsecs = 1 + (isize > 0) + (dsize > 0);

    uint32_t fofs = FILE_ALIGNMENT;
    uint32_t vaddr = SECTION_ALIGNMENT;

    /* .text */
    L->text.secofs = fofs;
    L->text.secsize = ALIGN_UP(csize, FILE_ALIGNMENT);
    L->text.virtaddr = vaddr;
    L->text.virtsize = csize;

    fofs += L->text.secsize;
    vaddr += ALIGN_UP(L->text.virtsize, SECTION_ALIGNMENT);

    /* .idata */
    L->idata.secofs = fofs;
    L->idata.secsize = ALIGN_UP(isize, FILE_ALIGNMENT);
    L->idata.virtaddr = vaddr;
    L->idata.virtsize = isize;

    fofs += L->idata.secsize;
    vaddr += ALIGN_UP(L->idata.virtsize, SECTION_ALIGNMENT);

    /* .data */
    L->data.secofs = fofs;
    L->data.secsize = ALIGN_UP(dsize, FILE_ALIGNMENT);
    L->data.virtaddr = vaddr;
    L->data.virtsize = dsize;

    vaddr += ALIGN_UP(L->data.virtsize, SECTION_ALIGNMENT);

    L->imgsize = vaddr;

    calc_import_rvas(V);
}

static uint32_t rva_import(Str* label)
{
    uint32_t rva = 0;
    for(uint32_t i = 0; i < vec_len(V->imports); i++)
    {
        ImportDLL* dll = &V->imports[i];
        for(uint32_t j = 0; j < vec_len(dll->funcs); j++)
        {
            ImportFunc* func = &dll->funcs[j];
            if(func->name == label)
            {
                rva = func->rva;
            }
        }
    }
    vp_assertX(rva, "external function not found: %.*s",
                (int)label->len, str_data(label));
    return (V->L.idata.virtaddr - V->L.text.virtaddr) + rva;
}

static uint32_t rva_str(Str* label)
{
    uint32_t i;
    for(i = 0; i < vec_len(V->strs); i++)
    {
        if(V->strs[i] == label)
            break;
    }
    return (V->L.data.virtaddr - V->L.text.virtaddr) + V->strofs[i];
}

void vp_link(void)
{
    for(uint32_t i = 0; i < vec_len(V->patches); i++)
    {
        PatchInfo* p = &V->patches[i];
        int32_t from = p->ofs;
        int32_t rel = 0;

        switch(p->kind)
        {
            case PATCH_LEA_REL:
            case PATCH_JMP_REL:
            case PATCH_CALL_REL:
            {
                int32_t ofs = p->kind == PATCH_JMP_REL ? p->target->ofs : p->code->ofs;
                rel = ofs - (from + 4);
                break;
            }
            case PATCH_CALL_ABS:
            {
                /* offset = RVA_IAT - next_instruction_offset */
                uint32_t iat_rva = rva_import(p->label);
                uint32_t next_instr = from + 4;
                rel = iat_rva - next_instr;
                break;
            }
            case PATCH_LEA_ABS:
            {
                /* offset = RVA_IAT - next_instruction_offset */
                uint32_t iat_rva = rva_str(p->label);
                uint32_t next_instr = from + 4;
                rel = iat_rva - next_instr;
                break;
            }
            default:
                vp_assertX(0, "?");
                break;
        }

        uint8_t* code = (uint8_t*)(V->code.b + p->ofs);
        *(uint32_t*)code = (uint32_t)rel;
    }
}