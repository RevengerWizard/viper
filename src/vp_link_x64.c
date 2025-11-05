/*
** vp_link_x64.c
** Address patching
*/

#include "vp_link.h"

static uint32_t resolve_iat_rva(Str* label)
{
    if(memcmp(str_data(label), "GetStdHandle", label->len) == 0) return 0x1000 + 0x40;
    if(memcmp(str_data(label), "WriteConsoleA", label->len) == 0) return 0x1000 + 0x48;
    vp_assertX(0, "external function not found");
    return 0;
}

static uint32_t resolve_str_rva(Str* label)
{
    uint32_t i;
    for(i = 0; i < vec_len(V->strs); i++)
    {
        if(V->strs[i] == label)
            break;
    }
    return 0x2000 + V->strofs[i];
}

void vp_patch_infos(void)
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
                uint32_t iat_rva = resolve_iat_rva(p->label);
                uint32_t next_instr = from + 4;
                rel = iat_rva - next_instr;
                break;
            }
            case PATCH_LEA_ABS:
            {
                /* offset = RVA_IAT - next_instruction_offset */
                uint32_t iat_rva = resolve_str_rva(p->label);
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