/*
** vp_link_x64.c
** Address patching
*/

#include "vp_link.h"
#include "vp_state.h"
#include "vp_buf.h"

/*static Section* sec_find(Layout* L, SectionKind kind)
{
    for(uint32_t i = 0; i < vec_len(L->secs); i++)
    {
        if(L->secs[i].kind == kind)
            return &L->secs[i];
    }
    return NULL;
}*/

void vp_layout_init(Layout* L)
{
    {
        Section sec = {.kind = SEC_TEXT};
        vec_push(L->secs, sec);
    }

    {
        Section sec = {.kind = SEC_DATA};
        vec_push(L->secs, sec);
    }

    {
        Section sec = {.kind = SEC_RDATA};
        vec_push(L->secs, sec);
    }

    L->nsecs = vec_len(L->secs);
}

static void layout_data()
{
    Section* datasec = NULL;
    Section* rdatasec = NULL;

    /* Find sections */
    for(uint32_t i = 0; i < vec_len(V->L.secs); i++)
    {
        if(V->L.secs[i].kind == SEC_DATA)
            datasec = &V->L.secs[i];
        else if(V->L.secs[i].kind == SEC_RDATA)
            rdatasec = &V->L.secs[i];
    }

    vp_assertX(datasec && rdatasec, "missing data sections");

    /* Sort globals: variables in .data, strings in .rdata */
    for(uint32_t i = 0; i < vec_len(V->globdata); i++)
    {
        DataEntry* entry = V->globdata[i];
        Section* sec = NULL;
        /* Select target section based on kind */
        switch(entry->kind)
        {
            case DATA_VAR:
            case DATA_ANON:
                sec = datasec;
                break;
            case DATA_STR:
                sec = rdatasec;
                break;
            default:
                vp_assertX(0, "unknown data kind");
        }

        /* Align current section offset */
        uint32_t offset = sbuf_len(&sec->sb);
        offset = ALIGN_UP(offset, entry->align);

        /* Pad to alignment */
        while(sbuf_len(&sec->sb) < offset)
        {
            vp_buf_putb(&sec->sb, 0);
        }

        /* Record offset */
        entry->ofs = offset;
        vp_assertX(entry->name, "no entry name");

        printf("OFFSET %s (kind=%d) IS %u in section %s\n",
            str_data(entry->name),
            entry->kind,
            offset,
            sec == datasec ? ".data" : ".rdata");

        /* Emit data */
        vp_buf_putmem(&sec->sb, entry->data, entry->size);
    }
}

void vp_link(void)
{
    layout_data();
    for(uint32_t i = 0; i < vec_len(V->patches); i++)
    {
        PatchInfo* p = &V->patches[i];
        int32_t from = p->ofs;
        int32_t rel = 0;

        switch(p->kind)
        {
            case PATCH_JMP_REL:
            case PATCH_LEA_REL:
            {
                int32_t ofs = p->kind == PATCH_JMP_REL ? p->target->ofs : p->code->ofs;
                rel = ofs - (from + 4);
                break;
            }
            case PATCH_CALL_REL:
            case PATCH_CALL_ABS:
            case PATCH_LEA_ABS:
                continue;
            default:
                vp_assertX(0, "?");
                break;
        }

        uint8_t* code = (uint8_t*)(V->code.b + p->ofs);
        *(uint32_t*)code = (uint32_t)rel;
    }

    uint32_t csize = sbuf_len(&V->code);
    Section* sec = &V->L.secs[SEC_TEXT];
    char* p = vp_buf_need(&sec->sb, csize);
    memcpy(p, V->code.b, csize);
    sec->sb.w = p + csize;
}