/*
** vp_opt.c
** Optimization pass (IR -> (opt) -> IR)
*/

#include "vp_opt.h"
#include "vp_ir.h"
#include "vp_mem.h"
#include "vp_regalloc.h"
#include "vp_tab.h"
#include "vp_vec.h"

/* Get last JMP IR, if any */
static IR* ir_lastjmp(BB* bb)
{
    uint32_t len = vec_len(bb->irs);
    IR* ir;
    if(len > 0 && (ir = bb->irs[len - 1])->kind == IR_JMP)
        return ir;
    return NULL;
}

/* Replace jump destination */
static void jmp_replace(vec_t(BB*) bbs, BB* dst, BB* src)
{
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        if(bb == src)
            continue;

        IR* ir = ir_lastjmp(bb);
        if(ir != NULL && ir->jmp.bb == src)
        {
            ir->jmp.bb = dst;
        }
    }
}

static void bb_remove_unused(vec_t(BB*)* bbs)
{
    Tab keep;
    while(true)
    {
        vp_tab_init(&keep);
        vp_assertX(vec_len(*bbs) > 0, "len bbs == 0");

        for(uint32_t i = 0; i < vec_len(*bbs); i++)
        {
            BB* bb = (*bbs)[i];
            bool rem = false;
            IR* irjmp = ir_lastjmp(bb);
            if(irjmp && irjmp->jmp.bb == bb->next)  /* .L1: JMP .L2; .L2 ...  */
            {
                /* Remove jump to next instruction */
                vec_pop(bb->irs);
                irjmp = NULL;
            }
            if(vec_len(bb->irs) == 0 && bb->next != NULL)
            {
                /* Empty BB */
                jmp_replace(*bbs, bb->next, bb);
                rem = true;
            }
            else if(vec_len(bb->irs) == 1 && irjmp != NULL &&
                    irjmp->jmp.cond == COND_ANY &&
                    bb != irjmp->jmp.bb)
            {
                /* Single jump */
                jmp_replace(*bbs, irjmp->jmp.bb, bb);
                if(i > 0)
                {
                    BB* pbb = (*bbs)[i - 1];
                    IR* ir0 = ir_lastjmp(pbb);
                    if(ir0 != NULL && ir0->jmp.cond != COND_ANY &&
                        ir0->jmp.bb == bb->next &&
                        !(ir0->jmp.cond & COND_FLO))
                    {
                        ir0->jmp.cond = vp_cond_invert(ir0->jmp.cond);
                        ir0->jmp.bb = irjmp->jmp.bb;
                        rem = true;
                    }
                }
            }

            if(irjmp != NULL)
            {
                vp_tab_set(&keep, irjmp->jmp.bb->label, bb);
            }
            if((irjmp == NULL || irjmp->jmp.cond != COND_ANY) && bb->next != NULL)
            {
                vp_tab_set(&keep, bb->next->label, bb);
            }

            if(rem)
            {
                vp_tab_remove(&keep, bb->label);
            }
        }

        bool again = false;
        for(uint32_t i = 1; i < vec_len(*bbs); i++)
        {
            BB* bb = (*bbs)[i];
            if(!vp_tab_get(&keep, bb->label))
            {
                if(i > 0)
                {
                    BB* pbb = (*bbs)[i - 1];
                    pbb->next = bb->next;
                }

                vec_remove_at(*bbs, i);
                i--;
                again = true;
            }
        }
        if(!again)
            break;
    }
}

static void vreg_remove_unused(RegAlloc* ra, vec_t(BB*) bbs)
{
    uint32_t len = vec_len(ra->vregs);
    uint8_t* vread = vp_mem_calloc(1, len);
    while(true)
    {
        /* Ignore parameters and & ref taken */
        for(uint32_t i = 0; i < len; i++)
        {
            VReg* vr = ra->vregs[i];
            vread[i] = vr != NULL && (vr->flag & (VRF_PARAM | VRF_REF)) != 0;
        }

        /* Check vreg usage */
        for(uint32_t i = 0; i < vec_len(bbs); i++)
        {
            BB* bb = bbs[i];
            for(uint32_t j = 0; j < vec_len(bb->irs); j++)
            {
                IR* ir = bb->irs[j];
                VReg* vregs[] = { ir->src1, ir->src2 };
                for(uint32_t k = 0; k < 2; k++)
                {
                    VReg* vr = vregs[k];
                    if(vr && !vrf_const(vr))
                    {
                        vread[vr->virt] = true;
                    }
                }
            }
        }

        /* Remove instruction if dst is unread */
        for(uint32_t i = 0; i < vec_len(bbs); i++)
        {
            BB* bb = bbs[i];
            for(uint32_t j = 0; j < vec_len(bb->irs); j++)
            {
                IR* ir = bb->irs[j];
                if(ir->dst == NULL || vread[ir->dst->virt])
                    continue;
                if(ir->kind == IR_CALL)
                {
                    /* Keep the call, discard result */
                    ir->dst = NULL;
                }
                else
                {
                    vec_remove_at(bb->irs, j);
                    j--;
                }
            }
        }

        /* Mark unused vregs */
        bool again = false;
        for(uint32_t i = 0; i < len; i++)
        {
            VReg* vr;
            if(!vread[i] && (vr = ra->vregs[i]) != NULL)
            {
                ra->vregs[i] = NULL;
                vr->flag |= VRF_UNUSED;
                again = true;
            }
        }
        if(!again)
            break;
    }

    vp_mem_free(vread);
}

/* Peephole optimizations */
static void peep(BB* bb)
{
    uint32_t irslen = vec_len(bb->irs);
    for(uint32_t i = 0; i < irslen; i++)
    {
        IR* ir = bb->irs[i];
        switch(ir->kind)
        {
        case IR_BOFS:
        case IR_IOFS:
            if(i < irslen - 1)
            {
                IR* next = bb->irs[i + 1];
                if((next->kind == IR_ADD || next->kind == IR_SUB) &&
                    next->src1 == ir->dst && vrf_const(next->src2))
                {
                    vp_assertX(!vrf_flo(next->src2), "float src2");
                    VReg* dst = next->dst;
                    int64_t ofs = next->src2->i64;
                    if(next->kind == IR_SUB)
                    {
                        ofs = -ofs;
                    }
                    *next = *ir;
                    next->dst = dst;
                    if(ir->kind == IR_BOFS)
                    {
                        next->bofs.ofs += ofs;
                    }
                    else
                    {
                        next->iofs.ofs += ofs;
                    }
                }
            }
            break;
        default:
            break;
        }
    }
}

/* Optimization pass */
void vp_opt(Code* code)
{
    RegAlloc* ra = code->ra;

    /* Clean up unused IRs */
    for(uint32_t i = 1; i < vec_len(code->bbs); i++)
    {
        BB* bb = code->bbs[i];
        if(vec_len(bb->frombbs) == 0)
        {
            vec_clear(bb->irs);
        }
    }

    /* Peephole */
    for(uint32_t i = 0; i < vec_len(code->bbs); i++)
    {
        BB* bb = code->bbs[i];
        peep(bb);
    }

    vreg_remove_unused(ra, code->bbs);
    bb_remove_unused(&code->bbs);

    vp_bb_detect(code->bbs);
}