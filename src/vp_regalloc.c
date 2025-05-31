/*
** vp_regalloc.c
** Register allocation
*/

#include "vp_regalloc.h"
#include "vp_mem.h"
#include "vp_state.h"
#include "vp_vec.h"

/* Create a new register allocator state */
RegAlloc* vp_regalloc_new()
{
    RegAlloc* ra = vp_arena_alloc(&V->irarena, sizeof(*ra));
    ra->vconsts = NULL;
    ra->vregs = NULL;
    return ra;
}

/* Spawn a new virtual register */
VReg* vp_regalloc_spawn(VRegSize vsize, uint8_t vflag)
{
    RegAlloc* ra = V->ra;
    VReg* vr = vp_arena_alloc(&V->irarena, sizeof(*vr));
    vr->vsize = vsize;
    vr->flag = vflag;
    if(!(vflag & VRF_CONST))
    {
        vr->vreg = vr;
        vr->virt = vec_len(ra->vregs);
        vr->phys = 0;
        vec_push(ra->vregs, vr);
    }
    else
    {
        vec_push(ra->vconsts, vr);
    }
    return vr;
}

VReg* vp_vreg_ku(uint64_t u64, VRegSize vsize)
{
    RegAlloc* ra = V->ra;
    for(uint32_t i = 0; i < vec_len(ra->vconsts); i++)
    {
        VReg* vr = ra->vconsts[i];
        if(!(vr->flag & VRF_NUM) && vr->u64 == u64 && vr->vsize == vsize)
            return vr;
    }
    VReg* vr = vp_regalloc_spawn(vsize, VRF_UINT);
    vr->u64 = u64;
    return vr;
}

VReg* vp_vreg_ki(int64_t i64, VRegSize vsize)
{
    RegAlloc* ra = V->ra;
    for(uint32_t i = 0; i < vec_len(ra->vconsts); i++)
    {
        VReg* vr = ra->vconsts[i];
        if(!(vr->flag & VRF_NUM) && vr->i64 == i64 && vr->vsize == vsize)
            return vr;
    }
    VReg* vr = vp_regalloc_spawn(vsize, VRF_INT);
    vr->i64 = i64;
    return vr;
}

VReg* vp_vreg_kf(double n, VRegSize vsize)
{
    RegAlloc* ra = V->ra;
    union { double d; uint64_t q; } u1, u2;
    u1.d = n;
    for(uint32_t i = 0; i < vec_len(ra->vconsts); i++)
    {
        VReg* vr = ra->vconsts[i];
        if(vr->flag & VRF_NUM)
        {
            u2.d = vr->n;
            if(u1.q == u2.q && vr->vsize == vsize)
                return vr;
        }
    }
    VReg* vr = vp_regalloc_spawn(vsize, VRF_NUM);
    vr->n = n;
    return vr;
}