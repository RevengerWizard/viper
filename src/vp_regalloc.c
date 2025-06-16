/*
** vp_regalloc.c
** Register allocation
*/

#include <stdlib.h>

#include "vp_regalloc.h"
#include "vp_ir.h"
#include "vp_mem.h"
#include "vp_state.h"
#include "vp_vec.h"

/* -- Virtual registers --------------------------------------------- */

/* Spawn an unsigned integer vreg */
VReg* vp_vreg_ku(uint64_t u64, VRegSize vsize)
{
    RegAlloc* ra = V->ra;
    for(uint32_t i = 0; i < vec_len(ra->vconsts); i++)
    {
        VReg* vr = ra->vconsts[i];
        if(!(vr->flag & VRF_FLO) && vr->u64 == u64 && vr->vsize == vsize)
            return vr;
    }
    VReg* vr = vp_ra_spawn(vsize, VRF_CONST | VRF_UNSIGNED);
    vr->u64 = u64;
    return vr;
}

/* Spawn a signed integer vreg */
VReg* vp_vreg_ki(int64_t i64, VRegSize vsize)
{
    RegAlloc* ra = V->ra;
    for(uint32_t i = 0; i < vec_len(ra->vconsts); i++)
    {
        VReg* vr = ra->vconsts[i];
        if(!(vr->flag & VRF_FLO) && vr->i64 == i64 && vr->vsize == vsize)
            return vr;
    }
    VReg* vr = vp_ra_spawn(vsize, VRF_CONST);
    vr->i64 = i64;
    return vr;
}

/* Spawn a floating vreg */
VReg* vp_vreg_kf(double n, VRegSize vsize)
{
    RegAlloc* ra = V->ra;
    union { double d; uint64_t q; } u1, u2;
    u1.d = n;
    for(uint32_t i = 0; i < vec_len(ra->vconsts); i++)
    {
        VReg* vr = ra->vconsts[i];
        if(vr->flag & VRF_FLO)
        {
            u2.d = vr->n;
            if(u1.q == u2.q && vr->vsize == vsize)
                return vr;
        }
    }
    VReg* vr = vp_ra_spawn(vsize, VRF_FLO | VRF_CONST);
    vr->n = n;
    return vr;
}

/* -- Liveness analysis --------------------------------------------- */

/* Physical register set */
typedef struct
{
    LiveInterval** active;
    uint32_t len;
    uint32_t physmax;   /* Max # of physical registers */
    uint32_t phystemp;  /* Bitmask of volatile registers */
    uint64_t usebits;   /* Current register bits in use */
    uint64_t bits;      /* Final register bits */
} PhysRegSet;

/* Insert an active live interval */
static uint32_t live_insert(LiveInterval** active, uint32_t len, LiveInterval* li)
{
    uint32_t j;
    for(j = 0; j < len; j++)
    {
        LiveInterval* p = active[j];
        if(li->end < p->end)
            break;
    }
    if(j < len)
    {
        memmove(&active[j + 1], &active[j], sizeof(LiveInterval*) * (len - j));
    }
    active[j] = li;
    return j;
}

/* Remove an active live interval */
static void live_remove(LiveInterval** active, uint32_t len, uint32_t start, uint32_t n)
{
    if(n == 0)
        return;
    uint32_t tail = len - (start + n);
    if(tail > 0)
    {
        memmove(&active[start], &active[start + n], sizeof(LiveInterval*) * tail);
    }
}

/* Expire old active intervals */
static void live_expire(PhysRegSet* p, uint32_t start)
{
    uint32_t len = p->len;
    uint64_t usebits = p->usebits;
    uint32_t j;
    for(j = 0; j < len; j++)
    {
        LiveInterval* li = p->active[j];
        if(li->end > start)
            break;
        uint32_t phys = li->phys;
        usebits &= ~(1ULL << phys);
    }
    live_remove(p->active, len, 0, j);
    p->len = len - j;
    p->usebits = usebits;
}

/* Sort liveness intervals */
static int live_sort(const void* pa, const void* pb)
{
    LiveInterval* ia = *(LiveInterval**)pa, *ib = *(LiveInterval**)pb;
    uint32_t d = ia->start - ib->start;
    if(d == 0)
    {
        d = ib->end - ia->end;
        if(d == 0)
        {
            d = ia->virt - ib->virt;
        }
    }
    return d;
}

/* Occupy a set of active integer/float registers */
static void live_occupy(RegAlloc* ra, LiveInterval** actives, uint64_t ioccupy, uint64_t foccupy)
{
    for(uint32_t k = 0; k < vec_len(actives); k++)
    {
        LiveInterval* li = actives[k];
        VReg* vr = ra->vregs[li->virt];
        vp_assertX(vr, "empty vreg");
        li->regbits |= (vr->flag & VRF_FLO) ? foccupy : ioccupy;
    }
}

/* Spill a live interval */
static void live_spill(RegAlloc* ra, LiveInterval** active, uint32_t len, LiveInterval* li)
{
    vp_assertX(len > 0, "empty active list");
    LiveInterval* spill = active[len - 1];
    if(spill->end > li->end)
    {
        li->phys = spill->phys;
        spill->phys = ra->set->iphysmax;
        spill->state = LI_SPILL;
        live_insert(active, len - 1, li);
    }
    else
    {
        li->phys = ra->set->iphysmax;
        li->state = LI_SPILL;
    }
}

/* Detect occupied registers and flags */
static void live_detect(RegAlloc* ra, BB** bbs, uint32_t vreglen, LiveInterval** sorted)
{
    LiveInterval** inactives = NULL;
    LiveInterval** actives = NULL;

    /* Initialize active/inactive intervals */
    for(uint32_t i = 0; i < vreglen; i++)
    {
        LiveInterval* li = sorted[i];
        if(li->end == REG_NO)
            continue;
        if(li->start == REG_NO)
        {
            vec_push(actives, li);
        }
        else
        {
            vec_push(inactives, li);
        }
    }

    const RASettings* set = ra->set;
    uint64_t iargset = 0, fargset = 0;
    uint32_t nip = 0;   /* IR position */
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        for(uint32_t j = 0; j < vec_len(bb->irs); j++, nip++)
        {
            IR* ir = bb->irs[j];
            /* Detect extra occupied registers */
            if(set->extra != NULL)
            {

            }

            if(iargset != 0 || fargset != 0)
            {
                live_occupy(ra, actives, iargset, fargset);
            }

            /* Deactivate registers which end at this IR */
            for(uint32_t k = 0; k < vec_len(actives); k++)
            {
                LiveInterval* li = actives[k];
                if(li->end <= nip)
                {
                    vec_remove_at(actives, k); k--;
                }
            }

            /* Map function parameters to registers */
            if(ir->kind == IR_PUSHARG)
            {
                VReg* src = ir->src1;
                if(src->flag & VRF_FLO)
                {
                    uint64_t n = set->fmap[ir->arg.idx];
                    fargset |= 1ULL << n;
                }
                else
                {
                    uint64_t n = set->imap[ir->arg.idx];
                    iargset |= 1ULL << n;
                }
            }

            /* Call instruction breaks temporary registers */
            if(ir->kind == IR_CALL)
            {
                uint64_t ibroke = set->itemp;
                uint64_t fbroke = set->ftemp;
                live_occupy(ra, actives, ibroke, fbroke);
                iargset = fargset = 0;
            }

            /* Activate registers after usage */
            while(vec_len(inactives) > 0)
            {
                LiveInterval* li = inactives[0];
                if(li->start > nip)
                    break;
                vec_remove_at(inactives, 0);
                vec_push(actives, li);
            }
        }
    }

    vec_free(inactives);
    vec_free(actives);
}

/* Build live intervals by scanning the IR instructions */
static void live_build(RegAlloc* ra, BB** bbs, uint32_t vreglen, LiveInterval* intervals)
{
    /* Initialize all intervals */
    for(uint32_t i = 0; i < vreglen; i++)
    {
        LiveInterval* li = &intervals[i];
        li->state = LI_NORMAL;
        li->start = REG_NO;
        li->end = REG_NO;
        li->virt = i;
        li->phys = REG_NO;
        li->regbits = 0;
        /* Spill invervals */
        VReg* vr = ra->vregs[i];
        if(vr == NULL)
            continue;
        vp_assertX(!(vr->flag & VRF_CONST), "const vreg");
        if(vr->flag & VRF_SPILL)
        {
            li->state = LI_SPILL;
            li->phys = vr->phys;
        }
    }

    uint32_t nip = 0;   /* IR position */
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        for(uint32_t j = 0; j < vec_len(bb->irs); j++, nip++)
        {
            IR* ir = bb->irs[j];
            /* Live intervals from dst, src1, src2 */
            VReg* vregs[] = {ir->dst, ir->src1, ir->src2};
            for(uint32_t k = 0; k < 3; k++)
            {
                VReg* vr = vregs[k];
                if(vr == NULL || (vr->flag & VRF_CONST))
                    continue;
                
                LiveInterval* li = &intervals[vr->virt];
                if(li->start == REG_NO && !(vr->flag & VRF_PARAM))
                {
                    li->start = nip;
                }
                if(li->end == REG_NO || li->end < nip)
                {
                    li->end = nip;
                }
            }
        }
    }
}

/* -- Register allocation ------------------------------------------- */

enum
{
    src1 = 1 << 0,
    src2 = 1 << 1,
    dst = 1 << 2,
    d12 = dst | src1 | src2,
    ___ = -1
};

static const int ir_spilltab[] = {
#define IRSPILL(name, sp) [IR_##name] = sp,
    IRDEF(IRSPILL)
#undef IRSPILL
};

/* Insert load/store spills in the IR */
static uint32_t ra_spill(RegAlloc* ra, BB** bbs)
{
    uint32_t inserted = 0;
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        IR** irs = bb->irs;
        for(uint32_t j = 0; j < vec_len(irs); j++)
        {
            IR* ir = irs[j];
            uint32_t flag = ir_spilltab[ir->kind];
            if(flag == ___)
                continue;

            /* Insert spill for src1 */
            if(ir->src1 && (flag & src1) && (ir->src1->flag & VRF_SPILL))
            {
                VReg* sp = ir->src1;
                vp_assertX(!(sp->flag & VRF_CONST), "spill const vreg?");
                VReg* tmp = vp_ra_spawn(sp->vsize, VRF_NO_SPILL);
                IR* is = vp_ir_load_s(tmp, sp, tmp->vsize);
                vec_insert(irs, j, is); j++;
                ir->src1 = tmp;
                inserted++;
            }
            
            /* Insert spill for src2 */
            if(ir->src2 && (flag & src2) && (ir->src2->flag & VRF_SPILL))
            {
                VReg* sp = ir->src2;
                vp_assertX(!(sp->flag & VRF_CONST), "spill const vreg?");
                VReg* tmp = vp_ra_spawn(sp->vsize, VRF_NO_SPILL);
                IR* is = vp_ir_load_s(tmp, sp, tmp->vsize);
                vec_insert(irs, j, is); j++;
                ir->src2 = tmp;
                inserted++;
            }

            /* Insert spill for dst */
            if(ir->dst && (flag & dst) && (ir->dst->flag & VRF_SPILL))
            {
                VReg* sp = ir->dst;
                vp_assertX(!(sp->flag & VRF_CONST), "spill const vreg?");
                VReg* tmp = vp_ra_spawn(sp->vsize, VRF_NO_SPILL);
                IR* is = vp_ir_store_s(tmp, sp);
                vec_insert(irs, j, is); j++;
                ir->dst = tmp;
                inserted++;
            }
        }
    }
    return inserted;
}

/* Linear scan register allocation */
static void ra_scan(RegAlloc* ra, LiveInterval** sorted, uint32_t vreglen)
{
    PhysRegSet iregset = {
        .active = vp_mem_alloc(sizeof(LiveInterval*) * ra->set->iphysmax),
        .len = 0,
        .physmax = ra->set->iphysmax,
        .phystemp = ra->set->itemp,
        .usebits = 0,
        .bits = 0
    };

    PhysRegSet fregset = {
        .active = vp_mem_alloc(sizeof(LiveInterval*) * ra->set->fphysmax),
        .len = 0,
        .physmax = ra->set->fphysmax,
        .phystemp = ra->set->ftemp,
        .usebits = 0,
        .bits = 0
    };

    for(uint32_t i = 0; i < vreglen; i++)
    {
        LiveInterval* li = sorted[i];
        VReg* vr = ra->vregs[li->virt];
        if(vr == NULL)
            continue;
        if(li->state != LI_NORMAL)
            continue;
        /* Expire integer and float register sets */
        live_expire(&iregset, li->start);
        live_expire(&fregset, li->start);

        PhysRegSet* prsp = &iregset;
        if(vr->flag & VRF_FLO)
        {
            prsp = &fregset;
        }
        
        uint32_t mask = prsp->phystemp;  /* Volatile registers mask */
        uint32_t regno = REG_NO;
        uint32_t ip = vr->param;
        uint64_t occupied = prsp->usebits | li->regbits;
        if(ip != REG_NO)
        {
            if(vr->flag & VRF_FLO)
            {
                /* Map floating registers */
                ip = ra->set->fmap[ip];
            }
            else
            {
                /* Map integer registers */
                ip = ra->set->imap[ip];
            }

            if(!(occupied & (1ULL << ip)))
            {
                regno = ip;
            }
        }
        if(regno == REG_NO)
        {
            for(uint32_t j = 0; j < prsp->physmax; j++)
            {
                if(!(occupied & (1ULL << j)) && (mask & (1ULL << j)))
                {
                    regno = j;
                    break;
                }
            }
        }
        if(regno != REG_NO)
        {
            li->phys = regno;
            prsp->usebits |= 1ULL << regno;
            live_insert(prsp->active, prsp->len, li);
            prsp->len++;
        }
        else
        {
            live_spill(ra, prsp->active, prsp->len, li);
        }

        prsp->bits |= prsp->usebits;
    }

    ra->iregbits = iregset.bits;
    ra->fregbits = fregset.bits;

    free(iregset.active);
    free(fregset.active);
}

/* Allocate physical registers */
void vp_ra_alloc(RegAlloc *ra, BB** bbs)
{
    uint32_t vreglen = vec_len(ra->vregs);

    LiveInterval* intervals = vp_mem_alloc(sizeof(*intervals) * vreglen);
    LiveInterval** sorted = vp_mem_alloc(sizeof(*sorted) * vreglen);

    while(true)
    {
        live_build(ra, bbs, vreglen, intervals);

        /* Sort by start, end */
        for(uint32_t i = 0; i < vreglen; i++)
        {
            sorted[i] = &intervals[i];
        }
        qsort(sorted, vreglen, sizeof(*sorted), live_sort);

        live_detect(ra, bbs, vreglen, sorted);
        ra_scan(ra, sorted, vreglen);

        /* Spill vregs */
        for(uint32_t i = 0; i < vreglen; i++)
        {
            LiveInterval* li = &intervals[i];
            if(li->state == LI_SPILL)
            {
                VReg* vr = ra->vregs[i];
                if(vr->flag & VRF_SPILL)
                    continue;
                vreg_spill(vr);
            }
        }

        if(ra_spill(ra, bbs) == 0)
            break;

        if(vreglen != vec_len(ra->vregs))
        {
            vreglen = vec_len(ra->vregs);
            free(intervals);
            free(sorted);
            intervals = vp_mem_alloc(sizeof(*intervals) * vreglen);
            sorted = vp_mem_alloc(sizeof(*sorted) * vreglen);
        }
    }

    ra->intervals = intervals;
    ra->sorted = sorted;

    /* Map virtual registers to physical ones */
    for(uint32_t i = 0; i < vreglen; i++)
    {
        LiveInterval* li = &intervals[i];
        VReg* vr = ra->vregs[i];
        if(vr)
        {
            vp_assertX(!(vr->flag & VRF_CONST), "const vreg");
            vr->phys = li->phys;
        }
    }
}

/* Create a new register allocator state */
RegAlloc* vp_ra_new(const RASettings* set)
{
    RegAlloc* ra = vp_arena_alloc(&V->irarena, sizeof(*ra));
    ra->set = set;
    ra->vconsts = NULL;
    ra->vregs = NULL;
    ra->intervals = NULL;
    ra->sorted = NULL;
    ra->iregbits = 0;
    ra->fregbits = 0;
    ra->flag = 0;
    return ra;
}

/* Spawn a new virtual register */
VReg* vp_ra_spawn(VRegSize vsize, uint8_t vflag)
{
    RegAlloc* ra = V->ra;
    VReg* vr = vp_arena_alloc(&V->irarena, sizeof(*vr));
    vr->vsize = vsize;
    vr->flag = vflag;
    if(!(vflag & VRF_CONST))
    {
        vr->vreg = vr;
        vr->virt = vec_len(ra->vregs);
        vr->phys = REG_NO;
        vr->param = REG_NO;
        vr->fi.ofs = 0;
        vec_push(ra->vregs, vr);
    }
    else
    {
        vec_push(ra->vconsts, vr);
    }
    return vr;
}