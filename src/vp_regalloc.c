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

/* Spawn an integer vreg */
VReg* vp_vreg_ki(int64_t i64, VRSize vsize)
{
    RegAlloc* ra = V->ra;
    for(uint32_t i = 0; i < vec_len(ra->vconsts); i++)
    {
        VReg* vr = ra->vconsts[i];
        if(!vrf_flo(vr) && vr->i64 == i64 && vr->vsize == vsize)
            return vr;
    }
    VReg* vr = vp_ra_spawn(vsize, VRF_CONST);
    vr->i64 = i64;
    return vr;
}

/* Spawn a float vreg */
VReg* vp_vreg_kf(double n, VRSize vsize)
{
    RegAlloc* ra = V->ra;
    union { double d; uint64_t q; } u1, u2;
    u1.d = n;
    for(uint32_t i = 0; i < vec_len(ra->vconsts); i++)
    {
        VReg* vr = ra->vconsts[i];
        if(vrf_flo(vr))
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

VRSize vp_vreg_elem(uint32_t size, uint32_t align)
{
    uint32_t s = MIN(align, size);
    if(!IS_POW2(s) || s > TARGET_PTR_SIZE)
    {
        for(s = TARGET_PTR_SIZE; s > 1; s >>= 1)
        {
            if(s <= size && size % s == 0)
                break;
        }
    }
    vp_assertX(s > 0, "s == 0");
    return vp_msb(s);
}

/* -- Liveness analysis --------------------------------------------- */

/* Physical register set */
typedef struct
{
    LiveInterval** active;
    uint32_t len;
    uint32_t physmax;   /* Max # of physical registers */
    RegSet phystemp;  /* Bitmask of volatile registers */
    RegSet usebits;   /* Current register bits in use */
    RegSet bits;      /* Final register bits */
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
    RegSet usebits = p->usebits;
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
    int d = ia->start - ib->start;
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
static void live_occupy(RegAlloc* ra, LiveInterval** actives, RegSet ioccupy, RegSet foccupy)
{
    for(uint32_t k = 0; k < vec_len(actives); k++)
    {
        LiveInterval* li = actives[k];
        VReg* vr = ra->vregs[li->virt];
        vp_assertX(vr, "empty vreg");
        li->regbits |= vrf_flo(vr) ? foccupy : ioccupy;
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
    vec_t(LiveInterval*) inactives = NULL;
    vec_t(LiveInterval*) actives = NULL;

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
    RegSet iargset = 0, fargset = 0;
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
                RegSet ioccupy = set->extra(ra, ir);
                if(ioccupy != 0)
                {
                    live_occupy(ra, actives, ioccupy, 0);
                }
            }

            if(iargset != 0 || fargset != 0)
            {
                live_occupy(ra, actives, iargset, fargset);
            }

            /* Deactivate registers which end at this IR */
            for(uint32_t k = 0; k < vec_len(actives); k++)
            {
                LiveInterval* li = actives[k];
                if(li->end != REG_NO && li->end <= nip)
                {
                    vec_remove_at(actives, k); k--;
                }
            }

            /* Map function parameters to registers */
            if(ir->kind == IR_PUSHARG)
            {
                VReg* src = ir->src1;
                if(vrf_flo(src))
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
                RegSet ibroke = set->itemp;
                RegSet fbroke = set->ftemp;
                live_occupy(ra, actives, ibroke, fbroke);
                iargset = fargset = 0;
            }

            /* Activate registers after usage */
            while(vec_len(inactives) > 0)
            {
                LiveInterval* li = inactives[0];
                if(li->start != REG_NO && li->start > nip)
                    break;
                vec_remove_at(inactives, 0);
                vec_push(actives, li);
            }
        }
    }

    vec_free(inactives);
    vec_free(actives);
}

static void live_set_inout(vec_t(VReg*) vregs, LiveInterval* intervals, uint32_t nip)
{
    for(uint32_t i = 0; i < vec_len(vregs); i++)
    {
        VReg* vr = vregs[i];
        LiveInterval* li = &intervals[vr->virt];
        if(vr->flag & VRF_PARAM)
        {
            /* Keep live interval start as is for parameters */
        }
        else
        {
            if(li->start == REG_NO || li->start > nip)
            {
                li->start = nip;
            }
        }
        if(li->end == REG_NO || li->end < nip)
        {
            li->end = nip;
        }
    }
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
        vp_assertX(!vrf_const(vr), "const vreg");
        if(vrf_spill(vr))
        {
            li->state = LI_SPILL;
            li->phys = vr->phys;
        }
    }

    uint32_t nip = 0;   /* IR position */
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        live_set_inout(bb->inregs, intervals, nip);
        for(uint32_t j = 0; j < vec_len(bb->irs); j++, nip++)
        {
            IR* ir = bb->irs[j];
            /* Live intervals from dst, src1, src2 */
            VReg* vregs[] = {ir->dst, ir->src1, ir->src2};
            for(uint32_t k = 0; k < 3; k++)
            {
                VReg* vr = vregs[k];
                if(vr == NULL || vrf_const(vr))
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
        live_set_inout(bb->outregs, intervals, nip);
    }
}

/* -- Register allocation ------------------------------------------- */

enum
{
    src1 = 1 << 0,
    src2 = 1 << 1,
    dst = 1 << 2,
    d12 = dst | src1 | src2,
    ___ = REG_NO
};

static const int ir_spilltab[] = {
#define IRSPILL(name, sp) [IR_##name] = sp,
    IRDEF(IRSPILL)
#undef IRSPILL
};

/* Insert load/store spills in the IR */
static uint32_t ra_spill(RegAlloc* ra, BB** bbs)
{
    UNUSED(ra);
    uint32_t inserted = 0;
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        for(uint32_t j = 0; j < vec_len(bb->irs); j++)
        {
            IR* ir = bb->irs[j];
            uint32_t flag = ir_spilltab[ir->kind];
            if(flag == (uint32_t)___)
                continue;

            /* Insert spill for src1 */
            if(ir->src1 && (flag & src1) && vrf_spill(ir->src1))
            {
                VReg* sp = ir->src1;
                vp_assertX(!vrf_const(sp), "spill const vreg?");
                VReg* tmp = vp_ra_spawn(sp->vsize, VRF_NO_SPILL | (sp->flag & VRF_MASK));
                IR* is = vp_ir_load_s(tmp, sp, ir->flag);
                vec_insert(bb->irs, j, is); j++;
                ir->src1 = tmp;
                inserted++;
            }

            /* Insert spill for src2 */
            if(ir->src2 && (flag & src2) && vrf_spill(ir->src2))
            {
                VReg* sp = ir->src2;
                vp_assertX(!vrf_const(sp), "spill const vreg?");
                VReg* tmp = vp_ra_spawn(sp->vsize, VRF_NO_SPILL | (sp->flag & VRF_MASK));
                IR* is = vp_ir_load_s(tmp, sp, ir->flag);
                vec_insert(bb->irs, j, is); j++;
                ir->src2 = tmp;
                inserted++;
            }

            /* Insert spill for dst */
            if(ir->dst && (flag & dst) && vrf_spill(ir->dst))
            {
                VReg* sp = ir->dst;
                vp_assertX(!vrf_const(sp), "spill const vreg?");
                VReg* tmp = vp_ra_spawn(sp->vsize, VRF_NO_SPILL | (sp->flag & VRF_MASK));
                IR* is = vp_ir_store_s(ir->dst, tmp);
                j++; vec_insert(bb->irs, j, is);
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
        if(vrf_flo(vr))
        {
            prsp = &fregset;
        }

        bool useall = true;
        RegSet mask = prsp->phystemp;  /* Callee registers mask */
        uint32_t regno = REG_NO;
        uint32_t ip = vr->param;
        RegSet occupied = prsp->usebits | li->regbits;
        if(ip != REG_NO)
        {
            if(vrf_flo(vr))
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
            else
            {
                useall = false;
            }
        }
        if(regno == REG_NO)
        {
            for(uint32_t j = 0; j < prsp->physmax; j++)
            {
                if(useall)
                {
                    if(!(occupied & (1ULL << j)))
                    {
                        regno = j;
                        break;
                    }
                }
                else
                {
                    if(mask & (1ULL << j))
                    {
                        regno = j;
                        break;
                    }
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

    vp_mem_free(iregset.active);
    vp_mem_free(fregset.active);
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
                if(vrf_spill(vr))
                    continue;
                vreg_spill(vr);
            }
        }

        if(ra_spill(ra, bbs) == 0)
            break;

        if(vreglen != vec_len(ra->vregs))
        {
            vreglen = vec_len(ra->vregs);
            vp_mem_free(intervals);
            vp_mem_free(sorted);
            intervals = vp_mem_alloc(sizeof(*intervals) * vreglen);
            sorted = vp_mem_alloc(sizeof(*sorted) * vreglen);
        }
    }

    ra->intervals = intervals;
    ra->sorted = sorted;

    /* Map virtual registers to physical ones */
    for(uint32_t i = 0; i < vreglen; i++)
    {
        VReg* vr = ra->vregs[i];
        if(vr)
        {
            vp_assertX(!vrf_const(vr) && vr->virt != REG_NO, "invalid vreg");
            vr->phys = ra->intervals[vr->virt].phys;
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
VReg* vp_ra_spawn(VRSize vsize, uint8_t vflag)
{
    RegAlloc* ra = V->ra;
    VReg* vr = vp_arena_alloc(&V->irarena, sizeof(*vr));
    vr->vsize = vsize;
    vr->flag = vflag;
    if(!(vflag & VRF_CONST))
    {
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