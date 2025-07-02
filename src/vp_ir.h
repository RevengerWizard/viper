/*
** vp_ir.h
** Intermediate representation
*/

#ifndef _VP_IR_H
#define _VP_IR_H

#include "vp_str.h"
#include "vp_type.h"

/* Frame offset */
typedef struct FrameInfo
{
    int32_t ofs;
} FrameInfo;

/* Virtual register flags */
#define VRFDEF(_) \
    _(CONST, 0)         /* Integer constant */ \
    _(FLO, 1)           /* Floating */ \
    _(REF, 2)           /* Reference & */ \
    _(PARAM, 3)         /* Function parameter */ \
    _(STACK_PARAM, 4)   /* Stack parameter (spilled) */ \
    _(SPILL, 5)         /* Spilled register */ \
    _(NO_SPILL, 6)      /* Stop spilling */ \

#define VRF_MASK (VRF_FLO)

enum
{
#define VRFENUM(name, bit) VRF_##name = (1 << bit),
    VRFDEF(VRFENUM)
#undef VRFENUM
};

#define vrf_const(vr) ((vr)->flag & VRF_CONST)
#define vrf_flo(vr) ((vr)->flag & VRF_FLO)
#define vrf_spill(vr) ((vr)->flag & VRF_SPILL)

/* Size of a virtual register */
typedef enum VRSize
{
    VRSize1,
    VRSize2,
    VRSize4,
    VRSize8,
} VRSize;

#define REG_NO ((uint32_t)-1)

/* Virtual register */
typedef struct VReg
{
    VRSize vsize;
    uint8_t flag;
    union
    {
        /* Non-const */
        struct
        {
            struct VReg* vreg;
            uint32_t virt;  /* Virtual reg number */
            uint32_t phys;  /* Physical reg number */
            uint32_t param; /* Index of function parameter register, if any */
            FrameInfo fi;   /* Frame info for spilled register */
        };
        /* Constant */
        int64_t i64;
        double n;
    };
} VReg;

/* Viper IR instructions */
#define IRDEF(_) \
    _(BOFS, dst)    /* dst = [rbp + offset] */ \
    _(IOFS, dst)    /* dst = [rip + label] */ \
    _(SOFS, dst)    /* dst = [rsp + offset] */ \
    _(MOV, d12)     /* dst = src1 */ \
    _(STORE, d12)   /* [src2] = src1 */ \
    _(LOAD, d12)    /* dst = [src1] */ \
    _(STORE_S, ___) /* [src2 (spill)] = src1 */ \
    _(LOAD_S, ___)  /* dst = [src1 (spill)] */ \
    _(RET, src1) \
    _(COND, d12) \
    _(JMP, d12) \
    _(MEMZERO, dst) \
    _(MEMCPY, dst) \
    _(PUSHARG, d12) \
    _(CALL, d12) \
    _(CAST, d12) \
    /* Binary operators */ \
    _(ADD, d12) _(SUB, d12) \
    _(MUL, d12) _(DIV, d12) _(MOD, d12) \
    _(BAND, d12) _(BOR, d12) _(BXOR, d12) \
    _(LSHIFT, d12) _(RSHIFT, d12) \
    /* Unary operators */ \
    _(NEG, d12) \
    _(BNOT, d12)

typedef enum IrKind
{
#define IRENUM(name, sp) IR_##name,
    IRDEF(IRENUM)
#undef IRENUM
} IrKind;

/* Condition flags (lower bits) */
typedef enum CondKind
{
    COND_NONE,
    COND_ANY,
    COND_EQ,
    COND_NEQ,
    COND_LT,
    COND_LE,
    COND_GT,
    COND_GE,

    /* Condition masks */
    COND_MASK = 0x07,
    COND_UNSIGNED = 1 << 3,
    COND_FLO = 1 << 4,

    /* Unsigned masks */
    COND_EQ_U = COND_EQ | COND_UNSIGNED,
    COND_NEQ_U = COND_NEQ | COND_UNSIGNED,
    COND_LT_U = COND_LT | COND_UNSIGNED,
    COND_GT_U = COND_GT | COND_UNSIGNED,
    COND_LE_U = COND_LE | COND_UNSIGNED,
    COND_GE_U = COND_GE | COND_UNSIGNED,
} CondKind;

typedef struct IRCallInfo
{
    uint32_t argnum;    /* Number of arguments */
    uint32_t regargs;   /* Number of register arguments */
    uint32_t stacksize; /* Stack space for arguments */
    Str* label;
    VReg** args;
} IRCallInfo;

/* IR flags */
enum
{
    IRF_UNSIGNED = 1 << 0
};

#define irf_unsigned(ir) ((ir)->flag & IRF_UNSIGNED)

typedef struct IR
{
    IrKind kind;
    uint8_t flag;
    VReg* dst;
    VReg* src1;
    VReg* src2;
    union
    {
        CondKind cond;
        struct
        {
            int64_t ofs;
            Str* label;
        } iofs;
        struct
        {
            FrameInfo* fi;
            int64_t ofs;
        } bofs;
        struct
        {
            uint32_t ofs;
        } sofs;
        struct
        {
            struct BB* bb;
            CondKind cond; 
        } jmp;
        struct
        {
            uint32_t size;
        } mem;
        struct
        {
            uint32_t idx;   /* Parameter index */
        } arg;
        IRCallInfo* call;
    };
} IR;

/* Basic blocks */
typedef struct BB
{
    struct BB* next;
    Str* label;
    IR** irs;
} BB;

extern const char* const vp_ir_name[];

/* IR instructions */
IR* vp_ir_bofs(FrameInfo* fi);
IR* vp_ir_iofs(Str* label);
IR* vp_ir_sofs(uint32_t ofs);
IR* vp_ir_mov(VReg* dst, VReg* src, uint8_t flag);
IR* vp_ir_store(VReg* dst, VReg* src, uint8_t flag);
IR* vp_ir_load(VReg* src, VRSize vsize, uint8_t flag);
IR* vp_ir_store_s(VReg* dst, VReg* src);
IR* vp_ir_load_s(VReg* dst, VReg* src, uint8_t flag);
IR* vp_ir_ret(VReg* src, uint8_t flag);
IR* vp_ir_cond(VReg* src1, VReg* src2, CondKind cond);
IR* vp_ir_jmp(BB* bb);
void vp_ir_cjmp(VReg* src1, VReg* src2, CondKind cond, BB* bb);
IR* vp_ir_memzero(VReg* dst, uint32_t size);
IR* vp_ir_memcpy(VReg* dst, VReg* src, uint32_t size);
IR* vp_ir_pusharg(VReg* src, uint32_t idx);
IR* vp_ir_call(IRCallInfo* ci, VReg* dst, VReg* freg);
IR* vp_ir_cast(VReg* src, VRSize dstsize, uint8_t vflag);
VReg* vp_ir_binop(IrKind kind, VReg* src1, VReg* src2, VRSize vsize, uint8_t flag);
VReg* vp_ir_unary(IrKind kind, VReg* src, VRSize vsize, uint8_t flag);

/* Frame info */
FrameInfo* vp_frameinfo_new();

/* IR call info */
IRCallInfo* vp_ircallinfo_new(VReg** args, uint32_t argnum, Str* label);

/* Conditions */
CondKind vp_cond_swap(CondKind cond);
CondKind vp_cond_invert(CondKind cond);

/* Basic blocks */
BB* vp_bb_new();
void vp_bb_setcurr(BB* bb);

Str* vp_label_new();

static VP_AINLINE uint8_t ir_flag(Type* ty)
{
    return ty_isunsigned(ty) ? IRF_UNSIGNED : 0;
}

static VP_AINLINE uint8_t cond_flag(Type* ty)
{
    uint8_t flag = 0;
    if(ty_isunsigned(ty))
        flag = COND_UNSIGNED;
    if(ty_isflo(ty))
        flag |= COND_FLO;
    return flag;
}

#endif