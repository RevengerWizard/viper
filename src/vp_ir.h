/*
** vp_ir.h
** Intermediate representation
*/

#ifndef _VP_IR_H
#define _VP_IR_H

#include "vp_str.h"

/* Frame offset */
typedef struct FrameInfo
{
    int64_t ofs;
} FrameInfo;

#define VRF_INT (1 << 0)    /* Signed integer */
#define VRF_UINT (1 << 1)   /* Unsigned integer */
#define VRF_NUM (1 << 2)    /* Floating point */
#define VRF_REF (1 << 3)    /* Reference & */
#define VRF_PARAM (1 << 4)  /* Function parameter */
#define VRF_STACK_PARAM (1 << 5)  /* Stack parameter (spilled) */
#define VRF_SPILL (1 << 6)  /* Spilled register */
#define VRF_NO_SPILL (1 << 7)   /* Stop spilling */

#define VRF_CONST (VRF_INT | VRF_UINT | VRF_NUM)

/* Size of a virtual register */
typedef enum VRegSize
{
    VRegSize1,
    VRegSize2,
    VRegSize4,
    VRegSize8,
} VRegSize;

/* Virtual register */
typedef struct VReg
{
    VRegSize vsize;
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
        uint64_t u64;
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
    _(NOT, d12) \
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
    COND_GE
} CondKind;

/* Condition masks */
enum
{
    COND_MASK = 0x07,
    COND_UNSIGNED = 1 << 3,
    COND_NUM = 1 << 4
};

typedef struct IRCallInfo
{
    uint32_t argnum;    /* Number of arguments */
    uint32_t regargs;   /* Number of register arguments */
    uint32_t stacksize; /* Stack space for arguments */
    Str* label;
    VReg** args;
} IRCallInfo;

typedef struct IR
{
    IrKind kind;
    VReg* dst;
    VReg* src1;
    VReg* src2;
    union
    {
        CondKind cond;
        Str* label;
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
IR* vp_ir_mov(VReg* dst, VReg* src);
IR* vp_ir_store(VReg* dst, VReg* src);
IR* vp_ir_load(VReg* src, VRegSize vsize);
IR* vp_ir_store_s(VReg* dst, VReg* src);
IR* vp_ir_load_s(VReg* dst, VReg* src, VRegSize vsize);
IR* vp_ir_ret(VReg* src);
IR* vp_ir_cond(VReg* src1, VReg* src2, CondKind cond);
IR* vp_ir_jmp(BB* bb);
void vp_ir_cjmp(VReg* src1, VReg* src2, CondKind cond, BB* bb);
IR* vp_ir_memzero(VReg* dst, uint32_t size);
IR* vp_ir_memcpy(VReg* dst, VReg* src, uint32_t size);
IR* vp_ir_pusharg(VReg* src, uint32_t idx);
IR* vp_ir_call(IRCallInfo* ci, VReg* dst, VReg* freg);
IR* vp_ir_cast(VReg* src, VRegSize dstsize);
VReg* vp_ir_binop(IrKind kind, VReg* src1, VReg* src2, VRegSize vsize);
VReg* vp_ir_unary(IrKind kind, VReg* src, VRegSize vsize);

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

#endif