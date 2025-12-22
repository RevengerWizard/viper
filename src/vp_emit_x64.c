/*
** vp_emit_x64.c
** x64 Instruction emitter
*/

#include "vp_emit.h"
#include "vp_state.h"
#include "vp_target_x64.h"
#include "vp_emit_x64.h"

/* REX prefix bits */
#define REX_W 0x48  /* 64 bit operand size */
#define REX_R 0x44  /* Extension of ModR/M reg field */
#define REX_X 0x42  /* Extension of SIB index field */
#define REX_B 0x41  /* Extension of ModR/M r/m, SIB base, or opcode reg */

/* ModR/M encoding */
#define MODRM(mod, reg, rm) ((((mod) & 3) << 6) | (((reg) & 7) << 3) | ((rm) & 7))

#define RM_SIB 4    /* R/M = 4; SIB byte follows */

/* SIB encoding */
#define SIB(scale, idx, base) ((((scale) & 3) << 6) | (((idx) & 7) << 3) | ((base) & 7))

/* Check for R8-R15 registers (needs REX extension) */
#define regext(r) ((r) & 0x8)

/* Calculate REX prefix from dst, src */
static VP_AINLINE uint8_t rexRR(uint8_t size, uint8_t rd, uint8_t rs)
{
    uint8_t rex = (size == 8) ? REX_W : 0;
    if(regext(rs)) rex |= REX_R;
    if(regext(rd)) rex |= REX_B;
    return rex;
}

/* Calculate REX prefix from reg, mem */
static VP_AINLINE uint8_t rexM(uint8_t reg, X64Mem mem, uint8_t rex)
{
    uint8_t base = MEM_BASE(mem);
    uint8_t idx = MEM_INDEX(mem);
    if(regext(reg)) rex |= REX_R;
    if(!MEM_ISRIP(mem))
    {
        if(regext(base)) rex |= REX_B;
        if(regext(idx)) rex |= REX_X;
    }
    return rex;
}

static void emit_mem(VpState* V, uint8_t reg, X64Mem mem)
{
    uint8_t base  = MEM_BASE(mem);
    uint8_t idx = MEM_INDEX(mem);
    uint8_t scale = MEM_SCALE(mem);
    int32_t disp  = MEM_DISP(mem);
    uint8_t mod;
    bool sib = (idx != NOREG) || ((base & 7) == 4);

    if(MEM_ISRIP(mem))
    {
        emit_u8(V, MODRM(0, reg & 7, 5));  /* mod=00, r/m=101 */
        emit_im32(V, disp);
        return;
    }

    /* Determine ModR/M mod field based on displacement size */
    if((disp == 0) && (base & 7) != 5)
    {
        /* no displacement (except for RBP/R13 which need disp8) */
        mod = 0;
    }
    else if(vp_isimm8(disp))
    {
        /* 8-bit displacement */
        mod = 1;
    }
    else
    {
        /* 32-bit displacement */
        mod = 2;
    }

    /* Emit ModR/M byte */
    if(sib)
    {
        emit_u8(V, MODRM(mod, reg & 7, RM_SIB));
        emit_u8(V, SIB(__builtin_ctz(scale), idx & 7, base & 7));
    }
    else
    {
        emit_u8(V, MODRM(mod, reg & 7, base & 7));
    }

    if(mod == 1)
    {
        emit_u8(V, (uint8_t)disp);
    }
    else if(mod == 2)
    {
        emit_im32(V, disp);
    }
    /* mod == 0: no displacement */
}

/* MOV gpr, gpr */
void EMITX64(movRR)(X64Reg dst, X64Reg src)
{
    vp_assertX(REG_CLASS(dst) == RC_GPR, "dst != gpr");
    vp_assertX(REG_CLASS(src) == RC_GPR, "src != gpr");
    vp_assertX(REG_SIZE(dst) == REG_SIZE(src), "size mismatch");
    uint8_t size = REG_SIZE(dst);
    uint8_t rd = REG_NUM(dst);
    uint8_t rs = REG_NUM(src);
    uint8_t op = size == 1 ? 0x88 : 0x89;
    uint8_t rex = rexRR(size, rd, rs);

    if(size == 2) emit_u8(V, 0x66);
    if(rex) emit_u8(V, rex);
    emit_u8(V, op);
    emit_u8(V, MODRM(3, rs & 7, rd & 7));
}

/* MOV gpr, imm */
void EMITX64(movRI)(X64Reg dst, int64_t imm)
{
    vp_assertX(REG_CLASS(dst) == RC_GPR, "dst != gpr");
    uint8_t size = REG_SIZE(dst);
    uint8_t rd = REG_NUM(dst);
    uint8_t rex = (size == 8) ? REX_W : 0;

    if(regext(rd)) rex |= REX_B;
    if(size == 2) emit_u8(V, 0x66);
    if(rex) emit_u8(V, rex);

    if(size == 1)
    {
        emit_u8(V, 0xB0 | (rd & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else if(size == 4)
    {
        emit_u8(V, 0xB8 | (rd & 7));
        emit_im32(V, (int32_t)imm);
    }
    else
    {
        emit_u8(V, 0xC7);
        emit_u8(V, MODRM(3, 0, rd & 7));
        if(size == 2) emit_im16(V, (int16_t)imm);
        else emit_im32(V, (int32_t)imm);
    }
}

/* MOV gpr, [mem] */
void EMITX64(movRM)(X64Reg dst, X64Mem src)
{
    vp_assertX(REG_CLASS(dst) == RC_GPR, "dst != gpr");
    uint8_t size = REG_SIZE(dst);
    uint8_t rd = REG_NUM(dst);
    uint8_t rex = rexM(rd, src, (size == 8) ? REX_W : 0);
    if(size == 2) emit_u8(V, 0x66);
    if(rex) emit_u8(V, rex);
    uint8_t op = (size == 1) ? 0x8A : 0x8B;
    emit_u8(V, op);
    emit_mem(V, rd, src);
}

/* MOV [mem], gpr */
void EMITX64(movMR)(X64Mem dst, X64Reg src)
{
    vp_assertX(REG_CLASS(src) == RC_GPR, "src != gpr");
    uint8_t size = REG_SIZE(src);
    uint8_t rs = REG_NUM(src);
    uint8_t rex = rexM(rs, dst, (size == 8) ? REX_W : 0);
    if(size == 2) emit_u8(V, 0x66);
    if(rex) emit_u8(V, rex);
    uint8_t op = (size == 1) ? 0x88 : 0x89;
    emit_u8(V, op);
    emit_mem(V, REG_NUM(src), dst);
}

/* MOV [mem], imm */
void EMITX64(movMI)(X64Mem dst, int64_t imm)
{
    uint8_t size = MEM_SIZE(dst);
    uint8_t rex = rexM(0, dst, (size == 8) ? REX_W : 0);

    if(size == 2) emit_u8(V, 0x66);
    if(rex) emit_u8(V, rex);

    uint8_t op = (size == 1) ? 0xC6 : 0xC7;
    emit_u8(V, op);
    emit_mem(V, 0, dst);

    switch(size)
    {
    case 1: emit_u8(V, (uint8_t)imm); break;
    case 2: emit_im16(V, (int16_t)imm); break;
    case 4: case 8: emit_im32(V, (int32_t)imm); break;
    }
}

/* MOVSX gpr, gpr */
void EMITX64(movsxRR)(X64Reg dst, X64Reg src)
{
    vp_assertX(REG_CLASS(dst) == RC_GPR, "dst != gpr");
    vp_assertX(REG_CLASS(src) == RC_GPR, "src != gpr");
    vp_assertX(REG_SIZE(dst) > REG_SIZE(src), "dst < src");
    uint8_t ds = REG_SIZE(dst);
    uint8_t ss = REG_SIZE(src);
    uint8_t rd = REG_NUM(dst);
    uint8_t rs = REG_NUM(src);
    uint8_t rex = rexRR(ds, rd, rs);

    if(ds == 2 && ss == 1) emit_u8(V, 0x66);
    if(rex) emit_u8(V, rex);

    if(ss == 1)
    {
        emit_u8(V, 0x0F);
        emit_u8(V, 0xBE);
    }
    else if(ss == 2)
    {
        emit_u8(V, 0x0F);
        emit_u8(V, 0xBF);
    }
    else /* ss == 4, ds == 8 */
    {
        vp_assertX(ss == 4 && ds == 8, "MOVSX r32, r64");
        emit_u8(V, 0x63);
    }
    emit_u8(V, MODRM(3, rd & 7, rs & 7));
}

/* MOVZX gpr, gpr */
void EMITX64(movzxRR)(X64Reg dst, X64Reg src)
{
    vp_assertX(REG_CLASS(dst) == RC_GPR, "dst != gpr");
    vp_assertX(REG_CLASS(src) == RC_GPR, "src != gpr");
    vp_assertX(REG_SIZE(dst) > REG_SIZE(src), "dst < src");
    uint8_t ds = REG_SIZE(dst);
    uint8_t ss = REG_SIZE(src);
    uint8_t rd = REG_NUM(dst);
    uint8_t rs = REG_NUM(src);
    uint8_t op = (ss == 1) ? 0xB6 : 0xB7;
    uint8_t rex = rexRR(ds, rd, rs);

    if(ds == 2) emit_u8(V, 0x66);
    if(rex) emit_u8(V, rex);

    emit_u8(V, 0x0F);
    emit_u8(V, op);
    emit_u8(V, MODRM(3, rd & 7, rs & 7));
}

/* MOVD xmm, gpr */
void EMITX64(movdXR)(X64Reg dst, X64Reg src)
{
    vp_assertX(REG_CLASS(dst) == RC_XMM, "dst != xmm");
    vp_assertX(REG_CLASS(src) == RC_GPR, "src != gpr");
    uint8_t size = REG_SIZE(src);
    uint8_t rd = REG_NUM(dst);
    uint8_t rs = REG_NUM(src);
    uint8_t rex = (size == 8) ? REX_W : 0;
    emit_u8(V, 0x66);
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0x6E);
    emit_u8(V, MODRM(3, rd & 7, rs & 7));
}

/* MOVQ xmm, gpr */
void EMITX64(movqXR)(X64Reg dst, X64Reg src)
{
    EMITX64(movdXR)(dst, src);
}

/* LEA gpr, [mem] */
void EMITX64(leaRM)(X64Reg dst, X64Mem src)
{
    vp_assertX(REG_CLASS(dst) == RC_GPR, "dst != gpr");
    uint8_t size = REG_SIZE(dst);
    uint8_t rd = REG_NUM(dst);
    uint8_t rex = rexM(rd, src, (size == 8) ? REX_W : 0);
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x8D);
    emit_mem(V, REG_NUM(dst), src);
}

static VP_AINLINE void emit_aluRR(VpState* V, uint8_t op1, uint8_t op8, X64Reg dst, X64Reg src)
{
    vp_assertX(REG_CLASS(dst) == RC_GPR, "dst != gpr");
    vp_assertX(REG_CLASS(src) == RC_GPR, "src != gpr");
    vp_assertX(REG_SIZE(dst) == REG_SIZE(src), "size mismatch");
    uint8_t size = REG_SIZE(dst);
    uint8_t rd = REG_NUM(dst);
    uint8_t rs = REG_NUM(src);

    if(size == 2) emit_u8(V, 0x66);

    uint8_t rex = rexRR(size, rd, rs);
    if(rex) emit_u8(V, rex);

    emit_u8(V, (size == 1) ? op8 : op1);
    emit_u8(V, MODRM(3, rs & 7, rd & 7));
}

/* ADD gpr, gpr */
void EMITX64(addRR)(X64Reg dst, X64Reg src) { emit_aluRR(V, 0x01, 0x00, dst, src); }

/* SUB gpr, gpr */
void EMITX64(subRR)(X64Reg dst, X64Reg src) { emit_aluRR(V, 0x29, 0x28, dst, src); }

/* AND gpr, gpr */
void EMITX64(andRR)(X64Reg dst, X64Reg src) { emit_aluRR(V, 0x21, 0x20, dst, src); }

/* OR gpr, gpr */
void EMITX64(orRR)(X64Reg dst, X64Reg src)  { emit_aluRR(V, 0x09, 0x08, dst, src); }

/* XOR gpr, gpr */
void EMITX64(xorRR)(X64Reg dst, X64Reg src) { emit_aluRR(V, 0x31, 0x30, dst, src); }

/* CMP gpr, gpr */
void EMITX64(cmpRR)(X64Reg dst, X64Reg src) { emit_aluRR(V, 0x39, 0x38, dst, src); }

/* TEST gpr, gpr */
void EMITX64(testRR)(X64Reg dst, X64Reg src){ emit_aluRR(V, 0x85, 0x84, dst, src); }

static void emit_aluRI(VpState* V, uint8_t modrmreg, uint8_t raxop, X64Reg dst, int64_t imm)
{
    vp_assertX(REG_CLASS(dst) == RC_GPR, "dst != gpr");
    uint8_t size = REG_SIZE(dst);
    uint8_t rd = REG_NUM(dst);

    if(size == 2) emit_u8(V, 0x66);

    uint8_t rex = rexRR(size, rd, 0);
    if(rex) emit_u8(V, rex);

    /* AL/AX/EAX/RAX with non-imm8 */
    if(rd == 0 && (size == 1 || !vp_isimm8(imm)))
    {
        emit_u8(V, raxop);
        goto emit_imm;
    }

    /* Use imm8 encoding if possible */
    if(size > 1 && vp_isimm8(imm))
    {
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, modrmreg, rd & 7));
        emit_u8(V, (uint8_t)imm);
        return;
    }

    emit_u8(V, (size == 1) ? 0x80 : 0x81);
    emit_u8(V, MODRM(3, modrmreg, rd & 7));

emit_imm:
    switch(size)
    {
    case 1: emit_u8(V, (uint8_t)imm); break;
    case 2: emit_im16(V, (int16_t)imm); break;
    default: emit_im32(V, (int32_t)imm); break;
    }
}

/* ADD gpr, imm */
void EMITX64(addRI)(X64Reg dst, int64_t imm) { emit_aluRI(V, 0, 0x04, dst, imm); }

/* OR gpr, imm */
void EMITX64(orRI)(X64Reg dst, int64_t imm)  { emit_aluRI(V, 1, 0x0C, dst, imm); }

/* AND gpr, imm */
void EMITX64(andRI)(X64Reg dst, int64_t imm) { emit_aluRI(V, 4, 0x24, dst, imm); }

/* SUB gpr, imm */
void EMITX64(subRI)(X64Reg dst, int64_t imm) { emit_aluRI(V, 5, 0x2C, dst, imm); }

/* XOR gpr, imm */
void EMITX64(xorRI)(X64Reg dst, int64_t imm) { emit_aluRI(V, 6, 0x34, dst, imm); }

/* CMP gpr, imm */
void EMITX64(cmpRI)(X64Reg dst, int64_t imm) { emit_aluRI(V, 7, 0x3C, dst, imm); }

static VP_AINLINE void emit_unary(VpState* V, uint8_t op1, uint8_t op8, uint8_t modrm_reg, X64Reg reg)
{
    vp_assertX(REG_CLASS(reg) == RC_GPR, "reg != gpr");
    uint8_t size = REG_SIZE(reg);
    uint8_t rr = REG_NUM(reg);

    if(size == 2) emit_u8(V, 0x66);

    uint8_t rex = rexRR(size, rr, 0);
    if(rex) emit_u8(V, rex);

    emit_u8(V, (size == 1) ? op8 : op1);
    emit_u8(V, MODRM(3, modrm_reg, rr & 7));
}

/* MUL gpr */
void EMITX64(mulR)(X64Reg reg)  { emit_unary(V, 0xF7, 0xF6, 4, reg); }

/* DIV gpr */
void EMITX64(divR)(X64Reg reg)  { emit_unary(V, 0xF7, 0xF6, 6, reg); }

/* IDIV gpr */
void EMITX64(idivR)(X64Reg reg) { emit_unary(V, 0xF7, 0xF6, 7, reg); }

/* NEG gpr */
void EMITX64(negR)(X64Reg reg)  { emit_unary(V, 0xF7, 0xF6, 3, reg); }

/* NOT gpr */
void EMITX64(notR)(X64Reg reg)  { emit_unary(V, 0xF7, 0xF6, 2, reg); }

/* INC gpr */
void EMITX64(incR)(X64Reg reg)  { emit_unary(V, 0xFF, 0xFE, 0, reg); }

/* DEC gpr */
void EMITX64(decR)(X64Reg reg)  { emit_unary(V, 0xFF, 0xFE, 1, reg); }

static VP_AINLINE void emit_shift(VpState* V, X64Reg reg, uint8_t modrm_reg, int64_t imm, bool cl)
{
    vp_assertX(REG_CLASS(reg) == RC_GPR, "dst != gpr");
    uint8_t size = REG_SIZE(reg);
    uint8_t rr = REG_NUM(reg);

    if(size == 2) emit_u8(V, 0x66);

    uint8_t rex = rexRR(size, rr, 0);
    if(rex) emit_u8(V, rex);

    if(cl)
    {
        emit_u8(V, (size == 1) ? 0xD2 : 0xD3);
    }
    else if(imm == 1)
    {
        emit_u8(V, (size == 1) ? 0xD0 : 0xD1);
    }
    else
    {
        emit_u8(V, (size == 1) ? 0xC0 : 0xC1);
    }

    emit_u8(V, MODRM(3, modrm_reg, rr & 7));
    if(!cl && imm != 1) emit_u8(V, (uint8_t)imm);
}

/* SHL gpr, imm */
void EMITX64(shlRI)(X64Reg dst, int64_t imm) { emit_shift(V, dst, 4, imm, false); }

/* SHL gpr, CL */
void EMITX64(shlRCL)(X64Reg reg) { emit_shift(V, reg, 4, 0, true); }

/* SHR gpr, imm */
void EMITX64(shrRI)(X64Reg dst, int64_t imm) { emit_shift(V, dst, 5, imm, false); }

/* SHR gpr, CL */
void EMITX64(shrRCL)(X64Reg reg) { emit_shift(V, reg, 5, 0, true); }

/* SAR gpr, imm */
void EMITX64(sarRI)(X64Reg dst, int64_t imm) { emit_shift(V, dst, 7, imm, false); }

/* SAR gpr, CL */
void EMITX64(sarRCL)(X64Reg reg) { emit_shift(V, reg, 7, 0, true); }

static VP_AINLINE void emit_cvt(VpState* V, uint8_t prefix, uint8_t op, X64Reg dst, X64Reg src)
{
    uint8_t size = REG_SIZE(src);
    uint8_t rd = REG_NUM(dst);
    uint8_t rs = REG_NUM(src);

    uint8_t rex = (size == 8) ? REX_W : 0;
    if(regext(rd)) rex |= REX_R;
    if(regext(rs)) rex |= REX_B;

    if(prefix) emit_u8(V, prefix);
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, op);
    emit_u8(V, MODRM(3, rd & 7, rs & 7));
}

/* CVTTSS2SI gpr, xmm */
void EMITX64(cvttss2siRX)(X64Reg dst, X64Reg src) { emit_cvt(V, 0xF3, 0x2C, dst, src); }

/* CVTTSD2SI gpr, xmm */
void EMITX64(cvttsd2siRX)(X64Reg dst, X64Reg src) { emit_cvt(V, 0xF2, 0x2C, dst, src); }

/* CVTSI2SS xmm, gpr */
void EMITX64(cvtsi2ssXR)(X64Reg dst, X64Reg src) { emit_cvt(V, 0xF3, 0x2A, dst, src); }

/* CVTSI2SD xmm, gpr */
void EMITX64(cvtsi2sdXR)(X64Reg dst, X64Reg src) { emit_cvt(V, 0xF2, 0x2A, dst, src); }

/* CVTSD2SS xmm, xmm */
void EMITX64(cvtsd2ssXX)(X64Reg dst, X64Reg src) { emit_cvt(V, 0xF2, 0x5A, dst, src); }

/* CVTSS2SD xmm, xmm */
void EMITX64(cvtss2sdXX)(X64Reg dst, X64Reg src) { emit_cvt(V, 0xF3, 0x5A, dst, src); }

/* Emit common SSE/SSE2 instructions */
static VP_AINLINE void emit_sseRR(VpState* V, uint8_t prefix, uint8_t op, X64Reg dst, X64Reg src)
{
    vp_assertX(REG_CLASS(dst) == RC_XMM, "dst != xmm");
    vp_assertX(REG_CLASS(src) == RC_XMM, "src != xmm");
    uint8_t rd = REG_NUM(dst);
    uint8_t rs = REG_NUM(src);
    uint8_t rex = 0;
    if(regext(rs)) rex |= REX_R;
    if(regext(rd)) rex |= REX_B;
    if(prefix) emit_u8(V, prefix);
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, op);
    emit_u8(V, MODRM(3, rd & 7, rs & 7));
}

static VP_AINLINE void emit_sseM(VpState* V, uint8_t prefix, uint8_t op, uint8_t reg, X64Mem mem)
{
    uint8_t rex = rexM(reg, mem, 0);

    if(prefix) emit_u8(V, prefix);
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, op);

    emit_mem(V, reg, mem);
}

/* MOVSS [mem], gpr */
void EMITX64(movssMX)(X64Mem dst, X64Reg src)
{
    vp_assertX(REG_CLASS(src) == RC_XMM, "src != xmm");
    emit_sseM(V, 0xF3, 0x11, REG_NUM(src), dst);
}

/* MOVSS xmm, [mem] */
void EMITX64(movssXM)(X64Reg dst, X64Mem src)
{
    vp_assertX(REG_CLASS(dst) == RC_XMM, "dst != xmm");
    emit_sseM(V, 0xF3, 0x10, REG_NUM(dst), src);
}

/* MOVSD [mem], gpr */
void EMITX64(movsdMX)(X64Mem dst, X64Reg src)
{
    vp_assertX(REG_CLASS(src) == RC_XMM, "src != xmm");
    emit_sseM(V, 0xF2, 0x11, REG_NUM(src), dst);
}

/* MOVSD xmm, [mem] */
void EMITX64(movsdXM)(X64Reg dst, X64Mem src)
{
    vp_assertX(REG_CLASS(dst) == RC_XMM, "dst != xmm");
    emit_sseM(V, 0xF2, 0x10, REG_NUM(dst), src);
}

/* MOVSD xmm, xmm */
void EMITX64(movsdXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0xF2, 0x10, dst, src); }

/* ADDSD xmm, xmm */
void EMITX64(addsdXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0xF2, 0x58, dst, src); }

/* SUBSD xmm, xmm */
void EMITX64(subsdXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0xF2, 0x5C, dst, src); }

/* MULSD xmm, xmm */
void EMITX64(mulsdXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0xF2, 0x59, dst, src); }

/* DIVSD xmm, xmm */
void EMITX64(divsdXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0xF2, 0x5E, dst, src); }

/* XORPD xmm, xmm */
void EMITX64(xorpdXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0x66, 0x57, dst, src); }

/* COMISD xmm, xmm */
void EMITX64(comisdXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0x66, 0x2F, dst, src); }

/* UCOMISD xmm, xmm */
void EMITX64(ucomisdXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0x66, 0x2E, dst, src); }

/* MOVSS xmm, xmm */
void EMITX64(movssXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0xF3, 0x10, dst, src); }

/* ADDSS xmm, xmm */
void EMITX64(addssXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0xF3, 0x58, dst, src); }

/* SUBSS xmm, xmm */
void EMITX64(subssXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0xF3, 0x5C, dst, src); }

/* MULSS xmm, xmm */
void EMITX64(mulssXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0xF3, 0x59, dst, src); }

/* DIVSS xmm, xmm */
void EMITX64(divssXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0xF3, 0x5E, dst, src); }

/* XORPS xmm, xmm */
void EMITX64(xorpsXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0x00, 0x57, dst, src); }

/* COMISS xmm, xmm */
void EMITX64(comissXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0x00, 0x2F, dst, src); }

/* UCOMISS xmm, xmm */
void EMITX64(ucomissXX)(X64Reg dst, X64Reg src) { emit_sseRR(V, 0x00, 0x2E, dst, src); }

/* PUSH gpr */
void EMITX64(pushR)(X64Reg reg)
{
    vp_assertX(REG_CLASS(reg) == RC_GPR, "reg != gpr");
    vp_assertX(REG_SIZE(reg) == 8, "reg must be 64 bit");
    uint8_t rr = REG_NUM(reg);
    if(regext(rr)) emit_u8(V, REX_W | REX_B);
    emit_u8(V, 0x50 + (rr & 7));
}

/* POP gpr */
void EMITX64(popR)(X64Reg reg)
{
    vp_assertX(REG_CLASS(reg) == RC_GPR, "reg != gpr");
    vp_assertX(REG_SIZE(reg) == 8, "reg must be 64 bit");
    uint8_t rr = REG_NUM(reg);
    if(regext(rr)) emit_u8(V, REX_W | REX_B);
    emit_u8(V, 0x58 + (rr & 7));
}

/* CALL gpr */
void EMITX64(callR)(X64Reg reg)
{
    vp_assertX(REG_CLASS(reg) == RC_GPR, "reg != gpr");
    uint8_t rr = REG_NUM(reg);
    if(regext(rr)) emit_u8(V, REX_B);
    emit_u8(V, 0xFF);
    emit_u8(V, MODRM(3, 2, rr & 7));
}

/* CALL rel32 */
void EMITX64(callREL32)(int32_t rel)
{
    emit_u8(V, 0xE8);
    emit_im32(V, rel);
}

/* CALL [mem] */
void EMITX64(callRIP)(int32_t rel)
{
    emit_u8(V, 0xFF);
    emit_u8(V, MODRM(0, 2, 5));
    emit_im32(V, rel);
}

/* JMP rel32 */
void EMITX64(jmpREL32)(int32_t rel)
{
    emit_u8(V, 0xE9);
    emit_im32(V, rel);
}

/* Jcc rel32 */
void EMITX64(jccREL32)(X64CC cc, int32_t rel)
{
    vp_assertX(cc >= CC_O && cc <= CC_G, "invalid cc");
    emit_u8(V, 0x0F);
    emit_u8(V, 0x80 + cc);
    emit_im32(V, rel);
}

/* SETcc reg8 */
void EMITX64(setcc)(X64CC cc, X64Reg reg)
{
    vp_assertX(cc >= CC_O && cc <= CC_G, "invalid cc");
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0x0F);
    emit_u8(V, 0x90 | (uint8_t)cc);
    emit_u8(V, MODRM(3, 0, reg & 7));
}

/* CWDE EAX, AX */
void EMITX64(cwde)()
{
    emit_u8(V, 0x98);
}

/* CDQ EDX:EAX, EAX */
void EMITX64(cdq)()
{
    emit_u8(V, 0x99);
}

/* CQO RDX:RAX, RAX */
void EMITX64(cqo)()
{
    emit_u8(V, REX_W);
    emit_u8(V, 0x99);
}

/* RET */
void EMITX64(ret)()
{
    emit_u8(V, 0xC3);
}

/* RDTSC */
void EMITX64(rdtsc)()
{
    emit_u8(V, 0x0f);
    emit_u8(V, 0x31);
}

/* CPUID */
void EMITX64(cpuid)()
{
    emit_u8(V, 0x0f);
    emit_u8(V, 0xa2);
}