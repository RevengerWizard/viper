/*
** vp_emit_x64.h
** x64 Instruction emitter
*/

#include "vp_state.h"
#include "vp_target_x64.h"

/* MOV */
void emit_mov64_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_mov32_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_mov16_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_mov8_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_mov64_ri(VpState* V, X64Reg reg, int64_t imm);

void emit_mov32_ri(VpState* V, X64Reg reg, int32_t imm);
void emit_mov16_ri(VpState* V, X64Reg reg, int16_t imm);
void emit_mov8_ri(VpState* V, X64Reg reg, int8_t imm);

void emit_mov8_rm(VpState* V, X64Reg dst, X64Mem mem);
void emit_mov16_rm(VpState* V, X64Reg dst, X64Mem mem);
void emit_mov32_rm(VpState* V, X64Reg dst, X64Mem mem);
void emit_mov64_rm(VpState* V, X64Reg dst, X64Mem mem);

void emit_mov8_mr(VpState* V, X64Mem mem, X64Reg src);
void emit_mov16_mr(VpState* V, X64Mem mem, X64Reg src);
void emit_mov32_mr(VpState* V, X64Mem mem, X64Reg src);
void emit_mov64_mr(VpState* V, X64Mem mem, X64Reg src);

void emit_mov8_mi(VpState* V, X64Reg reg, int8_t imm);
void emit_mov16_mi(VpState* V, X64Reg reg, int16_t imm);
void emit_mov32_mi(VpState* V, X64Reg reg, int32_t imm);
void emit_mov64_mi(VpState* V, X64Reg reg, int32_t imm);

/* MOVSX */
void emit_movsx_r16r8(VpState* V, X64Reg dst, X64Reg src);
void emit_movsx_r32r8(VpState* V, X64Reg dst, X64Reg src);
void emit_movsx_r32r16(VpState* V, X64Reg dst, X64Reg src);
void emit_movsx_r64r8(VpState* V, X64Reg dst, X64Reg src);
void emit_movsx_r64r16(VpState* V, X64Reg dst, X64Reg src);
void emit_movsx_r64r32(VpState* V, X64Reg dst, X64Reg src);

/* MOVZX */
void emit_movzx_r16r8(VpState* V, X64Reg dst, X64Reg src);
void emit_movzx_r32r8(VpState* V, X64Reg dst, X64Reg src);
void emit_movzx_r32r16(VpState* V, X64Reg dst, X64Reg src);
void emit_movzx_r64r8(VpState* V, X64Reg dst, X64Reg src);
void emit_movzx_r64r16(VpState* V, X64Reg dst, X64Reg src);

/* MOVQ/MOVD */
void emit_movq_xr(VpState* V, X64Reg dst, X64Reg src);
void emit_movd_xr(VpState* V, X64Reg dst_xmm, X64Reg src_gpr32);

/* LEA */
void emit_lea64_rm(VpState* V, X64Reg dst, X64Mem mem);

/* ADD */
void emit_add64_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_add32_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_add16_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_add8_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_add64_ri(VpState* V, X64Reg reg, int32_t imm);
void emit_add32_ri(VpState* V, X64Reg reg, int32_t imm);
void emit_add16_ri(VpState* V, X64Reg reg, int16_t imm);
void emit_add8_ri(VpState* V, X64Reg reg, int8_t imm);

/* SUB */
void emit_sub_r64i32(VpState* V, X64Reg reg, int32_t imm);
void emit_sub32_ri(VpState* V, X64Reg reg, int32_t imm);
void emit_sub16_ri(VpState* V, X64Reg reg, int16_t imm);
void emit_sub8_ri(VpState* V, X64Reg reg, int8_t imm);
void emit_sub64_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_sub32_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_sub16_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_sub8_rr(VpState* V, X64Reg dst, X64Reg src);

/* CALL */
void emit_call64_r(VpState* V, X64Reg reg);
void emit_call_rel32(VpState* V, int32_t rel);
void emit_call_rip_rel32(VpState* V, int32_t rel);

/* INC */
void emit_inc64_r(VpState* V, X64Reg reg);
void emit_inc32_r(VpState* V, X64Reg reg);
void emit_inc16_r(VpState* V, X64Reg reg);
void emit_inc8_r(VpState* V, X64Reg reg);

/* DEC */
void emit_dec64_r(VpState* V, X64Reg reg);
void emit_dec32_r(VpState* V, X64Reg reg);
void emit_dec16_r(VpState* V, X64Reg reg);
void emit_dec8_r(VpState* V, X64Reg reg);

/* NEG */
void emit_neg64_r(VpState* V, X64Reg reg);
void emit_neg32_r(VpState* V, X64Reg reg);
void emit_neg16_r(VpState* V, X64Reg reg);
void emit_neg8_r(VpState* V, X64Reg reg);

/* NOT */
void emit_not64_r(VpState* V, X64Reg reg);
void emit_not32_r(VpState* V, X64Reg reg);
void emit_not16_r(VpState* V, X64Reg reg);
void emit_not8_r(VpState* V, X64Reg reg);

/* RET */
void emit_ret(VpState* V);

/* PUSH */
void emit_push64_r(VpState* V, X64Reg reg);

/* POP */
void emit_pop64_r(VpState* V, X64Reg reg);

/* JMP */
void emit_jmp_rel32(VpState* V, int32_t rel);
void emit_jcc_rel32(VpState* V, X64CC cc, int32_t rel);

/* SETcc */
void emit_setcc(VpState* V, X64CC cc, X64Reg reg);

/* Sign-extension */
void emit_cwde(VpState* V);
void emit_cdq(VpState* V);
void emit_cqo(VpState* V);

/* MUL */
void emit_mul64_r(VpState* V, X64Reg reg);
void emit_mul32_r(VpState* V, X64Reg reg);
void emit_mul16_r(VpState* V, X64Reg reg);
void emit_mul8_r(VpState* V, X64Reg reg);

/* DIV */
void emit_div64_r(VpState* V, X64Reg reg);
void emit_div32_r(VpState* V, X64Reg reg);
void emit_div16_r(VpState* V, X64Reg reg);
void emit_div8_r(VpState* V, X64Reg reg);

/* IDIV */
void emit_idiv64_r(VpState* V, X64Reg reg);
void emit_idiv32_r(VpState* V, X64Reg reg);
void emit_idiv16_r(VpState* V, X64Reg reg);
void emit_idiv8_r(VpState* V, X64Reg reg);

/* AND */
void emit_and32_ri(VpState* V, X64Reg reg, int32_t imm);
void emit_and16_ri(VpState* V, X64Reg reg, int16_t imm);
void emit_and8_ri(VpState* V, X64Reg reg, int8_t imm);
void emit_and64_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_and32_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_and16_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_and8_rr(VpState* V, X64Reg dst, X64Reg src);

/* OR */
void emit_or32_ri(VpState* V, X64Reg reg, int32_t imm);
void emit_or16_ri(VpState* V, X64Reg reg, int16_t imm);
void emit_or8_ri(VpState* V, X64Reg reg, int8_t imm);
void emit_or64_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_or32_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_or16_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_or8_rr(VpState* V, X64Reg dst, X64Reg src);

/* XOR */
void emit_xor64_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_xor32_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_xor16_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_xor8_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_xor32_ri(VpState* V, X64Reg reg, int32_t imm);
void emit_xor16_ri(VpState* V, X64Reg reg, int16_t imm);
void emit_xor8_ri(VpState* V, X64Reg reg, int8_t imm);

/* SHL */
void emit_shl_r64i8(VpState* V, X64Reg reg, uint8_t imm);
void emit_shl_r32i8(VpState* V, X64Reg reg, uint8_t imm);
void emit_shl_r16i8(VpState* V, X64Reg reg, uint8_t imm);
void emit_shl_r8i8(VpState* V, X64Reg reg, uint8_t imm);
void emit_shl_r64cl(VpState* V, X64Reg reg);
void emit_shl_r32cl(VpState* V, X64Reg reg);
void emit_shl_r16cl(VpState* V, X64Reg reg);
void emit_shl_r8cl(VpState* V, X64Reg reg);

/* SHR */
void emit_shr_r64i8(VpState* V, X64Reg reg, uint8_t imm);
void emit_shr_r32i8(VpState* V, X64Reg reg, uint8_t imm);
void emit_shr_r16i8(VpState* V, X64Reg reg, uint8_t imm);
void emit_shr8_ri(VpState* V, X64Reg reg, uint8_t imm);
void emit_shr_r64cl(VpState* V, X64Reg reg);
void emit_shr_r32cl(VpState* V, X64Reg reg);
void emit_shr_r16cl(VpState* V, X64Reg reg);
void emit_shr_r8cl(VpState* V, X64Reg reg);

/* SAR */
void emit_sar_r64i8(VpState* V, X64Reg reg, uint8_t imm);
void emit_sar_r32i8(VpState* V, X64Reg reg, uint8_t imm);
void emit_sar_r16i8(VpState* V, X64Reg reg, uint8_t imm);
void emit_sar_r8i8(VpState* V, X64Reg reg, uint8_t imm);
void emit_sar_r64cl(VpState* V, X64Reg reg);
void emit_sar_r32cl(VpState* V, X64Reg reg);
void emit_sar_r16cl(VpState* V, X64Reg reg);
void emit_sar_r8cl(VpState* V, X64Reg reg);

/* TEST */
void emit_test64_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_test32_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_test16_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_test8_rr(VpState* V, X64Reg dst, X64Reg src);

/* CMP */
void emit_cmp32_ri(VpState* V, X64Reg reg, int32_t imm);
void emit_cmp16_ri(VpState* V, X64Reg reg, int16_t imm);
void emit_cmp8_ri(VpState* V, X64Reg reg, int8_t imm);
void emit_cmp64_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_cmp32_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_cmp16_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_cmp8_rr(VpState* V, X64Reg dst, X64Reg src);

void emit_movsd_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_addsd_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_subsd_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_mulsd_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_divsd_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_xorpd_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_comisd_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_ucomisd_rr(VpState* V, X64Reg dst, X64Reg src);

void emit_movss_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_addss_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_subss_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_mulss_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_divss_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_xorps_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_comiss_rr(VpState* V, X64Reg dst, X64Reg src);

void emit_ucomiss_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_movss_rm(VpState* V, X64Reg dst, X64Mem mem);
void emit_movss_mr(VpState* V, X64Mem mem, X64Reg src);
void emit_movsd_rm(VpState* V, X64Reg dst, X64Mem mem);
void emit_movsd_mr(VpState* V, X64Mem mem, X64Reg src);

/* CVTSD2SS */
void emit_cvtsd2ss_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_cvtss2sd_rr(VpState* V, X64Reg dst, X64Reg src);
void emit_cvttss2si_r32x(VpState* V, X64Reg dst, X64Reg src);
void emit_cvttss2si_r64x(VpState* V, X64Reg dst, X64Reg src);
void emit_cvttsd2si_r32x(VpState* V, X64Reg dst, X64Reg src);
void emit_cvttsd2si_r64x(VpState* V, X64Reg dst, X64Reg src);
void emit_cvtsi2ss_xr32(VpState* V, X64Reg dst, X64Reg src);
void emit_cvtsi2ss_xr64(VpState* V, X64Reg dst, X64Reg src);
void emit_cvtsi2sd_xr32(VpState* V, X64Reg dst, X64Reg src);
void emit_cvtsi2sd_xr64(VpState* V, X64Reg dst, X64Reg src);

/* RDTSC */
void emit_rdtsc(VpState* V);

/* CPUID */
void emit_cpuid(VpState* V);
void emit_mov_rr(VRSize p, uint32_t r1, uint32_t r2);
void emit_mov_ri(VRSize p, uint32_t r, int64_t i);

void emit_add_rr(VRSize p, uint32_t r1, uint32_t r2);
void emit_add_ri(VRSize p, uint32_t r, int64_t i);
void emit_sub_rr(VRSize p, uint32_t r1, uint32_t r2);
void emit_sub_ri(VRSize p, uint32_t r, int64_t i);
void emit_mul_r(VRSize p, uint32_t r);
void emit_div_r(VRSize p, uint32_t r);
void emit_idiv_r(VRSize p, uint32_t r);
void emit_and_rr(VRSize p, uint32_t r1, uint32_t r2);
void emit_and_ri(VRSize p, uint32_t r, int64_t i);
void emit_or_rr(VRSize p, uint32_t r1, uint32_t r2);
void emit_or_ri(VRSize p, uint32_t r, int64_t i);
void emit_xor_rr(VRSize p, uint32_t r1, uint32_t r2);
void emit_xor_ri(VRSize p, uint32_t r, int64_t i);
void emit_shl_ri(VRSize p, uint32_t r, uint8_t i);
void emit_shl_rcl(VRSize p, uint32_t r);
void emit_shr_ri(VRSize p, uint32_t r, uint8_t i);
void emit_shr_rcl(VRSize p, uint32_t r);
void emit_sar_ri(VRSize p, uint32_t r, uint8_t i);
void emit_sar_rcl(VRSize p, uint32_t r);
void emit_inc_r(VRSize p, uint32_t r);
void emit_dec_r(VRSize p, uint32_t r);
void emit_neg_r(VRSize p, uint32_t r);
void emit_not_r(VRSize p, uint32_t r);
void emit_test_r(VRSize p, uint32_t r1, uint32_t r2);
void emit_cmp_ri(VRSize p, uint32_t r, int64_t i);
void emit_cmp_rr(VRSize p, uint32_t r1, uint32_t r2);
void emit_movsx_rr(VRSize pd, VRSize ps, X64Reg dst, X64Reg src);
void emit_movzx_rr(VRSize pd, VRSize ps, uint32_t r1, uint32_t r2);
void emit_cvttss2si_rx(VRSize p, uint32_t r1, uint32_t r2);
void emit_cvttsd2si_rx(VRSize p, uint32_t r1, uint32_t r2);
void emit_cvtsi2ss_xr(VRSize p, uint32_t r1, uint32_t r2);
void emit_cvtsi2sd_xr(VRSize p, uint32_t r1, uint32_t r2);