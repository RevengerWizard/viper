/*
** vp_emit_x64.h
** x64 Instruction emitter
*/

#include "vp_target_x64.h"

#define EMITX64(op) vp_emitX64_##op

void EMITX64(movRR)(X64Reg dst, X64Reg src);
void EMITX64(movRI)(X64Reg dst, int64_t imm);
void EMITX64(movRM)(X64Reg dst, X64Mem src);
void EMITX64(movMR)(X64Mem dst, X64Reg src);
void EMITX64(movMI)(X64Mem dst, int64_t imm);
void EMITX64(movsxRR)(X64Reg dst, X64Reg src);
void EMITX64(movzxRR)(X64Reg dst, X64Reg src);
void EMITX64(movdXR)(X64Reg dst, X64Reg src);
void EMITX64(movqXR)(X64Reg dst, X64Reg src);
void EMITX64(leaRM)(X64Reg dst, X64Mem src);
void EMITX64(addRR)(X64Reg dst, X64Reg src);
void EMITX64(addRI)(X64Reg dst, int64_t imm);
void EMITX64(subRR)(X64Reg dst, X64Reg src);
void EMITX64(subRI)(X64Reg dst, int64_t imm);
void EMITX64(mulR)(X64Reg reg);
void EMITX64(divR)(X64Reg reg);
void EMITX64(idivR)(X64Reg reg);
void EMITX64(andRR)(X64Reg dst, X64Reg src);
void EMITX64(andRI)(X64Reg dst, int64_t imm);
void EMITX64(orRR)(X64Reg dst, X64Reg src);
void EMITX64(orRI)(X64Reg dst, int64_t imm);
void EMITX64(xorRR)(X64Reg dst, X64Reg src);
void EMITX64(xorRI)(X64Reg dst, int64_t imm);
void EMITX64(shlRI)(X64Reg dst, int64_t imm);
void EMITX64(shlRCL)(X64Reg reg);
void EMITX64(shrRI)(X64Reg dst, int64_t imm);
void EMITX64(shrRCL)(X64Reg reg);
void EMITX64(sarRI)(X64Reg dst, int64_t imm);
void EMITX64(sarRCL)(X64Reg reg);
void EMITX64(incR)(X64Reg reg);
void EMITX64(decR)(X64Reg reg);
void EMITX64(negR)(X64Reg reg);
void EMITX64(notR)(X64Reg reg);
void EMITX64(testRR)(X64Reg dst, X64Reg src);
void EMITX64(cmpRI)(X64Reg dst, int64_t imm);
void EMITX64(cmpRR)(X64Reg dst, X64Reg src);
void EMITX64(cvttss2siRX)(X64Reg dst, X64Reg src);
void EMITX64(cvttsd2siRX)(X64Reg dst, X64Reg src);
void EMITX64(cvtsi2ssXR)(X64Reg dst, X64Reg src);
void EMITX64(cvtsi2sdXR)(X64Reg dst, X64Reg src);
void EMITX64(cvtsd2ssXX)(X64Reg dst, X64Reg src);
void EMITX64(cvtss2sdXX)(X64Reg dst, X64Reg src);
void EMITX64(movssMX)(X64Mem dst, X64Reg src);
void EMITX64(movssXM)(X64Reg dst, X64Mem src);
void EMITX64(movsdMX)(X64Mem dst, X64Reg src);
void EMITX64(movsdXM)(X64Reg dst, X64Mem src);
void EMITX64(movsdXX)(X64Reg dst, X64Reg src);
void EMITX64(addsdXX)(X64Reg dst, X64Reg src);
void EMITX64(subsdXX)(X64Reg dst, X64Reg src);
void EMITX64(mulsdXX)(X64Reg dst, X64Reg src);
void EMITX64(divsdXX)(X64Reg dst, X64Reg src);
void EMITX64(xorpdXX)(X64Reg dst, X64Reg src);
void EMITX64(comisdXX)(X64Reg dst, X64Reg src);
void EMITX64(ucomisdXX)(X64Reg dst, X64Reg src);
void EMITX64(movssXX)(X64Reg dst, X64Reg src);
void EMITX64(addssXX)(X64Reg dst, X64Reg src);
void EMITX64(subssXX)(X64Reg dst, X64Reg src);
void EMITX64(mulssXX)(X64Reg dst, X64Reg src);
void EMITX64(divssXX)(X64Reg dst, X64Reg src);
void EMITX64(xorpsXX)(X64Reg dst, X64Reg src);
void EMITX64(comissXX)(X64Reg dst, X64Reg src);
void EMITX64(ucomissXX)(X64Reg dst, X64Reg src);
void EMITX64(pushR)(X64Reg reg);
void EMITX64(popR)(X64Reg reg);
void EMITX64(callR)(X64Reg reg);
void EMITX64(callREL32)(int32_t rel);
void EMITX64(callRIP)(int32_t rel);
void EMITX64(jmpREL32)(int32_t rel);
void EMITX64(jccREL32)(X64CC cc, int32_t rel);
void EMITX64(setcc)(X64CC cc, X64Reg reg);
void EMITX64(cwde)();
void EMITX64(cdq)();
void EMITX64(cqo)();
void EMITX64(rdtsc)();
void EMITX64(cpuid)();
void EMITX64(ret)();
void EMITX64(syscall)();