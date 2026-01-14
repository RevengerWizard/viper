/*
** vp_state.c
** State management
*/

#include <string.h>

#include "vp_state.h"
#include "vp_codegen.h"
#include "vp_lex.h"
#include "vp_link.h"
#include "vp_mem.h"
#include "vp_mod.h"
#include "vp_target.h"
#include "vp_var.h"

VpState* V;

VpState* vp_state_open()
{
    V = (VpState*)vp_mem_alloc(sizeof(*V));
    memset(V, 0, sizeof(*V));
    vp_lex_init();
    V->cachefunc = vec_init(Type*);
    V->cachearr = vec_init(Type*);
    V->codes = vec_init(Code*);
    V->strs = vec_init(Str*);
    V->strofs = vec_init(uint32_t);
    V->patches = vec_init(PatchInfo);
    V->imports = vec_init(ImportDLL);
    V->mods = vec_init(Module*);
    V->L.secs = vec_init(Section);
    V->globscope = vp_scope_new(NULL);
    V->currscope = V->globscope;
    const TargetInfo* target = vp_target_init(TARGET_X64_WINDOWS);
    vp_assertX(target, "empty target");
    V->T = target;
    return V;
}

void vp_state_close(VpState* V)
{
    vp_mem_free(V);
}