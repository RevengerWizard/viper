/*
** vp_link.h
** Linker interface
*/

#ifndef _VP_LINK_H
#define _VP_LINK_H

#include "vp_def.h"

#include "vp_ir.h"
#include "vp_codegen.h"
#include "vp_str.h"

typedef struct ImportFunc
{
    Str* name;
    uint32_t rva;
} ImportFunc;

typedef struct ImportDLL
{
    Str* name;
    vec_t(ImportFunc) funcs;
} ImportDLL;

void vp_layout_init(Layout* L);
void vp_import_calc_rvas();

typedef enum PatchKind
{
    PATCH_LEA_REL,
    PATCH_LEA_ABS,
    PATCH_JMP_REL,
    PATCH_CALL_REL,
    PATCH_CALL_ABS,
} PatchKind;

typedef struct PatchInfo
{
    PatchKind kind; /* The type of patch */
    int32_t ofs;   /* Offset to patch */
    union
    {
        Str* label;
        struct Code* code;
        BB* target;
    };
} PatchInfo;

void vp_link(void);

static VP_AINLINE void patchinfo_learel(Code* fn, uint32_t ofs)
{
    PatchInfo pi = {.kind = PATCH_LEA_REL, .ofs = ofs, .code = fn};
    vec_push(V->patches, pi);
}

static VP_AINLINE void patchinfo_leaabs(Str* label, uint32_t ofs)
{
    PatchInfo pi = {.kind = PATCH_LEA_ABS, .ofs = ofs, .label = label};
    vec_push(V->patches, pi);
}

static VP_AINLINE void patchinfo_jmprel(BB* bb, uint32_t ofs)
{
    PatchInfo pi = {.kind = PATCH_JMP_REL, .ofs = ofs, .target = bb};
    vec_push(V->patches, pi);
}

static VP_AINLINE void patchinfo_callrel(Code* fn, uint32_t ofs)
{
    PatchInfo pi = {.kind = PATCH_CALL_REL, .ofs = ofs, .code = fn};
    vec_push(V->patches, pi);
}

static VP_AINLINE void patchinfo_callabs(Str* label, uint32_t ofs)
{
    PatchInfo pi = {.kind = PATCH_CALL_ABS, .ofs = ofs, .label = label};
    vec_push(V->patches, pi);
}

#endif