/*
** vp_state.h
** State management
*/

#ifndef _VP_STATE_H
#define _VP_STATE_H

#include <stdio.h>

#include "vp_ir.h"
#include "vp_mem.h"
#include "vp_target.h"
#include "vp_regalloc.h"
#include "vp_type.h"
#include "vp_tab.h"
#include "vp_map.h"

typedef enum SectionKind
{
    SEC_TEXT,
    SEC_DATA,
    SEC_IDATA
} SectionKind;

typedef struct
{
    SectionKind kind;
    uint32_t secofs;   /* Section alignment */
    uint32_t secsize;  /* Aligned section size */
    uint32_t virtaddr;  /* Relative virtual address */
    uint32_t virtsize;  /* Virtual address size */
    SBuf sb;
} Section;

typedef struct Layout
{
    vec_t(Section) secs;    /* Sections */
    uint32_t entry; /* Entry point */
    uint32_t imgsize;
    uint32_t nsecs; /* Number of sections */
} Layout;

#define FILE_ALIGNMENT 512
#define SECTION_ALIGNMENT 4096

typedef const char* (*VpReader)(void* ud, size_t* size);

/* Viper State */
typedef struct VpState
{
    SBuf code;
    Map cachequal;
    Map cacheptr;
    vec_t(Type*) cachefunc;
    vec_t(Type*) cachearr;
    Tab strtab;
    Arena instarena;
    Arena strarena;
    Arena astarena;
    Arena typearena;
    Arena symarena;
    Arena irarena;
    Tab globtab;
    struct Scope* globscope;
    struct Scope* currscope;
    BB* bb;
    struct Code* fncode;
    RegAlloc* ra;
    vec_t(struct Code*) codes;
    SBuf tmpbuf;
    Tab funcs;
    Tab ifuncs;
    Layout L;
    vec_t(Str*) strs;
    vec_t(uint32_t) strofs;
    vec_t(struct PatchInfo) patches;
    const TargetInfo* T;
    vec_t(struct ImportDLL) imports;
    uint32_t importsize;
    Tab modules;
    vec_t(struct Module*) mods;
    struct Module* mod; /* Current module */
} VpState;

#define TARGET_PTR_SIZE (8)

extern VpState* V;

vec_t(struct Decl*) vp_load_file(VpState* V, const char* filename);
void vp_load(VpState* V, const char* filename);

VpState* vp_state_open();
void vp_state_close(VpState* V);

#endif