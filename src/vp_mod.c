/*
** vp_mod.c
** Module handling
*/

#include <string.h>
#include <stdlib.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "vp_mod.h"
#include "vp_str.h"
#include "vp_err.h"
#include "vp_sema.h"

static Module* mod_new(Str* path)
{
    Module* mod = vp_mem_calloc(1, sizeof(*mod));
    mod->path = path;
    mod->syms = vec_init(Sym*);
    mod->decls = vec_init(Decl*);
    mod->sorted = vec_init(Decl*);
    vp_tab_set(&V->modules, path, mod);
    vec_push(V->mods, mod);
    return mod;
}

static Str* mod_abspath(const char* path)
{
    char buf[4096];
#ifdef _WIN32
    if(!_fullpath(buf, path, sizeof(buf)))
        return NULL;
#else
    if(!realpath(path, buf))
        return NULL;
#endif
    return vp_str_new(buf, strlen(buf));
}

static Str* mod_addext(Str* path, const char* ext)
{
    size_t len = path->len + strlen(ext);
    char* buf = vp_mem_alloc(len + 1);
    memcpy(buf, str_data(path), path->len);
    strcpy(buf + path->len, ext);
    return vp_str_new(buf, len);
}

static bool readable(const char* filename)
{
    FILE* file = fopen(filename, "r");
    if(file)
    {
        fclose(file);
        return true;
    }
    return false;
}

static const char* mod_execdir(char* buf, size_t buflen)
{
    if(!GetModuleFileNameA(NULL, buf, (DWORD)buflen))
        return NULL;
    char* sep = strrchr(buf, '\\');
    if(sep)
        *sep = '\0';
    return buf;
}

Module* vp_mod_enter(Module* mod)
{
    Module* old = V->mod;
    V->mod = mod;
    return old;
}

void vp_mod_leave(Module* mod)
{
    V->mod = mod;
}

static Str* mod_resolve(SrcLoc loc, Str* relpath)
{
    /* Ensure .vp extension */
    Str* withext = relpath;
    if(!strstr(str_data(relpath), ".vp"))
        withext = mod_addext(relpath, ".vp");

    /* 1. Try as-is (absolute or cwd-relative) */
    if(readable(str_data(withext)))
    {
        Str* abs = mod_abspath(str_data(withext));
        if(abs) return abs;
    }

    /* 2. Search relative to compiler executable directory */
    char execbuf[4096];
    const char* execdir = mod_execdir(execbuf, sizeof(execbuf));
    if(execdir)
    {
        char trybuf[4096];
        int n = snprintf(trybuf, sizeof(trybuf), "%s/%s", execdir, str_data(withext));
        if(n > 0 && n < (int)sizeof(trybuf) && readable(trybuf))
        {
            Str* abs = mod_abspath(trybuf);
            if(abs) return abs;
        }
    }

    vp_err_error(loc, "cannot resolve module '%s'", str_data(relpath));
    return NULL; /* unreachable */
}

/* Get cached module or create new */
Module* vp_mod_get(SrcLoc loc, Str* name)
{
    Str* path = mod_resolve(loc, name);
    Module* mod = vp_tab_get(&V->modules, path);
    if(!mod)
    {
        mod = mod_new(path);
        Module* old = vp_mod_enter(mod);
        vec_t(Decl*) decls = vp_load_file(V, str_data(mod->path));
        mod->decls = decls;
        {
            vp_sema_decls(decls);
            vp_sema_imports(decls);
        }
        vp_mod_leave(old);
    }
    return mod;
}