/*
** vp_mod.c
** Module handling
*/

#include <stdlib.h>

#include "vp_mod.h"
#include "vp_str.h"
#include "vp_err.h"
#include "vp_sema.h"

static Module* mod_new(Str* path)
{
    Module* mod = vp_mem_calloc(1, sizeof(*mod));
    mod->path = path;
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

/* Get cached module or create new */
Module* vp_mod_get(SrcLoc loc, Str* name)
{
    Str* path = name;
    if(!strstr(str_data(name), ".vp"))
    {
        path = mod_addext(name, ".vp");
    }

    path = mod_abspath(str_data(path));
    if(!readable(str_data(path)))
    {
        vp_err_error(loc, "cannot resolve path '%s'", str_data(path));
    }

    printf("MODULE PATH %s\n", str_data(path));

    Module* mod = vp_tab_get(&V->modules, path);
    if(!mod)
    {
        mod = mod_new(path);
        vec_t(Decl*) decls = vp_load_file(V, str_data(mod->path));
        mod->decls = decls;
        Module* old = vp_mod_enter(mod);
        {
            vp_sema_decls(decls);
            vp_sema_imports(decls);
        }
        vp_mod_leave(old);
    }
    return mod;
}