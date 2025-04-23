/*
** vp_tab.c
** Hash table handling
*/

#ifndef _VP_TAB_H
#define _VP_TAB_H

#include "vp_str.h"

typedef struct
{
    Str* key;
    void* val;
} TabEntry;

typedef struct
{
    TabEntry* entries;
    uint32_t size;
    uint32_t count;
} Tab;

void vp_tab_init(Tab* tab);
void vp_tab_free(Tab* tab);
void* vp_tab_get(Tab* tab, Str* key);
void vp_tab_set(Tab* tab, Str* key, void* val);
void vp_tab_remove(Tab* tab, Str* key);

#endif