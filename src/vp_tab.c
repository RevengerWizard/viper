/*
** vp_tab.c
** Hash table handling
*/

#include <stdlib.h>

#include "vp_tab.h"

static TabEntry* tab_findkey(TabEntry* entries, uint32_t size, Str* key)
{
    uint32_t idx = key->hash & (size - 1);
    TabEntry* tomb = NULL;

    while(true)
    {
        TabEntry* entry = &entries[idx];
        if(entry->key == NULL)
        {
            if(entry->val == NULL)
            {
                /* Empty entry */
                return tomb != NULL ? tomb : entry;
            }
            else
            {
                /* Found a tombstone */
                if(tomb == NULL)
                    tomb = entry;
            }
        }
        else if(entry->key == key)
        {
            /* Found the key */
            return entry;
        }

        idx = (idx + 1) & (size - 1);
    }
}

static void tab_resize(Tab* tab, uint32_t size)
{
    TabEntry* entries = (TabEntry*)calloc(size, sizeof(TabEntry));

    uint32_t count = 0;
    for(uint32_t i = 0; i < tab->size; i++)
    {
        TabEntry* entry = &tab->entries[i];
        if(entry->key != NULL)
        {
            TabEntry* dest = tab_findkey(entries, size, entry->key);
            dest->key = entry->key;
            dest->val = entry->val;
            count++;
        }
    }

    free(tab->entries);
    tab->entries = entries;
    tab->size = size;
    tab->count = count;
}

void vp_tab_init(Tab* tab)
{
    tab->count = 0;
    tab->size = 0;
    tab->entries = NULL;
}

void vp_tab_free(Tab *tab)
{
    if(tab->entries)
    {
        free(tab->entries);
        tab->entries = NULL;
    }
    tab->size = 0;
    tab->count = 0;
}

void* vp_tab_get(Tab* tab, Str* key)
{
    if(tab->count == 0)
        return NULL;

    TabEntry* entry =  tab_findkey(tab->entries, tab->size, key);
    if(entry->key == NULL)
        return NULL;

    return entry->val;
}

#define TAB_MAX_LOAD 0.75

void vp_tab_set(Tab* tab, Str* key, void* val)
{
    if(tab->count + 1 > tab->size * TAB_MAX_LOAD)
    {
        uint32_t size = (tab->size) < 8 ? 8 : (tab->size) * 2;
        tab_resize(tab, size);
    }

    TabEntry* entry = tab_findkey(tab->entries, tab->size, key);
    if(entry->key == NULL)
        tab->count++;

    entry->key = key;
    entry->val = val;
}

void vp_tab_remove(Tab* tab, Str* key)
{
    if(tab->count == 0)
        return;

    TabEntry* entry = tab_findkey(tab->entries, tab->size, key);
    if(entry->key == NULL)
        return;

    entry->key = NULL;
    entry->val = ((void*)(uintptr_t*)-1);
}