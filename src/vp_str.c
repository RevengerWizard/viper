/*
** vp_str.c
** String handling
*/

#include <string.h>
#include <stdlib.h>

#include "vp_str.h"
#include "vp_tab.h"

static Tab strtab;

static Str* str_find(const char* chars, uint32_t len, uint32_t hash)
{
    Tab* tab = &strtab;
    if(tab->count == 0)
        return NULL;

    uint32_t idx = hash & (tab->size - 1);

    while(true)
    {
        TabEntry* entry = &tab->entries[idx];
        if(entry->key == NULL)
        {
            if(entry->val == NULL)
                return NULL;
        }
        else if(entry->key->len == len && entry->key->hash == hash 
                && memcmp(str_data(entry->key), chars, len) == 0)
        {
            return entry->key;
        }

        idx = (idx + 1) & (tab->size - 1);
    }
}

static uint32_t str_hash(const char* key, uint32_t len)
{
    uint32_t hash = 2166136261u;
    for(uint32_t i = 0; i < len; i++)
    {
        hash ^= (uint8_t)key[i];
        hash *= 16777619u;
    }
    return hash;
}

Str* vp_str_new(const char* chars, uint32_t len)
{
    uint32_t hash = str_hash(chars, len);
    Str* s = str_find(chars, len, hash);
    if(s != NULL)
    {
        return s;
    }
    s = (Str*)malloc(vp_str_size(len));
    s->reserved = 0;
    s->len = len;
    s->hash = hash;
    memcpy(str_datawr(s), chars, len);
    str_datawr(s)[len] = '\0';
    /* Add to string hash table */
    vp_tab_set(&strtab, s, NULL);
    return s;
}