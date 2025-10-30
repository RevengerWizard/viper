/*
** vp_map.c
** Map handling
*/

#include <stdlib.h>

#include "vp_map.h"
#include "vp_mem.h"

static uint64_t hash_u64(uint64_t x)
{
    x *= 0xff51afd7ed558ccd;
    x ^= x >> 32;
    return x;
}

/*static uint64_t hash_ptr(const void* p)
{
    return hash_u64((uintptr_t)p);
}

static uint64_t hash_mix(uint64_t x, uint64_t y)
{
    x ^= y;
    x *= 0xff51afd7ed558ccd;
    x ^= x >> 32;
    return x;
}*/

static void map_grow(Map* map, uint32_t newsize)
{
    newsize = CLAMP_MIN(newsize, 16);
    Map newmap = {
        .keys = vp_mem_calloc(newsize, sizeof(*map->keys)),
        .vals = vp_mem_alloc(newsize * sizeof(*map->vals)),
        .size = newsize
    };
    for(uint32_t i = 0; i < map->size; i++)
    {
        if(map->keys[i])
        {
            vp_map_put_u64u64(&newmap, map->keys[i], map->vals[i]);
        }
    }
    if(map->keys)
    {
        vp_mem_free(map->keys);
    }
    if(map->vals)
    {
        vp_mem_free(map->vals);
    }
    *map = newmap;
}

uint64_t vp_map_get_u64u64(Map* map, uint64_t key)
{
    if(map->count == 0)
        return 0;
    vp_assertX(IS_POW2(map->size), "not power of 2");
    vp_assertX(map->count < map->size, "count overflow");
    uint64_t i = hash_u64(key);
    while(true)
    {
        i &= map->size - 1;
        if(map->keys[i] == key)
        {
            return map->vals[i];
        }
        else if(!map->keys[i])
        {
            return 0;
        }
        i++;
    }
    return 0;
}

void* vp_map_get(Map* map, const void* key)
{
    return (void*)(uintptr_t)vp_map_get_u64u64(map, (uint64_t)(uintptr_t)key);
}

void vp_map_put_u64u64(Map* map, uint64_t key, uint64_t val)
{
    vp_assertX(key, "key");
    if(!val)
        return;
    if(2 * map->count >= map->size)
    {
        map_grow(map, 2 * map->size);
    }
    vp_assertX(2 * map->count < map->size, "map overflow");
    vp_assertX(IS_POW2(map->size), "not power of 2");
    uint64_t i = hash_u64(key);
    while(true)
    {
        i &= map->size - 1;
        if(!map->keys[i])
        {
            map->count++;
            map->keys[i] = key;
            map->vals[i] = val;
            return;
        }
        else if(map->keys[i] == key)
        {
            map->vals[i] = val;
            return;
        }
        i++;
    }
}

void vp_map_put(Map* map, const void* key, const void* val)
{
    vp_map_put_u64u64(map, (uint64_t)(uintptr_t)key, (uint64_t)(uintptr_t)val);
}