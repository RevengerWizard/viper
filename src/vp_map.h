/*
** vp_map.h
** Map handling
*/

#ifndef _VP_MAP_H
#define _VP_MAP_H

#include "vp_def.h"

typedef struct Map
{
    uint64_t* keys;
    uint64_t* vals;
    uint32_t count;
    uint32_t size;
} Map;

void* vp_map_get(Map* map, const void* key);
uint64_t vp_map_get_u64u64(Map* map, uint64_t key);
void vp_map_put(Map* map, const void* key, const void* val);
void vp_map_put_u64u64(Map* map, uint64_t key, uint64_t val);

#endif