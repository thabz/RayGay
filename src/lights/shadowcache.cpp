
#include <cstdlib>
#include "lights/shadowcache.h"

ShadowCache::ShadowCache() {
    for(int i = 0; i < LIGHTS_SHADOW_CACHE_MAX_DEPTH; i++) {
	objectlist[i] = NULL;
    }
}

Object* ShadowCache::get(unsigned int depth) const {
    if (depth < LIGHTS_SHADOW_CACHE_MAX_DEPTH) {
	return objectlist[depth];
    } else {
	return NULL;
    }
}

void ShadowCache::put(unsigned int depth, Object* object) {
    if (depth < LIGHTS_SHADOW_CACHE_MAX_DEPTH) {
	objectlist[depth] = object;
    }
}

