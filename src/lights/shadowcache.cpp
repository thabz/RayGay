
#include <cstdlib>
#include "objects/object.h"
#include "lights/shadowcache.h"
#include "space/spacesubdivider.h"
#include "ray.h"

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

bool ShadowCache::occluded(const Ray& ray_to_light, const double dist_to_light, unsigned int depth, SpaceSubdivider* space) {
    // TODO: Check other objects in hints voxel
    Object* hint = get(depth);
    if (hint != NULL) {
	double t = hint->fastIntersect(ray_to_light);
	if (t > 0 && t < dist_to_light) {
	    return true;
	}
    }
    hint = space->intersectForShadow(ray_to_light,dist_to_light);
    put(depth,hint);
    return hint != NULL;
}

