
#include <cstdlib>
#include "objects/object.h"
#include "lights/shadowcache.h"
#include "space/spacesubdivider.h"
#include "ray.h"
#include "stats.h"

ShadowCache::ShadowCache() {
    for(int i = 0; i < LIGHTS_SHADOW_CACHE_MAX_DEPTH; i++) {
	hints[i] = NULL;
	voxels[i] = NULL;
    }
}

Object* ShadowCache::getHint(unsigned int depth) const {
    if (depth < LIGHTS_SHADOW_CACHE_MAX_DEPTH) {
	return hints[depth];
    } else {
	return NULL;
    }
}

void ShadowCache::putHint(unsigned int depth, Object* object) {
    if (depth < LIGHTS_SHADOW_CACHE_MAX_DEPTH) {
	hints[depth] = object;
    }
}

vector<Object*>* ShadowCache::getVoxel(unsigned int depth) const {
    if (depth < LIGHTS_SHADOW_CACHE_MAX_DEPTH) {
	return voxels[depth];
    } else {
	return NULL;
    }
}

void ShadowCache::putVoxel(unsigned int depth, vector<Object*>* voxel) {
    if (depth < LIGHTS_SHADOW_CACHE_MAX_DEPTH) {
	voxels[depth] = voxel;
    }
}


bool ShadowCache::occluded(const Ray& ray_to_light, const double dist_to_light, unsigned int depth, SpaceSubdivider* space) {
    // Check last shadowing object first
    Object* hint = getHint(depth);
    if (hint != NULL) {
	double t = hint->fastIntersect(ray_to_light);
	if (t > 0 && t < dist_to_light) {
	    Stats::getUniqueInstance()->inc(STATS_SHADOW_HINT_HIT);
	    return true;
	}
    }
    
    // Check other objects in the same BSP-voxel
    vector<Object*>* voxel = getVoxel(depth);
    if (voxel != NULL) {
	unsigned int size = voxel->size();
	const vector<Object*> &objects = *(voxel);
	for(unsigned int i = 0; i < size; i++) {
           double t = objects[i]->fastIntersect(ray_to_light);
	   if (t > 0 && t < dist_to_light) {
	       Stats::getUniqueInstance()->inc(STATS_SHADOW_VOXEL_HIT);
               return true;
	   }
	}
    }

    // Finally do a real intersection test
    Stats::getUniqueInstance()->inc(STATS_SHADOW_RAYS_CAST);
    hint = space->intersectForShadow(ray_to_light,dist_to_light);
    putHint(depth,hint);
    putVoxel(depth,NULL);
    return hint != NULL;
}

