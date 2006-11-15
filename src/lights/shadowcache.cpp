
#include <cstdlib>
#include "objects/object.h"
#include "lights/shadowcache.h"
#include "space/kdtree.h"
#include "ray.h"

CounterStats* ShadowCache::shadow_rays_cast = new CounterStats("Lights","Shadow rays cast");
CounterStats* ShadowCache::shadow_cache_hint_hit = new CounterStats("Lights","Shadow cache hint hit");

ShadowCache::ShadowCache() {
    for(int i = 0; i < LIGHTS_SHADOW_CACHE_MAX_DEPTH; i++) {
	hints[i] = NULL;
	voxels[i] = NULL;
    }
}

Object* ShadowCache::getHint(uint32_t depth) const {
    if (depth < LIGHTS_SHADOW_CACHE_MAX_DEPTH) {
	return hints[depth];
    } else {
	return NULL;
    }
}

void ShadowCache::putHint(uint32_t depth, Object* object) {
    if (depth < LIGHTS_SHADOW_CACHE_MAX_DEPTH) {
	hints[depth] = object;
    }
}

vector<Object*>* ShadowCache::getVoxel(uint32_t depth) const {
    if (depth < LIGHTS_SHADOW_CACHE_MAX_DEPTH) {
	return voxels[depth];
    } else {
	return NULL;
    }
}

void ShadowCache::putVoxel(uint32_t depth, vector<Object*>* voxel) {
    if (depth < LIGHTS_SHADOW_CACHE_MAX_DEPTH) {
	voxels[depth] = voxel;
    }
}


bool ShadowCache::occluded(const Ray& ray_to_light, const double dist_to_light, uint32_t depth, KdTree* space, Object* ignore) {
    // Check last shadowing object first
    Object* hint = getHint(depth);
    if (hint != NULL && hint != ignore) {
	double t = hint->fastIntersect(ray_to_light);
	if (t > 0 && t < dist_to_light) {
	    Stats::getUniqueInstance()->inc(STATS_SHADOW_HINT_HIT);
	    shadow_cache_hint_hit->inc();
	    return true;
	}
    }
    
    // Check other objects in the same BSP-voxel
    vector<Object*>* voxel = getVoxel(depth);
    if (voxel != NULL) {
	uint32_t size = voxel->size();
	const vector<Object*> &objects = *(voxel);
	for(uint32_t i = 0; i < size; i++) {
	    if (objects[i] != ignore) {
               double t = objects[i]->fastIntersect(ray_to_light);
   	       if (t > 0 && t < dist_to_light) {
	           Stats::getUniqueInstance()->inc(STATS_SHADOW_VOXEL_HIT);
                   return true;
	       }
	    }
	}
    }

    // Finally do a real intersection test
    shadow_rays_cast->inc();
    Stats::getUniqueInstance()->inc(STATS_SHADOW_RAYS_CAST);
    hint = space->intersectForShadow(ray_to_light,dist_to_light,ignore);
    putHint(depth,hint);
    putVoxel(depth,NULL);
    return hint != NULL;
}

