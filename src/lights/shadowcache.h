
#ifndef LIGHTS_SHADOW_CACHE_H
#define LIGHTS_SHADOW_CACHE_H

#define LIGHTS_SHADOW_CACHE_MAX_DEPTH 32

#include <vector>
#include "stats.h"

using namespace std;

class Ray;
class Object;

/**
 * A Shadow cache.
 *
 * This class does occlusion checking and caches results.
 */
class ShadowCache {
    public:
	ShadowCache();
	bool occluded(const Ray& ray_to_light, const double dist_to_light, unsigned int depth, KdTree* space);

    private:
	// Methods
	Object* getHint(unsigned int depth) const;
	void putHint(unsigned int depth, Object* object);
	vector<Object*>* getVoxel(unsigned int depth) const;
	void putVoxel(unsigned int depth, vector<Object*>* voxel);

	// Data
	Object* hints[LIGHTS_SHADOW_CACHE_MAX_DEPTH];
	vector<Object*>* voxels[LIGHTS_SHADOW_CACHE_MAX_DEPTH];

	// Stats
	static CounterStats* shadow_rays_cast;
	static CounterStats* shadow_cache_hint_hit;
};

#endif

