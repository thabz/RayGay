
#ifndef LIGHTS_SHADOW_CACHE_H
#define LIGHTS_SHADOW_CACHE_H

#define LIGHTS_SHADOW_CACHE_MAX_DEPTH 32

#include "space/kdtree.h"
#include "stats.h"
#include <vector>

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
  bool occluded(const Ray &ray_to_light, const double dist_to_light,
                uint32_t depth, KdTree *space, const Object *ignore = NULL);

private:
  // Methods
  Object *getHint(uint32_t depth) const;
  void putHint(uint32_t depth, Object *object);
  vector<Object *> *getVoxel(uint32_t depth) const;
  void putVoxel(uint32_t depth, vector<Object *> *voxel);

  // Data
  Object *hints[LIGHTS_SHADOW_CACHE_MAX_DEPTH];
  vector<Object *> *voxels[LIGHTS_SHADOW_CACHE_MAX_DEPTH];

  // Stats
  static CounterStats *shadow_rays_cast;
  static CounterStats *shadow_cache_hint_hit;
};

#endif
