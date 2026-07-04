
#include "lights/pointlight.h"
#include "intersection.h"
#include "lights/lightinfo.h"
#include "lights/shadowcache.h"
#include "objects/object.h"

#include <atomic>
#include <map>

class KdTree;

namespace {
uint64_t nextPointlightShadowCacheId() {
  static std::atomic<uint64_t> next_id(0);
  return ++next_id;
}

std::map<uint64_t, ShadowCache> &pointlightShadowCaches() {
  static thread_local std::map<uint64_t, ShadowCache> caches;
  return caches;
}
} // namespace

Pointlight::Pointlight(const Vector &pos) : Lightsource(pos) {
  shadowcache_id = nextPointlightShadowCacheId();
}

void Pointlight::getLightinfo(const Intersection &inter, KdTree *space,
                              Lightinfo *info, uint32_t depth) const {

  ShadowCache *shadowcache = &pointlightShadowCaches()[shadowcache_id];

  // Move intersection point ESPILON along surface normal to
  // avoid selfshadowing.
  Vector surface_point =
      inter.getPoint() + 1000 * EPSILON * inter.getOriginalNormal();

  info->direction_to_light = position - surface_point;
  double dist_to_light = info->direction_to_light.length();
  if (IS_ZERO(dist_to_light)) {
    info->cos = 0.0;
    info->intensity = 0.0;
    return;
  }
  info->direction_to_light = info->direction_to_light / dist_to_light;
  info->cos = info->direction_to_light * inter.getNormal();

  if (info->cos > 0.0) {
    Ray ray_to_light = Ray(surface_point, info->direction_to_light, -1.0);
    const Object *ignore = NULL;
    if (!inter.getObject()->canSelfshadow()) {
      ignore = inter.getObject();
    }
    bool occluded = shadowcache->occluded(ray_to_light, dist_to_light, depth,
                                          space, ignore);
    info->intensity = occluded ? 0.0 : 1.0;
  }
}
