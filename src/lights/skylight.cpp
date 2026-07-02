
#include "lights/skylight.h"
#include "intersection.h"
#include "math/functions.h"
#include "math/halton.h"

#include <atomic>
#include <map>

class KdTree;

namespace {
uint64_t nextSkylightShadowCacheId() {
  static std::atomic<uint64_t> next_id(0);
  return ++next_id;
}

std::map<uint64_t, std::vector<ShadowCache> > &skylightShadowCaches() {
  static thread_local std::map<uint64_t, std::vector<ShadowCache> > caches;
  return caches;
}
} // namespace

Skylight::Skylight(double radius, uint32_t num) : Lightsource(Vector(0, 0, 0)) {
  this->radius = radius;
  this->num = num;
  this->shadowcache_id = nextSkylightShadowCacheId();
  Halton qmc = Halton(2, 2);

  for (int i = 0; i < num; i++) {
    Vector pos = Math::perturbVector(Vector(0, 1, 0), DEG2RAD(89), &qmc);
    positions.push_back(pos * radius);
  }
}

void Skylight::getLightinfo(const Intersection &inter, KdTree *space,
                            Lightinfo *info, uint32_t depth) const {
  Vector surface_point =
      inter.getPoint() + 1000 * EPSILON * inter.getOriginalNormal();
  uint32_t count = 0;
  double cos_total = 0;
  double cos_tmp;
  for (uint32_t i = 0; i < num; i++) {
    Vector direction_to_light = positions[i] - surface_point;
    double dist_to_light = direction_to_light.length();
    direction_to_light = direction_to_light / dist_to_light;
    cos_tmp = direction_to_light * inter.getNormal();
    if (cos_tmp > 0.0) {
      Ray ray_to_light = Ray(surface_point, direction_to_light, -1.0);
      bool occluded = probe(i, ray_to_light, dist_to_light, depth, space);
      if (!occluded) {
        count++;
        cos_total += cos_tmp;
      }
    }
  }
  info->cos = cos_total / double(count);
  info->intensity = double(count) / num;
  info->direction_to_light = Vector(0, 1, 0); // Should be undefined
}

void Skylight::getSingleLightinfo(const Intersection &inter, KdTree *space,
                                  Lightinfo *info, uint32_t depth) const {
  Vector surface_point =
      inter.getPoint() + 1000 * EPSILON * inter.getOriginalNormal();
  uint32_t sublight = uint32_t(RANDOM(0, num));

  info->direction_to_light = positions[sublight] - surface_point;
  info->direction_to_light.normalize();
  info->cos = info->direction_to_light * inter.getNormal();
  double dist_to_light = info->direction_to_light.length();

  if (info->cos > 0.0) {
    Ray ray_to_light = Ray(surface_point, info->direction_to_light, -1.0);
    bool occluded = probe(num, ray_to_light, dist_to_light, depth, space);
    info->intensity = occluded ? 0 : 1;
  }
}

bool Skylight::probe(uint32_t i, const Ray &ray, double dist, uint32_t depth,
                     KdTree *space) const {
  std::vector<ShadowCache> &shadowcaches =
      skylightShadowCaches()[shadowcache_id];
  if (shadowcaches.size() != num) {
    shadowcaches = std::vector<ShadowCache>(num);
  }
  return shadowcaches[i].occluded(ray, dist, depth, space);
}
