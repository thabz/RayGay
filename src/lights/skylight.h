#ifndef SKYLIGHT_H
#define SKYLIGHT_H

#include "lights/lightsource.h"
#include "lights/shadowcache.h"
#include <stdint.h>
#include <vector>

class Object;

/// Hemispherical lightsource
class Skylight : public Lightsource {

public:
  Skylight(double radius, uint32_t num);
  virtual ~Skylight(){};
  void getLightinfo(const Intersection &inter, KdTree *space, Lightinfo *info,
                    uint32_t depth) const;
  void getSingleLightinfo(const Intersection &inter, KdTree *space,
                          Lightinfo *info, uint32_t depth) const;
  void transform(const Matrix &m){};

private:
  std::vector<Vector> positions;
  uint64_t shadowcache_id;
  double radius;
  uint32_t num;

  bool probe(uint32_t num, const Ray &ray, double dist, uint32_t depth,
             KdTree *space) const;
};

#endif
