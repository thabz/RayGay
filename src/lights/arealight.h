#ifndef AREALIGHT_H
#define AREALIGHT_H

#include "lights/lightsource.h"
#include "lights/shadowcache.h"
#include <vector>

class Circle;
class Object;

/// Lightsource that produces soft shadows
class Arealight : public Lightsource {

public:
  /// Constructor
  Arealight(const Vector &pos, const Vector &dir, double radius, uint32_t num,
            double jitter);
  virtual ~Arealight();
  void getLightinfo(const Intersection &inter, KdTree *space, Lightinfo *info,
                    uint32_t depth) const;
  void getSingleLightinfo(const Intersection &inter, KdTree *space,
                          Lightinfo *info, uint32_t depth) const;
  void transform(const Matrix &m);

private:
  std::vector<Circle *> circles;
  std::vector<double> ts;
  double jitter;
  pthread_key_t shadowcaches_key;
  uint32_t num;

  Vector getPosition(uint32_t i) const;
  bool probeSublight(uint32_t i, const Intersection &inter, KdTree *space,
                     uint32_t depth) const;
};

#endif
