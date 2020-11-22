
#ifndef OBJECTS_ELLIPSOID_H
#define OBJECTS_ELLIPSOID_H

#include "objects/solid.h"
#include "objects/sphere.h"
#include "transformer.h"

/**
 * An ellipsoid object.
 */
class Ellipsoid : public Solid, public Transformer {
public:
  Ellipsoid(const Vector &center, const Vector &radii, Material *material);
  void transform(const Matrix &m);
  AABox getBoundingBox() const;
  SceneObject *clone() const;
  uint32_t allIntersections(const Ray &ray, Intersection *result) const;
  uint32_t maxIntersections() const;

  bool inside(const Vector &point) const;

private:
  double _fastIntersect(const Ray &ray) const;
  void _fullIntersect(const Ray &ray, const double t,
                      Intersection &result) const;

  Sphere *sphere;
};

#endif
