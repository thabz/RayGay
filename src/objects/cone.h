#ifndef CONE_H
#define CONE_H

#include "aabox.h"
#include "math/matrix.h"
#include "math/vector.h"
#include "object.h"
#include "solid.h"
#include "transformer.h"

class Vector;
class Intersection;
class Ray;
class Vector2;

/**
 * A cone object
 */
class Cone : public Solid, public Transformer {

public:
  /// Constructor
  Cone(const Vector &begin, const Vector &end, double radius_begin,
       double radius_end, bool has_caps, const Material *m);
  ~Cone(){};
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
  uint32_t allPositiveRoots(const Ray &world_ray, double roots[4]) const;
  Vector getNormal(const Vector &local_point) const;

  Vector begin;
  Vector end;

  double radius_begin; /// Radius at bottom of cone
  double radius_end;   /// Radius at top of cone
  bool has_caps;
};

#endif
