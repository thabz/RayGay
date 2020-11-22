#ifndef SPHERE_H
#define SPHERE_H

#include "aabox.h"
#include "math/vector.h"
#include "object.h"
#include "solid.h"

class Intersection;
class Ray;
class Matrix;
class Vector2;

/// A sphere object
class Sphere : public Solid {

  friend std::ostream &operator<<(std::ostream &os, const Sphere &s);

public:
  /// Constructor
  Sphere(const Vector &c, double r, const Material *material);

  /// Destructor
  virtual ~Sphere();

  /// Returns center of sphere
  const Vector &getCenter() const;

  /// Returns radius of sphere
  double getRadius() const;

  virtual void transform(const Matrix &m);

  virtual AABox getBoundingBox() const;

  AABox getContainedBox() const;

  virtual SceneObject *clone() const;
  double _fastIntersect(const Ray &ray) const;
  void _fullIntersect(const Ray &ray, const double t,
                      Intersection &result) const;
  int intersects(const AABox &voxel_bbox, const AABox &obj_bbox) const;
  uint32_t allIntersections(const Ray &ray, Intersection *result) const;
  uint32_t maxIntersections() const;

  Vector2 getUV(const Vector &normal) const;

  bool inside(const Vector &p) const;

private:
  Vector center;
  double radius;
};

#endif
