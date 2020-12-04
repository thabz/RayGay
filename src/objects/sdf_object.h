
#ifndef OBJECTS_SDF_OBJECT_H
#define OBJECTS_SDF_OBJECT_H

#include "objects/object.h"
#include "objects/solid.h"
#include "transformer.h"
#include <vector>

class Material;
/**
 * An object that is defined by a signed distance
 * function and is rendered by raymarching.
 */
class SDFObject : public Object {

public:
  SDFObject(const double accuracy, const Material *m);

  virtual double signedDistance(const Vector &point) const = 0;

  bool intersects(const AABox &b) const;
  void transform(const Matrix &m);
  /// Return the bounding box in world space coordinates
  AABox getBoundingBox() const;
  bool inside(const Vector &p) const;

protected:
  double _fastIntersect(const Ray &ray) const;
  void _fullIntersect(const Ray &ray, const double t,
                      Intersection &result) const;
  double accuracy;

private:
  Vector normal(const Vector &p) const;
};

#endif
