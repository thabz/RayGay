
#ifndef OBJECTS_SDF_OBJECT_H
#define OBJECTS_SDF_OBJECT_H

#include "objects/object.h"
#include "objects/solid.h"
#include "transformer.h"
#include <vector>

/**
 * A signed distance function object is solid that is grown.
 * Ie. a box gets a bigger rounded surface.
 */
class SDFObject : public Object, public Transformer {

public:
  SDFObject(Solid *solid, double grow, double accuracy, Material *m);

  bool intersects(const AABox &b) const;
  void transform(const Matrix &m);
  /// Return the bounding box in world space coordinates
  AABox getBoundingBox() const;
  bool inside(const Vector &p) const;

protected:
  /// Constructor
  SceneObject *clone() const;

  double _fastIntersect(const Ray &ray) const;
  void _fullIntersect(const Ray &ray, const double t,
                      Intersection &result) const;

  /// Returns the bounding box in object space coordinates
  AABox _getBoundingBox() const;

private:
  Vector normal(const Vector &p) const;
  double evaluateFunction(const Vector &v) const;
  Solid *solid;
  double grow;
  double accuracy;
};

#endif
