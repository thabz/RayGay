#include "objects/sdf_object.h"
#include "aabox.h"
#include "intersection.h"
#include <cmath>

class Material;

SDFObject::SDFObject(Solid *solid, double accuracy, Material *material)
    : Object(material) {
  this->solid = solid;
  this->accuracy = accuracy;
}

double SDFObject::evaluateFunction(const Vector &p) const {
  return solid->signedDistance(p);
}

SceneObject *SDFObject::clone() const {
  SDFObject *result = new SDFObject(*this);
  return result;
}

void SDFObject::_fullIntersect(const Ray &ray, const double t,
                               Intersection &result) const {
  Vector p = ray.getPoint(t * ray.t_scale);
  result = Intersection(p, t * ray.t_scale, normal(p), Vector2(0, 0));
}

#define MAX_ITER 200

double SDFObject::_fastIntersect(const Ray &ray) const {
  int i = 0;
  double t = 0;
  while (i++ < MAX_ITER) {
    double distance = fabs(evaluateFunction(ray.getPoint(t * ray.t_scale)));
    if (distance < accuracy) {
      return t / ray.t_scale;
    }
    t += distance;
  }
  return -1;
}

/**
 * Finds the surface normal at a point.
 *
 * @param p a surface point in object space
 * @return the surface normal at point
 */
Vector SDFObject::normal(const Vector &p) const {
  double off = accuracy;
  double x = evaluateFunction(p - Vector(off, 0, 0)) -
             evaluateFunction(p + Vector(off, 0, 0));
  double y = evaluateFunction(p - Vector(0, off, 0)) -
             evaluateFunction(p + Vector(0, off, 0));
  double z = evaluateFunction(p - Vector(0, 0, off)) -
             evaluateFunction(p + Vector(0, 0, off));
  Vector normal = Vector(0.5 * x, 0.5 * y, 0.5 * z);
  normal.normalize();
  return normal;
}

// bool SDFObject::intersects(const AABox &b) const {
//   return b.inside(getBoundingBox());
// }

void SDFObject::transform(const Matrix &m) { solid->transform(m); }

AABox SDFObject::getBoundingBox() const {
  AABox result = solid->getBoundingBox();
  result.grow(20 * EPSILON);
  return result;
}
