#include "objects/sdf_object.h"
#include "aabox.h"
#include "intersection.h"
#include <cmath>

class Material;

SDFObject::SDFObject(const double accuracy, const Material *material)
    : Object(material) {
  this->accuracy = accuracy;
}

void SDFObject::_fullIntersect(const Ray &ray, const double t,
                               Intersection &result) const {
  Vector p = ray.getPoint(t * ray.t_scale);
  result = Intersection(p, t * ray.t_scale, normal(p), Vector2(0, 0));
}

#define MAX_ITER 200

// Raymarches using the signed distance function
// to get safe steps.
double SDFObject::_fastIntersect(const Ray &ray) const {
  int i = 0;
  double t = 0;
  while (i++ < MAX_ITER) {
    double distance = fabs(signedDistance(ray.getPoint(t * ray.t_scale)));
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
  double x = signedDistance(p - Vector(off, 0, 0)) -
             signedDistance(p + Vector(off, 0, 0));
  double y = signedDistance(p - Vector(0, off, 0)) -
             signedDistance(p + Vector(0, off, 0));
  double z = signedDistance(p - Vector(0, 0, off)) -
             signedDistance(p + Vector(0, 0, off));
  Vector normal = Vector(0.5 * x, 0.5 * y, 0.5 * z);
  normal.normalize();
  return normal;
}
