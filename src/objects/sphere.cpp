#include <cassert>
#include <cmath>
#include <iostream>

#include "aabox.h"
#include "image/rgb.h"
#include "intersection.h"
#include "materials/material.h"
#include "math/matrix.h"
#include "math/vector2.h"
#include "object.h"
#include "ray.h"
#include "sphere.h"

using namespace std;

Sphere::Sphere(const Vector &c, double r, const Material *mat) : Solid(mat) {
  assert(r > 0);
  center = c;
  radius = r;
}

Sphere::~Sphere() {}

void Sphere::transform(const Matrix &m) { center = m * center; }

const Vector &Sphere::getCenter() const { return center; }

double Sphere::getRadius() const { return radius; }

void Sphere::_fullIntersect(const Ray &ray, const double t,
                            Intersection &result) const {
  Vector p = ray.getPoint(t);
  Vector n = p - center;
  n.normalize();
  Vector2 uv;
  if (getMaterial() != NULL && getMaterial()->requiresUV()) {
    // cout << "Getting UV" << endl;
    uv = getUV(n);
  }
  result = Intersection(p, t, n, uv);
}

#define DOT(v1, v2) (v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2])
#define SUB(dest, v1, v2)                                                      \
  dest[0] = v1[0] - v2[0];                                                     \
  dest[1] = v1[1] - v2[1];                                                     \
  dest[2] = v1[2] - v2[2];

double Sphere::_fastIntersect(const Ray &ray) const {

  // See CGPP page 1101
  const Vector &v = ray.getDirection();
  double QmP[3];
  SUB(QmP, ray.getOrigin(), center);
  double a = DOT(v, v);
  double b = 2 * DOT(v, QmP);
  double c = (DOT(QmP, QmP) - radius * radius);
  double D = b * b - 4 * a * c;
  if (D < 0.0) {
    // No roots
    return -1;
  } else if (D == 0.0) {
    // One root
    double t = -b / (2 * a);
    if (IS_NZERO(t)) {
      return t;
    }
  } else {
    // Two roots
    double sq = sqrt(D);
    double t1 = (-b - sq) / (2 * a);
    double t2 = (-b + sq) / (2 * a);
    if (t1 > EPSILON && t1 < t2 && t2 > EPSILON) {
      return t1;
    } else if (t2 > EPSILON) {
      return t2;
    }
  }
  return -1;
}

uint32_t Sphere::maxIntersections() const { return 2; }

uint32_t Sphere::allIntersections(const Ray &ray, Intersection *result) const {

  Vector v = ray.getDirection();
  double QmP[3];
  SUB(QmP, ray.getOrigin(), center);
  double a = v * v;
  double b = 2 * DOT(v, QmP);
  double c = (DOT(QmP, QmP) - radius * radius);
  double D = b * b - 4 * a * c;
  uint32_t n = 0;
  if (D > EPSILON) {
    // Two roots
    double sq = sqrt(D);
    double t1 = (-b - sq) / (2 * a);
    double t2 = (-b + sq) / (2 * a);
    if (t1 > EPSILON && t2 > EPSILON) {
      Intersection i1;
      fullIntersect(ray, t1, i1);
      Intersection i2;
      fullIntersect(ray, t2, i2);
      if (t1 < t2) {
        i1.isEntering(true);
        i2.isEntering(false);
        result[0] = i1;
        result[1] = i2;
      } else {
        i2.isEntering(true);
        i1.isEntering(false);
        result[0] = i2;
        result[1] = i1;
      }
      n = 2;
    } else if (t1 <= EPSILON && t2 > EPSILON) {
      Intersection i2;
      fullIntersect(ray, t2, i2);
      i2.isEntering(false);
      result[0] = i2;
      n = 1;
    } else if (t2 <= EPSILON && t1 > EPSILON) {
      Intersection i1;
      fullIntersect(ray, t1, i1);
      i1.isEntering(false);
      result[0] = i1;
      n = 1;
    }
  }
  return n;
}

ostream &operator<<(ostream &os, const Sphere &s) {
  os << '(' << s.center << ',' << s.radius << ')';
  return os;
}

AABox Sphere::getBoundingBox() const {
  Vector r = Vector(radius, radius, radius);
  r += Vector(5 * EPSILON, 5 * EPSILON, 5 * EPSILON);
  return AABox(center - r, center + r);
}

AABox Sphere::getContainedBox() const {
  double a = radius / M_SQRT3 - 5 * EPSILON;
  Vector v = Vector(a, a, a);
  return AABox(center - v, center + v);
}

// See http://astronomy.swin.edu.au/~pbourke/texture/spheremap/
Vector2 Sphere::getUV(const Vector &p) const {
  double u, v;
  v = acos(p[1]) / M_PI;
  if (IS_ZERO(sin((v)*M_PI))) {
    u = double(0.5);
    return Vector2(u, v);
  }
  if (p[2] <= 0.0) {
    u = acos(p[0] / (sin((v)*M_PI))) / M_2PI;
  } else {
    u = 1 - (acos(p[0] / (sin((v)*M_PI))) / M_2PI);
  }
  return Vector2(u, v);
}

/**
 * Check to see if the sphere overlaps the voxel_bbox by
 * finding the squared distance from the sphere to the bbox.
 *
 * J. Arvo: "A simple method for box-sphere intersection testing" in: A.
 * Glassner (ed.), <i>Graphics Gems</i>, pp. 335-339, Academic Press, Boston,
 * MA, 1990.
 */
int Sphere::intersects(const AABox &voxel_bbox, const AABox &obj_bbox) const {
  double s;
  double d = 0.0;
  for (int i = 0; i < 3; i++) {
    if (center[i] < voxel_bbox.minimum()[i]) {
      s = center[i] - voxel_bbox.minimum()[i];
      d += s * s;
    } else if (center[i] > voxel_bbox.maximum()[i]) {
      s = center[i] - voxel_bbox.maximum()[i];
      d += s * s;
    }
  }
  return d <= radius * radius + EPSILON ? 1 : -1;
}

SceneObject *Sphere::clone() const {
  Sphere *result = new Sphere(*this);
  return result;
}

bool Sphere::inside(const Vector &p) const {
  return (p - center).norm() < radius * radius;
}

double Sphere::signedDistance(const Vector &p) const {
  return (p - center).length() - radius;
}
