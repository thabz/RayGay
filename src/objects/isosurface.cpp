
#include "objects/isosurface.h"
#include "aabox.h"
#include "intersection.h"
#include "math/vector2.h"
#include <cassert>
#include <iostream>

using namespace std;

IsoSurface::IsoSurface(uint32_t steps, double accuracy, double iso,
                       Material *mat)
    : Object(mat) {
  this->steps = steps;
  this->accuracy = accuracy;
  this->iso = iso;
}

void IsoSurface::_fullIntersect(const Ray &world_ray, const double t,
                                Intersection &result) const {
  Ray ray = rayToObject(world_ray);
  Vector p = ray.getPoint(t * ray.t_scale);
  result = Intersection(p, t * ray.t_scale, normal(p), Vector2(0, 0));
  intersectionToWorld(result);
}

#define func(x) (evaluateFunction(ray.getPoint(x)) - iso)
#define MAX_ITER 200

double IsoSurface::refine(const Ray &ray, double t_begin, double t_end,
                          double f_t_end) const {
  double f_t_begin = func(t_begin);
  double f_t_mid, t_mid;
  uint32_t i = 0;

  // assert(!SAME_SIGN(f_t_begin, f_t_end));

  while (i++ < MAX_ITER) {
    t_mid = (f_t_end * t_begin - f_t_begin * t_end) / (f_t_end - f_t_begin);
    f_t_mid = func(t_mid);

    if (fabs(f_t_mid) < accuracy / 100) {
      return t_mid;
    }

    if (SAME_SIGN(f_t_begin, f_t_mid)) {
      t_begin = t_mid;
      f_t_begin = f_t_mid;
    } else {
      t_end = t_mid;
      f_t_end = f_t_mid;
    }
  }
  // Refinement failed. Return the best guess.
  return 0.5 * (t_begin + t_end);
}

double IsoSurface::_fastIntersect(const Ray &world_ray) const {

  Ray ray = rayToObject(world_ray);
  double res = -1;

  const AABox &bbox = this->_getBoundingBox();
  Vector2 inout = bbox.intersect(ray);
  double t_begin = std::max(inout[0], accuracy);
  double t_end = inout[1] + accuracy;
  if (t_end > t_begin) {

    int start_sign = SIGN(func(t_begin));
    double t_step = (t_end - t_begin) / double(steps);
    double f_t;

    for (double t = t_begin; t <= t_end; t += t_step) {
      f_t = func(t);
      if (SIGN(f_t) != start_sign) {
        res = refine(ray, t - t_step, t, f_t);
        break;
      }
    }
  }

  if (res > EPSILON) {
    return res / ray.t_scale;
  } else {
    return -1;
  }
}

/**
 * Finds the surface normal at a point.
 *
 * @param p a surface point in object space
 * @return the surface normal at point
 */
Vector IsoSurface::normal(const Vector &p) const {
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

void IsoSurface::transform(const Matrix &m) { Transformer::transform(m); }

AABox IsoSurface::getBoundingBox() const {
  AABox result = bboxToWorld(_getBoundingBox());
  result.grow(20 * EPSILON);
  return result;
}
