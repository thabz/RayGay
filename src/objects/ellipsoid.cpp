
#include "objects/ellipsoid.h"
#include "aabox.h"
#include "materials/material.h"

Ellipsoid::Ellipsoid(const Vector &center, const Vector &radii,
                     Material *material)
    : Solid(material) {
  // The ellipsoid in object space is simply a unit-sphere
  // with its center at the origin.
  sphere = new Sphere(Vector(0, 0, 0), 1, NULL);
  transform(Matrix::matrixScale(radii));
  transform(Matrix::matrixTranslate(center));
}

void Ellipsoid::transform(const Matrix &m) { Transformer::transform(m); }

AABox Ellipsoid::getBoundingBox() const {
  return bboxToWorld(sphere->getBoundingBox());
}

void Ellipsoid::_fullIntersect(const Ray &world_ray, const double t,
                               Intersection &result) const {
  Ray ray = rayToObject(world_ray);
  double new_t = t * ray.t_scale;
  Vector p = ray.getPoint(new_t);

  Vector2 uv;
  if (getMaterial() != NULL && getMaterial()->requiresUV()) {
    uv = sphere->getUV(p);
  }

  // The normalized normal at a surface point of a unit-sphere
  // is the same as the surface point itself.
  result = Intersection(p, new_t, p, uv);
  intersectionToWorld(result);
}

double Ellipsoid::_fastIntersect(const Ray &world_ray) const {
  Ray local_ray = rayToObject(world_ray);
  double res = sphere->fastIntersect(local_ray);
  return res / local_ray.t_scale;
}

SceneObject *Ellipsoid::clone() const { return new Ellipsoid(*this); }

uint32_t Ellipsoid::maxIntersections() const { return 2; }

uint32_t Ellipsoid::allIntersections(const Ray &world_ray,
                                     Intersection *result) const {
  Ray local_ray = rayToObject(world_ray);
  uint32_t num = sphere->allIntersections(local_ray, result);
  for (uint32_t i = 0; i < num; i++) {
    Intersection is = result[i];
    intersectionToWorld(is);
    is.setT(is.getT() / local_ray.t_scale);
    result[i] = is;
  }
  return num;
}

bool Ellipsoid::inside(const Vector &point) const {
  Vector p = pointToObject(point);
  return p.x() * p.x() + p.y() * p.y() + p.z() * p.z() < 1;
}
