
#include "paths/circle.h"
#include <cmath>

/**
 * Constructs a circle path
 *
 * @param center Center of the circle
 * @param radius Radius of the circle
 * @param normal Normal of the plane the circle should live in
 */
Circle::Circle(const Vector &center, double radius, const Vector &normal) {
  c = center;
  r = radius;
  n = normal;
  n.normalize();
  Vector y = Vector(0, 1, 0);
  Vector x = Vector(1, 0, 0);
  Vector a =
      n == y ? x : y; // TODO: This doesn't work... y-axis aligned normals fails
  m = Matrix::matrixOrient(n, Vector::xProduct(a, n));
  orient = m;
  m = m * Matrix::matrixTranslate(center);
}

Vector Circle::getPoint(double t) const {
  double rad = M_2PI * t;
  Vector result = Vector(cos(rad) * r, sin(rad) * r, 0);
  return m * result;
}

Vector Circle::getTangent(double t) const {
  double rad = M_2PI * t;
  Vector result = Vector(-sin(rad), cos(rad), 0);
  return orient * result;
}

void Circle::transform(const Matrix &m) {
  c = m * c;
  n = m.extractRotation() * n;
}
