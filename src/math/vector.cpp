/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Copyright 2003 by Jesper Christensen <jesper@kalliope.org>
 *
 * Added Thu Apr 17 2003
 */

#include <cassert>
#include <cmath>
#include <cstdlib>
#include <iostream>

#include "constants.h"
#include "math/matrix.h"
#include "math/vector.h"

using namespace std;

const Vector Vector::reflect(const Vector &N) {
  Vector L = *(this);
  Vector R = (2 * (N * L)) * N - L;
  return R;
}

ostream &operator<<(ostream &os, const Vector &x) {
  os << '(' << x[0] << ',';
  os << x[1] << ',';
  os << x[2] << ')';

  return os;
}

bool Vector::operator==(const Vector &x) const {
  return IS_EQUAL(x[0], _vector[0]) && IS_EQUAL(x[1], _vector[1]) &&
         IS_EQUAL(x[2], _vector[2]);
}

bool Vector::operator!=(const Vector &x) const { return !(*this == x); }

/*
Vector Vector::xProduct(const Vector& v1, const Vector& v2) {
    return Vector(v1[1]*v2[2] - v1[2]*v2[1],
                  v1[2]*v2[0] - v1[0]*v2[2],
                  v1[0]*v2[1] - v1[1]*v2[0]);
}
*/
/**
 * Convert to polar aka spherical coordinates.
 *
 * @return (r,theta,phi) where r is the distance from the origin, theta is the
 * angle around y and phi is the angle around z.
 */
Vector Vector::toPolar() const {
  double r = this->length();
  assert(IS_NZERO(r)); // Maybe just return (0,0,0) instead?
  double theta, phi;
  theta = acos(_vector[2] / r);
  phi = atan2(_vector[1], _vector[0]);
  return Vector(r, theta, phi);
}

/**
 * Converts from polar (r,theta,phi) to rectangular (x,y,z) coordinates
 */
Vector Vector::toRectangular() const {
  double x = _vector[0] * sin(_vector[1]) * cos(_vector[2]);
  double y = _vector[0] * sin(_vector[1]) * sin(_vector[2]);
  double z = _vector[0] * cos(_vector[1]);
  return Vector(x, y, z);
}

double Vector::area(const Vector &v0, const Vector &v1, const Vector &v2) {
  return xProduct(v1 - v0, v2 - v0).length() / 2.0;
}

//----------------------------------------
// Friends
// ---------------------------------------

/**
 * Returns a random unit vector.
 *
 * Works by finding a random point in the double unit box and
 * rejecting it if its length is greater than 1. Otherwise
 * it is normalized and returned.
 *
 * The volume of the unit-sphere is PI*4/3 and the box is 8, so
 * only 48% are rejected.
 */
Vector Vector::randomUnitVector() {
  Vector v;
  do {
    v = Vector(RANDOM(-1, 1), RANDOM(-1, 1), RANDOM(-1, 1));
  } while (v.norm() > 1.0);

  v.normalize();
  return v;
}

/**
 * Returns this vector refracted around a normal.
 *
 * @param normal the normal
 * @param ior indice of refraction
 *
 * @see page 757
 */
Vector Vector::refract(const Vector &normal, double ior) const {
  Vector N;
  Vector I = (*this);
  if ((*this) * normal > double(0)) {
    N = -normal;
  } else {
    N = normal;
    ior = double(1.0) / ior;
  }
  double IdotN = (*this) * N;
  double k = 1.0 - ior * ior * (1.0 - IdotN * IdotN);
  Vector T =
      (k < 0) ? Vector(0, 0, 0) : (ior * I + (ior * IdotN - sqrt(k)) * N);
  T.normalize();
  return T;
}

/**
 * Get a cosine distributed random direction on the hemisphere
 * with this vector as normal.
 */
Vector Vector::randomHemisphere(const double rnd1, const double rnd2) const {
  Matrix m = Matrix::matrixOrient(*this);
  m = m.inverse();
  double u = 2 * M_PI * rnd1;
  double v = rnd2;
  double s = sqrt(v);
  double s1 = sqrt(1.0 - v);
  Vector result = Vector(cos(u) * s, sin(u) * s, s1);
  result = m * result;
  result.normalize();
  return result;
}

Vector Vector::randomHemisphere() const {
  return randomHemisphere(RANDOM(0, 1), RANDOM(0, 1));
}

Vector Vector::randomHemisphere(const double rnd1, const double rnd2,
                                double jitter) const {
  double a1 = (1.0 - jitter) * rnd1 + RANDOM(0, jitter);
  double a2 = (1.0 - jitter) * rnd2 + RANDOM(0, jitter);
  return randomHemisphere(a1, a2);
}

int Vector::largestDimension() const {
  if (_vector[0] > _vector[1]) {
    return _vector[0] > _vector[2] ? 0 : 2;
  } else {
    return _vector[1] > _vector[2] ? 1 : 2;
  }
}
