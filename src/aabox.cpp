
#include "aabox.h"

#include <cassert>
#include <iosfwd>
#include <iostream>

#include "exception.h"
#include "image/rgb.h"
#include "intersection.h"
#include "math/constants.h"
#include "math/matrix.h"
#include "math/vector2.h"
#include "ray.h"

AABox::AABox() {}

AABox::AABox(const Vector &c1, const Vector &c2) {
  _c1[0] = fmin(c1[0], c2[0]);
  _c1[1] = fmin(c1[1], c2[1]);
  _c1[2] = fmin(c1[2], c2[2]);
  _c2[0] = fmax(c1[0], c2[0]);
  _c2[1] = fmax(c1[1], c2[1]);
  _c2[2] = fmax(c1[2], c2[2]);
}

AABox::AABox(const std::vector<Vector> &swarm) {
  int num = swarm.size();
  if (num < 2)
    throw_exception("At least two Vectors are needed");
  for (int i = 0; i < num; i++) {
    Vector c = swarm[i];
    _c1[0] = fmin(_c1[0], c[0]);
    _c1[1] = fmin(_c1[1], c[1]);
    _c1[2] = fmin(_c1[2], c[2]);
    _c2[0] = fmax(c[0], _c2[0]);
    _c2[1] = fmax(c[1], _c2[1]);
    _c2[2] = fmax(c[2], _c2[2]);
  }
}

bool AABox::inside(const Vector &p) const {
  return p[0] > _c1[0] && p[0] < _c2[0] && p[1] > _c1[1] && p[1] < _c2[1] &&
         p[2] > _c1[2] && p[2] < _c2[2];
}

bool AABox::insideOrTouching(const Vector &p) const {
  return p[0] >= _c1[0] && p[0] <= _c2[0] && p[1] >= _c1[1] && p[1] <= _c2[1] &&
         p[2] >= _c1[2] && p[2] <= _c2[2];
}

Vector AABox::center() const {
  return 0.5 *
         (Vector(_c1[0], _c1[1], _c1[2]) + Vector(_c2[0], _c2[1], _c2[2]));
}

bool AABox::inside(const Vector *points, int num) const {
  assert(num > 0);
  for (int i = 0; i < num; i++) {
    if (!inside(points[i]))
      return false;
  }
  return true;
}

bool AABox::inside(const AABox &b) const {
  return inside(b.minimum()) && inside(b.maximum());
}

bool AABox::onEdge(const Vector &p) const {
  bool p0bounded = p[0] >= _c1[0] && p[0] <= _c2[0];
  bool p1bounded = p[1] >= _c1[1] && p[1] <= _c2[1];
  bool p2bounded = p[2] >= _c1[2] && p[2] <= _c2[2];

  return ((IS_EQUAL(p[0], _c1[0]) || IS_EQUAL(p[0], _c2[0])) && p1bounded &&
          p2bounded) ||
         ((p[1] == _c1[1] || p[1] == _c2[1]) && p0bounded && p2bounded) ||
         ((p[2] == _c1[2] || p[2] == _c2[2]) && p1bounded && p0bounded);
}

/**
 * Finds the two distances \f$(t_{min},t_{max})\f$ where a ray intersect this
 * boundingbox. The values are returned as an ordered pair
 * \f$(t_{min},t_{max})\f$
 * in a Vector2 so that \f$ t_{min} \leq t_{max} \f$.
 *
 * The method will return a pair where \f$ t_{min} > t_{max} \f$
 * only in the case where no intersection was found.
 *
 * Fast algorithm from http://www.cs.utah.edu/~awilliam/box/
 */
Vector2 AABox::intersect(const Ray &ray) const {
  const Vector &B = ray.getOrigin();
  const Vector &v_inv = ray.getInverseDirection();
  const Vector &v = ray.getDirection();

  double tmin, tmax, tymin, tymax, tzmin, tzmax;

  if (v[0] >= 0) {
    tmin = (_c1[0] - B[0]) * v_inv[0];
    tmax = (_c2[0] - B[0]) * v_inv[0];
  } else {
    tmin = (_c2[0] - B[0]) * v_inv[0];
    tmax = (_c1[0] - B[0]) * v_inv[0];
  }
  if (v[1] >= 0) {
    tymin = (_c1[1] - B[1]) * v_inv[1];
    tymax = (_c2[1] - B[1]) * v_inv[1];
  } else {
    tymin = (_c2[1] - B[1]) * v_inv[1];
    tymax = (_c1[1] - B[1]) * v_inv[1];
  }
  if ((tmin > tymax) || (tymin > tmax))
    return Vector2(2, 1); // No intersection
  if (tymin > tmin)
    tmin = tymin;
  if (tymax < tmax)
    tmax = tymax;
  if (v[2] >= 0) {
    tzmin = (_c1[2] - B[2]) * v_inv[2];
    tzmax = (_c2[2] - B[2]) * v_inv[2];
  } else {
    tzmin = (_c2[2] - B[2]) * v_inv[2];
    tzmax = (_c1[2] - B[2]) * v_inv[2];
  }
  if ((tmin > tzmax) || (tzmin > tmax))
    return Vector2(2, 1); // No intersection
  if (tzmin > tmin)
    tmin = tzmin;
  if (tzmax < tmax)
    tmax = tzmax;

  return Vector2(tmin, tmax);
}

bool AABox::checkIntersect(const Ray &ray) const {
  const Vector &B = ray.getOrigin();
  const Vector &v_inv = ray.getInverseDirection();
  const Vector &v = ray.getDirection();

  double tmin, tmax, tymin, tymax, tzmin, tzmax;

  if (v[0] >= 0) {
    tmin = (_c1[0] - B[0]) * v_inv[0];
    tmax = (_c2[0] - B[0]) * v_inv[0];
  } else {
    tmin = (_c2[0] - B[0]) * v_inv[0];
    tmax = (_c1[0] - B[0]) * v_inv[0];
  }
  if (v[1] >= 0) {
    tymin = (_c1[1] - B[1]) * v_inv[1];
    tymax = (_c2[1] - B[1]) * v_inv[1];
  } else {
    tymin = (_c2[1] - B[1]) * v_inv[1];
    tymax = (_c1[1] - B[1]) * v_inv[1];
  }
  if ((tmin > tymax) || (tymin > tmax))
    return false; // No intersection
  if (tymin > tmin)
    tmin = tymin;
  if (tymax < tmax)
    tmax = tymax;
  if (v[2] >= 0) {
    tzmin = (_c1[2] - B[2]) * v_inv[2];
    tzmax = (_c2[2] - B[2]) * v_inv[2];
  } else {
    tzmin = (_c2[2] - B[2]) * v_inv[2];
    tzmax = (_c1[2] - B[2]) * v_inv[2];
  }
  if ((tmin > tzmax) || (tzmin > tmax))
    return false; // No intersection
  if (tzmin > tmin)
    tmin = tzmin;
  if (tzmax < tmax)
    tmax = tzmax;

  return ((tmax < HUGE_DOUBLE) && (tmax > -1));
}

Vector AABox::normal(const Vector &p) const {
  if (IS_EQUAL(p[0], _c1[0])) {
    return Vector(-1, 0, 0);
  } else if (IS_EQUAL(p[0], _c2[0])) {
    return Vector(1, 0, 0);
  }
  if (IS_EQUAL(p[1], _c1[1])) {
    return Vector(0, -1, 0);
  } else if (IS_EQUAL(p[1], _c2[1])) {
    return Vector(0, 1, 0);
  }
  if (IS_EQUAL(p[2], _c1[2])) {
    return Vector(0, 0, -1);
  } else if (IS_EQUAL(p[2], _c2[2])) {
    return Vector(0, 0, -1);
  } else {
    assert(false);
  }
}

/**
 * @param must be a pointer to an array with length 8.
 */
void AABox::getCorners(Vector *dest) const {
  Vector *c = dest;
  assert(c != NULL);
  c[0] = Vector(_c1[0], _c1[1], _c1[2]);
  c[1] = Vector(_c1[0], _c1[1], _c2[2]);
  c[2] = Vector(_c1[0], _c2[1], _c1[2]);
  c[3] = Vector(_c1[0], _c2[1], _c2[2]);
  c[4] = Vector(_c2[0], _c1[1], _c1[2]);
  c[5] = Vector(_c2[0], _c1[1], _c2[2]);
  c[6] = Vector(_c2[0], _c2[1], _c1[2]);
  c[7] = Vector(_c2[0], _c2[1], _c2[2]);
}

/**
 * The array must be deleted after use.
 */
Vector *AABox::getCorners() const {
  Vector *corners = new Vector[8];
  getCorners(corners);
  return corners;
}

AABox AABox::doUnion(const AABox &b1, const AABox &b2) {
  Vector mini = Vector(fmin(b1._c1[0], b2._c1[0]), fmin(b1._c1[1], b2._c1[1]),
                       fmin(b1._c1[2], b2._c1[2]));
  Vector maxi = Vector(fmax(b1._c2[0], b2._c2[0]), fmax(b1._c2[1], b2._c2[1]),
                       fmax(b1._c2[2], b2._c2[2]));
  return AABox(mini, maxi);
}

// This should return NULL is they don't intersect...!
AABox AABox::doIntersection(const AABox &b1, const AABox &b2) {
  Vector mini = Vector(fmax(b1._c1[0], b2._c1[0]), fmax(b1._c1[1], b2._c1[1]),
                       fmax(b1._c1[2], b2._c1[2]));
  Vector maxi = Vector(fmin(b1._c2[0], b2._c2[0]), fmin(b1._c2[1], b2._c2[1]),
                       fmin(b1._c2[2], b2._c2[2]));
  return AABox(mini, maxi);
}

AABox AABox::doDifference(const AABox &a, const AABox &b) {
  AABox result = a;

  for (uint32_t x = 0; x < 3; x++) {
    uint32_t y = (x + 1) % 3;
    uint32_t z = (x + 2) % 3;

    // Handling x axis for maximum
    if (a._c1[x] < b._c1[x] && a._c2[x] > b._c1[x] && a._c2[x] < b._c2[x] &&
        a._c1[y] > b._c1[y] && a._c1[z] > b._c1[z] && a._c2[y] < b._c2[y] &&
        a._c2[z] < b._c2[z])
      result._c2[x] = b._c1[x];

    // Handling x axis for minimum
    if (a._c1[x] < b._c2[x] && a._c2[x] > b._c2[x] && a._c1[x] > b._c1[x] &&
        a._c1[y] > b._c1[y] && a._c1[z] > b._c1[z] && a._c2[y] < b._c2[y] &&
        a._c2[z] < b._c2[z])
      result._c1[x] = b._c2[x];
  }

  return result;
}

AABox AABox::enclosure(Vector *points, int num) {
  Vector mini = Vector(HUGE_DOUBLE, HUGE_DOUBLE, HUGE_DOUBLE);
  Vector maxi = Vector(-HUGE_DOUBLE, -HUGE_DOUBLE, -HUGE_DOUBLE);
  for (int i = 0; i < num; i++) {
    for (int j = 0; j < 3; j++) {
      mini[j] = fmin(mini[j], points[i][j]);
      maxi[j] = fmax(maxi[j], points[i][j]);
    }
  }
  return AABox(mini, maxi);
}

bool AABox::operator==(const AABox &b) const {
  return b.minimum() == minimum() && b.maximum() == maximum();
}

/**
 * The surfacearea of all six faces.
 */
double AABox::area() const {
  double w = _c2[0] - _c1[0];
  double h = _c2[1] - _c1[1];
  double d = _c2[2] - _c1[2];
  return 2.0 * (w * h + w * d + h * d);
}

/**
 * Says whether this boundingbox is on the upper or lower side of an
 * axis-aligned plane
 *
 * @param cutplane_dimension The axis the plane cuts (0,1,2)
 * @param cutplane_value The axis-value where the plane cuts.
 *
 * @return 1 (higher), -1 (lower) or 0 (intersects)
 */
int AABox::cutByPlane(int cutplane_dimension, double cutplane_value) const {
  assert(cutplane_dimension == 0 || cutplane_dimension == 1 ||
         cutplane_dimension == 2);
  const double min = minimum(cutplane_dimension);
  const double max = maximum(cutplane_dimension);
  if (cutplane_value > max) {
    return -1;
  } else if (cutplane_value < min) {
    return 1;
  } else {
    return 0;
  }
}

ostream &operator<<(ostream &os, const AABox &b) {
  os << "Boundingbox:(" << b.minimum() << ',' << b.maximum() << ')';
  return os;
}

void AABox::grow(double nudge) {
  assert(nudge >= 0.0);
  _c1[0] -= nudge;
  _c1[1] -= nudge;
  _c1[2] -= nudge;
  _c2[0] += nudge;
  _c2[1] += nudge;
  _c2[2] += nudge;
}

Vector AABox::lengths() const {
  return Vector(_c2[0] - _c1[0], _c2[1] - _c1[1], _c2[2] - _c1[2]);
}

void AABox::setMinimumLengths(double l) {
  Vector ls = lengths();
  for (uint32_t i = 0; i < 3; i++) {
    if (ls[i] < l) {
      _c1[i] -= 0.5 * l;
      _c2[i] += 0.5 * l;
    }
  }
}

/**
 * @param percent where 1.0 is 1%
 */
void AABox::growPercentage(double percent) {
  Vector l = lengths();
  percent /= 100.0;
  _c1[0] -= percent * l[0];
  _c1[1] -= percent * l[1];
  _c1[2] -= percent * l[2];
  _c2[0] += percent * l[0];
  _c2[1] += percent * l[1];
  _c2[2] += percent * l[2];
}

// Stolen from http://www.gamasutra.com/features/19991018/Gomez_4.htm
bool AABox::intersectSphere(const Vector &center, double squared_radius) const {
  double s, d = 0;

  for (int i = 0; i < 3; i++) {
    if (center[i] < minimum(i)) {
      s = center[i] - minimum(i);
      d += s * s;
    } else if (center[i] > maximum(i)) {
      s = center[i] - maximum(i);
      d += s * s;
    }
  }
  return d <= squared_radius;
}

bool AABox::split(AABox &left, AABox &right, const uint32_t dim,
                  const double axis) const {
  if (axis > maximum(dim) || axis < minimum(dim)) {
    return false;
  }

  Vector split_max = maximum();
  split_max[dim] = axis;
  Vector split_min = minimum();
  split_min[dim] = axis;

  left = AABox(minimum(), split_max);
  right = AABox(split_min, maximum());
  return true;
}
