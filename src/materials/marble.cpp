
#include "marble.h"
#include "intersection.h"
#include "perlin.h"

Marble::Marble(const RGB &color1, const RGB &color2) : Material() {
  this->color1 = color1;
  this->color2 = color2;
}

RGB Marble::getDiffuseColor(const Intersection &i) const {
  Vector p = i.getPoint() / 10;

  double t = cos(p[0] / 2 + 3 * Perlin::noise(p));
  t = (t + 1) / 2.0;
  return RGB(t + 0.5, t + 0.6, 0.9 * t + 0.7);
}
