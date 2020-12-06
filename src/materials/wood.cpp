
#include "wood.h"
#include "intersection.h"
#include "perlin.h"

Wood::Wood(const RGB &color1, const RGB &color2) : Material() {
  this->color1 = color1;
  this->color2 = color2;
}

RGB Wood::getDiffuseColor(const Intersection &i) const {
  Vector p = i.getPoint() / 250;
  p[1] = p[1] / 10.0;
  double g = Perlin::noise(p) * 20;
  g = g - floor(g);
  //   g = (1.0 + g) / 2.0;
  return g * color1 + (1.0 - g) * color2;
  return RGB(g, g, g);
}
