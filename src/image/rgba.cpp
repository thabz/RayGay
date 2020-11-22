
#include "image/rgba.h"
#include <iostream>

std::ostream &operator<<(std::ostream &os, const RGBA &x) {
  os << '(' << x[0] << ',';
  os << x[1] << ',';
  os << x[2] << ',';
  os << x[3] << ')';
  return os;
}

/**
 * Returns the average of an array of colors with
 * correct handling of alpha, ie. color components of
 * input colors with zero alpha are ignored and don't
 * mess up the average.
 */
RGBA RGBA::avg(RGBA *c, int num) {
  RGBA r = RGBA(0, 0, 0, 0);
  double c_num = 0.0;
  for (int i = 0; i < num; i++) {
    r._vector[0] += c[i]._vector[0] * c[i].alpha;
    r._vector[1] += c[i]._vector[1] * c[i].alpha;
    r._vector[2] += c[i]._vector[2] * c[i].alpha;
    r.alpha += c[i].alpha;
    c_num += c[i].a();
  }
  if (c_num > 0) {
    r._vector[0] /= c_num;
    r._vector[1] /= c_num;
    r._vector[2] /= c_num;
    r.alpha /= num;
  }
  return r;
}
