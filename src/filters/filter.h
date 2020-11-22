
#ifndef FILTERS_FILTER
#define FILTERS_FILTER

#include "math/vector2.h"

class Filter {

public:
  Filter(double w, double h);
  double filter(const Vector2 &pos) const;
  virtual double filter(double x, double y) const = 0;
  virtual ~Filter(){};

private:
  double w;
  double h;
};

#endif
