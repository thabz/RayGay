
#ifndef CIRCLE_H
#define CIRCLE_H

#include "math/matrix.h"
#include "math/vector.h"
#include "paths/path.h"

/// A circle path
class Circle : public Path {

public:
  Circle(const Vector &center, double radius, const Vector &normal);
  virtual ~Circle(){};
  Vector getPoint(double t) const;
  Vector getTangent(double t) const;
  void transform(const Matrix &m);

private:
  Vector c;
  Vector n;
  double r;
  Matrix m;
  Matrix orient;
};

#endif
