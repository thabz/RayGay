
#ifndef PATHS_ELLIPSE_H
#define PATHS_ELLIPSE_H

#include "math/matrix.h"
#include "math/vector.h"
#include "paths/path.h"

/// An ellipse path
class Ellipse : public Path {

public:
  Ellipse(const Vector &center, double r1, double r2, const Vector &normal);
  virtual ~Ellipse(){};
  Vector getPoint(double t) const;
  Vector getTangent(double t) const;
  void transform(const Matrix &m);

private:
  Vector c;
  Vector n;
  double r1;
  double r2;
  Matrix m;
  Matrix orient;
};

#endif
