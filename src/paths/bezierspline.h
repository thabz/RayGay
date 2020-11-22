
#ifndef PATHS_BEZIER_SPLINE_H
#define PATHS_BEZIER_SPLINE_H

#include "math/matrix.h"
#include "math/vector.h"
#include "paths/path.h"
#include <vector>

/**
 * A Beziér-spline path.
 *
 * Given a set of \f$ n+1 \f$ control points \f$ P_0, P_1, ..., P_n \f$ the
 * corresponding Beziér-curve is defined by
 * \f[ C(t) = \sum_{i=0}^n P_i B_{i,n}(t) \f]
 * where \f$ B_{i,n}\f$  is the Bernstein polynomial defined by
 * \f[ B_{i,n}(t) = {n \choose i} t^i (1-t)^{n-i} \quad \mbox{for } t \in
 * {[0,1]} \qquad \f]
 *
 * See http://mathworld.wolfram.com/BezierCurve.html
 */
class BezierSpline : public Path {

public:
  /// Constructor
  BezierSpline(const std::vector<Vector> &points);
  /// Constructor
  BezierSpline(Vector *controlpoints, uint32_t num);
  virtual ~BezierSpline();
  Vector getPoint(double t) const;
  Vector getTangent(double t) const;
  void transform(const Matrix &m);

  /// Returns a controlpoint for 0 < n < num
  Vector getControlPoint(uint32_t n) const { return controlpoints[n]; };

private:
  std::vector<Vector> controlpoints;
  uint32_t num;
};

#endif
