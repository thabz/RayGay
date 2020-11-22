
#ifndef CATMULL_ROM_SPLINE
#define CATMULL_ROM_SPLINE

#include "paths/path.h"
#include <vector>

class Vector;
class Matrix;

/**
 * The Catmull-Rom spline.
 *
 *
 * The characteristic feature of this spline interpolation is that the spline
 * passes through all but the first and last control points. The curve is
 * \f$ C^1 \f$ continuous which means that the first derivative has no
 * discontinuities in the tangent direction and magnitude. The curve is not
 * \f$ C^2 \f$ continuous as the second derivative is a linear interpolation
 * between the control points.
 *
 * The Catmull-Rom spline from the segment \f$P_i\f$ to \f$P_{i+1}\f$ is defined
 * as
 *
 * \f[ f(t) = \frac{-t^3 + 2t^2 - t}{2}P_{i-1} +
 *            \frac{3t^3 - 5t^2 + 2}{2}P_{i} +
 *            \frac{-3t^3 + 4t^2 + t}{2}P_{i+1} +
 *            \frac{t^3 - t^2}{2}P_{i+2}  \f]
 *
 * for \f$ t \in [0,1] \f$.
 *
 * The spline interpolates the positions \f$P_i\f$ to \f$P_{i+1}\f$ as
 *
 * \f[ f(0) = P_i \f]
 * \f[ f(1) = P_{i+1} \f]
 *
 * The first-order derivative is
 *
 * \f[ f'(t) = \frac{-3t^2 + 4t - 1}{2}P_{i-1} +
 *            \frac{9t^2 - 10t}{2}P_{i} +
 *            \frac{-9t^2 + 8t + 1}{2}P_{i+1} +
 *            \frac{3t^2 - 2t}{2}P_{i+2}  \f]
 *
 * One see that
 *
 * \f[ f'(0) = \frac{P_{i+1} - P_{i-1}}{2} \f]
 * \f[ f'(1) = \frac{P_{i+2} - P_{i}}{2} \f]
 *
 * or that the tangent at the end of spline segments are the vector from the
 * previous point to the current point.
 *
 * The spline is first proposed in
 * Catmull, E. and R. Rom, ``A Class of Local Interpolationg Splines,'' in
 * Barnhill R.E. and R.F. Riesenfled (eds.), <i>Computer Aided Geometric
 * Design</i>, Academic Press, New York, 1974.
 */
class CatmullRomSpline : public Path {
public:
  CatmullRomSpline(const std::vector<Vector> &points);
  CatmullRomSpline(Vector *points, uint32_t num);
  virtual ~CatmullRomSpline();
  Vector getPoint(double t) const;
  Vector getTangent(double t) const;
  void transform(const Matrix &m);

private:
  uint32_t segmentBegin(const double t) const;
  double adjustT(const double t) const;

  uint32_t points_num;
  std::vector<Vector> P;
};

#endif
