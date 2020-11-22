
#ifndef BEZIER_PATCH
#define BEZIER_PATCH

#include "parametrizedsurface.h"
#include <vector>
using namespace std;

/**
 * A bicubic bezier patch.
 */
class BezierPatch : public ParametrizedSurface {
public:
  BezierPatch(const vector<Vector> &points, const uint32_t xResolution,
              uint32_t yResolution, const Material *material);

protected:
  Vector eval(double u, double v) const;

private:
  const Vector &getControlPoint(uint32_t i, uint32_t j) const;
  Vector controlPoints[16];
};

#endif
