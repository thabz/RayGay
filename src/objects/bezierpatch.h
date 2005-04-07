
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
	BezierPatch(const vector<Vector> &points, const unsigned int xResolution, unsigned int yResolution, const Material* material);

    protected:
	Vector eval(double u, double v) const;

    private:
	const Vector& getControlPoint(unsigned int i, unsigned int j) const;
	Vector controlPoints[16];
};

#endif
