
#ifndef BEZIER_PATCH
#define BEZIER_PATCH

#include "mesh.h"
#include <vector>
using namespace std;

/**
 * A bicubic bezier patch.
 */
class BezierPatch : public Mesh {
    public:
	BezierPatch(const vector<Vector> &points, const unsigned int xResolution, unsigned int yResolution, const Material* material);

    private:
	const Vector& getControlPoint(unsigned int i, unsigned int j) const;
	Vector getPoint(const Vector2& c) const;
	Vector controlPoints[16];
};

#endif
