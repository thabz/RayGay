
#ifndef PATHS_BEZIER_SPLINE_H 
#define PATHS_BEZIER_SPLINE_H 

#include "paths/path.h"
#include "math/vector.h"
#include "math/matrix.h"

/// A Bezier-spline path
class BezierSpline : public Path {

    public:
	/// Constructor
	BezierSpline(Vector* controlpoints, unsigned int num);
	virtual ~BezierSpline();
	Vector getPoint(double t) const;
	Vector getTangent(double t) const;
        void transform(const Matrix& m);

    private:
	Vector* controlpoints;
	unsigned int num;
};


#endif

