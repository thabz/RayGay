
#ifndef PATHS_ARC_H
#define PATHS_ARC_H

#include "paths/path.h"
#include "paths/circle.h"
#include "math/vector.h"
#include "math/matrix.h"

/// A circle-arc path
class Arc : public Path {

    public:
	/// Constructor
	Arc(const Vector& center, double radius, const Vector& normal, double begin_degree, double end_degree);
	
	/// Constructor
	Arc(Circle* circle, double begin_degree, double end_degree);
	virtual ~Arc() {};
	Vector getPoint(double t) const;
	Vector getTangent(double t) const;
        void transform(const Matrix& m);

    private:
	double convert(const double t) const;
	Circle* circle;
	double begin_t;
	double end_t;
};


#endif

