
#ifndef SPIRAL_H
#define SPIRAL_H

#include "path.h"

class Circle;

/// A spiral around another Path
class Spiral : public Path {

    public:

	/// Constructor
	Spiral(Path* path, double radius, double windings);
	
	/// Constructor
	Spiral(Path* path, double radius, double windings, double offset);

	/// Get a point on the path where t in [0,1]
	Vector getPoint(double t) const;
	
	/// Get a tangent to the path where t in [0,1]
	Vector getTangent(double t) const;

	/// Transform the path
	void transform(const Matrix& m);

    private:

	Path* center;
        double radius;
	double windings;
	double offset;
	
};

#endif
