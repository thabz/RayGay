#ifndef LINESEGMENT_H
#define LINESEGMENT_H

#include "math/vector.h"
#include "paths/path.h"

/// A linesegment path
class Linesegment : public Path {

    public:
	Linesegment(const Vector& begin, const Vector& end);
	Vector getPoint(double t) const;
	Vector getTangent(double t) const;
        void transform(const Matrix& m);

	/// Returns begin point
	Vector begin() const { return b; };
	
	/// Returns end point
	Vector end() const { return e; };

	/// Returns the squared distance from p to the linesegment
	double sqrDistance(const Vector& p) const;
	
	/// Returns the distance from p to the linesegment
	double distance(const Vector& p) const;

    private:
	Vector b;
	Vector e;
	Vector tgt;
};

#endif
