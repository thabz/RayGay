#ifndef LINESEGMENT_H
#define LINESEGMENT_H

#include "math/vector.h"
#include "paths/path.h"

/// A linesegment path
class Linesegment : public Path {

    public:
	/// Constructor
	Linesegment(const Vector& begin, const Vector& end);
	Vector getPoint(double t) const;
	Vector getTangent(double t) const;
        void transform(const Matrix& m);

	/// Returns begin point
	Vector begin() const { return b; };
	
	/// Returns end point
	Vector end() const { return e; };

    private:
	Vector b;
	Vector e;
	Vector tgt;
};

#endif
