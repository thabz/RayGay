#ifndef LINESEGMENT_H
#define LINESEGMENT_H

#include "path.h"
#include "vector.h"

/// A linesegment path
class Linesegment : public Path {

    public:
	Linesegment(const Vector& begin, const Vector& end);
	Vector getPoint(double t) const;
	Vector getTangent(double t) const;
        void transform(const Matrix& m);

	Vector begin() const { return b; };
	Vector end() const { return e; };

	/// Internal test
	static void test();

    private:
	Vector b;
	Vector e;
	Vector tgt;
};

#endif
