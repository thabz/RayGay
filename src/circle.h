
#ifndef CIRCLE_H
#define CIRCLE_H

#include "path.h"
#include "vector.h"
#include "matrix.h"

/// A circle path
class Circle : public Path {

    public:
	Circle() { };
	Circle(const Vector& center, double radius, const Vector& normal);
	Vector getPoint(double t) const;
	Vector getTangent(double t) const;
        void transform(const Matrix& m);

	/// Internal test
	static void test();

    private:
	Vector c;
	Vector n;
	double r;
	Matrix m;
};


#endif
