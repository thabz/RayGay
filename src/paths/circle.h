
#ifndef CIRCLE_H
#define CIRCLE_H

#include "path.h"

/// A circle path
class Circle : public Path {

    public:
	Circle(const Vector& center, double radius, const Vector& normal);
	virtual ~Circle() {};
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
	Matrix orient;
};


#endif
