
#include "bezierspline.h"
#include <cassert>

BezierSpline::BezierSpline(Vector* controlpoints, unsigned int num) {

   assert(num > 2);
   this->controlpoints = new Vector[num];
   for(unsigned int i = 0; i < num; i++) {
       this->controlpoints[i] = controlpoints[i];
   }
}

BezierSpline::~BezierSpline() {
   delete [] controlpoints;
}

void BezierSpline::transform(const Matrix& m) {
   for(unsigned int i = 0; i < num; i++) {
       controlpoints[i] = m * controlpoints[i];
   }
}

/**
 * Returns a point on the spline
 *
 * See http://mathworld.wolfram.com/BezierCurve.html
 */
Vector BezierSpline::getPoint(double t) const {
    Vector result = Vector(0,0,0);
    for(unsigned int i = 0; i < num; i++) {
	result = result + bernsteinPolynomial(i,num,t) * controlpoints[i];
    }
    return result;
}

Vector BezierSpline::getTangent(double t) const {
    /// TODO: Implement
}

double BezierSpline::bernsteinPolynomial(unsigned int i, unsigned int n, double t) const {
    return binomialCoefficient(n,i) * pow(t,i) * pow((1-t),n-i);
}

/**
 * Implemented as a slow recursive function.
 *
 * See http://www.brpreiss.com/books/opus4/html/page467.html for a O(n^2) version.
 */
unsigned long BezierSpline::binomialCoefficient(unsigned long n, unsigned long k) const {
    if (k == 0 || k == n) {
	return 1;
    } else {
        return binomialCoefficient(n-1,k-1) + binomialCoefficient(n-1,k);
    }
}
