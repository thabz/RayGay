
#include "bezierspline.h"
#include "math/functions.h"
#include <cassert>

BezierSpline::BezierSpline(Vector* controlpoints, unsigned int num) {

   assert(num > 2);
   this->controlpoints = new Vector[num];
   this->num = num;
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

Vector BezierSpline::getPoint(double t) const {
    Vector result = Vector(0,0,0);
    for(unsigned int i = 0; i < num; i++) {
	result += Math::bernsteinPolynomial(i,num-1,t) * getControlPoint(i);
    }
    return result;
}

/**
 * Get a tangent vector.
 *
 * The derivative of the Bernstein polynomial above is
 *
 * \f[ \frac{d}{dt}B_{i,n}(t) = n \left( B_{i-1,n-1}(t) - B_{i,n-1}(t) \right) \f]
 */
Vector BezierSpline::getTangent(double t) const {
    Vector result = Vector(0,0,0);
    for(unsigned int i = 0; i < num; i++) {
	result += num*(Math::bernsteinPolynomial(i-1,num-2,t) - Math::bernsteinPolynomial(i,num-2,t)) * getControlPoint(i);
    }
    result.normalize();
    return result;
}

