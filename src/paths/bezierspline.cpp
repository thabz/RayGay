
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

/**
 * Returns a point on the spline
 *
 * See http://mathworld.wolfram.com/BezierCurve.html
 */
Vector BezierSpline::getPoint(double t) const {
    Vector result = Vector(0,0,0);
    for(unsigned int i = 0; i < num; i++) {
	result += Math::bernsteinPolynomial(i,num-1,t) * getControlPoint(i);
    }
    return result;
}

Vector BezierSpline::getTangent(double t) const {
    /* From http://medialab.di.unipi.it/web/IUM/Waterloo/node123.html
     *
     * The derivative of Bin(t) is
     * d/dt Bin(t) = n ( Bi-1n-1(t) - Bin-1(t) )
     */
    Vector result = Vector(0,0,0);
    for(unsigned int i = 0; i < num; i++) {
	result += num*(Math::bernsteinPolynomial(i-1,num-2,t) - Math::bernsteinPolynomial(i,num-2,t)) * getControlPoint(i);
    }
    result.normalize();
    return result;
}

