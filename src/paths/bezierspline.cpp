
#include "bezierspline.h"
#include "math/functions.h"
#include <cassert>

BezierSpline::BezierSpline(Vector* controlpoints, uint num) {

   assert(num > 2);
   this->num = num;
   for(uint i = 0; i < num; i++) {
       this->controlpoints.push_back(controlpoints[i]);
   }
}

BezierSpline::BezierSpline(const std::vector<Vector>& points) {
    controlpoints = points;
    num = points.size();
    assert(num > 2);
}

BezierSpline::~BezierSpline() {
   controlpoints.clear();
}

void BezierSpline::transform(const Matrix& m) {
   for(uint i = 0; i < num; i++) {
       controlpoints[i] = m * controlpoints[i];
   }
}

Vector BezierSpline::getPoint(double t) const {
    Vector result = Vector(0,0,0);
    for(uint i = 0; i < num; i++) {
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
    for(uint i = 0; i < num; i++) {
	result += num*(Math::bernsteinPolynomial(i-1,num-2,t) - Math::bernsteinPolynomial(i,num-2,t)) * getControlPoint(i);
    }
    result.normalize();
    return result;
}

