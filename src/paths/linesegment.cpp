
#include <cassert>

#include "paths/linesegment.h"
#include "math/matrix.h"

/**
 * Constructor
 * 
 * @param begin Where the linesegment begins
 * @param end Where the linesegment ends
 */
Linesegment::Linesegment(const Vector& begin, const Vector& end) {
    b = begin;
    e = end;
    tgt = end - begin;
    tgt.normalize();
}

Vector Linesegment::getPoint(double t) const {
    return (1-t)*b + t*e;
}

inline
Vector Linesegment::getTangent(double t) const {
    return tgt;
}

void Linesegment::transform(const Matrix& m) {
    b = m * b;
    e = m * e;
    tgt = m.extractRotation() * tgt;
}
	
double Linesegment::sqrDistance(const Vector& p) const {
    Vector np; 		// Nearest point
    Vector d = e - b;   // Direction vector

    if (d == Vector(0,0,0)) {
	// This linesegment is a point
	return (b - p).norm();
    }

    double t = ((p - b) * d) / d.norm();

    if (t < 0) {
	np = b;
    } else if (t > 1) {
	np = e;
    } else {
	np = b + t * d;
    }

    return (np - p).norm();
}

double Linesegment::distance(const Vector& p) const {
    return sqrt(sqrDistance(p));
}
