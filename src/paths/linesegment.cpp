
#include <cassert>

#include "paths/linesegment.h"
#include "math/matrix.h"

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
	
