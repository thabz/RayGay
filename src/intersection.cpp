
#include "intersection.h"
#include "vector.h"

Intersection::Intersection() {
    intersected = false;
}

Intersection::Intersection(Vector p, double s) {
    point = p;
    t = s;
    intersected = true;
}

Intersection::~Intersection() {
}

ostream & operator<<(ostream &os, const Intersection &i) {
    os << i.point;
    return os;
}
