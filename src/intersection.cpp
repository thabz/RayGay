
#include "intersection.h"
#include "vector.h"


Intersection::Intersection(const Vector& p, double s) {
    point = p;
    t = s;
    intersected = true;
}

ostream & operator<<(ostream &os, const Intersection &i) {
    os << i.point;
    return os;
}
