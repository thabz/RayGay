
#include <cassert>

#include "spiral.h"
#include "vector.h"
#include "matrix.h"
#include "circle.h"

Spiral::Spiral(Path* path, double radius, double windings) {
    center = path;
    this->radius = radius;
    this->windings = windings;
    this->offset = 0;
}

Spiral::Spiral(Path* path, double radius, double windings, double offset) {
    center = path;
    this->radius = radius;
    this->windings = windings;
    this->offset = offset;
}

/// Get a point on the path where t in [0,1]
Vector Spiral::getPoint(double t) const {
    Vector c = center->getPoint(t);
    Vector n = center->getTangent(t);
    Circle circle = Circle(c,radius,n);
    double tn = t * windings + offset;
    tn = tn - int(tn);
    return circle.getPoint(tn);

};
	
/// Get a tangent to the path where t in [0,1]
Vector Spiral::getTangent(double t) const {
    Vector c = center->getPoint(t);
    Vector n = center->getTangent(t);
    Circle circle = Circle(c,radius,n);
    double tn = t * windings + offset;
    tn = tn - int(tn);
    Vector circle_tangent = circle.getTangent(tn);
    Vector path_tangent = center->getTangent(t);
    Vector result = circle_tangent + path_tangent;
    result.normalize();
    return result;
};

/// Transform the path
void Spiral::transform(const Matrix& m) {
    center->transform(m);
};


void Spiral::test() {

}
