
#include <cmath>
#include "objects/superellipsoid.h"
#include "boundingbox.h"

class Material;

SuperEllipsoid::SuperEllipsoid(double n1, double n2, unsigned int steps, double accuracy, Material* m) : IsoSurface(steps,accuracy,-1,m) {
    this->n1 = n1;
    this->n2 = n2;
}

double SuperEllipsoid::evaluateFunction(const Vector& v) const {
    return -1 * (pow(pow(v.x(),2.0/n2)+pow(v.y(),2.0/n2),n2/n1) + pow(v.z(),2.0/n1));
}

SceneObject* SuperEllipsoid::clone() const {
    SuperEllipsoid* result = new SuperEllipsoid(*this);
    return result;
}

BoundingBox SuperEllipsoid::_boundingBoundingBox() const {
    static double b = 1.00 + 20*EPSILON;
    return BoundingBox(Vector(-b,-b,-b),Vector(b,b,b));
}

