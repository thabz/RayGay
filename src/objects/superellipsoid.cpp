
#include "superellipsoid.h"
#include <cmath>

class Material;

SuperElliopsoid::SuperElliopsoid(double n1, double n2, unsigned int steps, double accuracy, Material* m) : IsoSurface(steps,accuracy,-1,m) {
    this->n1 = n1;
    this->n2 = n2;
}

double SuperElliopsoid::evaluateFunction(const Vector& v) const {
    return -pow(pow(v.x(),2/n2)+pow(v.y(),2/n2),n2/n1) + pow(v.z(),2/n1);
}
