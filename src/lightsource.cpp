
#include "lightsource.h"
#include "vector.h"
#include "matrix.h"
#include "ray.h"
#include "intersection.h"
#include "rgb.h"

Lightsource::Lightsource(Vector pos) {
    position = pos;
}

Lightsource::~Lightsource() {
}

void Lightsource::transform(const Matrix& m) {
    position = m * position;
}

Intersection Lightsource::intersect(const Ray& ray) {
}

RGB Lightsource::getDiffuseColor(const Vector& p) {
    return RGB(0.0,0.0,0.0);
};

