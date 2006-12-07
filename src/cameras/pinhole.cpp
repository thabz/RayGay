
#include "cameras/pinhole.h"
#include "ray.h"
#include "math/functions.h"

Pinhole::Pinhole() : Camera () {
}

Ray Pinhole::_getRay(const double x, const double y) {
    double du = -au + (2.0 * au * x);
    double dv = -av + (2.0 * av * y);
    Vector dir = basis * Vector(du,dv,-1);
    Vector pos = position;
    dir.normalize();
    return Ray(pos, dir, 1.0);
}

