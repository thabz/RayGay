
#include "cameras/latlong.h"
#include "ray.h"
#include "math/functions.h"

LatLong::LatLong() : Camera () {
}

Ray LatLong::_getRay(const double x, const double y) {
    double du = -au + ((2.0 * au * x) / (width));
    double dv = -av + ((2.0 * av * y) / (height));
    Vector dir = basis * Vector(du,dv,-1);
    Vector pos = position;
    dir.normalize();
    return Ray(pos, dir, 1.0);
}

