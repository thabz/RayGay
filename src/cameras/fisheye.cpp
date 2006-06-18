
#include "cameras/fisheye.h"
#include "ray.h"
#include "math/functions.h"

Fisheye::Fisheye() : Camera () {
}

Ray Fisheye::_getRay(const double x, const double y) 
{
    double x0 = 2.0 * x / height - 1.0;
    double y0 = 2.0 * x / width - 1.0;
    double r = sqrt(x0*x0 + y0*y0);
    if (r > 1.0) {
	return Ray(position, Vector(0,0,0), 1.0);
    }
    Vector dir = Vector(1,0,0);
    dir.normalize();
    return Ray(position, dir, 1.0);
}

