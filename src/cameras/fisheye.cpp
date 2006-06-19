
#include "cameras/fisheye.h"
#include "ray.h"
#include "math/functions.h"

Fisheye::Fisheye() : Camera () {
}

Ray Fisheye::_getRay(const double x, const double y) 
{
    double phi, theta;

    // TODO: Force into a circle of radius = min(width,height)

    // Make x and y in [-1,1]
    double _x = 2.0 * x / width - 1.0;
    double _y = 2.0 * y / height - 1.0;
    
    double r = _x*_x + _y*_y;
    
    if (r > 1.0) {
	return Ray();
    }

    r = sqrt(r);
    if (IS_ZERO(r)) {
	phi = 0.0;
    } else {
	phi = asin(_y / r);
	if (_x < 0.0) {
	    phi = M_PI - phi;
	}
    }
    theta = r * getFieldOfView() / 2.0 ;

    Vector dir = Vector(sin(theta)*cos(phi), sin(theta)*sin(phi), cos(theta));
    dir = basis * dir;
    dir.normalize(); // TODO: Necessary?
    return Ray(position, dir, 1.0);
}

