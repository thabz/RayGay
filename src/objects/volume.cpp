
#include "objects/volume.h"
#include "intersection.h"
#include "boundingbox.h"

Volume::Volume(unsigned int steps, double accuracy) {
    this->steps = steps;
    this->accuracy = accuracy;
}

Intersection Volume::_intersect(const Ray& ray) const {
    const BoundingBox& bbox = this->boundingBoundingBox();
    Vector2 inout = bbox.intersect(ray);
    double t_begin = inout[0];
    double t_end = inout[1];
    double t_step = (t_end - t_begin) / double(steps);

    for(double t = t_begin; t <= t_end; t += t_step) {
	if (inside(ray.getPoint(t))) {
	    double intersection_t = recurse(t-t_step,t);
	    return Intersection(ray.getPoint(intersection_t),intersection_t);
	}
    }
    // No intersection
    return Intersection();
}

/**
 * @param t_begin an outside t
 * @param t_end an inside t
 */
double recurse(double t_begin, double t_end) {
    if (t_end - t_begin < accuracy)
	return t_begin;

    double t_mid = 0.5 * (t_begin + t_end);

    if (inside(t_mid)) {
	return recurse(t_begin,t_mid);
    } else {
	return recurse(t_mid,t_end);
    }
}

