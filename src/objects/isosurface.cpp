
#include "objects/isosurface.h"
#include "intersection.h"
#include "boundingbox.h"
#include "math/vector2.h"

IsoSurface::IsoSurface(unsigned int steps, double accuracy, double iso) {
    this->steps = steps;
    this->accuracy = accuracy;
    this->iso = iso;
}

Intersection IsoSurface::_intersect(const Ray& ray) const {
    const BoundingBox& bbox = this->boundingBoundingBox();
    Vector2 inout = bbox.intersect(ray);
    double t_begin = inout[0];
    double t_end = inout[1];
    double t_step = (t_end - t_begin) / double(steps);

    if (inside(ray.getPoint(t_begin))) {
	return Intersection(ray.getPoint(t_begin),t_begin);
    }

    for(double t = t_begin; t <= t_end; t += t_step) {
	if (inside(ray.getPoint(t))) {
	    double intersection_t = recurse(ray,t-t_step,t);
	    return Intersection(ray.getPoint(intersection_t),intersection_t);
	}
    }
    // No intersection
    return Intersection();
}

bool IsoSurface::inside(const Vector& v) const {
    return evaluateFunction(v) >= iso;
}

/**
 * Refine an interval containing a root.
 *
 * @param t_begin an outside t
 * @param t_end an inside t
 */
double IsoSurface::recurse(const Ray& ray, double t_begin, double t_end) const {
    double t_mid = 0.5 * (t_begin + t_end);

    if (t_end - t_begin < accuracy)
	return t_mid;

    if (inside(ray.getPoint(t_mid))) {
	return recurse(ray, t_begin,t_mid);
    } else {
	return recurse(ray, t_mid,t_end);
    }
}

Vector IsoSurface::normal(const Intersection & i) const {
    Vector p = i.getPoint();
    double x = evaluateFunction(p - Vector(1,0,0)) - 
	       evaluateFunction(p + Vector(1,0,0));
    double y = evaluateFunction(p - Vector(0,1,0)) - 
	       evaluateFunction(p + Vector(0,1,0));
    double z = evaluateFunction(p - Vector(0,0,1)) - 
	       evaluateFunction(p + Vector(0,0,1));
    Vector normal = Vector(0.5*x,0.5*y,0.5*z);
    normal.normalize();
    return normal;
}

bool IsoSurface::intersects(const BoundingBox& b) const {
    return b.inside(boundingBoundingBox());
}

Vector2 IsoSurface::getUV(const Intersection& intersection) const {

}

