
#include "objects/isosurface.h"
#include "intersection.h"
#include "boundingbox.h"
#include "math/vector2.h"

IsoSurface::IsoSurface(unsigned int steps, double accuracy, double iso, Material* mat) : Object(mat) {
    this->steps = steps;
    this->accuracy = accuracy;
    this->iso = iso;
}

Intersection IsoSurface::_fullIntersect(const Ray& ray, const double t) const {
    Vector p = ray.getPoint(t);
    return Intersection(p,t,normal(p),Vector2(0,0));
}

double IsoSurface::_fastIntersect(const Ray& ray) const {
    const BoundingBox& bbox = this->boundingBoundingBox();
    Vector2 inout = bbox.intersect(ray);
    double t_end = inout[1];
    if (t_end < 0) {
	return -1;
    }
    double t_begin = inout[0];
    double t_step = (t_end - t_begin) / double(steps);
    
    if (inside(ray.getPoint(t_begin))) {
	return t_begin;
    }

    for(double t = t_begin; t <= t_end; t += t_step) {
	if (inside(ray.getPoint(t))) {
	    return refine(ray,t-t_step,t);
	}
    }
    // No intersection
    return -1;
}

/**
 * Refine an interval containing a root.
 *
 * @param t_begin an outside t
 * @param t_end an inside t
 */
double IsoSurface::refine(const Ray& ray, double t_begin, double t_end) const {
    double t_mid;

    while (t_end - t_begin > accuracy) {
	t_mid = 0.5 * (t_begin + t_end);
	if (inside(ray.getPoint(t_mid))) {
	    t_end = t_mid;
	} else {
	    t_begin = t_mid;
	}
    }
    return 0.5 * (t_begin + t_end);
}

Vector IsoSurface::normal(const Vector& p) const {
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

