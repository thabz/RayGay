
#include <iostream>
#include <cassert>
#include "objects/isosurface.h"
#include "intersection.h"
#include "boundingbox.h"
#include "math/vector2.h"

using namespace std;

IsoSurface::IsoSurface(uint steps, double accuracy, double iso, Material* mat) : Object(mat) {
    this->steps = steps;
    this->accuracy = accuracy;
    this->iso = iso;
}

Intersection IsoSurface::_fullIntersect(const Ray& world_ray, const double t) const {
    Ray ray = rayToObject(world_ray);
    Vector p = ray.getPoint(t*ray.t_scale);
    Intersection local_i = Intersection(p,t*ray.t_scale,normal(p),Vector2(0,0));
    return intersectionToWorld(local_i);
}

double IsoSurface::_fastIntersect(const Ray& world_ray) const {

    Ray local_ray = rayToObject(world_ray);
    double res = -1;
    
    const BoundingBox& bbox = this->_boundingBoundingBox();
    Vector2 inout = bbox.intersect(local_ray);
//    if (inout[0] < 0) 
//	return -1;
    double t_begin = fmax(inout[0],accuracy);
    double t_end = inout[1] + accuracy;
 //   cout << t_begin << endl;
 //   cout << t_end << endl;
    if (t_end > t_begin) {
	bool began_inside = inside(local_ray.getPoint(t_begin));
//	cout << "Inside:" << (began_inside ? "true" : "false") << endl;
	double t_step = (t_end - t_begin) / double(steps);
	bool cur_inside;
	for(double t = t_begin; t <= t_end; t += t_step) {
	    cur_inside = inside(local_ray.getPoint(t));
	    if (began_inside && !cur_inside) {
		res = refine(local_ray,t,t-t_step);
		goto DONE;
	    } else if (!began_inside && cur_inside) {
		res = refine(local_ray,t-t_step,t);
		goto DONE;
	    }
	}
    }
DONE:
    if (res > EPSILON) {
	return res / local_ray.t_scale;
    } else {
	return -1;
    }
}

#if 1
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

#else

/**
 * Refine an interval containing a root.
 * Using Brent's method.
 *
 * @param t_begin an outside t
 * @param t_end an inside t
 */
double IsoSurface::refine(const Ray& ray, double x1, double x3) const {

#define func(x) (evaluateFunction(ray.getPoint(x)) - iso)
#define MAX_ITER 10000    

    double x2;
    double R,S,T;
    double P,Q;
    double fx1,fx2,fx3;

    x2 = 0.5 * (x1 + x3);

    fx1 = func(x1);
    fx2 = func(x2);
    fx3 = func(x3);

    assert(SIGN(fx1) != SIGN(fx3));

    uint num = 0;
    while(num++ < MAX_ITER) {
	
	R = fx2 / fx3;
	S = fx2 / fx1;
	T = fx1 / fx3;
	
	P = S*(R*(R-T)*(x3-x2)-(1-R)*(x2-x1));
	Q = (T-1)*(R-1)*(S-1);

	x2 = x2 + (P / Q);

	fx2 = func(x2);
	if (fabs(fx2) < accuracy) {
	    return x2;
	}
    }
    return x2;
}
#endif

/**
 * Finds the surface normal at a point.
 * 
 * @param p a surface point in object space
 * @return the surface normal at point
 */
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

void IsoSurface::transform(const Matrix& m) {
    Transformer::transform(m);
}

BoundingBox IsoSurface::boundingBoundingBox() const {
    BoundingBox result = bboxToWorld(_boundingBoundingBox());
    result.grow(20*EPSILON);
    return result;
}

