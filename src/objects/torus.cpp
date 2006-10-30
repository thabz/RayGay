
#include <cassert>
#include <iostream>
#include "torus.h"
#include "ray.h"
#include "aabox.h"
#include "math/functions.h"
#include "math/vector2.h"

/**
 * Constructs a torus symmetric about the y-axis, ie. placed in zx-plane centered in origin.
 *
 * @param R major radius aka outer radius or distance from center of the hole to
            the center of the tube.
 * @param r minor radius aka inner radius or radius of the tube
 * @param m material of the torus
 */
Torus::Torus(double R, double r, const Material* m) : Solid(m) {
    assert(R > r);
    assert(r > 0);
    this->R = R;
    this->r = r;
}

void Torus::transform(const Matrix& m) {
    Transformer::transform(m);
}

void Torus::_fullIntersect(const Ray& world_ray, const double t, Intersection& result) const {
    Ray ray = rayToObject(world_ray);
    Vector p  = ray.getPoint(t*ray.t_scale);
    Vector n = normal(p);
    result = Intersection(p,t,n,Vector2(0,0));
    intersectionToWorld(result);
}

double Torus::_fastIntersect(const Ray& world_ray) const {
    Ray ray = rayToObject(world_ray);

    Vector Rd = ray.getDirection();
    Vector Ro = ray.getOrigin();

    // Move ray's origin closer to torus to improve accuracy.
    // Trick learned from http://www.hassings.dk/l3/povtorus/povtorus.html
    //
    // We moved O to a point on a bounding sphere with radisu R + r + r
    double BoundingRadius = R + r + r;
    double distP = Ro.norm();
    double closer = 0.0;
    if (distP > BoundingRadius) {
	distP = sqrt(distP);
	closer = distP - BoundingRadius;
	Ro += closer * ray.getDirection();
    }
    
    double xo = Ro.x();
    double yo = Ro.y();
    double zo = Ro.z();
    double xd = Rd.x();
    double yd = Rd.y();
    double zd = Rd.z();

    double R2 = R*R;
    double r2 = r*r;
    double Py2 = yo*yo;
    double Dy2 = yd*yd;
    double pdy2 = yo * yd;

    double k1 = xo*xo + zo*zo + Py2 - R2 -r2;
    double k2 = xo*xd + zo*zd + pdy2;

    double a1 = 4.0 * k2;
    double a2 = 2.0 * (k1 + 2.0 * (k2 * k2 + R2 * Dy2));
    double a3 = 4.0 * (k2 * k1 + 2.0 * R2 * pdy2);
    double a4 = k1 * k1 + 4.0 * R2 * (Py2 - r2);

    // Find all roots
    double root;
    int num = Math::solveQuarticSingle(a1,a2,a3,a4, 2*EPSILON - closer, &root);

    return num == 0 ? -1 : (root + closer) / ray.t_scale;
}

uint32_t Torus::allPositiveRoots(const Ray& world_ray, double roots[4]) const {
    Ray ray = rayToObject(world_ray);

    Vector Rd = ray.getDirection();
    Vector Ro = ray.getOrigin();

    // Move ray's origin closer to torus to improve accuracy.
    // Trick learned from http://www.hassings.dk/l3/povtorus/povtorus.html
    //
    // We moved O to a point on a bounding sphere with radisu R + r + r
    double BoundingRadius = R + r + r;
    double distP = Ro.norm();
    double closer = 0.0;
    if (distP > BoundingRadius) {
	distP = sqrt(distP);
	closer = distP - BoundingRadius;
	Ro += closer * ray.getDirection();
    }
    
    double xo = Ro.x();
    double yo = Ro.y();
    double zo = Ro.z();
    double xd = Rd.x();
    double yd = Rd.y();
    double zd = Rd.z();

    double R2 = R*R;
    double r2 = r*r;
    double Py2 = yo*yo;
    double Dy2 = yd*yd;
    double pdy2 = yo * yd;

    double k1 = xo*xo + zo*zo + Py2 - R2 -r2;
    double k2 = xo*xd + zo*zd + pdy2;

    double a1 = 4.0 * k2;
    double a2 = 2.0 * (k1 + 2.0 * (k2 * k2 + R2 * Dy2));
    double a3 = 4.0 * (k2 * k1 + 2.0 * R2 * pdy2);
    double a4 = k1 * k1 + 4.0 * R2 * (Py2 - r2);

    // Find all roots
    double all_roots[4];
    int num = Math::solveQuartic(a1,a2,a3,a4,all_roots);

    // Prune out the negative roots
    int positive_roots_num = 0;
    for(int i = 0; i < num; i++) {
	if (all_roots[i] + closer > 2*EPSILON) {
	    roots[positive_roots_num++] = (all_roots[i] + closer) / ray.t_scale;
	}
    }
    return positive_roots_num;
}


/**
 * Finds the normal at a surface point
 *
 * The normal of a point P on the torus has the direction of a vector which
 * ends in P and begins in the nearest point on the circle created by the
 * major radius.
 *
 * @param point a surface point P on torus in local space
 * @see http://research.microsoft.com/~hollasch/cgindex/render/raytorus.html
 */
Vector Torus::normal(const Vector& point) const {
    Vector p = Vector(point.x(),0,point.z());
    p.normalize();
    p = R * p;
    Vector result = point - p;
    result.normalize();
    return result;
}

AABox Torus::getBoundingBox() const {
    double tmin = -R - r;
    double tmax = R + r;
    AABox bbox = AABox(Vector(tmin,tmin,tmin), Vector(tmax,tmax,tmax));
    return bboxToWorld(bbox);
}

SceneObject* Torus::clone() const {
    return new Torus(*this);
}

void Torus::allIntersections(const Ray& ray, vector<Intersection>& result) const {
    double roots[4];
    int num = allPositiveRoots(ray,roots);
    result.reserve(num);
    bool entering = (num % 2) == 0;
    for(int i = 0; i < num; i++) {
	Intersection inter;
	fullIntersect(ray,roots[i],inter);
	inter.isEntering(entering);
	entering = !entering;
	result.push_back(inter);
    }
}

bool Torus::inside(const Vector& point) {
   Vector p = pointToObject(point);
   double d = R - sqrt(p.x() * p.x() + p.z() * p.z());
   return d * d + p.y() * p.y() - r * r < 0;
}
