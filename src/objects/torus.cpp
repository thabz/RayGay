
#include <cassert>
#include <iostream>
#include "torus.h"
#include "ray.h"
#include "boundingbox.h"
#include "math/functions.h"
#include "math/vector2.h"

/**
 * Constructs a torus in zx-plane centered in origin.
 *
 * @param R major radius aka outer radius
 * @param r minor radius aka inner radius
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

Intersection Torus::_fullIntersect(const Ray& world_ray, const double t) const {
    Ray ray = rayToObject(world_ray);
    Vector p  = ray.getPoint(t);
    Vector n = normal(p);
    return intersectionToWorld(Intersection(p,t,n,Vector2(0,0)));
}

double Torus::_fastIntersect(const Ray& world_ray) const {
    double roots[4];
    unsigned int num = allPositiveRoots(world_ray,roots);
    return num == 0 ? -1 : roots[0];
}

unsigned int Torus::allPositiveRoots(const Ray& world_ray, double roots[4]) const {
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
	    roots[positive_roots_num++] = all_roots[i] + closer;
	}
    }
    return positive_roots_num;
}


/**
 * Finds the normal at a point of intersection.
 *
 * The normal of a point P on the torus has the direction of a vector which
 * ends in P and begins in the nearest point on the circle created by the
 * major radius.
 *
 * @param i an positive intersection with this torus
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

BoundingBox Torus::boundingBoundingBox() const {
    double tmin = -R - r;
    double tmax = R + r;
    BoundingBox bbox = BoundingBox(Vector(tmin,tmin,tmin), Vector(tmax,tmax,tmax));
    return bboxToWorld(bbox);
}

SceneObject* Torus::clone() const {
    return new Torus(*this);
}

vector<Intersection> Torus::allIntersections(const Ray& ray) const {
    double roots[4];
    int num = allPositiveRoots(ray,roots);
    vector<Intersection> result;
    result.reserve(num);
    bool entering = (num % 2) == 0;
    for(int i = 0; i < num; i++) {
	Intersection inter = fullIntersect(ray,roots[i]);
	inter.isEntering(entering);
	entering = !entering;
	result.push_back(inter);
    }
    return result;
}
