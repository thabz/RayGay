
#include <cassert>
#include <iostream>
#include "torus.h"
#include "ray.h"
#include "boundingbox.h"
#include "materials/material.h"
#include "math/functions.h"
#include "math/vector2.h"

/**
 * Constructs a torus in xy-plane centered in origin.
 *
 * @param R major radius aka outer radius
 * @param r minor radius aka inner radius
 * @param m material of the torus
 */
Torus::Torus(double R, double r, Material m) {
    assert(R > r);
    assert(r > 0);
    this->R = R;
    this->r = r;
    this->material = m;
}

/**
 * @see http://www.robots.ox.ac.uk/~awf/graphics/ray-torus.html
 */
Intersection Torus::_intersect(const Ray& ray) const {
    double xo = ray.getOrigin().x();
    double yo = ray.getOrigin().y();
    double zo = ray.getOrigin().z();
    double xd = ray.getDirection().x();
    double yd = ray.getDirection().y();
    double zd = ray.getDirection().z();

    double a = r;
    double b = R;

    double on = ray.getOrigin().norm(); // Origin norm
    double dn = ray.getDirection().norm(); // Direction norm
    double od = (xo*xd+yo*yd+zo*zd); 
    double rr = on-(a*a+b*b);


    double a4 = dn*dn;
    double a3 = 4*od*dn;
    double a2 = 2*dn*rr + 4*od-4*a*a*zd*zd;
    double a1 = 4*od*rr + 8*a*a*zo*zd;
    double a0 = rr*rr - 4*a*a*(b*b-zo*zo);

    a1 /= a0;
    a2 /= a0;
    a3 /= a0;
    a4 /= a0;
    double roots[4];
    int num = Math::solveQuartic(a1,a2,a3,a4,roots);
    if (num == 0) {
	return Intersection();
    } else {
	double t = roots[0];
	return Intersection(ray.getPoint(t),t);
    }
}


/**
 * Finds the normal at a point of intersection.
 *
 * @param i an positive intersection with this torus
 * @see http://research.microsoft.com/~hollasch/cgindex/render/raytorus.html
 */
Vector Torus::normal(const Intersection& i) const {
    Vector x = i.getPoint();
    Vector p = Vector(x.x(),x.y(),0);
    p.normalize();
    p = R * p;
    Vector result = x-p;
    result.normalize();
    return result;
}

/**
 * Says whether a point is on the surface of the torus.
 *
 * @param p the point the check for
 */
bool Torus::onEdge(const Vector &p) const {
    Vector Q = Vector(p[0],0,p[2]);
    double l = Q.length();
    if (IS_ZERO(l)) {
	return (r == R) ? true : false;
    }
    Vector X = (R/l) * Q; // Closest point on big circle
    return IS_EQUAL((p-X).length(),r);
}

bool Torus::inside(const Vector &p) const {
    Vector Q = Vector(p[0],0,p[2]);
    double l = Q.length();
    if (IS_ZERO(l)) {
	std::cout << "whoops" << std::endl;
	return (r < R) ? false : true;
    }
    Vector X = (R/l) * Q; // Closest point on big circle
    return (p-X).length() < r;
}

void Torus::transform(const Matrix& m) {

}

BoundingBox Torus::boundingBoundingBox() const {
    double min = -R-r;
    double max = R+r;
    return BoundingBox(Vector(min,min,min),Vector(max,max,max));

}

const Material& Torus::getMaterial() const {
    return material;
}

Vector2 Torus::getUV(const Intersection& intersection) const {

}

bool Torus::intersects(const BoundingBox& bb) const {
    Vector* c = boundingBoundingBox().getCorners();
    bool result = false;
    for(int i = 0; i < 8; i++) {
	if (bb.inside(c[i]) || bb.onEdge(c[i]))
	    result = true;
    }
    delete [] c;
    return result;
}

SceneObject* Torus::clone() const {
    return new Torus(*this);
}
