
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
    // TODO: Implement
    return Intersection();
}

/**
 * @see http://research.microsoft.com/~hollasch/cgindex/render/raytorus.html
 * TODO: test
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

bool Torus::intersects(const BoundingBox& b) const {

}

SceneObject* Torus::clone() const {
    return new Torus(*this);
}
