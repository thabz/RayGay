
#include <cassert>
#include "torus.h"
#include "ray.h"
#include "math/functions.h"

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
 *
 */
bool Torus::_intersect(const Ray& ray) const {
    // TODO: implement
}

/**
 * @see http://research.microsoft.com/~hollasch/cgindex/render/raytorus.html
 * TODO: test
 */
Vector Torus::normal(const Intersection& i) const {
    Vector x = i.getPoint();
    Vector p = (x.x(),x.y(),0);
    p.normalize;
    p *= R;
    return (x-p).normalize();
}

/**
 * Says whether a point is on the surface of the torus.
 *
 * Using the formula
 * 
 * \f[ (x^2 + y^2 + z^2 + R^2 - r^2)^2 - 4R^2(x^2+y^2) = 0 \f]
 *
 * @param p the point the check for
 */
bool Torus::onEdge(const Vector &p) const {
    double x = p[0];
    double y = p[1];
    double z = p[2];
    double term = x*x + y*y + z*z + R*R - r*r;
    return IS_ZERO(term*term - 4*R*R(x*x + y*y));
}

bool Torus::inside(const Vector &p) const {

}

SceneObject* Torusclone() const {
    return new Torus(*this);
}
