
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
    this->transformation = Matrix();
    prepareMatrices();
}

void Torus::prepareMatrices() {
    inverse_transformation = transformation.inverse();
    rotation = transformation.extractRotation();
    inverse_rotation = rotation.inverse();
}

void Torus::transform(const Matrix& m) {
    transformation = transformation * m;
    scene_transformation = scene_transformation * m;
    prepareMatrices();
}


Intersection Torus::_intersect(const Ray& ray) const {

    Vector Rd = inverse_rotation * ray.getDirection();
    Vector Ro = inverse_transformation * ray.getOrigin();

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

    double roots[4];
    int num = Math::solveQuartic(a1,a2,a3,a4,roots);
    if (num == 0) {
	return Intersection();
    } else {
	double t = roots[0];
	return Intersection(transformation * ray.getPoint(t + closer),t + closer);
    }
}


/**
 * Finds the normal at a point of intersection.
 *
 * @param i an positive intersection with this torus
 * @see http://research.microsoft.com/~hollasch/cgindex/render/raytorus.html
 */
Vector Torus::normal(const Intersection& i) const {
    Vector x = inverse_transformation * i.getPoint();
    Vector p = Vector(x.x(),x.y(),0);
    p.normalize();
    p = R * p;
    Vector result = x-p;
    result.normalize();
    result = rotation * result;
    return result;
}

bool Torus::onEdge(const Vector &point) const {
    Vector p = inverse_transformation * point;
    Vector Q = Vector(p[0],0,p[2]);
    double l = Q.length();
    if (IS_ZERO(l)) {
	return (r == R) ? true : false;
    }
    Vector X = (R/l) * Q; // Closest point on big circle
    return IS_EQUAL((p-X).length(),r);
}

bool Torus::inside(const Vector &point) const {
    Vector p = inverse_transformation * point;
    Vector Q = Vector(p[0],0,p[2]);
    double l = Q.length();
    if (IS_ZERO(l)) {
	std::cout << "whoops" << std::endl;
	return (r < R) ? false : true;
    }
    Vector X = (R/l) * Q; // Closest point on big circle
    return (p-X).length() < r;
}

BoundingBox Torus::boundingBoundingBox() const {
    double min = -R-r;
    double max = R+r;
    return BoundingBox(scene_transformation * Vector(min,min,min),
	    scene_transformation * Vector(max,max,max));

}

const Material& Torus::getMaterial() const {
    return material;
}

Vector2 Torus::getUV(const Intersection& intersection) const {
    // TODO: Implement
    return Vector2(0,0);
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
