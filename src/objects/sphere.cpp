#include <iostream>
#include <cmath>
#include <cassert>

#include "sphere.h"
#include "ray.h"
#include "intersection.h"
#include "math/matrix.h"
#include "math/vector2.h"
#include "image/rgb.h"
#include "boundingbox.h"
#include "object.h"
#include "intersection.h"

using namespace std;

Sphere::Sphere(const Vector& c, double r, const Material* mat) : BooleanOperand(mat) {
    assert(r > 0);
    center = c;
    radius = r;
}

Sphere::~Sphere() {
}

void Sphere::transform(const Matrix& m) {
    center = m * center;
}

const Vector& Sphere::getCenter() const {
    return center;
}
Intersection Sphere::_fullIntersect(const Ray& ray, const double t) const {
    Vector p = ray.getPoint(t);
    Vector n = p - center;
    n.normalize();
    Vector2 uv = Vector2(0,0); //getUV(p);
    return Intersection(p,t,n,uv);
}

double Sphere::_fastIntersect(const Ray& ray) const {
    
    // See CGPP page 1101
    Vector v = ray.getDirection();
    Vector QmP = ray.getOrigin() - center;
    // TODO: Q - P udregnes for mange gange herunder
    double a = v * v;
    double b = 2 * v * QmP;
    double c = (QmP * QmP - radius * radius);
    double D = b * b - 4 * a * c;
    if (D < 0.0) {
	// No roots
        return -1;
    } else if (D == 0.0) {
	// One root
	double t = -b / (2 * a);
	if (!IS_ZERO(t)) {
	    return t;
	}
    } else {
	// Two roots
       double sq = sqrt(D);
       double t1 = (-b - sq ) / (2 * a);
       double t2 = (-b + sq ) / (2 * a);
       if (t1 > 0 && t1 < t2 && !IS_ZERO(t1)) {
	   return t1;
       } else if (t2 > 0 && !IS_ZERO(t2)) {
	   return t2;
       }
    }
    return -1;
}

ostream & operator<<(ostream &os, const Sphere &s) {
    os << '(' << s.center << ',' << s.radius << ')';
    return os;
}


bool Sphere::onEdge(const Vector& p) const {
    Vector d = p - center;
    double dd = radius*radius - d.norm();
    return IS_ZERO(abs(dd));
}

bool Sphere::inside(const Vector& p) const {
    Vector d = p - center;
    double dd =  radius*radius - d.norm();
    return dd > 0 && !IS_ZERO(abs(dd));
}


BoundingBox Sphere::boundingBoundingBox() const {
    Vector r = Vector(radius,radius,radius);
    return BoundingBox(center - r, center + r);
}

// See http://astronomy.swin.edu.au/~pbourke/texture/spheremap/
Vector2 Sphere::getUV(const Vector& point) const {
    Vector p = point - center;
    p.normalize();
    double u,v;
    v = acos(p[1]) / M_PI;
    if (IS_ZERO(sin((v) * M_PI))) {
	u = double(0.5);
	return Vector2(u,v);
    } 
    if (p[2] <= 0.0) {
       u = acos(p[0] / (sin((v) * M_PI))) / M_2PI; 
    } else {
       u = 1 - (acos(p[0] / (sin((v) * M_PI))) / M_2PI); 
    }
    return Vector2(u,v);
}

SceneObject* Sphere::clone() const {
    Sphere* result = new Sphere(*this);
    return result;
}

