#include <iostream>
#include <math.h>
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

Sphere::Sphere(const Vector& c, double r, const Material& mat) {
    assert(r > 0);
    center = c;
    radius = r;
    material = mat;
}

Sphere::~Sphere() {
}

void Sphere::transform(const Matrix& m) {
    center = m * center;
}

const Vector& Sphere::getCenter() const {
    return center;
}

/// Return the nearest intersection to ray's origin
Intersection Sphere::_intersect(const Ray& ray) const {
    Intersection result = Intersection();
    
    // See CGPP page 1101
    Vector v = ray.getDirection();
    Vector Q = ray.getOrigin();
    Vector P = center;
    double a = v * v;
    double b = 2 * v * (Q - P);
    double c = ((Q - P) * (Q - P) - radius * radius);
    double D = b*b - 4*a*c;
    if (D < 0.0) {
	// No roots
        return result;
    } else if (D == 0.0) {
	// One root
	double t = -b / (2 * a);
	if (!IS_ZERO(t)) {
	    result = Intersection(Q + t * v,t);
	}
    } else {
	// Two roots
       double sq = sqrt(D);
       double t1 = (-b - sq ) / (2 * a);
       double t2 = (-b + sq ) / (2 * a);
       if (t1 > 0 && t1 < t2 && !IS_ZERO(t1)) {
	   result = Intersection(Q + (t1 * v),t1);
       } else if (t2 > 0 && !IS_ZERO(t2)) {
	   result = Intersection(Q + (t2 * v),t2);
       }
    }
    return result;
}

Vector Sphere::normal(const Intersection& i) const {
    Vector normal = i.getPoint() - center;
    normal.normalize();
    return normal;
}

const Material& Sphere::getMaterial() const {
    return material;
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


// Stolen from http://www.gamasutra.com/features/19991018/Gomez_4.htm
bool Sphere::intersects(const BoundingBox& b) const {
    double s, d = 0;
    Vector mini = b.minimum();
    Vector maxi = b.maximum();

    for (int i = 0; i < 3; i++) {
	if (center[i] < mini[i]) {
	    s = center[i] - mini[i];
	    d += s*s;
	} else if (center[i] > maxi[i]) {
	    s = center[i] - maxi[i];
	    d += s*s;
	}
    }
    return d <= radius*radius;
}

BoundingBox Sphere::boundingBoundingBox() const {
    Vector r = Vector(radius,radius,radius);
    return BoundingBox(center - r, center + r);
}

// See http://astronomy.swin.edu.au/~pbourke/texture/spheremap/
Vector2 Sphere::getUV(const Intersection& intersection) const {
    Vector p = intersection.getPoint() - center;
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

