#include <iostream>
#include <math.h>
#include <cassert>

#include "sphere.h"
#include "vector.h"
#include "ray.h"
#include "intersection.h"
#include "matrix.h"
#include "rgb.h"
#include "constants.h"
#include "material.h"
#include "perlin.h"
#include "boundingbox.h"
#include "object.h"

using namespace std;

Sphere::Sphere(Vector c, double r, Material mat) {
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
    Vector v = ray.direction;
    Vector Q = ray.origin;
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
    Vector normal = i.point - center;
    normal.normalize();
    return normal;
}

/*! Returns the diffuse color at at point */
RGB Sphere::getDiffuseColor(const Vector& p) const {
    RGB col = material.getDiffuseColor();
    return col;
};

Material Sphere::getMaterial() const {
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
void Sphere::getUV(const Intersection& intersection, double* u, double* v) const {
    Vector p = intersection.point - center;
    p.normalize();
    *v = acos(p[1]) / M_PI;
    if (IS_ZERO(sin((*v) * M_PI))) {
	*u = double(0.5);
	return;
    } 
    if (p[2] <= 0.0) {
       *u = acos(p[0] / (sin((*v) * M_PI))) / M_2PI; 
    } else {
       *u = 1 - (acos(p[0] / (sin((*v) * M_PI))) / M_2PI); 
    }
}

void Sphere::test() {
    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Sphere s = Sphere(Vector(0,0,0),10.0,m);
    assert(s.inside(Vector(0,0,0)));
    assert(s.inside(Vector(9,0,0)));
    assert(!s.inside(Vector(10,0,0)));
    assert(!s.inside(Vector(11,0,0)));
    assert(s.onEdge(Vector(10,0,0)));
    assert(s.onEdge(Vector(0,10,0)));
    assert(!s.onEdge(Vector(0,0,0)));

    s = Sphere(Vector(0,0,0),60.0,m);

    Ray r = Ray(Vector(0,0,1000),Vector(0,0,-1),1);
    assert(IS_ZERO(s.intersect(r).point[2] - 60.0));

    r = Ray(Vector(0,0,0),Vector(0,0,-1),1);
    double z = s.intersect(r).point[2];
    assert(IS_ZERO( z + 60.0));

    r = Ray(Vector(0,0,-1000),Vector(0,0,1),1);
    z = s.intersect(r).point[2];
    assert(IS_ZERO( z + 60.0));

    r = Ray(Vector(0,0,-100),Vector(0,0,-1),1);
    assert(!s.intersect(r).intersected);

    r = Ray(Vector(0,0,-60),Vector(0,0,-1),1);
    assert(!s.intersect(r).intersected);

    /* Test intersects(BoundingBox) */
    s = Sphere(Vector(0,0,0),10.0,m);
    BoundingBox b1 = BoundingBox(Vector(-20,-20,-20),Vector(0,0,0));
    assert(s.intersects(b1));
    b1 = BoundingBox(Vector(-20,-20,-20),Vector(-15,-15,-15));
    assert(!s.intersects(b1));
    b1 = BoundingBox(Vector(-20,-20,-20),Vector(20,20,20));
    assert(s.intersects(b1));
    b1 = BoundingBox(Vector(-5,-5,-5),Vector(5,5,5));
    assert(s.intersects(b1));

    /* Test boundingBoundingBox() */
    s = Sphere(Vector(0,0,0),20.0,m);
    assert(s.boundingBoundingBox() == BoundingBox(Vector(-20,-20,-20),Vector(20,20,20)));

    
    cout << "Sphere::test() done." << endl;
}

