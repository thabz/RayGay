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

Sphere::Sphere(const Vector& c, double r, const Material* mat) : Solid(mat) {
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

#define DOT(v1,v2) (v1[0]*v2[0]+v1[1]*v2[1]+v1[2]*v2[2])
#define SUB(dest,v1,v2) \
          dest[0]=v1[0]-v2[0]; \
          dest[1]=v1[1]-v2[1]; \
          dest[2]=v1[2]-v2[2]; 


double Sphere::_fastIntersect(const Ray& ray) const {
    
    // See CGPP page 1101
    const Vector& v = ray.getDirection();
    double QmP[3];
    SUB(QmP,ray.getOrigin(),center);
    double a = DOT(v,v);
    double b = 2 * DOT(v,QmP);
    double c = (DOT(QmP,QmP) - radius * radius);
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
       if (t1 > EPSILON && t1 < t2 && t2 > EPSILON) {
	   return t1;
       } else if (t2 > EPSILON) {
	   return t2;
       }
    }
    return -1;
}

vector<Intersection> Sphere::allIntersections(const Ray& ray) const {
    vector<Intersection> result;// = vector<Intersection>(2);

    Vector v = ray.getDirection();
    double QmP[3];
    SUB(QmP,ray.getOrigin(),center);
    double a = v * v;
    double b = 2 * DOT(v,QmP);
    double c = (DOT(QmP,QmP) - radius * radius);
    double D = b * b - 4 * a * c;
    if (D > EPSILON) {
	// Two roots
       double sq = sqrt(D);
       double t1 = (-b - sq ) / (2 * a);
       double t2 = (-b + sq ) / (2 * a);
       if (t1 > EPSILON && t2 > EPSILON) {
	   Intersection i1 = fullIntersect(ray,t1);
	   Intersection i2 = fullIntersect(ray,t2);
	   if (t1 < t2) {
	       i1.isEntering(true);
	       i2.isEntering(false);
	       result.push_back(i1);
	       result.push_back(i2);
	   } else {
	       i2.isEntering(true);
	       i1.isEntering(false);
	       result.push_back(i2);
	       result.push_back(i1);
	   }
       } else if (t1 <= EPSILON && t2 > EPSILON) {
	   Intersection i2 = fullIntersect(ray,t2);
	   i2.isEntering(false);
	   result.push_back(i2);
       } else if (t2 <= EPSILON && t1 > EPSILON) {
	   Intersection i1 = fullIntersect(ray,t1);
	   i1.isEntering(false);
	   result.push_back(i1);
       }
    }
    return result;
}

ostream & operator<<(ostream &os, const Sphere &s) {
    os << '(' << s.center << ',' << s.radius << ')';
    return os;
}

BoundingBox Sphere::boundingBoundingBox() const {
    Vector r = Vector(radius,radius,radius);
    r += Vector(5*EPSILON,5*EPSILON,5*EPSILON);
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

/**
 * Check to see if the sphere overlaps the voxel_bbox by
 * finding the squared distance from the sphere to the bbox.
 *
 * J. Arvo: "A simple method for box-sphere intersection testing" in: A. Glassner (ed.), <i>Graphics Gems</i>, pp. 335-339, Academic Press, Boston, MA, 1990.
 */
int Sphere::intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const {
    double s;
    double d = 0.0;
    for(int i = 0; i < 3; i++) {
	if (center[i] < voxel_bbox.minimum()[i]) {
	    s = center[i] - voxel_bbox.minimum()[i];
	    d += s*s;
	} else if (center[i] > voxel_bbox.maximum()[i]) {
	    s = center[i] - voxel_bbox.maximum()[i];
	    d += s*s;
	}
    }
    return d <= radius*radius + EPSILON ? 1 : -1;
}

SceneObject* Sphere::clone() const {
    Sphere* result = new Sphere(*this);
    return result;
}

