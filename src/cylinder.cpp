

#include <cassert>

#include "cylinder.h"
#include "boundingbox.h"
#include "math/vector2.h"

/**
 * Construct a cylinder object
 *
 * @param begin The bottom point
 * @param end	The top point
 * @param radius The radius of the cylinder
 * @param m Material
 */
Cylinder::Cylinder(const Vector& begin, const Vector& end, double radius, Material m) {
    this->begin = begin;
    this->end = end;
    this->material = m;
    this->r = radius;
    this->rr = radius*radius;
    this->height = (end-begin).length();

    transformation = Matrix::matrixTranslate(begin);
    Vector endt = end - begin;
    endt.normalize();
    Matrix rot = Matrix::matrixOrient(Vector(0,0,1),endt);
    rot = rot.inverse();

    transformation = rot * transformation;
    prepareMatrices();
}

void Cylinder::prepareMatrices() {
    inverse_transformation = transformation.inverse();
    rotation = transformation.extractRotation();
    inverse_rotation = rotation.inverse();
}

void Cylinder::transform(const Matrix& m) {
    transformation = m * transformation;
    prepareMatrices();
}

Vector Cylinder::normal(const Intersection & i) const {
    Vector p = i.getPoint();
    p[1] = 0;
    p.normalize();
    return p;
}

const Material& Cylinder::getMaterial() const {
    return material;
}

bool Cylinder::onEdge(const Vector &point) const {
   Vector p = inverse_transformation * point;
   return (IS_EQUAL(p[0]*p[0] + p[2]*p[2], rr)
              && p[1] < height 
   	      && p[1] > double(0)) /* On stem */
         || (p[0]*p[0] + p[2]*p[2] < rr
	      && (IS_EQUAL(p[1],height) 
		  || IS_EQUAL(p[1],double(0)))); /* On end discs */
}

bool Cylinder::inside(const Vector &point) const {
   Vector p = inverse_transformation * point;
   return    p[0]*p[0] + p[2]*p[2] < rr 
          && p[1] < height 
	  && p[1] > double(0);
   return    IS_LESS_THAN(p[0]*p[0] + p[2]*p[2], rr)
          && IS_LESS_THAN(p[1], height) 
	  && IS_GREATER_THAN(p[1],double(0));
   
}

bool Cylinder::intersects(const BoundingBox& bb) const {
    // TODO: This sucks ... but is it needed? 
    Vector* c = boundingBoundingBox().getCorners();
    bool result = false;
    for(int i = 0; i < 8; i++) {
	if (bb.inside(c[i]) || bb.onEdge(c[i]))
	    result = true;
    }
    delete [] c;
    return result;
}

BoundingBox Cylinder::boundingBoundingBox() const {
    Vector rv = Vector(r,r,r);
    return BoundingBox(begin-rv,end+rv);
}

Vector2 Cylinder::getUV(const Intersection& intersection) const {
    // TODO: Implement
    return Vector2(0,0);
}

/**
 * The cylinder is transformed so that it begin-point is (0,0,0)
 * and its axis is the y-axis. The intersection results are
 * transformed back.
 *
 * The cylinder intersection is done by finding the roots of
 *
 * x^2 + z^2 = r^2 for the ray (x,y,z) = Ro + t*Rd = Ray(t)
 * 
 * => gives (Rox + (t*Rdx))^2 + (Roz + (t*Rdz))^2 = r^2
 * => Rox^2 + t^2*Rdx^2 + 2*Rox*t*Rdx + Roz^2 + t^2*Rdz^2 + 2*Roz*t*Rdz = r^2
 * => (Rdx^2 + Rdz^2)*t^2 + (2*Rox*Rdx + 2*Roz*Rdz)*t + (Rox^2 + Roz^2 - r^2) = 0
 * => a*t^2 + b*t + c = 0
 * where
 * a = Rdx^2 + Rdz^2
 * b = 2 * (Rox*Rdx + Roz*Rdz)
 * c = (Rox^2 + Roz^2 - r^2)
 *
 * Afterwards we must check that Ray(t) where t is a root
 * are within the y-axis interval that defines the lenght of the cylinder.
 */
Intersection Cylinder::_intersect(const Ray& ray) const {

    Vector Rd = inverse_rotation * ray.getDirection();
    Vector Ro = inverse_transformation * ray.getOrigin();

    double a = Rd[0]*Rd[0] + Rd[2]*Rd[2];
    double b = 2 * (Ro[0]*Rd[0] + Ro[2]*Rd[2]);
    double c = Ro[0]*Ro[0] + Ro[2]*Ro[2] - rr;
    double D = b*b - 4*a*c;
    if (D < 0.0) {
	// No roots
	return Intersection();
    } else if (IS_ZERO(D)) {
	// One root
	double t = -b / (2 * a);
	Vector result_p = Ro + t * Rd;
	double result_t = t;
	if (!IS_ZERO(t) && result_p[1] >= double(0) && result_p[1] <= height) {
	    return Intersection(transformation * result_p,result_t);
	} else {
	    return Intersection();
	}
    } else {
	// Two roots
	Intersection i1;
	Intersection i2;
	double sq = sqrt(D);
	double t1 = (-b - sq ) / (2 * a);
	double t2 = (-b + sq ) / (2 * a);
	Vector ip1 =  Ro + t1 * Rd;
	Vector ip2 =  Ro + t2 * Rd;
	if (ip1[1] >= double(0) && ip1[1] <= height) {
	    i1 = Intersection(transformation * ip1,t1);
	}
	if (ip2[1] >= double(0) && ip2[1] <= height) {
	    i2 = Intersection(transformation * ip2,t2);
	}
	if (t1 > 0 && t1 < t2 && !IS_ZERO(t1) && i1.isIntersected()) {
	    return i1;
	} else if (t2 > 0 && !IS_ZERO(t2) && i2.isIntersected()) {
	    return i2;
	} else {
	    return Intersection();
	}
    }
}

