

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
Cylinder::Cylinder(const Vector& begin, const Vector& end, double radius, const Material* m) : Solid (m) {

    this->begin = begin;
    this->end = end;
    this->r = radius;
    this->rr = radius*radius;

    Vector endt = end - begin;
    this->height = endt.length();

    Matrix translation = Matrix::matrixTranslate(begin);
    Matrix rotation = Matrix::matrixOrient(endt);
    rotation = rotation.inverse();

    transform(rotation * translation);
}

void Cylinder::transform(const Matrix& m) {
    Transformer::transform(m);
}

/**
 * Returns whether \f$\sqrt{x^2 +y^2} < r \f$ where point = \f$(x,y,z)\f$
 */
bool Cylinder::inside(const Vector &point) const {
   Vector p = pointToObject(point);
   return    p[0]*p[0] + p[1]*p[1] < rr 
          && p[2] < height 
	  && p[2] > double(0);
   return    IS_LESS_THAN(p[0]*p[0] + p[1]*p[1], rr)
          && IS_LESS_THAN(p[2], height) 
	  && IS_GREATER_THAN(p[2],double(0));
   
}

BoundingBox Cylinder::boundingBoundingBox() const {
    Vector rv = Vector(r,r,r);
    Vector real_begin = begin;
    Vector real_end = end;
    BoundingBox bbox = BoundingBox(real_begin - rv,real_end + rv);
    return bboxToWorld(bbox);
}

double Cylinder::_fastIntersect(const Ray& ray) const {
    return _intersect(ray).getT();
}

Intersection Cylinder::_fullIntersect(const Ray& ray, const double t) const {
    return _intersect(ray);
}


/**
 * The cylinder is transformed so that it begin-point is (0,0,0)
 * and its axis is the y-axis. The intersection results are
 * transformed back.
 *
 * The cylinder intersection is done by finding the roots of
 *
 * x^2 + y^2 = r^2 for the ray (x,y,z) = Ro + t*Rd = Ray(t)
 * 
 * => gives (Rox + (t*Rdx))^2 + (Roy + (t*Rdy))^2 = r^2
 * => Rox^2 + t^2*Rdx^2 + 2*Rox*t*Rdx + Roy^2 + t^2*Rdy^2 + 2*Roy*t*Rdy = r^2
 * => (Rdx^2 + Rdy^2)*t^2 + (2*Rox*Rdx + 2*Roy*Rdy)*t + (Rox^2 + Roy^2 - r^2) = 0
 * => a*t^2 + b*t + c = 0
 * where
 * a = Rdx^2 + Rdy^2
 * b = 2 * (Rox*Rdx + Roy*Rdy)
 * c = (Rox^2 + Roy^2 - r^2)
 *
 * Afterwards we must check that Ray(t) where t is a root
 * are within the z-axis interval that defines the lenght of the cylinder.
 */
Intersection Cylinder::_intersect(const Ray& world_ray) const {
    Ray ray = rayToObject(world_ray);

    Vector Rd = ray.getDirection();
    Vector Ro = ray.getOrigin();

    double a = Rd[0]*Rd[0] + Rd[1]*Rd[1];
    double b = 2 * (Ro[0]*Rd[0] + Ro[1]*Rd[1]);
    double c = Ro[0]*Ro[0] + Ro[1]*Ro[1] - rr;
    double D = b*b - 4*a*c;
    if (D < 0.0) {
	// No roots
	return Intersection();
    } else if (IS_ZERO(D)) {
	// One root
	double t = -b / (2 * a);
	Vector result_p = Ro + t * Rd;
	double result_t = t;
	if (!IS_ZERO(t) && result_p[2] >= double(0) && result_p[2] <= height) {
	    Vector normal = Vector(result_p[0],result_p[1],0);
	    normal.normalize();
	    Intersection res = Intersection(result_p,result_t,normal,Vector2(0,0));
	    return intersectionToWorld(res);
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
	if (ip1[2] >= double(0) && ip1[2] <= height) {
	    Vector normal = Vector(ip1[0],ip1[1],0);
	    normal.normalize();
	    i1 = Intersection(ip1,t1,normal,Vector2(0,0));
	}
	if (ip2[2] >= double(0) && ip2[2] <= height) {
	    Vector normal = Vector(ip2[0],ip2[1],0);
	    normal.normalize();
	    i2 = Intersection(ip2,t2,normal,Vector2(0,0));
	}
	if (t1 > 0 && t1 < t2 && !IS_ZERO(t1) && i1.isIntersected()) {
	    return intersectionToWorld(i1);
	} else if (t2 > 0 && !IS_ZERO(t2) && i2.isIntersected()) {
	    return intersectionToWorld(i2);
	} else {
	    return Intersection();
	}
    }
}

SceneObject* Cylinder::clone() const {
    return new Cylinder(*this);
}

vector<Intersection> Cylinder::allIntersections(const Ray& ray) const {
    vector<Intersection> result;
    return result;
}

