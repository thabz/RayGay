
#include <cassert>
#include <iostream>

#include "cylinder.h"
#include "boundingbox.h"
#include "math/vector2.h"

/**
 * Construct a cylinder object
 *
 * @param begin The bottom point
 * @param end	The top point
 * @param radius The radius of the cylinder
 * @param has_caps Whether caps should be checked for intersection too
 * @param m Material
 */
Cylinder::Cylinder(const Vector& begin, const Vector& end, double radius, bool has_caps, const Material* m) : Solid (m) {

    this->has_caps = has_caps;

    Vector axis = end - begin;

    transform(Matrix::matrixScale(Vector(radius,radius,axis.length())));
    transform(Matrix::matrixOrient(axis).inverse());
    transform(Matrix::matrixTranslate(begin));
}

void Cylinder::transform(const Matrix& m) {
    Transformer::transform(m);
}

BoundingBox Cylinder::boundingBoundingBox() const {
    Vector mini = Vector(-1,-1,0);
    Vector maxi = Vector(1,1,1);
    BoundingBox bbox = BoundingBox(mini,maxi);
    bbox.grow(10*EPSILON);
    return bboxToWorld(bbox);
}

double Cylinder::_fastIntersect(const Ray& ray) const {
    double roots[2];
    unsigned int num = allPositiveRoots(ray,roots);
    return num == 0 ? -1 : roots[0];
}

Intersection Cylinder::_fullIntersect(const Ray& world_ray, const double t) const {
    Ray ray = rayToObject(world_ray);
    Vector p = ray.getPoint(t*ray.t_scale);
    Vector n = getNormal(p);
    return intersectionToWorld(Intersection(p,t,n,Vector2(0,0)));
}

inline
Vector Cylinder::getNormal(const Vector& local_point) const {
    if (has_caps) {
	if (IS_EQUAL(local_point.z(),0)) {
	    return Vector(0,0,-1);
	} else if (IS_EQUAL(local_point.z(),1)) {
	    return Vector(0,0,1);
	}
    }
    // The vector below is normalized as the cylinder has a radius
    // of 1 in object space.
    return Vector(local_point.x(),local_point.y(),0);
}


/**
 * The cylinder is transformed so that it begin-point is (0,0,0)
 * and its axis is the z-axis. It's end-point is (0,0,1);
 *
 * It's radius is 1.
 *
 * The intersection results are transformed back.
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
unsigned int Cylinder::allPositiveRoots(const Ray& world_ray, double roots[2]) const {
    unsigned int roots_found = 0;

    Ray local_ray = rayToObject(world_ray);

    Vector Rd = local_ray.getDirection();
    Vector Ro = local_ray.getOrigin();

    double a = Rd[0]*Rd[0] + Rd[1]*Rd[1];
    double b = 2 * (Ro[0]*Rd[0] + Ro[1]*Rd[1]);
    double c = Ro[0]*Ro[0] + Ro[1]*Ro[1] - 1;
    double D = b*b - 4*a*c;
    if (D > EPSILON) {
	// Two possible roots. We ignore the cases where the
	// ray is tangent to the cylinder and we only have one root.
	double sq = sqrt(D);
	double t1 = (-b - sq ) / (2 * a);
	double t2 = (-b + sq ) / (2 * a);
	double ip1_z =  Ro[2] + t1 * Rd[2];
	double ip2_z =  Ro[2] + t2 * Rd[2];
	if (ip1_z >= EPSILON && ip1_z <= 1.0 && t1 > EPSILON) {
	    roots[roots_found++] = t1;
	}
	if (ip2_z >= EPSILON && ip2_z <= 1.0 && t2 > EPSILON) {
	    roots[roots_found++] = t2;
	}
    }
    if (roots_found < 2 && has_caps && !IS_ZERO(Rd[2])) {
	double t, i_x, i_y;
	// Check intersection with bottom cap
	t = (0.0 - Ro[2]) / Rd[2];
	if (t > EPSILON) {
	    i_x = Ro[0] + t * Rd[0];
	    i_y = Ro[1] + t * Rd[1];
	    if ((i_x * i_x + i_y * i_y) < 1.0) {
		roots[roots_found++] = t;
	    }
	}

	// Check intersection with top cap
	t = (1.0 - Ro[2]) / Rd[2];
	if (t > EPSILON) {
	    i_x = Ro[0] + t * Rd[0];
	    i_y = Ro[1] + t * Rd[1];
	    if ((i_x * i_x + i_y * i_y) < 1.0) {
		roots[roots_found++] = t;
	    }
	}
    }

    // Sort roots
    if (roots_found == 2) {
	if (roots[0] > roots[1]) {
	    double tmp = roots[0];
	    roots[0] = roots[1];
	    roots[1] = tmp;
	}
    }
    
    roots[0] /= local_ray.t_scale;
    roots[1] /= local_ray.t_scale;

    return roots_found;
}

SceneObject* Cylinder::clone() const {
    return new Cylinder(*this);
}

void Cylinder::allIntersections(const Ray& ray, vector<Intersection>& result) const {
    double roots[2];
    unsigned int num = allPositiveRoots(ray,roots);
    result.reserve(num);
    for(unsigned int i = 0; i < num; i++) {
	Intersection inter = fullIntersect(ray,roots[i]);
	result.push_back(inter);
    }
    if (num == 1) {
	result[0].isEntering(false);
    } else if (num == 2) {
	result[0].isEntering(true);
	result[1].isEntering(false);
    }
    return;
}

