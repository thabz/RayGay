

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

BoundingBox Cylinder::boundingBoundingBox() const {
    Vector rv = Vector(r,r,r);
    Vector real_begin = begin;
    Vector real_end = end;
    BoundingBox bbox = BoundingBox(real_begin - rv,real_end + rv);
    return bboxToWorld(bbox);
}

double Cylinder::_fastIntersect(const Ray& ray) const {
    double roots[2];
    unsigned int num = allPositiveRoots(ray,roots);
    return num == 0 ? -1 : roots[0];
}

Intersection Cylinder::_fullIntersect(const Ray& ray, const double t) const {
    Vector world_point = ray.getPoint(t);
    Vector local_normal = getNormal(pointToObject(world_point));
    return Intersection(world_point,t,dirToWorld(local_normal),Vector2(0,0));
}

inline
Vector Cylinder::getNormal(const Vector& local_point) const {
    if (IS_EQUAL(local_point[2],0)) {
	return Vector(0,0,-1);
    } else if (IS_EQUAL(local_point[2],height)) {
	return Vector(0,0,1);
    } else {
	Vector normal = Vector(local_point[0],local_point[1],0);
	normal.normalize();
	return normal;
    }
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
unsigned int Cylinder::allPositiveRoots(const Ray& world_ray, double roots[4]) const {
    unsigned int roots_found = 0;

    Ray local_ray = rayToObject(world_ray);

    Vector Rd = local_ray.getDirection();
    Vector Ro = local_ray.getOrigin();

    double a = Rd[0]*Rd[0] + Rd[1]*Rd[1];
    double b = 2 * (Ro[0]*Rd[0] + Ro[1]*Rd[1]);
    double c = Ro[0]*Ro[0] + Ro[1]*Ro[1] - rr;
    double D = b*b - 4*a*c;
    if (D < 0.0) {
	// No roots
	return 0;
    } else if (IS_ZERO(D)) {
	// One root
	double t = -b / (2 * a);
	double result_p_z = Ro[2] + t * Rd[2];
	if (t > EPSILON && result_p_z >= double(0) && result_p_z <= height) {
	    roots[0] = t;
	    roots_found = 1;
	}
    } else {
	// Two roots
	Intersection i1;
	Intersection i2;
	double sq = sqrt(D);
	double t1 = (-b - sq ) / (2 * a);
	double t2 = (-b + sq ) / (2 * a);
	double ip1_z =  Ro[2] + t1 * Rd[2];
	double ip2_z =  Ro[2] + t2 * Rd[2];
	if (ip1_z >= EPSILON && ip1_z <= height) {
	    roots[roots_found++] = t1;
	}
	if (ip2_z >= EPSILON && ip2_z <= height) {
	    roots[roots_found++] = t2;
	}

	// Sort roots and return if two found
	if (roots_found == 2) {
	    if (roots[0] > roots[1]) {
		double tmp = roots[0];
		roots[0] = roots[2];
		roots[2] = tmp;
	    }
	    return 2;
	}
    }
    // TODO: Intersect with caps
    return roots_found;
}

SceneObject* Cylinder::clone() const {
    return new Cylinder(*this);
}

vector<Intersection> Cylinder::allIntersections(const Ray& ray) const {
    double roots[2];
    int num = allPositiveRoots(ray,roots);
    vector<Intersection> result;
    result.reserve(num);
    for(int i = 0; i < num; i++) {
	Intersection inter = fullIntersect(ray,roots[i]);
	result.push_back(inter);
    }
    if (num == 1) {
	result[0].isEntering(false);
    } else if (num == 2) {
	result[0].isEntering(true);
	result[1].isEntering(false);
    }
    return result;
}

