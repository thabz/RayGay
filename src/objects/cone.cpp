
#include <cassert>
#include <iostream>

#include "cone.h"
#include "aabox.h"
#include "math/vector2.h"

/**
 * Construct a cone object.
 *
 * @param begin	    	The bottom point
 * @param end	    	The top point
 * @param radius_begin  The radius at the base of the cone 
 * @param radius_end	The radius at the apex of the cone
 * @param has_caps  	Whether caps should be checked for intersection too
 * @param material	Material of the cone
 */
Cone::Cone(const Vector& begin, const Vector& end, double radius_begin, double radius_end, bool has_caps, const Material* material) : Solid (material) {

    this->begin = begin;
    this->end = end;
    this->radius_begin = radius_begin;
    this->radius_end = radius_end;

    Vector endt = end - begin;

    this->has_caps = has_caps;

    Matrix translation = Matrix::matrixTranslate(begin);
    Matrix scale = Matrix::matrixScale(Vector(1,1,endt.length()));
    Matrix rotation = Matrix::matrixOrient(endt);

    transform(scale);
    transform(rotation.inverse());
    transform(translation);
}

void Cone::transform(const Matrix& m) {
    Transformer::transform(m);
}

AABox Cone::getBoundingBox() const {
    double r = max(radius_end,radius_begin);
    Vector mini = Vector(-r,-r,0);
    Vector maxi = Vector(r,r,1);
    AABox bbox = AABox(mini,maxi);
    bbox.grow(10*EPSILON);
    return bboxToWorld(bbox);
}

double Cone::_fastIntersect(const Ray& ray) const {
    double roots[2];
    unsigned int num = allPositiveRoots(ray,roots);
    return num == 0 ? -1 : roots[0];
}

Vector Cone::getNormal(const Vector& local_point) const {
    if (has_caps) {
	if (IS_EQUAL(local_point[2],0)) {
	    return Vector(0,0,-1);
	} else if (IS_EQUAL(local_point[2],1)) {
	    return Vector(0,0,1);
	}
    }
    // TODO: The z-part is probably wrong.
    Vector normal = Vector(local_point[0],local_point[1],(radius_begin-radius_end)/2);
    normal.normalize();
    return normal;
}


void Cone::_fullIntersect(const Ray& world_ray, const double t, Intersection& result) const {
    Ray ray = rayToObject(world_ray);
    Vector p = ray.getPoint(t*ray.t_scale);
    Vector n = getNormal(p);
    result = Intersection(p,t,n,Vector2(0,0));
    intersectionToWorld(result);
}


/**
 * The cone is transformed so that it begin-point is (0,0,0)
 * and its axis is the z-axis. It's end-point is (0,0,1);
 *
 * The intersection results are transformed back.
 *
 * The cylinder intersection is done by finding the roots of
 *
 * x^2 + y^2 = (rB - z(rB - rT))^2 for the ray (x,y,z) = O + t*D = Ray(t)
 *
 * which reduces to a*t^2 + b*t + c = 0
 * where
 * a = D_z^2*r_B^2 - 2*D_z^2*r_T*r_B + (D_z^2*r_T^2 + (-D_x^2 - D_y^2))
 * b = (2*D_z*O_z - 2*D_z)*r_B^2 + (-4*D_z*r_T*O_z + 2*D_z*r_T)*r_B + 2*D_z*r_T^2*O_z - 2*(D_x*O_x + D_y*O_y)
 * c = (O_z^2 - 2*O_z + 1)*r_B^2 + (-2*r_T*O_z^2 + 2*r_T*O_z)*r_B + r_T^2*O_z^2 - O_x^2 - O_y^2
 *
 * Afterwards we must check that Ray(t) where t is a root
 * are within the z-axis interval [0,1] that defines the length of the cone.
 *
 */
unsigned int Cone::allPositiveRoots(const Ray& world_ray, double roots[2]) const {
    unsigned int roots_found = 0;

    Ray local_ray = rayToObject(world_ray);

    double r_B = radius_begin;
    double r_T = radius_end;
    double r_T2 = r_T * r_T;
    double r_B2 = r_B * r_B;

    double D_x = local_ray.getDirection()[0];
    double D_y = local_ray.getDirection()[1];
    double D_z = local_ray.getDirection()[2];
    double O_x = local_ray.getOrigin()[0];
    double O_y = local_ray.getOrigin()[1];
    double O_z = local_ray.getOrigin()[2];

    double a = D_z*D_z* (r_B2 - 2*r_T*r_B + r_T2) - D_x*D_x - D_y*D_y;
    double b = 2*((O_z - 1)*D_z*r_B2 + 
	       (1 - 2*O_z)*r_B*D_z*r_T + 
	       D_z*r_T2*O_z - D_x*O_x - D_y*O_y);
    double c = (O_z*O_z - 2*O_z + 1)*r_B2 + 
	       (1 - O_z)*r_B*2*r_T*O_z +  
	       r_T2*O_z*O_z - O_x*O_x - O_y*O_y;
    double D = b*b - 4*a*c;
    if (D > EPSILON) {
	// Two possible roots
	double sq = sqrt(D);
	double t1 = (-b - sq ) / (2 * a);
	double t2 = (-b + sq ) / (2 * a);
	double ip1_z =  O_z + t1 * D_z;
	double ip2_z =  O_z + t2 * D_z;
	if (ip1_z >= EPSILON && ip1_z <= 1 && t1 > EPSILON) {
	    roots[roots_found++] = t1;
	}
	if (ip2_z >= EPSILON && ip2_z <= 1 && t2 > EPSILON) {
	    roots[roots_found++] = t2;
	}
    }

    // Check intersection with caps
    if (roots_found < 2 && has_caps && IS_NZERO(D_z)) {
	double t, i_x, i_y;

	// Check intersection with bottom cap
	if (radius_begin > EPSILON) {
	    t = (0.0 - O_z) / D_z;
	    if (t > EPSILON) {
		i_x = O_x + t * D_x;
		i_y = O_y + t * D_y;
		if ((i_x * i_x + i_y * i_y) < radius_begin*radius_begin) {
		    roots[roots_found++] = t;
		}
	    }
	}

	// Check intersection with top cap
	if (radius_end > EPSILON) {
	    t = (1.0 - O_z) / D_z;
	    if (t > EPSILON) {
		i_x = O_x + t * D_x;
		i_y = O_y + t * D_y;
		if ((i_x * i_x + i_y * i_y) < radius_end*radius_end) {
		    roots[roots_found++] = t;
		}
	    }
	}
    }

    // Sort roots
    if (roots_found == 2 && roots[0] > roots[1]) {
	double tmp = roots[0];
	roots[0] = roots[1];
	roots[1] = tmp;
    }

    roots[0] /= local_ray.t_scale;
    roots[1] /= local_ray.t_scale;

    return roots_found;
}

SceneObject* Cone::clone() const {
    return new Cone(*this);
}

void Cone::allIntersections(const Ray& ray, vector<Intersection>& result) const {
    double roots[2];
    int num = allPositiveRoots(ray,roots);
    result.reserve(num);
    for(int i = 0; i < num; i++) {
	Intersection inter;
	fullIntersect(ray,roots[i],inter);
	result.push_back(inter);
    }
    if (num == 1) {
	result[0].isEntering(false);
    } else if (num == 2) {
	result[0].isEntering(true);
	result[1].isEntering(false);
    }
}

