
#ifndef RAY_H
#define RAY_H

#include "math/vector.h"

class Object;

/// Describing a ray, that is an origin and a direction Vector.
class Ray {
    public:
	Ray();
	/// Constructor
	Ray(const Vector& o, const Vector&  direction, const double indice);
	/// Destructor
	~Ray() { };

	/// The rays origin
	const Vector& getOrigin() const { return origin; };
	/// The rays direction
	const Vector& getDirection() const { return direction; };

        /// Rays direction (x,y,z) with inverted components ((1/x),(1/y),(1/x))
	Vector getInverseDirection() const;

        /// The material the ray is travelling in where 1.0 is vacuum.
	double getIndiceOfRefraction() const { return indice_of_refraction; };

	/// Accessor for the unique id of this ray, that is automatically assigned.
	const long getId() const { return id; };

	/// Get a point on the ray
	Vector getPoint(const double t) const { return origin + t * direction; };

	bool isCaustic() const;

	/// Number of specular bounces since leaving light
        int specularBounces;
	int diffuseBounces;
	void* fromObject;
	
    private:
	long id;
	static long seq;
	double indice_of_refraction; 
	Vector origin; ///< The rays origin
	Vector direction; ///< Unit vector of rays direction

};

inline
Vector Ray::getInverseDirection() const {
    Vector inv_direction;
    inv_direction[0] = direction[0] != 0.0 ? 1/direction[0] : HUGE_DOUBLE;
    inv_direction[1] = direction[1] != 0.0 ? 1/direction[1] : HUGE_DOUBLE;
    inv_direction[2] = direction[2] != 0.0 ? 1/direction[2] : HUGE_DOUBLE;
    return inv_direction;
}

inline
Ray::Ray(const Vector& o, const Vector& d, const double indice) : indice_of_refraction(indice), origin(o), direction(d) {
    id = ++seq;
    specularBounces = 0;
    diffuseBounces = 0;
    fromObject = NULL;
}

#endif
