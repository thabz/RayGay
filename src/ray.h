
#ifndef RAY_H
#define RAY_H

#include "math/vector.h"

/// Describing a ray, that is an origin and a direction Vector.
class Ray {
    public:
	Ray();
	/// Constructor
	Ray(Vector o, Vector direction, double indice);
	/// Destructor
	~Ray() { };

	/// The rays origin
	const Vector& getOrigin() const { return origin; };
	/// The rays direction
	const Vector& getDirection() const { return direction; };

        /// Rays direction (x,y,z) with inverted components ((1/x),(1/y),(1/x))
	const Vector& getInverseDirection() const { return inv_direction; };

        /// The material the ray is travelling in where 1.0 is vacuum.
	double getIndiceOfRefraction() const { return indice_of_refraction; };

	/// Accessor for the unique id of this ray, that is automatically assigned.
	long getId() const { return id; };

	mutable double lowest_t; ///< Used in BSP

	/// Get a point on the ray
	Vector getPoint(const double t) const { return origin + t * direction; };

	bool isCaustic() const;

	/// Number of specular bounces since leaving light
        int specularBounces;
	int diffuseBounces;
	
    private:
	long id;
	static long seq;
	double indice_of_refraction; 
	Vector inv_direction; 
	Vector origin; ///< The rays origin
	Vector direction; ///< Unit vector of rays direction
};

#endif
