
#ifndef RAY_H
#define RAY_H

#include "vector.h"

/// Describing a ray, that is an origin and a direction Vector.
class Ray {
    public:
	Ray();
	Ray(Vector o, Vector direction, double indice);
	~Ray();
	Vector origin; ///< The rays origin
	Vector direction; ///< Unit vector of rays direction
	Vector inv_direction; ///< Rays direction (x,y,z) with inverted components ((1/x),(1/y),(1/x))
	double indice_of_refraction; ///< The material the ray is travelling in. (1.0 = vacuum)
	long getId() const { return id; }; ///< This rays unique id.

    private:
	long id;
	static long seq;
};

#endif
