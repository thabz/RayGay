
#ifndef RAY_H
#define RAY_H

#include "vector.h"

/// Describing a ray, that is an origin and a direction Vector.
class Ray {
    public:
	Ray();
	/// Constructor
	Ray(Vector o, Vector direction, double indice);
	/// Destructor
	~Ray();
	Vector origin; ///< The rays origin
	Vector direction; ///< Unit vector of rays direction

        /// Rays direction (x,y,z) with inverted components ((1/x),(1/y),(1/x))
	Vector inv_direction; 

        /// The material the ray is travelling in where 1.0 is vacuum.
	double indice_of_refraction; 

	/// Accessor for the unique id of this ray, that is automatically assigned.
	long getId() const { return id; };

    private:
	long id;
	static long seq;
};

#endif
