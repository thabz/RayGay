
#ifndef OBJECTS_ISOSURFACE_H
#define OBJECTS_ISOSURFACE_H

#include "objects/object.h"

/**
 * An isosurface is points described by a function.
 *
 * An isosurface is any collection of points satisfying
 * \f[ f(x,y,z) - K = 0 \f]
 * for any scalar field function \f$ f(x,y,z) \f$ and any value \f$ K \f$.
 *
 * As an example can a sphere be defined by the function 
 * \f[ f(x,y,z) = x^2 + y^2 + z^2 \f]
 * and a \f$ K \f$  which acts as the squared radius.
 */ 
class IsoSurface : public Object {

    public:
	virtual Vector normal(const Intersection & i) const;
	bool intersects(const BoundingBox& b) const;
	virtual Vector2 getUV(const Intersection& intersection) const;

    protected:
	/// Constructor
	IsoSurface(unsigned int steps, double accuracy, double iso);
	/// Evaluate the scalar field function defining this surface
	virtual double evaluateFunction(const Vector& point) const = 0;

	Intersection _intersect(const Ray& ray) const;

    private:
	bool inside(const Vector& v) const;
	double recurse(const Ray& ray, double t_begin, double t_end) const;

	unsigned int steps;
	double accuracy;
	double iso;

};

#endif

