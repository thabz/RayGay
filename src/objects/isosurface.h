
#ifndef OBJECTS_ISOSURFACE_H
#define OBJECTS_ISOSURFACE_H

#include "objects/object.h"

/**
 * An isosurface is points described by a function. This is also called
 * an implicit surface.
 *
 * The surface uses a function from \f$ \Re^3 \f$ to \f$ \Re \f$ to classify 
 * points as inside, on or outside.
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
	bool intersects(const BoundingBox& b) const;

    protected:
	/// Constructor
	IsoSurface(unsigned int steps, double accuracy, double iso, Material* mat);
	/// Evaluate the scalar field function defining this surface
	virtual double evaluateFunction(const Vector& point) const = 0;

	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;

    private:
	virtual Vector normal(const Vector& p) const;
	bool inside(const Vector& v) const;
	double recurse(const Ray& ray, const double t_begin, const double t_end) const;
	unsigned int steps;
	double accuracy;
	double iso;

};

inline
bool IsoSurface::inside(const Vector& v) const {
    return evaluateFunction(v) >= iso;
}

#endif

