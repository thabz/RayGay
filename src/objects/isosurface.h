
#ifndef OBJECTS_ISOSURFACE_H
#define OBJECTS_ISOSURFACE_H

#include "objects/object.h"
#include "transformer.h"

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
class IsoSurface : public Object, public Transformer {

    public:
	bool intersects(const AABox& b) const;
	void transform(const Matrix& m);
	/// Return the bounding box in world space coordinates
	AABox getBoundingBox() const;

	bool inside(const Vector& p) const;

	double getAccuracy() { return accuracy; };

    protected:
	/// Constructor
	IsoSurface(uint32_t steps, double accuracy, double iso, Material* mat);
	/// Evaluate the scalar field function defining this surface
	virtual double evaluateFunction(const Vector& point) const = 0;

	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;

	/// Returns the bounding box in object space coordinates
	virtual AABox _getBoundingBox() const = 0;


	double accuracy;

    private:
	virtual Vector normal(const Vector& p) const;
	double refine(const Ray& ray, double t_begin, double t_end, double f_t_end) const;
	uint32_t steps;
	double iso;

};

inline
bool IsoSurface::inside(const Vector& p) const {
    return evaluateFunction(p) - iso < 0;
}

#endif

