
#ifndef OBJECTS_SOLID_H
#define OBJECTS_SOLID_H

#include "objects/object.h"
#include <vector>

/**
 * Objects that can be used as part of CSG objects must
 * implement this interface.
 */
class Solid : public Object {

    public:

	/**
	 * Find all intersections with ray.
	 *
	 * The intersections are added to the result vector. The intersections
	 * are sorted by distance along ray with nearest first.
	 *
	 * @param ray the ray to intersect with.
	 * @param result the intersections are added here.
	 */
	virtual void allIntersections(const Ray& ray, vector<Intersection>& result) const = 0;

	/**
	 * Returns largest AABox inscribed in this object.
	 */
	virtual AABox getContainedBox() const;

        /**
         * Says whether a point is inside the object
         */
        virtual bool inside(const Vector& p) const = 0;

    protected:
	/// Protected constructor
	Solid(const Material* mat) : Object(mat) {};
};

#endif
