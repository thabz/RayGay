
#ifndef SPACESUBDIVIDER_H
#define SPACESUBDIVIDER_H

#include "object.h"

class object;

/**
 * All spacesubdividers must extend this abstract class.
 */
class SpaceSubdivider {

    public:
	virtual void addObject(object* obj) = 0; ///< Place a object in the hierarchy
	virtual bool intersect(const Ray& ray) const = 0; ///< Calculate an intersection with the hierarchy
	virtual bool intersectForShadow(const Ray& ray) const = 0; ///< Calculate an intersection with the hierarchy
	virtual bool intersectForShadow(const Ray& ray, const object* hint) const = 0; ///< Calculate an intersection with the hierarchy
	/// This gets called after all objects are added and before any intersection methods are called.
	virtual void prepare() = 0;

	virtual Intersection* getLastIntersection() const = 0;

    protected:
	SpaceSubdivider() {};
};

#endif

