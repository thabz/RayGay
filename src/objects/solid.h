
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
	/// Find all intersections with ray
	virtual void allIntersections(const Ray& ray, vector<Intersection>& result) const = 0;
    protected:
	/// Protected constructor
	Solid(const Material* mat) : Object(mat) {};

};

#endif
