
#ifndef OBJECTS_SOLID_H
#define OBJECTS_SOLID_H

#include "objects/object.h"
#include <vector>

/**
 * Objects that can be used as part of CSG objects.
 */
class Solid : public Object {

    public:
	/// Constructor
	Solid(const Material* mat) : Object(mat) {};
	/// Find all intersections with ray
	virtual vector<Intersection> allIntersections(const Ray& ray) const = 0;
};

#endif
