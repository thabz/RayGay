
#ifndef OBJECTS_SOLID_H
#define OBJECTS_SOLID_H

#include "objects/object.h"
#include <vector>

class Solid : public Object {

    public:
	Solid(const Material* mat) : Object(mat) {};
	virtual vector<Intersection> allIntersections(const Ray& ray) const = 0;
};

#endif
