#ifndef INTERSECTION_H 
#define INTERSECTION_H

#include <iosfwd>

#include "vector.h"
#include "ray.h"

class object;

using namespace std;

/// An intersection is returned from the various intersect(Ray) methods.
class Intersection {
    friend ostream & operator<< (ostream &os, const Intersection &i);

    public:
	Intersection();
	Intersection(Vector point, double t);
	~Intersection();	

	void setObject(object* obj) { o = obj; };
	object* getObject() { return o; };

	Vector point;
	Vector local_point; ///< The Intersection point in the objects own coordinate system. Optional.
	object* local_object; ///< A local subobject, eg. a triangle in a mesh or a component of a Boolean.
	double t;
	bool intersected;

    private:
	object* o; /// The object that was intersected
};

#endif


