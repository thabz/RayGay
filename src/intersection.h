#ifndef INTERSECTION_H 
#define INTERSECTION_H

#include <iosfwd>

#include "math/vector.h"
#include "ray.h"

class object;
class Triangle;

using namespace std;

/// An intersection is returned from the various intersect(Ray) methods.
class Intersection {
    friend ostream & operator<< (ostream &os, const Intersection &i);

    public:
	Intersection();
	Intersection(const Vector& point, double t);

	void setObject(object* obj) { o = obj; };
	object* getObject() const { return o; };

	Vector point; 	///< The intersection point
	Vector local_point; ///< The intersection point in the objects own coordinate system. Optional.
	object* local_object; ///< A local subobject, eg. a component of a Boolean.

	const Triangle* local_triangle; ///< A local subobject, eg. a component of a Mesh.
	double t;
	bool intersected;

	double u,v; ///< Texel coordinates (mostly unused)

    private:
	object* o; /// The object that was intersected
};

inline
Intersection::Intersection() {
    intersected = false;
}

#endif


