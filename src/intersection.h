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
	object* local_object; ///< A local subobject, eg. a component of a Boolean.

	const Triangle* local_triangle; ///< A local subobject, eg. a component of a Mesh.
	double t;
	bool isIntersected() const { return t >= 0.0; };

    private:
	object* o; /// The object that was intersected
};

inline
Intersection::Intersection() {
    t = -1.0;
}

inline
Intersection::Intersection(const Vector& p, double s) {
    point = p;
    t = s;
}

#endif


