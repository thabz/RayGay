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

	Vector getPoint() const { return point; };

	void setLocalObject(const object* obj) { local_object = obj; };
	const object* getLocalObject() const { return local_object; };

	double getT() const { return t; }; 
	void setT(double new_t) { t = new_t; };

	
	bool isIntersected() const { return t >= 0.0; };
	double u,v;

    private:
	object* o; /// The object that was intersected
	const object* local_object; ///< A local subobject, eg. a component of a Boolean.
	Vector point; 	///< The intersection point
	double t;
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


