#ifndef INTERSECTION_H 
#define INTERSECTION_H

#include <iosfwd>

#include "math/vector.h"
#include "math/vector2.h"
#include "ray.h"

class Object;
class Triangle;

using namespace std;

/// An intersection is returned from the various intersect(Ray) methods.
class Intersection {
    friend ostream & operator<< (ostream &os, const Intersection &i);

    public:
        /**
	 * Default constructor.
	 */
	Intersection();
	
	/**
	 * Constructor.
	 *
	 * @param p Point of intersection
	 * @param t Distance along ray
	 * @param n Surface normal at intersection
	 * @param uv Texture UV-coordinates at point of intersection
	 */
	Intersection(const Vector& p, double t, const Vector& n, const Vector2& uvs);

	void setObject(Object* obj) { o = obj; };
        /// The object that was intersected
	Object* getObject() const { return o; };

	void setPoint(const Vector& point) { this->point = point; };
 	/// The intersection point
	const Vector& getPoint() const { return point; };

        /// Surface normal at intersection point
	const Vector& getNormal() const { return normal; };

	const Vector2& getUV() const { return uv; };

	double getT() const { return t; }; 
	void setT(double new_t) { t = new_t; };
	
	bool isIntersected() const { return t >= 0.0; };

    private:
	Object* o; 
	Vector point;
	Vector normal;
	Vector2 uv;
	double t;
};

inline
Intersection::Intersection() {
    t = -1.0;
}

inline
Intersection::Intersection(const Vector& p, double s, const Vector& n, const Vector2& uvs) {
    this->point = p;
    this->t = s;
    this->normal = n;
    this->uv = uvs;
}

#endif


