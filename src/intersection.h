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

        /// Get surface normal at intersection point
	const Vector& getNormal() const { return normal; };
	
        /// Set surface normal at intersection point
	void setNormal(const Vector& normal) { this->normal = normal; };
	
        /// Get surface normal at intersection point
	const Vector& getOriginalNormal() const { return o_normal; };
	
        /// Set surface normal at intersection point
	void setOriginalNormal(const Vector& n) { this->o_normal = n; };

	/// Flip direction of normal
	void flipNormal() { normal *= -1.0; };

	const Vector2& getUV() const { return uv; };

	double getT() const { return t; }; 
	void setT(double new_t) { t = new_t; };
	
	bool isIntersected() const { return t >= 0.0; };

	void isEntering(bool entering) { this->entering = entering; };
	bool isEntering() const { return this->entering; };
    private:
	Vector point;
	Vector normal;
	Vector o_normal;
	Vector2 uv;
	double t;
	Object* o; 
	bool entering;
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


