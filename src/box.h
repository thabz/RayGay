#ifndef BOX_H
#define BOX_H

#include "vector.h"

class Intersection;
class Ray;
class Matrix;

/// An axis-aligned bounding box.

class Box {

    public:
	Box();
	Box(const Vector corner1, const Vector corner2);
	~Box();

	virtual Intersection intersect(const Ray& ray) const;
	virtual bool checkIntersect(const Ray& ray) const;

	/// The box' normal at a point. This vector is always axis-aligned.
	virtual Vector normal(const Vector& p) const;

	/// Test whether a point belongs to the closure of this box.
	virtual bool onEdge(const Vector &p) const;

	/// Tests whether a point is inside this box and not on the edge.
	virtual bool inside(const Vector &p) const;

	/// The corner with smallest x,y,z values
	const Vector& minimum() const { return _c1; };
	
	/// The corner with biggest x,y,z values
	const Vector& maximum() const { return _c2; };

	/// Returns the smallest box that contains b1 and b2.
	static Box doUnion(const Box& b1, const Box& b2); 

	/// Returns the smallest box that b1 and b2 have in common.
	static Box doIntersection(const Box& b1, const Box& b2);

	/// Returns a length 8 array of the corners
	Vector* corners() const; 

	/// Returns the area of this box' surface
        double area() const;
	
	/// Comparator
        bool operator==(const Box &b) const;

	static void test(); ///< Internal test

    private:
	Vector _c1; ///< The point with smallest x,y,z values
	Vector _c2; ///< The point with biggest x,y,z values
};

#endif


