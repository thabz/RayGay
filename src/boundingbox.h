#ifndef BOUNDINGBOX_H
#define BOUNDINGBOX_H

#include "math/vector.h"
#include <iosfwd>

class Intersection;
class Ray;
class Matrix;
class Vector2;

/// An axis-aligned bounding box.

class BoundingBox {

    friend std::ostream & operator<< (std::ostream &os, const BoundingBox &b);

    public:
        /// Constructor
	BoundingBox();
	
        /// Constructor
	BoundingBox(const Vector corner1, const Vector corner2);

	/// Destructor
	virtual ~BoundingBox();

	/// Returns (tmin,tmax) of intersection
	virtual Vector2 intersect(const Ray& ray) const;

	/// Simple check for intersection
	virtual bool checkIntersect(const Ray& ray) const;

	/// The box' normal at a point. This vector is always axis-aligned.
	virtual Vector normal(const Vector& p) const;

	/// Test whether a point belongs to the closure of this box.
	virtual bool onEdge(const Vector &p) const;

	/// Tests whether a point is inside this box and not on the edge.
	virtual bool inside(const Vector &p) const;
	//
	/// Tests whether a point is inside this box or on the edge.
	virtual bool insideOrTouching(const Vector &p) const;
	
	/// Tests whether an array of points are inside this box and not on the edge.
	virtual bool inside(const Vector* points, int num) const;

	/// Tests whether another boundingbox is inside this
	virtual bool inside(const BoundingBox& b) const;

        /// Tests whether this bbox is in back, front or is intersected by a axis-aligned plane.
	int cutByPlane(int cutplane_dimension, double cutplane_value) const;
	
	/// The corner with smallest x,y,z values
	const Vector& minimum() const { return _c1; };
	
	/// The corner with biggest x,y,z values
	const Vector& maximum() const { return _c2; };

	/// Returns the smallest box that contains b1 and b2.
	static BoundingBox doUnion(const BoundingBox& b1, const BoundingBox& b2); 

	/// Returns the smallest box that b1 and b2 have in common.
	static BoundingBox doIntersection(const BoundingBox& b1, const BoundingBox& b2);

	/// Returns the smallest box that inclose the points
	static BoundingBox enclosure(Vector* points, int num);

	/// Returns a length 8 array of the corners
	Vector* getCorners() const; 

	/// Returns the area of this box' surface
        double area() const;

	/// Grow box 2*amount in all dimensions 
	void grow(double amount);
	
	/// Comparator
        bool operator==(const BoundingBox &b) const;

	/// Says whether this bounding box intersects a sphere
	bool intersectSphere(const Vector& center, double squared_radius) const;

	Vector center() const;

    private:
	Vector _c1; ///< The point with smallest x,y,z values
	Vector _c2; ///< The point with biggest x,y,z values
	mutable Vector* corners;
};

#endif


