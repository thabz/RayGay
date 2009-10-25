
#ifndef OBJECTS_HALFSPACE_H
#define OBJECTS_HALFSPACE_H

#include "objects/object.h"
#include "objects/solid.h"

class Intersection;
class Ray;
class Matrix;
class Vector2;

class Halfspace : public Solid {

    public:
	/// Construct a halfspace from a normal and distance from origin
	Halfspace(const Vector& normal, double d, const Material* material);

	/// Construct a halfspace from any tree point on the surface which are not co-linear
	Halfspace(const Vector& a, const Vector& b, const Vector& c, const Material* material);
	void transform(const Matrix& m);
	AABox getBoundingBox() const;
	SceneObject* clone() const;
	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray&, double, Intersection&) const;
	int intersects(const AABox& voxel_bbox, const AABox& obj_bbox) const;
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;
        bool inside(const Vector& p) const;
	bool canSelfshadow() const;

    private:
	// We describe the plane in so-called Hessian normal form, which is
	// that all  points p on the surface satisfy the following:
	// n \dot p + d = 0 
	// The surface is then completely describe by a normal and distance
	// from origin.

	// Distance from origin
	double d;
	// Normal vector
	Vector normal;
};

#endif

