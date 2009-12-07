
#ifndef OBJECTS_HALFSPACE_H
#define OBJECTS_HALFSPACE_H

#include "objects/object.h"
#include "objects/solid.h"

// TODO: Because of numeric imprecision or something in the BSP-code  
// TODO: we can only return boundingboxes that are the whole world.
//#define HALFSPACE_OPTIMIZED_AABOX

class Intersection;
class Ray;
class Matrix;
class Vector2;

class Halfspace : public Solid {

    public:
	/// Construct a halfspace from a normal and distance from origin along normal.
	/// Thus origin is outside the halfspace if d < 0.
	Halfspace(const Vector& n, double d, const Material* mat);
	/// Construct a halfspace from a normal and a point on the surface
	Halfspace(const Vector& n, const Vector& p, const Material* mat);
	/// Construct a halfspace from any three non-co-linear points on the surface 
	Halfspace(const Vector& a, const Vector& b, const Vector& c, const Material* material);
	void transform(const Matrix& m);
	AABox getBoundingBox() const;
	SceneObject* clone() const;
	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray&, double, Intersection&) const;
	int intersects(const AABox& voxel_bbox, const AABox& obj_bbox) const;
	uint32_t allIntersections(const Ray& ray, Intersection* result) const;
	uint32_t maxIntersections() const;
        bool inside(const Vector& p) const;
	bool canSelfshadow() const;

    private:
	void init(const Vector& n, const Vector& p);

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

