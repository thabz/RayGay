
#ifndef OBJECTS_HALFSPACE_H
#define OBJECTS_HALFSPACE_H

#include "objects/object.h"
#include "objects/solid.h"

class Intersection;
class Ray;
class Matrix;
class Vector2;

// TODO: Implement this CSG object
class Halfspace : public Solid {

    public:
	/// Construct a halfspace from any point of the surface and a normal
	Halfspace(const Vector& point, const Vector& normal, const Material* material);

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

    private:
	Vector point;
	Vector normal;
};

#endif

