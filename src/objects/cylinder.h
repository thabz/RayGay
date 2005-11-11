#ifndef CYLINDER_H
#define CYLINDER_H

#include "math/matrix.h"
#include "math/vector.h"
#include "object.h"
#include "aabox.h"
#include "solid.h"
#include "transformer.h"

class Vector;
class Intersection;
class Ray;
class Vector2;

/** 
 * A cylinder object.
 *
 * In object space a cylinder begins at (0,0,0) ends at (0,0,1) and
 * have a radius of 1.
 * 
 * Rays are automatically transformed from world space to object space
 * whenever needed.
 */
class Cylinder : public Solid, public Transformer {

    public:
	/// Constructor
    	Cylinder(const Vector& begin, const Vector& end, double radius, bool has_caps, const Material* m);
	~Cylinder() {};
	void transform(const Matrix& m);

	AABox getBoundingBox() const;

	SceneObject* clone() const;
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

    private:
	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;
	uint32_t allPositiveRoots(const Ray& world_ray, double roots[4]) const;
	Vector getNormal(const Vector& local_point) const;

	double radius;
	double height;
	bool has_caps;
};

#endif
