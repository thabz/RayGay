#ifndef CYLINDER_H
#define CYLINDER_H

#include "math/matrix.h"
#include "math/vector.h"
#include "object.h"
#include "boundingbox.h"
#include "solid.h"
#include "transformer.h"

class Vector;
class Intersection;
class Ray;
class Vector2;

/** 
 * A cylinder object
 */
class Cylinder : public Solid, public Transformer {

    public:
	/// Constructor
    	Cylinder(const Vector& begin, const Vector& end, double radius, bool has_caps, const Material* m);
	~Cylinder() {};
	void transform(const Matrix& m);

	BoundingBox boundingBoundingBox() const;

	SceneObject* clone() const;
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

    private:
	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
	unsigned int allPositiveRoots(const Ray& world_ray, double roots[4]) const;
	Vector getNormal(const Vector& local_point) const;

	Vector begin;
	Vector end;

	double r; /// Radius
	double rr; /// Squared radius
	double height; /// Height of cylinder
	bool has_caps;
};

#endif
