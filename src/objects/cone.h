#ifndef CONE_H 
#define CONE_H

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
 * A cone object
 */
class Cone : public Solid, public Transformer {

    public:
	/// Constructor
    	Cone(const Vector& begin, const Vector& end, double radius_begin, double radius_end, bool has_caps, const Material* m);
	~Cone() {};
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

	double radius_begin; /// Radius at bottom of cone
	double radius_end; /// Radius at top of cone
	bool has_caps;
};

#endif
