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
 * 
 * \todo Implement caps
 */
class Cylinder : public Solid, public Transformer {

    public:
	/// Constructor
    	Cylinder(const Vector& begin, const Vector& end, double radius, const Material* m);
	~Cylinder() {};
	void transform(const Matrix& m);

	bool onEdge(const Vector &p) const;
	bool inside(const Vector &p) const;

	BoundingBox boundingBoundingBox() const;

	SceneObject* clone() const;
	vector<Intersection> allIntersections(const Ray& ray) const;

    private:
	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
	Intersection _intersect(const Ray& ray) const;
	void prepareMatrices();

	Vector begin;
	Vector end;

	double r; /// Radius
	double rr; /// Squared radius
	double height; /// Height of cylinder
};

#endif
