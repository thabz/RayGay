#ifndef CYLINDER_H
#define CYLINDER_H

#include "math/matrix.h"
#include "math/vector.h"
#include "object.h"
#include "boundingbox.h"
#include "booleanoperand.h"

class Vector;
class Intersection;
class Ray;
class Vector2;

/// A cylinder object
class Cylinder : public object {

    public:
	/// Constructor
    	Cylinder(const Vector& begin, const Vector& end, double radius, Material m);
	virtual ~Cylinder() {};
	virtual void transform(const Matrix& m);
	virtual Vector normal(const Intersection & i) const;
	virtual const Material& getMaterial() const;

	virtual bool onEdge(const Vector &p) const;
	virtual bool inside(const Vector &p) const;

	virtual bool intersects(const BoundingBox& b) const;
	virtual BoundingBox boundingBoundingBox() const;

	virtual Vector2 getUV(const Intersection& intersection) const;

    private:
	virtual Intersection _intersect(const Ray& ray) const;
	void prepareMatrices();

	Material material;

	Vector begin;
	Vector end;

	Matrix transformation;
	Matrix inverse_transformation;
	Matrix rotation; /// The rotation part extracted from the transformation
	Matrix inverse_rotation;

	double r; /// Radius
	double rr; /// Squared radius
	double height; /// Height of cylinder
};

#endif
