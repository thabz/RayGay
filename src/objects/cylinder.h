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

/** 
 * A cylinder object
 * 
 * \todo Implement caps
 * \todo Let Transformer class handle ray-transformations
 */
class Cylinder : public BooleanOperand {

    public:
	/// Constructor
    	Cylinder(const Vector& begin, const Vector& end, double radius, const Material* m);
	virtual ~Cylinder() {};
	virtual void transform(const Matrix& m);
	virtual Vector normal(const Intersection & i) const;

	virtual bool onEdge(const Vector &p) const;
	virtual bool inside(const Vector &p) const;

	virtual BoundingBox boundingBoundingBox() const;

	virtual SceneObject* clone() const;

    private:
	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
	Intersection _intersect(const Ray& ray) const;
	void prepareMatrices();

	Vector begin;
	Vector end;

	Matrix transformation;
	Matrix inverse_transformation;
	Matrix rotation; /// The rotation part extracted from the transformation
	Matrix inverse_rotation;
	Matrix scene_transformation;

	double r; /// Radius
	double rr; /// Squared radius
	double height; /// Height of cylinder
};

#endif
