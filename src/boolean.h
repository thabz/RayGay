#ifndef BOOLEAN_H
#define BOOLEAN_H

#include "object.h"
#include "vector.h"
#include "rgb.h"
#include "material.h"

class Intersection;
class Ray;
class Matrix;


/// Boolean solid
class Boolean : public object {

    public:
	/// The boolean operations
	enum BooleanOp {
	    BOOLEAN_UNION,       ///< Points that are in either object
	    BOOLEAN_DIFFERENCE,  ///< All points in lhs unless they're in rhs too
	    BOOLEAN_INTERSECTION ///< Points that are in common
	};
	Boolean(object* lhs, BooleanOp op, object* rhs, Material material);

	virtual void transform(const Matrix& m);
	virtual Vector normal(const Intersection& i) const;
	virtual RGB getDiffuseColor(const Vector& p) const;
	virtual Material getMaterial() const;
	virtual bool intersects(const BoundingBox&) const;
	virtual BoundingBox boundingBoundingBox() const;

	virtual bool onEdge(const Vector &p) const;
	virtual bool inside(const Vector &p) const;

	virtual void getUV(const Intersection& intersection, double* u, double* v) const;

	static void test();
    private:
	object* _lhs;
	object* _rhs;
	BooleanOp _op;
	Material _material;
	virtual Intersection _intersect(const Ray& ray) const;
};

#endif
