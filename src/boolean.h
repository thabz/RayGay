#ifndef BOOLEAN_H
#define BOOLEAN_H

#include "booleanoperand.h"
#include "math/vector.h"

class Intersection;
class Ray;
class Matrix;


/// Boolean solid
class Boolean : public BooleanOperand {

    public:
	/// The boolean operations
	enum BooleanOp {
	    BOOLEAN_UNION,       ///< Points that are in either object
	    BOOLEAN_DIFFERENCE,  ///< All points in lhs unless they're in rhs too
	    BOOLEAN_INTERSECTION ///< Points that are in common
	};

	/// Exception class
	class unknownOp {
	    public:
		/// Constructor
		unknownOp(BooleanOp op) { this->op = op; };
		/// The operation that is unknown
		BooleanOp op; 
	};

	/// Constructor
	Boolean(BooleanOperand* lhs, BooleanOp op, BooleanOperand* rhs, Material material);
	virtual ~Boolean() {};

	virtual void transform(const Matrix& m);
	virtual Vector normal(const Intersection& i) const;
	virtual const Material& getMaterial() const;
	virtual bool intersects(const BoundingBox&) const;
	virtual BoundingBox boundingBoundingBox() const;

	virtual bool onEdge(const Vector &p) const;
	virtual bool inside(const Vector &p) const;

	virtual Vector2 getUV(const Intersection& intersection) const;

    private:
	BooleanOperand* _lhs;
	BooleanOperand* _rhs;
	BooleanOp _op;
	Material _material;
	virtual Intersection _intersect(const Ray& ray) const;
};

#endif
