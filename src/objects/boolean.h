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
	Boolean(BooleanOperand* lhs, BooleanOp op, BooleanOperand* rhs, const Material* material);
	virtual ~Boolean() {};

	virtual void transform(const Matrix& m);
	virtual BoundingBox boundingBoundingBox() const;

	virtual bool onEdge(const Vector &p) const;
	virtual bool inside(const Vector &p) const;

	virtual SceneObject* clone() const;

    private:
	BooleanOperand* _lhs;
	BooleanOperand* _rhs;
	BooleanOp _op;
	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
	Intersection _intersect(const Ray& ray) const;
};

#endif
