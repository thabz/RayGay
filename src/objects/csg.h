
#ifndef OBJECTS_CSG_H
#define OBJECTS_CSG_H

#include "objects/solid.h"

class CSG : public Solid {

    public:
	enum CSGOperation {
	    UNION,       ///< Points that are in either object
	    DIFFERENCE,  ///< All points in lhs unless they're in rhs too
	    INTERSECTION ///< Points that are in common
	};

	CSG(Solid* left, CSGOperation op, Solid* right, const Material* mat); 
	bool inside(const Vector& point) const;
	vector<Intersection> allIntersections(const Ray& ray) const;

	void transform(const Matrix& m);
	BoundingBox boundingBoundingBox() const;
	SceneObject* clone() const;

    private:
	CSGOperation op;
	Solid* left;
	Solid* right;

	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
};

#endif