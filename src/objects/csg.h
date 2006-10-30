
#ifndef OBJECTS_CSG_H
#define OBJECTS_CSG_H

#include "objects/solid.h"

/**
 * Constructive Solid Geometry Union.
 * This implements the boolean union operation on 3D solids.
 */
class CSGUnion : public Solid {

    public:

	/// Constructor using two solids
	CSGUnion(Solid* left, Solid* right, const Material* mat); 

	/// Constructor taking more than two solids
	CSGUnion(vector<Solid*>* solids, const Material* mat); 

	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

	void transform(const Matrix& m);
	AABox getBoundingBox() const;
	SceneObject* clone() const;
	
        bool inside(const Vector& p) const;

    private:
	Solid* left;
	Solid* right;

	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;
};

/**
 * Constructive Solid Geometry Difference.
 * This implements the boolean difference operation on 3D solids.
 */
class CSGDifference : public Solid {

    public:

	/// Constructor
	CSGDifference(Solid* left, Solid* right, const Material* mat); 
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

	void transform(const Matrix& m);
	AABox getBoundingBox() const;
	SceneObject* clone() const;

        bool inside(const Vector& p) const;

    private:
	Solid* left;
	Solid* right;

	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;
};

/**
 * Constructive Solid Geometry Intersection.
 * This implements the boolean intersection operation on 3D solids.
 */
class CSGIntersection : public Solid {

    public:

	/// Constructor
	CSGIntersection(Solid* left, Solid* right, const Material* mat); 
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

	void transform(const Matrix& m);
	AABox getBoundingBox() const;
	SceneObject* clone() const;

        bool inside(const Vector& p) const;

    private:
	Solid* left;
	Solid* right;

	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;
};

#endif
