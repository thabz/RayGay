
#ifndef OBJECTS_CSG_H
#define OBJECTS_CSG_H

#include "objects/solid.h"

/**
 * Constructive Solid Geometry.
 */
class CSGUnion : public Solid {

    public:

	/// Constructor
	CSGUnion(Solid* left, Solid* right, const Material* mat); 
	CSGUnion(vector<Solid*>* solids, const Material* mat); 
	bool inside(const Vector& point) const;
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

	void transform(const Matrix& m);
	BoundingBox boundingBoundingBox() const;
	SceneObject* clone() const;

    private:
	Solid* left;
	Solid* right;

	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
};

class CSGDifference : public Solid {

    public:

	/// Constructor
	CSGDifference(Solid* left, Solid* right, const Material* mat); 
	bool inside(const Vector& point) const;
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

	void transform(const Matrix& m);
	BoundingBox boundingBoundingBox() const;
	SceneObject* clone() const;

    private:
	Solid* left;
	Solid* right;

	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
};

class CSGIntersection : public Solid {

    public:

	/// Constructor
	CSGIntersection(Solid* left, Solid* right, const Material* mat); 
	bool inside(const Vector& point) const;
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

	void transform(const Matrix& m);
	BoundingBox boundingBoundingBox() const;
	SceneObject* clone() const;

    private:
	Solid* left;
	Solid* right;

	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
};

#endif
