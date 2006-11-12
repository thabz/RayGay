
#ifndef OBJECTS_BOUND_H
#define OBJECTS_BOUND_H

#include "objects/object.h"
#include "space/kdtree.h"
#include "intersection.h"

class ObjectGroup;

/**
 * An ObjectGroup inside its own KdTree.
 */
class Bound : public Object {
    
    public:
	/// Constructor
	Bound(ObjectGroup* group);

	void transform(const Matrix& m);

	AABox getBoundingBox() const;

	SceneObject* clone() const;

	void prepare();

	void fullIntersect(const Ray& ray, double t, Intersection& result) const;


    protected:
    	double _fastIntersect(const Ray& ray) const;

    	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;

    private:
	KdTree* tree;
	ObjectGroup* group;
};

#endif
