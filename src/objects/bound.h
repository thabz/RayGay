
#ifndef OBJECTS_BOUND_H
#define OBJECTS_BOUND_H

#include "objects/object.h"

class ObjectGroup;
class KdTree;
class Intersection;

/**
 * An ObjectGroup inside its own KdTree.
 */
class Bound : public Object {
    
    public:
	/// Constructor
	Bound(ObjectGroup* group);

	Vector normal(const Intersection &i) const;

	void transform(const Matrix& m);

	BoundingBox boundingBoundingBox() const;

	Vector2 getUV(const Intersection& i) const;

	SceneObject* clone() const;

	void prepare();

    private:
	Intersection _intersect(const Ray& ray) const;

	KdTree* tree;
	ObjectGroup* group;
};

#endif
