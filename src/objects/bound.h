
#ifndef OBJECTS_BOUND_H
#define OBJECTS_BOUND_H

#include "space/kdtree.h"
#include "objects/object.h"
#include "intersection.h"

class ObjectGroup;

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

	//const Material* getMaterial() const;

	void prepare();

	bool intersect(const Ray& ray) const;

    private:
	Intersection _intersect(const Ray& ray) const;
	bool running;
	
	KdTree* tree;
	ObjectGroup* group;
};

#endif
