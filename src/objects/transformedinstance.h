
#ifndef OBJECTS_TRANSFORMED_INSTANCE_H
#define OBJECTS_TRANSFORMED_INSTANCE_H

#include "objects/transformer.h"

/**
 * A transformed instance of an object.
 *
 * This allows the same object to be duplicated 
 * around the scene in different positions, with
 * only one instance being kept in memory.
 */
class TransformedInstance : public Transformer {

    public:
	TransformedInstance(Object* object);
	TransformedInstance(Object* object, Material* material);
	SceneObject* clone() const;

    private:
	BoundingBox localBoundingBoundingBox() const;
	virtual Intersection localIntersect(const Ray& ray) const;

	Object* object;
};

#endif

