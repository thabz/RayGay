
#ifndef OBJECTS_TRANSFORMED_INSTANCE_H
#define OBJECTS_TRANSFORMED_INSTANCE_H

#include "transformer.h"
#include "objects/object.h"

/**
 * A transformed instance of an object.
 *
 * This allows the same object to be duplicated 
 * around the scene in different positions, with
 * only one instance being kept in memory.
 */
class TransformedInstance : public Object, public Transformer {

    public:
	TransformedInstance(Object* object);
	TransformedInstance(Object* object, Material* material);

	BoundingBox boundingBoundingBox() const;
	SceneObject* clone() const;
	void transform(const Matrix& m);


    private:
	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;

	Object* object;
};

#endif

