
#ifndef OBJECTS_TRANSFORMED_INSTANCE_H
#define OBJECTS_TRANSFORMED_INSTANCE_H

#include "objects/object.h"
#include "math/matrix.h"

/**
 * A transformed instance of an object.
 *
 * This allows the same object to be duplicated 
 * around the scene in different positions, with
 * only one instance being kept in memory.
 */
class TransformedInstance : public Object {

    public:
	TransformedInstance(Object* object);
	TransformedInstance(Object* object, Material* material);

	Vector normal(const Intersection &i) const;
	
	void transform(const Matrix& m);
	
	BoundingBox boundingBoundingBox() const;

	Vector2 getUV(const Intersection& i) const;

	SceneObject* clone() const;

    private:
	virtual Intersection _intersect(const Ray& ray) const;
	void prepareMatrices();

	Object* object;
	Matrix transformation;
	Matrix inverse_transformation;
	Matrix rotation; /// The rotation part extracted from the transformation
	Matrix inverse_rotation;
	Matrix scene_transformation;
};

#endif

