#ifndef OBJECTS_TRANSFORMER_H
#define OBJECTS_TRANSFORMER_H

#include "objects/object.h"
#include "math/matrix.h"

/**
 * Superclass for objects that handles world->local coordinate transformations.
 *
 * For many objects, such as cylinder and cone, it is much easier doing 
 * ray-object intersection if the object is placed along eg. the z-axis
 * and has height 1. These objects can be wrapped in a Transformer superclass
 * so that scaled, translated and rotated instances can the placing in the 
 * scene.
 */
class Transformer : public Object {

    public:
	
	void transform(const Matrix& m);
	Vector normal(const Intersection& i) const;
	Vector2 getUV(const Intersection& intersection) const;
	BoundingBox boundingBoundingBox() const;


    protected:
        Transformer(const Material* material);

    private:
	Intersection _intersect(const Ray& ray) const;

	virtual Intersection localIntersect(const Ray& ray) const = 0;
	virtual Vector localNormal(const Intersection& i) const = 0;
	virtual Vector2 localGetUV(const Intersection& intersection) const = 0;
	virtual BoundingBox localBoundingBoundingBox() const = 0;

	Matrix transformation;
	Matrix inverse_transformation;
	Matrix rotation; /// The rotation part extracted from the transformation
	Matrix inverse_rotation;
	Matrix scene_transformation;
};

#endif
