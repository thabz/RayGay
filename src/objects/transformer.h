#ifndef OBJECTS_TRANSFORMER_H
#define OBJECTS_TRANSFORMER_H

#include "math/matrix.h"
#include "boundingbox.h"
#include "ray.h"


/**
 * This is the superclass for objects that can't be transformed. 
 *
 * This class holds a world to object transformation and its inverse.
 *
 * For many objects, such as cylinder and cone, it is much easier doing 
 * ray-object intersection if the object is placed along eg. the z-axis
 * and has height 1. These objects can be wrapped in a Transformer superclass
 * so that scaled, translated and rotated instances can the placing in the 
 * scene.
 */
class Transformer {

    public:
	virtual void transform(const Matrix& m);

    protected:
	Vector pointToObject(const Vector& p) const;
	Vector dirToObject(const Vector& d) const;
	Vector pointToWorld(const Vector &p) const;
	Vector dirToWorld(const Vector& d) const;
	Ray rayToObject(const Ray& ray) const;
	Intersection intersectionToWorld(const Intersection& i) const;
	BoundingBox bboxToWorld(const BoundingBox& bbox) const;

    private:
	Matrix transformation;
	Matrix inverse_transformation;
	Matrix rotation; /// The rotation part extracted from the transformation
	Matrix inverse_rotation;
	Matrix scene_transformation;
};

#endif
