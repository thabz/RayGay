#ifndef OBJECTS_TRANSFORMER_H
#define OBJECTS_TRANSFORMER_H

#include "math/matrix.h"
#include "boundingbox.h"
#include "ray.h"
#include "intersection.h"


/**
 * This is the superclass for objects that can't be transformed by themselves.
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
	/// Apply a transformation
	virtual void transform(const Matrix& m);

    protected:
	Vector pointToObject(const Vector& p) const;
	Vector dirToObject(const Vector& d) const;
	Vector pointToWorld(const Vector &p) const;
	Vector normalToWorld(const Vector& d) const;
	Ray rayToObject(const Ray& ray) const;
	Intersection intersectionToWorld(const Intersection& i) const;
	BoundingBox bboxToWorld(const BoundingBox& bbox) const;

    private:
	Matrix transformation;
	Matrix inverse_transformation;
	Matrix rotation; /// The rotation part extracted from the transformation
	Matrix inverse_rotation;
	Matrix normal_transformation;
};

inline
Ray Transformer::rayToObject(const Ray& ray) const {
    Vector o = inverse_transformation * ray.getOrigin();
    Vector d = inverse_rotation * ray.getDirection();
    double l = d.length();
    d = d / l;
    double ior = ray.getIndiceOfRefraction();
    Ray result = Ray(o,d,ior);
    result.t_scale = l;
    return result;
}

inline
Vector Transformer::pointToWorld(const Vector &p) const {
    return transformation * p;
}

inline
Vector Transformer::normalToWorld(const Vector& d) const {
    Vector result = normal_transformation * d;
    result.normalize();
    return result;
}

inline
Intersection Transformer::intersectionToWorld(const Intersection& i) const {
    Intersection result = Intersection(i);
    result.setNormal(normalToWorld(i.getNormal()));
    result.setPoint(pointToWorld(i.getPoint()));
    return result;
}

#endif
