
#ifndef OBJECT_H
#define OBJECT_H

#include <string>

#include "intersection.h"
#include "sceneobject.h"

class Matrix;
class Ray;
class Vector;
class Vector2;
class BoundingBox;
class Material;
class KdTree;

/// The abstract superclass of all objects in the scene that can be rendered.

class Object : public SceneObject {
    public:
        /// Returns a full intersection at t distance from ray's origin
	virtual Intersection fullIntersect(const Ray& ray, const double t) const;
	double fastIntersect(const Ray& ray) const;
	
	/// Transform this object
	virtual void transform(const Matrix& m) = 0;

	/// Returns the material of this object
	virtual const Material* getMaterial() const { return material; }; 

	/// The smallest box containing this object
	virtual BoundingBox boundingBoundingBox() const = 0;

        virtual int intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const;
	
	/// Prepares the object before rendering
	virtual void prepare();

	/// Add this object to the kd-tree
	void addSelf(KdTree* space);

	/// Returns the surface area of object
	virtual double area() const;

    protected:
	/// Constructor
	Object(const Material* material);

	/// Internal intersect method that subclasses must implement
	virtual Intersection _fullIntersect(const Ray& ray, const double t) const = 0;
	/// Internal fast intersect method that subclasses must implement
	virtual double _fastIntersect(const Ray& ray) const = 0;

    private:	
	/// The material of this object
	const Material* material;
};

inline
Intersection Object::fullIntersect(const Ray& ray, const double t) const {
    Intersection result = _fullIntersect(ray,t);
    result.setObject(const_cast<Object*>(this));
    return result;
}

/**
 *  Finds the smallest distance along a ray where this object is intersected by the ray.
 *
 *  @param ray the Ray to intersect with
 *  @return positive distance along ray if and only if an intersection occured; otherwise -1 is returned.
 *
 * I used to do mailboxing here, but with the new Kd-Tree traversal 
 * algorithm mailboxing didn't add anything.
 */
inline
double Object::fastIntersect(const Ray& ray) const {
    return _fastIntersect(ray);
}

#endif
