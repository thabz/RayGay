
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
	/// Id of the last ray that successfully intersected this object
	mutable long last_ray;
	/// The ray's t-value at that intersection
	mutable double last_t;
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
 *  This is basically a caching proxy around the private _fastIntersect(Ray)
 *  method in subclasses, where the real intersection tests are done.
 *
 *  Because an object can exist in several bounding boxes we end up shooting
 *  the same ray at the same object several times. Therefore each ray
 *  is assigned an unique id which allows us to reuse previous object-ray
 *  intersection results.
 *
 *  This technique is also called mailboxing.
 */
inline
double Object::fastIntersect(const Ray& ray) const {
    if (ray.getId() != last_ray) {
	last_t = _fastIntersect(ray);
	last_ray = ray.getId();
	return last_t;
    }
    return last_t;
}

#endif
