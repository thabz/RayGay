
#ifndef OBJECT_H
#define OBJECT_H

#include <string>

#include "materials/material.h"
#include "intersection.h"

class Matrix;
class Ray;
class Vector;
class Vector2;
class BoundingBox;
class Material;

/// The abstract superclass of all objects in the scene that can be rendered.

class object {
    public:
        /// Return the nearest intersection to ray's origin
	virtual bool intersect(const Ray& ray) const;

	/// Returns the normalvector at a point on this objects surface
	virtual Vector normal(const Intersection &i) const = 0;
	
	/// Transform this object
	virtual void transform(const Matrix& m) = 0;

	/// Returns the materiale of this object
	virtual const Material& getMaterial() const = 0;

	/// Says whether this object is contained or partly contained in the BoundingBox
	virtual bool intersects(const BoundingBox&) const = 0;

	/// The smallest box containing this object
	virtual BoundingBox boundingBoundingBox() const = 0;
	
	/// Get texture coordinates at an intersection
	virtual Vector2 getUV(const Intersection& intersection) const = 0;

	/// Prepares the object before rendering
	virtual void prepare();

	/// Returns last successful intersection
	Intersection* getLastIntersection() const { return &last_intersection; }; 

    protected:
	object();
	/// Internal intersect method that subclasses must implement
	virtual Intersection _intersect(const Ray& ray) const = 0;

    private:	
	// Two members for caching last intersection
	mutable long last_ray;
	mutable Intersection last_intersection;
};

/**
 *  A caching proxy around the private _intersect(Ray) method in subclasses.
 */

/*  Because an object can exist in several bounding boxes we end up shooting
 *  the same ray at the same object several times.
 */

inline
bool object::intersect(const Ray& ray) const {
    if (ray.getId() != last_ray) {
	last_intersection = _intersect(ray);
	last_intersection.setObject(const_cast<object*>(this));
	last_ray = ray.getId();
    }
    return last_intersection.isIntersected();
}


#endif
