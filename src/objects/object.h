
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
class SpaceSubdivider;

/// The abstract superclass of all objects in the scene that can be rendered.

class Object : public SceneObject {
    public:
        /// Return the nearest intersection to ray's origin
	virtual Intersection fullIntersect(const Ray& ray, const double t) const;

	double fastIntersect(const Ray& ray) const;

	/// Returns the normalvector at a point on this objects surface
	virtual Vector normal(const Intersection &i) const = 0;
	
	/// Transform this object
	virtual void transform(const Matrix& m) = 0;

	/// Returns the materiale of this object
	virtual const Material* getMaterial() const { 
	    return material;
	    /*
	    if (last_intersection.isIntersected()) {
		return last_intersection.getObject()->material;
	    } else {
		return material;
	    }
	    */
	};

	/// The smallest box containing this object
	virtual BoundingBox boundingBoundingBox() const = 0;
	
	/// Get texture coordinates at an intersection
	virtual Vector2 getUV(const Intersection& intersection) const = 0;

	/// Prepares the object before rendering
	virtual void prepare();

	void addSelf(SpaceSubdivider* space);

    protected:
	Object(const Material* material);
	/// Internal intersect method that subclasses must implement
	virtual Intersection _fullIntersect(const Ray& ray, const double t) const;
	virtual Intersection _intersect(const Ray& ray) const = 0;
	virtual double _fastIntersect(const Ray& ray) const;

    private:	
	// Two members for caching last intersection
	mutable long last_ray;
	mutable double last_t;
	const Material* material;
};

/**
 *  A caching proxy around the private _intersect(Ray) method in subclasses.
 *  Because an object can exist in several bounding boxes we end up shooting
 *  the same ray at the same object several times.
 */
inline
Intersection Object::fullIntersect(const Ray& ray, const double t) const {
    Intersection result = _fullIntersect(ray,t);
    result.setObject(const_cast<Object*>(this));
    return result;
}

inline
Intersection Object::_fullIntersect(const Ray& ray, const double t) const {
    return _intersect(ray);
}

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
