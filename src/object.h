
#ifndef OBJECT_H
#define OBJECT_H

#include "material.h"
#include "intersection.h"

class Matrix;
class Ray;
class Vector;
class RGB;
class BoundingBox;

/// The abstract superclass of all objects in the scene that can be rendered.

class object {
    public:
        /// Return the nearest intersection to ray's origin
	virtual Intersection intersect(const Ray& ray);

	/// Returns the normalvector at a point on this objects surface
	virtual Vector normal(const Intersection &i) = 0;
	
	/// Returns the diffuse color at a point of surface of this object
        virtual RGB getDiffuseColor(const Vector& point) = 0;

	/// Transform this object
	virtual void transform(const Matrix& m) = 0;

	/// Returns the materiale of this object
	virtual Material getMaterial() = 0;

	/// Says whether this object are contained or partly contained in the BoundingBox
	virtual bool intersects(const BoundingBox&) = 0;

	/// The smallest box containing this object
	virtual BoundingBox boundingBoundingBox() = 0;

	/// Says whether the point belongs to the closure (surface)
	virtual bool onEdge(const Vector &p) = 0;

	/// Says whether the point belongs to the inside (and not the closure) of the object
	virtual bool inside(const Vector &p) = 0;

	/// Get texture coordinates at an intersection
	virtual void getUV(const Intersection& intersection, double* u, double* v) = 0;

    protected:
	object();
	virtual Intersection _intersect(const Ray& ray) = 0;

    private:	
	long last_ray;
	Intersection last_intersection;
};

#endif
