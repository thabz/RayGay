
#ifndef OBJECT_H
#define OBJECT_H

#include "material.h"
#include "intersection.h"

class Matrix;
class Ray;
class Vector;
class RGB;
class Box;

/// The superclass of all objects in the scene that can be rendered.

class object {
    public:
        /// Return the nearest intersection to ray's origin
	virtual Intersection intersect(const Ray& ray);

	virtual Vector normal(const Intersection &i) = 0;
        virtual RGB getDiffuseColor(const Vector& point) = 0;
	virtual void transform(const Matrix& m) = 0;
	virtual Material getMaterial() = 0;
	virtual bool intersects(const Box&) = 0;
	virtual Box boundingBox() = 0;

	virtual bool onEdge(const Vector &p) = 0;
	virtual bool inside(const Vector &p) = 0;

    protected:
	object();
	virtual Intersection _intersect(const Ray& ray) = 0;
	long last_ray;
	Intersection last_intersection;
};

#endif
