#ifndef SPHERE_H
#define SPHERE_H

#include <vector>
#include <iosfwd>

#include "object.h"
#include "vector.h"
#include "rgb.h"
#include "material.h"
#include "boundingbox.h"

class Intersection;
class Ray;
class Matrix;

/// A sphere object
class Sphere : public object {

    friend std::ostream & operator<< (std::ostream &os, const Sphere &s);
    
    public:
        /// Constructor
	Sphere(Vector c, double r, Material material);

	/// Destructor
	virtual ~Sphere();

        /// Returns center of sphere
	const Vector& getCenter() const; 

	virtual void transform(const Matrix& m);
	virtual Vector normal(const Intersection & i) const;
	virtual RGB getDiffuseColor(const Vector& p) const;
	virtual Material getMaterial() const;

	virtual bool onEdge(const Vector &p) const;
	virtual bool inside(const Vector &p) const;

	virtual bool intersects(const BoundingBox& b) const;
	virtual BoundingBox boundingBoundingBox() const;

	virtual void getUV(const Intersection& intersection, double* u, double* v) const;

        /// Internal test
	static void test();

    private:
	Vector center;
	double radius;
	Material material;
	BoundingBox _boundingBoundingBox;
	virtual Intersection _intersect(const Ray& ray) const;

};

#endif

