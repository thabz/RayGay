#ifndef SPHERE_H
#define SPHERE_H

#include <vector>
#include <iosfwd>

#include "object.h"
#include "vector.h"
#include "rgb.h"
#include "material.h"
#include "box.h"

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

	virtual Vector& getCenter(); ///< Returns center of sphere

	virtual void transform(const Matrix& m);
	virtual Vector normal(const Intersection & i);
	virtual RGB getDiffuseColor(const Vector& p);
	virtual Material getMaterial();

	virtual bool onEdge(const Vector &p);
	virtual bool inside(const Vector &p);

	virtual bool intersects(const Box& b);
	virtual Box boundingBox();

        /// Internal test
	static void test();

    private:
	Vector center;
	double radius;
	Material material;
	Box _boundingBox;
	virtual Intersection _intersect(const Ray& ray);

};

#endif

