#ifndef SPHERE_H
#define SPHERE_H

#include "solid.h"
#include "object.h"
#include "math/vector.h"
#include "boundingbox.h"

class Intersection;
class Ray;
class Matrix;
class Vector2;

/// A sphere object
class Sphere : public Solid {

    friend std::ostream & operator<< (std::ostream &os, const Sphere &s);
    
    public:
        /// Constructor
	Sphere(const Vector& c, double r, const Material* material);

	/// Destructor
	virtual ~Sphere();

        /// Returns center of sphere
	const Vector& getCenter() const; 

	/// Returns radius of sphere
	double getRadius() const;

	virtual void transform(const Matrix& m);

	virtual BoundingBox boundingBoundingBox() const;

	virtual SceneObject* clone() const;
	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;
	int intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const;
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

    private:
	Vector center;
	double radius;
	Vector2 getUV(const Vector& point) const;

};

#endif

