#ifndef SPHERE_H
#define SPHERE_H

#include <vector>
#include <iosfwd>

#include "object.h"
#include "math/vector.h"
#include "boundingbox.h"
#include "booleanoperand.h"

class Intersection;
class Ray;
class Matrix;
class Vector2;

/// A sphere object
class Sphere : public BooleanOperand {

    friend std::ostream & operator<< (std::ostream &os, const Sphere &s);
    
    public:
        /// Constructor
	Sphere(const Vector& c, double r, const Material* material);

	/// Destructor
	virtual ~Sphere();

        /// Returns center of sphere
	const Vector& getCenter() const; 

	virtual void transform(const Matrix& m);

	virtual bool onEdge(const Vector &p) const;
	virtual bool inside(const Vector &p) const;

	virtual BoundingBox boundingBoundingBox() const;

	virtual SceneObject* clone() const;
	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
        int intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const;

    private:
	Vector center;
	double radius;
	BoundingBox _boundingBoundingBox;
	Vector2 getUV(const Vector& point) const;

};

#endif

