
#ifndef OBJECTS_ELLIPSOID_H
#define OBJECTS_ELLIPSOID_H

#include "objects/solid.h"
#include "objects/transformer.h"
#include "objects/sphere.h"

/**
 * An ellipsoid object.
 * 
 * \todo Finish implementation.
 */
class Ellipsoid : public Solid, public Transformer {
    public:
	Ellipsoid(const Vector& center, const Vector& radii, Material* material);
	void transform(const Matrix& m);
	BoundingBox boundingBoundingBox() const;
	SceneObject* clone() const;
	vector<Intersection> allIntersections(const Ray& ray) const;

    private:
	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;

	Sphere* sphere;
};

#endif