
#ifndef OBJECTS_ELLIPSOID_H
#define OBJECTS_ELLIPSOID_H

#include "objects/solid.h"
#include "transformer.h"
#include "objects/sphere.h"

/**
 * An ellipsoid object.
 */
class Ellipsoid : public Solid, public Transformer {
    public:
	Ellipsoid(const Vector& center, const Vector& radii, Material* material);
	void transform(const Matrix& m);
	AABox getBoundingBox() const;
	SceneObject* clone() const;
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

    private:
	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;

	Sphere* sphere;
};

#endif
