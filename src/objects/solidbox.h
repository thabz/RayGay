
#ifndef OBJECTS_SOLID_BOX_H
#define OBJECTS_SOLID_BOX_H

#include "objects/solid.h"
#include "transformer.h"

/**
 * A box not build with triangles.
 *
 * This object can be used in CSG operations. The Box object
 * renders a little bit faster, but can't be used for CSG.
 */
class SolidBox : public Solid, public Transformer {

    public:
	/// Constructs a box with extremities at corner1 and corner2
	SolidBox(const Vector corner1, const Vector corner2, const Material* mat);

	bool inside(const Vector &p) const;
	uint32_t allIntersections(const Ray& ray, Intersection* result) const;
	uint32_t maxIntersections() const;

	AABox getBoundingBox() const;
	SceneObject* clone() const;
	void transform(const Matrix& m);

    private:
	AABox bbox;

	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;
};

#endif
