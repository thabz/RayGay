
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
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

	BoundingBox boundingBoundingBox() const;
	SceneObject* clone() const;
	void transform(const Matrix& m);

    private:
	BoundingBox bbox;

	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
};

#endif
