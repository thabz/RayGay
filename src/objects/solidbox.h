
#ifndef OBJECTS_SOLID_BOX_H
#define OBJECTS_SOLID_BOX_H

#include "objects/solid.h"
#include "objects/transformer.h"

class SolidBox : public Solid, public Transformer {

    public:
	/// Constructs a box with extremities at corner1 and corner2
	SolidBox(const Vector corner1, const Vector corner2, const Material* mat);

	bool inside(const Vector &p) const;
	vector<Intersection> allIntersections(const Ray& ray) const;

	BoundingBox boundingBoundingBox() const;
	SceneObject* clone() const;
	void transform(const Matrix& m);

    private:
	BoundingBox bbox;

	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
};

#endif
