#ifndef BOX_H
#define BOX_H

#include "vector.h"
#include "object.h"
#include "mesh.h"

class Intersection;
class Ray;
class Matrix;
class Material;

/// A box
class Box : public object {

    public:
	Box(const Vector corner1, const Vector corner2, Material mat);
	~Box();

	virtual void transform(const Matrix& m) { mesh->transform(m); };
	virtual Vector normal(const Intersection & i) { return mesh->normal(i); };
	virtual RGB getDiffuseColor(const Vector& p) { return mesh->getDiffuseColor(p); };
	virtual Material getMaterial() { return mesh->getMaterial(); };

	virtual bool onEdge(const Vector &p) { return mesh->onEdge(p); };
	virtual bool inside(const Vector &p) { return mesh->inside(p); };

	virtual bool intersects(const BoundingBox& b) { return mesh->intersects(b); };
	virtual BoundingBox boundingBoundingBox() {return mesh->boundingBoundingBox(); };

	virtual void getUV(const Intersection& intersection, double* u, double* v) {return mesh->getUV(intersection, u, v); };


    private:
	Mesh* mesh;

};

#endif


