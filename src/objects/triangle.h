#ifndef TRIANGLE_H
#define TRIANGLE_H

#include "object.h"

class Material;
class BoundingBox;
class Mesh;
class Intersection;
class Matrix;

/// The triangle of a Mesh
class Triangle : public Object {

    public:
	/// Constructor
	Triangle(Mesh* m, uint tri_index);

	void transform(const Matrix& m) { };
	const Material* getMaterial() const;
	BoundingBox boundingBoundingBox() const;

	void prepare();

	virtual SceneObject* clone() const { return NULL; };
	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
	double area() const;
        int intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const;

    private:
	Mesh* mesh;
	unsigned int _tri_idx;
};


#endif
