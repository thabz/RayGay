#ifndef TRIANGLE_H
#define TRIANGLE_H

#include "object.h"

class Material;
class BoundingBox;
class Mesh;
class Intersection;
class Matrix;

/// The triangle of a Mesh
class Triangle : public object {

    public:
	Triangle(Mesh* m);

	Vector normal(const Intersection &i) const;

	void transform(const Matrix& m) { };
	Material getMaterial() const;
	bool intersects(const BoundingBox&) const;
	BoundingBox boundingBoundingBox() const;
	void getUV(const Intersection& intersection, double* u, double* v) const;

	
	/// Indices into the mesh' array of vertices
        int vertex[3];
	/// Indices into the mesh' array of interpolated normals
        int interpolated_normal[3]; 
	/// The triangle's own normal
        int normali;

    private:
	Intersection _intersect(const Ray& ray) const;
	Mesh* mesh;
	
};


#endif
