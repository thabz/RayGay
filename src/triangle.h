#ifndef TRIANGLE_H
#define TRIANGLE_H

#include "object.h"

class Material;
class BoundingBox;
class Mesh;
class Intersection;
class Matrix;
class Vector2;

/// The triangle of a Mesh
class Triangle : public object {

    public:
	Triangle(Mesh* m);

	Vector normal(const Intersection &i) const;

	void transform(const Matrix& m) { };
	const Material& getMaterial() const;
	bool intersects(const BoundingBox&) const;
	BoundingBox boundingBoundingBox() const;
	Vector2 getUV(const Intersection& intersection) const;


	int getTri() const;
	void setTri(int);
	
	/// Indices into the mesh' array of vertices
        unsigned int vertex[3];
	/// Indices into the mesh' array of interpolated normals
        unsigned int interpolated_normal[3]; 
	/// The triangle's own normal
        unsigned int normali;

    private:
	Intersection _intersect(const Ray& ray) const;
	Mesh* mesh;
	int _tri_idx;
	
};


#endif
