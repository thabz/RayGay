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
class Triangle : public Object {

    public:
	/// Constructor
	Triangle(Mesh* m);

	Vector normal(const Intersection &i) const;

	void transform(const Matrix& m) { };
	const Material* getMaterial() const;
	bool intersects(const BoundingBox&) const;
	BoundingBox boundingBoundingBox() const;
	Vector2 getUV(const Intersection& intersection) const;


	/// Get index into mesh' array of Tris
	int getTri() const;
	/// Set index into mesh' array of Tris
	void setTri(int);
	
	/// Indices into the mesh' array of vertices
        unsigned int vertex[3];
	/// Indices into the mesh' array of interpolated normals
        unsigned int interpolated_normal[3]; 
	/// The triangle's own normal
        unsigned int normali;

	virtual SceneObject* clone() const { return NULL; };

    private:
	Intersection _intersect(const Ray& ray) const;
	Mesh* mesh;
	int _tri_idx;
	
};


#endif
