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

	void transform(const Matrix& m) { };
	const Material* getMaterial() const;
	BoundingBox boundingBoundingBox() const;

	void prepare();

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
	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
	double area() const;
        int intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const;

    private:
	Mesh* mesh;
	int _tri_idx;
	double edge1[3], edge2[3], vert0[3];
};


#endif
