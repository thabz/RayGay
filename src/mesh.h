#ifndef MESH_H
#define MESH_H

#include <vector>
#include <iosfwd>

#include "object.h"
#include "vector.h"
#include "rgb.h"
#include "material.h"
#include "box.h"

/// The triangle of a Mesh
class Triangle {
    public:
	/// Indices into the mesh' array of vertices
        int vertex[3];
	/// Indices into the mesh' array of interpolated normals
        int interpolated_normal[3]; 
	/// The triangle's own normal
        int normal;
};

/// An object consisting of a mesh of triangles
class Mesh : public object {

    public:
        /// The types of meshes
	enum MeshType {
	    MESH_FLAT,
	    MESH_INTERPOLATED
	};
	
	/// Constructor
	Mesh(MeshType type, Material mat);
	/// Destructor
	~Mesh();

	virtual void transform(const Matrix& m);
	virtual Vector normal(const Intersection & i);
	virtual RGB getDiffuseColor(const Vector& p);
	virtual Material getMaterial();

	virtual bool onEdge(const Vector &p);
	virtual bool inside(const Vector &p);

	virtual bool intersects(const Box& b);
	virtual Box boundingBox();
	virtual void getUV(const Intersection& intersection, double* u, double* v);

	void addTriangle(const Vector* corners);

	/// Internal test
	static void test();

    private:
	MeshType meshType;
	Material material;
	Box* _boundingBox;
	virtual Intersection _intersect(const Ray& ray);
	Intersection intersect_triangle(const Ray& ray, Vector vert0, Vector vert1, Vector vert2);

	std::vector<Vector> corners;
	std::vector<Vector> normals;
	std::vector<Triangle> triangles;
};

#endif
