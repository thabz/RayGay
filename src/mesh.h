#ifndef MESH_H
#define MESH_H

#include <vector>
#include <iosfwd>

#include "object.h"
#include "vector.h"
#include "rgb.h"
#include "material.h"
#include "boundingbox.h"
#include "triangle.h"

class Hierarchy;

/// An object consisting of a mesh of triangles
class Mesh : public object {

    public:
        /// The types of meshes
	enum MeshType {
	    MESH_FLAT,
	    MESH_INTERPOLATED
	};

	/// Default constructor
	Mesh();
	
	/// Constructor
	Mesh(MeshType type, const Material& mat);
	
	/// Destructor
	virtual ~Mesh();

	virtual void transform(const Matrix& m);
	virtual Vector normal(const Intersection & i) const;
	virtual Material getMaterial() const;

	virtual bool onEdge(const Vector &p) const;
	virtual bool inside(const Vector &p) const;
	virtual bool intersects(const BoundingBox& b) const;
	virtual BoundingBox boundingBoundingBox() const;
	virtual void getUV(const Intersection& intersection, double* u, double* v) const;

	/// Add a triangle to the mesh
	void addTriangle(const Vector* corners);
	/// Add a triangle to the mesh
        void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3);
	/// Internal test
	static void test();

	Vector normalAt(int i) { return normals[i]; };
	Vector cornerAt(int i) { return corners[i]; };

    private:
	MeshType meshType;
	Material material;
	mutable BoundingBox* _boundingBoundingBox;
	virtual Intersection _intersect(const Ray& ray) const;
	void prepare() const;
	mutable Hierarchy* hierarchy;
	mutable bool prepared;

	std::vector<Vector> corners;
	std::vector<Vector> normals;
	std::vector<Triangle*> triangles;
};

#endif
