#ifndef MESH_H
#define MESH_H

#include <vector>
#include <map>
#include <iosfwd>

#include "object.h"
#include "vector.h"
#include "rgb.h"
#include "material.h"
#include "boundingbox.h"
#include "triangle.h"
#include "edgekey.h"

class SpaceSubdivider;
class Linesegment;

/// An object consisting of a mesh of triangles
class Mesh : public object {

    public:
	
	class Tri;
        class Edge {
	    public:
		Edge(int iV0, int iV1);

	        int vertex[2];
   	        Tri* triangle[2];
	};

	class Tri {
	    public:
		Tri(int iV0, int iV1, int iV2);
		int vertex[3];
		int interpolated_normal[3];
		int normal_idx;
		double area;
		Edge* edge[3];
		// Adjacent tris
		Tri* triangle[3];
	};

	class Vertex {
	    public:
		Vertex(int iV);
		// Tris meeting at this vertex
		std::vector<Tri*> tris; // Denne kan slette efter computeInterpo.. 
		int index;
	};
	
        /// The types of meshes
	enum MeshType {
	    MESH_FLAT,
	    MESH_INTERPOLATED
	};

	typedef std::map<EdgeKey,Edge*> EdgeMapType;

	/// Default constructor
	Mesh();
	
	/// Constructor
	Mesh(MeshType type, const Material& mat);
	
	/// Destructor
	virtual ~Mesh();

	virtual void transform(const Matrix& m);
	virtual Vector normal(const Intersection & i) const;
	virtual Vector phong_normal(const Intersection & i) const;
	virtual Material getMaterial() const;

	virtual bool intersects(const BoundingBox& b) const;
	/// The bounding box containing all faces of this mesh
	virtual BoundingBox boundingBoundingBox() const;
	virtual void getUV(const Intersection& intersection, double* u, double* v) const;

	/// Add a triangle to the mesh
	void addTriangle(const Vector* corners);
	/// Add a triangle to the mesh
        void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3);
	/// Internal test
	static void test();

	std::vector<Vector>* getVertices();
	std::vector<Linesegment>* getEdges();

	Vector normalAt(int i) { return normals[i]; };
	Vector cornerAt(int i) { return corners[i]; };

    private:

	MeshType meshType;
	Material material;
	mutable BoundingBox* _boundingBoundingBox;
	virtual Intersection _intersect(const Ray& ray) const;
	SpaceSubdivider* hierarchy;
	bool prepared;
	int findExistingCorner(const Vector* c) const;
	void computeAdjacentTris();
	void computeTriAreas();
	void computeInterpolatedNormals();

	std::vector<Vector> normals;
	std::vector<Triangle*> triangles;
	EdgeMapType edgeMap;
	std::vector<Tri*> tris;
	std::vector<Vertex> vertices;

    protected:
	void prepare();
	std::vector<Vector> corners;

};

#endif
