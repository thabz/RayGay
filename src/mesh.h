#ifndef MESH_H
#define MESH_H

#include <vector>
#include <map>
#include <iosfwd>

#include "object.h"
#include "math/vector.h"
#include "math/vector2.h"
#include "image/rgb.h"
#include "boundingbox.h"
#include "triangle.h"
#include "edgekey.h"
#include "objectcollection.h"

class SpaceSubdivider;
class Linesegment;

/// An object consisting of a mesh of triangles
class Mesh : public ObjectCollection {

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
		Vector2 uv[3];
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
	
	/// Adds this or all subobjects to a space
	void addParts(SpaceSubdivider* space);

	virtual void transform(const Matrix& m);
	virtual Vector normal(const Intersection & i) const;
	virtual const Material& getMaterial() const;

	/// Intersection with a boundingbox
	virtual bool intersects(const BoundingBox& b) const;

	/// The bounding box containing all faces of this mesh
	virtual BoundingBox boundingBoundingBox() const;

	/// Add a triangle to the mesh
	void addTriangle(const Vector* corners, const Vector2* uv);
	/// Add a triangle to the mesh
        void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3);
	/// Add a triangle to the mesh with uv-texture-coordinates
        void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3, const Vector2& uv1, const Vector2& uv2,const Vector2& uv3);
	Vector2 getUV(const Intersection &i) const;
	/// Internal test
	static void test();

	std::vector<Vector>* getVertices();
	std::vector<Linesegment>* getEdges();

	const Vector& normalAt(unsigned int i) const { return normals[i]; };
	const Vector& cornerAt(unsigned int i) const { return corners[i]; };

	void prepare();

    private:
	MeshType meshType;
	Material material;
	mutable BoundingBox* _boundingBoundingBox;
	//virtual Intersection _intersect(const Ray& ray) const;
	bool prepared;
	int findExistingCorner(const Vector* c) const;
	void computeAdjacentTris();
	void computeTriAreas();
	void computeInterpolatedNormals();
	virtual Vector phong_normal(const Intersection & i) const;
	Vector getInterpolationWeights(unsigned int tri, Vector p) const;

	std::vector<Vector> normals;
	std::vector<Triangle*> triangles;
	EdgeMapType edgeMap;
	std::vector<Tri*> tris;
	std::vector<Vertex> vertices;
	std::vector<Vector> corners;


};

#endif
