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
#include "objectgroup.h"

class KdTree;
class Linesegment;
class Material;

/// An object consisting of a mesh of triangles
class Mesh : public ObjectGroup {

    public:

	void addObject(SceneObject* obj) {};
	
	class Tri;
	/// A class for storing edges
        class Edge {
	    public:
		Edge(int iV0, int iV1);

	        int vertex[2];
   	        Tri* triangle[2];
	};

	/// A class for storing triangles
	class Tri {
	    public:
		Tri(int iV0, int iV1, int iV2);
		int vertex[3];
		int interpolated_normal[3];
		Vector2 uv[3];
		int normal_idx;
		Edge* edge[3];
		// Adjacent tris
		Tri* triangle[3];
	};

	/// A class for storing vertices 
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

	/// Helper type
	typedef std::map<EdgeKey,Edge*> EdgeMapType;

	/// Default constructor
//	Mesh();
	
	/// Constructor
	Mesh(MeshType type, const Material* mat);
	
	/// Destructor
	virtual ~Mesh();
	
	/// Adds this or all subobjects to a space
	void addSelf(KdTree* space);

	virtual void transform(const Matrix& m);

	/// Returns a (possibly) phong-interpolated normal
	Vector normal(const Triangle* const triangle, double u, double v) const;
	
	/// Material of the mesh 
	virtual const Material* getMaterial() const;

	/// The bounding box containing all faces of this mesh
	virtual BoundingBox boundingBoundingBox() const;

	/// Add a triangle to the mesh
	void addTriangle(const Vector* corners, const Vector2* uv);
	
	/// Add a quad to the mesh
	void addQuad(const Vector* corners, const Vector2* uv);

	/// Add a triangle to the mesh
        void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3);
	
	/// Add a triangle to the mesh with uv-texture-coordinates
        void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3, const Vector2& uv1, const Vector2& uv2,const Vector2& uv3);
	
	/// Internal test
	static void test();

	/// A vector of all vertices
	std::vector<Vector>* getVertices();

	/// A vector of all unique edges
	std::vector<Linesegment>* getEdges();

	/// Index into normals
	const Vector& normalAt(unsigned int i) const { return normals[i]; };
	/// Index into vertices 
	const Vector& cornerAt(unsigned int i) const { return corners[i]; };

	Vector2 getUV(const Triangle* triangle, double u, double v) const;

	void prepare();
	virtual SceneObject* clone() const;

    private:
	MeshType meshType;
	mutable BoundingBox* _boundingBoundingBox;
	//virtual Intersection _intersect(const Ray& ray) const;
	bool prepared;
	int findExistingCorner(const Vector* c) const;
	void computeAdjacentTris();
	void computeInterpolatedNormals();
	Vector phong_normal(const Triangle* const triangle, double u, double v) const;
	std::vector<Vector> normals;
	std::vector<Triangle*> triangles;
	EdgeMapType edgeMap;
	std::vector<Tri*> tris;
	std::vector<Vertex> vertices;
	std::vector<Vector> corners;

	const Material* material;

};

#endif
