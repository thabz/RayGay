#ifndef MESH_H
#define MESH_H

#include <vector>
#include <map>
#include <iosfwd>

#include "object.h"
#include "math/vector.h"
#include "math/vector2.h"
#include "image/rgb.h"
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
	};

	/// The types of meshes
	enum MeshType {
	    MESH_FLAT,
	    MESH_PHONG
	};

	/// Helper type
	typedef std::map<EdgeKey,Edge*> EdgeMapType;

	/// Constructor
	Mesh(MeshType type, const Material* mat);
	
	/// Destructor
	virtual ~Mesh();
	
	/// Adds this or all subobjects to a space
	void addSelf(KdTree* space);

	virtual void transform(const Matrix& m);

	/// Returns a (possibly) phong-interpolated normal
	Vector normal(const uint face_idx, double u, double v) const;
	
	/// Material of the mesh 
	virtual const Material* getMaterial() const;

	/// Add a triangle to the mesh
	void addTriangle(const Vector* corners, const Vector2* uv);
	
	/// Add a quad to the mesh
	void addQuad(const Vector* corners, const Vector2* uv);
	
	/// Add a quad to the mesh
	void addQuad(const uint c[4], const Vector2 uv[4]);

	/// Add a triangle to the mesh
        void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3);
	
	/// Add a triangle to the mesh with uv-texture-coordinates
        void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3, const Vector2& uv1, const Vector2& uv2,const Vector2& uv3);

	/// Adds a new vertex to the mesh
	unsigned int addVertex(const Vector& point);

	/// Add a triangle by vertex indices
	void addTriangle(const uint v[3], const Vector2 uv[3]);
	
	/// Add a triangle by vertex indices
	void addTriangle(int v0, int v1, int v2, const Vector2 uv0, const Vector2 uv1, const Vector2 uv2);
	
	/// Add a triangle by vertex indices
	void addTriangle(const uint v[3]);
	
	/// A vector of all vertices
	std::vector<Vector>* getVertices();

	/// A vector of all unique edges
	std::vector<Linesegment>* getEdges();
	
	/// Index into face vertices
	const Vector& cornerAt(uint tri_idx, uint i) const;
	
	/// Index into vertices 
	const Vector& cornerAt(uint i) const { return corners[i]; };

	Vector2 getUV(const uint face_idx, double u, double v) const;

	void prepare();
	
	virtual SceneObject* clone() const;

	void hintVertexNum(uint num);
	void hintFaceNum(uint num);

    private:
	MeshType meshType;
	bool prepared;
	int findExistingCorner(const Vector* c) const;
	void computeInterpolatedNormals();
	Vector phong_normal(const uint face_idx, double u, double v) const;
	std::vector<Triangle> triangles;

	const Material* material;

	std::vector<Vector> normals;
	std::vector<Vector> corners;
	std::vector<Vector2> uv_coords;
	// 3 indices into corners for each face
	std::vector<uint> faces;
	// 3 indices into normals for each face
	std::vector<uint> i_normal_indices;

};

inline
const Vector& Mesh::cornerAt(uint tri_idx, unsigned int i) const {
    return corners[faces[3 * tri_idx + i]];
}

#endif
