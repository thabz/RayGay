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
#include "collections/vectorlist.h"

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

	/// Add a quad to the mesh
        void addQuad(const uint32_t c[4]);
        
	/// Add a triangle to the mesh
        void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3);
	
	/// Add a triangle to the mesh with uv-texture-coordinates
        void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3, const Vector2& uv1, const Vector2& uv2,const Vector2& uv3);

	/// Adds a new vertex to the mesh
	uint32_t addVertex(const Vector& point);

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
	Vector cornerAt(uint tri_idx, uint i) const;
	
	/// Index into face vertices
	void cornerAt(uint tri_idx, uint i, double dest[3]) const;
	
	/// Index into vertices 
	Vector cornerAt(uint i) const { return corners[i]; };

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

	VectorList normals;
	VectorList corners;
	std::vector<Vector2> uv_coords;
	// 3 indices into corners for each face
	std::vector<uint> faces;
	// 3 indices into normals for each face
	std::vector<uint> i_normal_indices;

    protected:
	uint facesNum() const;

};

inline
uint Mesh::facesNum() const {
    return faces.size() / 3;
}

inline
Vector Mesh::cornerAt(uint tri_idx, uint32_t i) const {
    return corners[faces[3 * tri_idx + i]];
}

inline
void Mesh::cornerAt(uint tri_idx, uint32_t i, double dest[3]) const {
    corners.get(faces[3 * tri_idx + i],dest);
}

#endif
