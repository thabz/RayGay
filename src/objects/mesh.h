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
	Vector normal(const uint32_t face_idx, double u, double v) const;
	
	/// Material of the mesh 
	virtual const Material* getMaterial() const;

	/// Add a triangle to the mesh
	void addTriangle(const Vector* corners, const Vector2* uv);

	/// Add a convex polygon to the mesh
	void addConvexPolygon(int num, const uint32_t* c, const uint32_t* uv = NULL, const uint32_t* n = NULL);
	
	/// Add a quad to the mesh
        void addQuad(const uint32_t c[4], const uint32_t uv[4] = NULL, const uint32_t n[4] = NULL);
        
	/// Add a triangle to the mesh
//    void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3);
	
	/// Add a triangle to the mesh with uv-texture-coordinates
//    void addTriangle(const Vector& c1, const Vector& c2, const Vector& c3, const Vector2& uv1, const Vector2& uv2,const Vector2& uv3);

	/// Adds a new vertex to the mesh
	uint32_t addVertex(const Vector& point);
	/// Reuse an existing vertex or add a new to the mesh
    uint32_t findOrAddVertex(const Vector& v);
    
	/// Adds a new normal to the mesh
	uint32_t addNormal(const Vector& normal);

	/// Add a triangle by vertex, normal and uv vertice 
	void addTriangle(const uint32_t v[3], const uint32_t uv[3] = NULL, const uint32_t n[3] = NULL);

	/// Add a triangle by vertex indices
	void addTriangle(uint32_t v0, uint32_t v1, uint32_t v2, uint32_t uv0, uint32_t uv1, uint32_t uv2);

	/// Add a triangle by vertex indices
	void addTriangle(uint32_t v0, uint32_t v1, uint32_t v2);
	
    /// Add a new uv texture point to the mesh
    uint32_t addUV(const Vector2& uv);
	
	/// A vector of all vertices
	std::vector<Vector>* getVertices();

	/// A vector of all unique edges
	std::vector<Linesegment>* getEdges();
	
	/// Index into face vertices
	Vector cornerAt(uint32_t tri_idx, uint32_t i) const;
	
	/// Index into face vertices
	void cornerAt(uint32_t tri_idx, uint32_t i, double dest[3]) const;
	
	/// Index into vertices 
	Vector cornerAt(uint32_t i) const { return corners[i]; };

	Vector2 getUV(const uint32_t face_idx, double u, double v) const;

	void prepare();
	
	virtual SceneObject* clone() const;

	void hintVertexNum(uint32_t num);
	void hintFaceNum(uint32_t num);

    private:
	    MeshType meshType;
	    bool prepared;
        bool interpolate_normals;
	    //int findExistingCorner(const Vector* c) const;
	    void computeInterpolatedNormals();
	    Vector phong_normal(const uint32_t face_idx, double u, double v) const;
	    std::vector<Triangle> triangles;

	const Material* material;

	VectorList normals;
	VectorList corners;
	std::vector<Vector2> uv_coords;
	// 3 indices into corners for each face
	std::vector<uint32_t> faces;
	// 3 indices into normals for each face
	std::vector<uint32_t> i_normal_indices;
	// 3 indices into uvs for each face
	std::vector<uint32_t> i_uv_indices;

    protected:
	uint32_t facesNum() const;

};

inline
uint32_t Mesh::facesNum() const {
    return faces.size() / 3;
}

inline
Vector Mesh::cornerAt(uint32_t tri_idx, uint32_t i) const {
    return corners[faces[3 * tri_idx + i]];
}

inline
void Mesh::cornerAt(uint32_t tri_idx, uint32_t i, double dest[3]) const {
    corners.get(faces[3 * tri_idx + i],dest);
}

#endif
