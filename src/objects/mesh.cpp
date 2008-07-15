
#include <iostream>
#include <vector>
#include <cassert>
#include <map>

#include "exception.h"
#include "objects/mesh.h"
#include "math/matrix.h"
#include "math/vector2.h"
#include "intersection.h"
#include "profiler.h"
#include "ray.h"
#include "triangle.h"
#include "paths/circle.h"
#include "extrusion.h"
#include "paths/linesegment.h"
#include "space/kdtree.h"

#define PHONG_ANGLETHRESHOLD 0.4226f // Smoothing threshold approx. 65 Deegrees :) 

using namespace std;

static Profiler* interpolate_normals_profiler = NULL;

// ----------------------------------------------------------------------------
Mesh::Mesh(MeshType type, const Material* mat) {
    meshType = type;
    prepared = false;
    material = mat;
    interpolate_normals = meshType == Mesh::MESH_PHONG;
    if (interpolate_normals_profiler == NULL) {
        interpolate_normals_profiler = Profiler::create("Interpolate normals", "Prepare objects");            
    }
}

// ----------------------------------------------------------------------------
Mesh::~Mesh() {
}

void Mesh::addSelf(KdTree* space) {
    for(uint32_t i = 0; i < triangles.size(); i++) {
	space->addObject(&triangles[i]);
    }
}

void Mesh::prepare() {
    if (prepared == true) return;

    if (interpolate_normals) {
        interpolate_normals_profiler->start();    
	computeInterpolatedNormals();
	interpolate_normals_profiler->stop();
    }

    for(uint32_t i = 0; i < triangles.size(); i++) {
	triangles[i].prepare();
    }

    prepared = true;
}

// ----------------------------------------------------------------------------
/**
 * Add a triangle to this mesh
 * @param c must a pointer to three Vectors
 * @param uv must a pointer to three Vector2s
 */
void Mesh::addTriangle(const Vector* c, const Vector2* uv) {

    uint32_t verts[3];

    for(int i = 0; i < 3; i++) {
	int new_index = findExistingCorner(&c[i]);
	if (new_index == -1) {
	   corners.push_back(c[i]);
	   new_index = corners.size() - 1;
	}
	verts[i] = new_index;
    }
    addTriangle(verts,uv);
}

/**
 * Add a quad to the mesh. This results in two triangles being added.
 *
 * @param corners A pointer to four vertices in clockwise direction
 * @param uv A pointer to four uv coordinates
 */
void Mesh::addQuad(const Vector* corners, const Vector2* uv) {
    addTriangle(corners[0],corners[1],corners[2],
	        uv[0],uv[1],uv[2]);
    addTriangle(corners[0],corners[2],corners[3],
	        uv[0],uv[2],uv[3]);
}

void Mesh::addQuad(const uint32_t c[4], const Vector2 uv[4]) {
    uint32_t k[3];
    Vector2 uvk[3];

    k[0] = c[0]; k[1] = c[1]; k[2] = c[2];
    uvk[0] = uv[0]; uvk[1] = uv[1]; uvk[2] = uv[2];
    addTriangle(k,uvk);

    k[0] = c[0]; k[1] = c[2]; k[2] = c[3];
    uvk[0] = uv[0]; uvk[1] = uv[2]; uvk[2] = uv[3];
    addTriangle(k,uvk);
}

void Mesh::addQuad(const uint32_t c[4]) {
    Vector2 uvk[4];
    addQuad(c,uvk);
}

/**
 * Adds a new vertex to the mesh.
 *
 * @param point the vertex to add
 * @return index of the new vertex
 */
uint32_t Mesh::addVertex(const Vector& point) {
    corners.push_back(point);
    return corners.size() -1;
}

/**
 * Adds a new normal to the mesh.
 *
 * @param point the normal to add
 * @return index of the new normal 
 */
uint32_t Mesh::addNormal(const Vector& normal) {
    normals.push_back(normal);
    interpolate_normals = false;
    return normals.size() -1;
}

/**
 * Adds a new uv texture point to the mesh.
 *
 * @param point the uv point to add
 * @return index of the new uv point 
 */
uint32_t Mesh::addUV(const Vector2& uv) {
    uv_coords.push_back(uv);
    return uv_coords.size() -1;
}

void Mesh::addTriangle(const uint32_t v[3]) {
    Vector2 uv[3];
    addTriangle(v,uv);
}

void Mesh::addTriangle(int v0, int v1, int v2, const Vector2 uv0, const Vector2 uv1, const Vector2 uv2) {
    uint32_t v[3];
    Vector2 uv[3];
    v[0] = v0; v[1] = v1; v[2] = v2;
    uv[0] = uv0; uv[1] = uv1; uv[2] = uv2;
    addTriangle(v,uv);
}

void Mesh::addTriangle(const uint32_t v[3], const uint32_t n[3], const uint32_t uv[3]) {
   faces.push_back(v[0]);
   faces.push_back(v[1]);
   faces.push_back(v[2]);
   i_normal_indices.push_back(n[0]);
   i_normal_indices.push_back(n[1]);
   i_normal_indices.push_back(n[2]);
   i_uv_indices.push_back(uv[0]);
   i_uv_indices.push_back(uv[1]);
   i_uv_indices.push_back(uv[2]);
   interpolate_normals = false;
}

void Mesh::addTriangle(const uint32_t v[3], const Vector2 uv[3]) {

    // Check vertex indices are within bounds
    uint32_t max_idx = corners.size() - 1;
    if (v[0] > max_idx || v[1] > max_idx || v[2] > max_idx ||
        v[0] < 0 || v[1] < 0 || v[2] < 0) {
	char vs[200];
	sprintf(vs, "Triangle (%d,%d,%d) out of bounds",v[0],v[1],v[2]);
	throw_exception(vs);
    }

    faces.push_back(v[0]);
    faces.push_back(v[1]);
    faces.push_back(v[2]);

    uv_coords.push_back(uv[0]);
    uv_coords.push_back(uv[1]);
    uv_coords.push_back(uv[2]);

    Vector c[3];
    c[0] = cornerAt(v[0]);
    c[1] = cornerAt(v[1]);
    c[2] = cornerAt(v[2]);
    Vector normal = Vector::xProduct(c[1] - c[0], c[2] - c[0]);
    normal.normalize();
    normals.push_back(normal);

    triangles.push_back(Triangle(this, (faces.size() / 3) - 1));
}

void Mesh::computeInterpolatedNormals() {

    assert(meshType == Mesh::MESH_PHONG);

    uint32_t face_num = faces.size() / 3;
    uint32_t vertex_num = corners.size();
    
    // For each vertex create a list of its adjacent faces
    vector<uint32_t>* adj = new vector<uint32_t>[vertex_num];
    for(uint32_t i = 0; i < face_num; i++) {
	for(uint32_t j = 0; j < 3; j++) {
	    uint32_t vertex_idx = faces[i*3+j];
	    adj[vertex_idx].push_back(i);
	}
    }

    i_normal_indices.reserve(face_num * 3);
    
    // FIXME 1: All interpolated normals are calculated and stored for each vertex that is 
    // using it. Fix by setting i_normal_indices to {-1,-1,...} and do some clever reusing of
    // data and setting multiple i_normal_indices at a time.
    // FIXME 2: Store the new interpolated normals and reused plain normals in a new array
    // so that we effectively can ditch the old unused plain normals.
    for(uint32_t i = 0; i < face_num; i++) {
	Vector normal = normals[i];
	for(uint32_t j = 0; j < 3; j++) {
	    Vector interpolated_normal = normal;
	    int num = 1;
	    uint32_t vertex_idx = faces[i*3+j];
	    vector<uint32_t>& adj_faces = adj[vertex_idx];
	    uint32_t fac_num = adj_faces.size();
	    for(uint32_t v = 0; v < fac_num; v++) {
		uint32_t other_face_idx = adj_faces[v];
		if (other_face_idx != i) {
		    Vector other_normal = normals[other_face_idx];
		    if (other_normal * normal > PHONG_ANGLETHRESHOLD) {
			interpolated_normal += other_normal;
			num++;
		    }
		}
	    }
	    uint32_t index;
	    if (num > 1) {
	        // Use a new interpolated normal    
		interpolated_normal.normalize();
		normals.push_back(interpolated_normal);
		index = normals.size() - 1;
	    } else {
	        // Reuse the old plain normal    
		index = i;
	    }
	    i_normal_indices.push_back(index);
	}
    }

    delete [] adj;
}

// TODO: Optimize by keeping a stl::set with all corners.
int Mesh::findExistingCorner(const Vector* c) const {
    uint32_t size = corners.size();
    for(uint32_t i = 0; i < size; i++) {
	if (corners[i] == *c) return i;
    }
    return -1;
}

// ----------------------------------------------------------------------------
/**
 * Add a triangle to this mesh
 */
void Mesh::addTriangle(const Vector& c1, const Vector& c2, const Vector& c3,
	const Vector2& uv1, const Vector2& uv2,const Vector2& uv3) {
    Vector c[3];
    c[0] = c1;
    c[1] = c2;
    c[2] = c3;
    Vector2 uv[3];
    uv[0] = uv1;
    uv[1] = uv2;
    uv[2] = uv3;
    addTriangle(c,uv);
}

/**
 * Add a triangle to this mesh
 */
void Mesh::addTriangle(const Vector& c1, const Vector& c2, const Vector& c3) {
    Vector c[3];
    c[0] = c1;
    c[1] = c2;
    c[2] = c3;
    Vector2 uv[3];
    addTriangle(c,uv);
}


// ----------------------------------------------------------------------------
void Mesh::transform(const Matrix& M) {
    if (prepared == false) {
        cout << "Warning: transforming unprepared mesh" << endl;
    }
    corners.transform(M);
    Matrix3 rotation = M.extractRotation();
    normals.transform(rotation);
    if (!rotation.isOrthogonal()) {
        normals.normalize();
       // TODO: if M is a scaling transform the recalculate normals
    }
}

// ----------------------------------------------------------------------------
Vector Mesh::normal(const uint32_t face_idx, double u, double v) const {
    if (meshType == Mesh::MESH_PHONG) {
	return phong_normal(face_idx,u,v);
    } else {
	return normals[face_idx];
    }
}

// TODO: Unroll loop below
Vector Mesh::phong_normal(const uint32_t face_idx, double u, double v) const {
    uint32_t offset = face_idx * 3;
    Vector result = Vector(0,0,0);
    Vector weight = Vector(1-u-v,u,v);
    for(uint32_t j = 0; j < 3; j++) {
	result += normals[i_normal_indices[offset + j]] * weight[j];
    }
    result.normalize();
    return result;
}

const Material* Mesh::getMaterial() const {
    return material;
}

Vector2 Mesh::getUV(const uint32_t face_idx, double u, double v) const {
    uint32_t offset = face_idx * 3;
    return uv_coords[offset + 0] * (1-u-v) +
           uv_coords[offset + 1] * u +
           uv_coords[offset + 2] * v;
}

std::vector<Vector>* Mesh::getVertices() {
    std::vector<Vector>* result = new std::vector<Vector>;
    for(uint32_t i = 0; i < corners.size(); i++) {
	result->push_back(corners[i]);
    }
    return result;
}

/**
 * Returns a vector of all unique edges in the mesh. 
 * An edge is represented as a Linesegment.
 */
std::vector<Linesegment>* Mesh::getEdges() {
    EdgeMapType edgeMap;

    uint32_t faces_num = faces.size() / 3;

    for(uint32_t t = 0; t < faces_num; t++) {
	// Insert all edges into edgeMap
	for(int i = 0; i < 3; i++) {
	    int j = (i + 1) % 3;
	    EdgeKey key = EdgeKey(faces[t*3+i],faces[t*3+j]);
	    Edge* edge = edgeMap[key];
	    if (edge == NULL) {
		edge = new Edge(faces[t*3+i],faces[t*3+j]);
		edgeMap[key] = edge;
	    }
	}
    }

    std::vector<Linesegment>* result = new std::vector<Linesegment>;
    for(EdgeMapType::iterator h = edgeMap.begin(); h != edgeMap.end(); h++) {
	Edge* edge = h->second;
	Linesegment s = Linesegment(corners[edge->vertex[0]],corners[edge->vertex[1]]);
	result->push_back(s);
    }
    return result;
}

// TODO: Can be much more effective by using addVertex.
SceneObject* Mesh::clone() const {
    Mesh* clone = new Mesh(meshType,material);

    // Copy vertices
    uint32_t num = corners.size();
    for(uint32_t i = 0; i < num; i++) {
	clone->addVertex(corners[i]);
    }

    // Copy triangles and that's it.
    num = faces.size() / 3;
    for (uint32_t i = 0; i < num; i++) {
	clone->addTriangle(&faces[i*3],&uv_coords[i*3]);
    }
    return clone;
}

// ----------------------------------------------------------------------------
Mesh::Edge::Edge(int iV0, int iV1) {
    vertex[0] = iV0;
    vertex[1] = iV1;
}

/**
 * Reserves space for num vertices.
 * 
 * This makes subsequent insertions of 
 * vertices a bit faster but also makes sure that the vertex-arrays are
 * the exact needed size.
 */
void Mesh::hintVertexNum(uint32_t num) {
    corners.reserve(num);
}

/**
 * Reserves space for num faces. This makes subsequent insertions of 
 * faces a bit faster but also makes sure that the faces-arrays are
 * the exact needed size.
 */
void Mesh::hintFaceNum(uint32_t num) {
    normals.reserve(num);
    faces.reserve(3 * num);
    triangles.reserve(num);
}

