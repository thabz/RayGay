
#include <iostream>
#include <vector>
#include <cassert>
#include <map>

#include "mesh.h"
#include "boundingbox.h"
#include "math/matrix.h"
#include "math/vector2.h"
#include "intersection.h"
#include "ray.h"
#include "sphere.h"
#include "triangle.h"
#include "hierarchy.h"
#include "bsp.h"
#include "paths/circle.h"
#include "cylinder.h"
#include "paths/linesegment.h"

#define PHONG_ANGLETHRESHOLD 0.4226f // Smoothing threshold approx. 65 Deegrees :) 

using namespace std;

// ----------------------------------------------------------------------------
Mesh::Mesh(MeshType type, const Material& mat) {
    meshType = type;
    material = mat;
    _boundingBoundingBox = NULL;
    prepared = false;
}

// ----------------------------------------------------------------------------
Mesh::~Mesh() {
    delete _boundingBoundingBox;
}

void Mesh::addParts(SpaceSubdivider* space) {
    for(unsigned int i = 0; i < triangles.size(); i++) {
	space->addObject(triangles[i]);
    }
}

void Mesh::prepare() {
    if (prepared == true) return;

    computeAdjacentTris();
    computeInterpolatedNormals();
    computeTriAreas();
    prepared = true;
}

// ----------------------------------------------------------------------------
/**
 * Add a triangle to this mesh
 * @param c must a pointer to three Vectors
 * @param uv must a pointer to three Vector2s
 */
void Mesh::addTriangle(const Vector* c, const Vector2* uv) {
    Vector normal = Vector::xProduct(c[1] - c[0], c[2] - c[0]);
    normal.normalize();
    normals.push_back(normal);
    Triangle* t = new Triangle(this);
    t->normali = normals.size() - 1;

    for(int i = 0; i < 3; i++) {
	int new_index = findExistingCorner(&c[i]);
	if (new_index == -1) {
	   corners.push_back(c[i]);
	   new_index = corners.size() - 1;
	   vertices.push_back(Vertex(new_index));
	}
        t->vertex[i] = new_index;
    }

    Tri* tri = new Tri(t->vertex[0],t->vertex[1],t->vertex[2]);
    tris.push_back(tri);
    tri->normal_idx = t->normali;

    triangles.push_back(t);
    t->setTri(triangles.size() - 1);

    tri->uv[0] = uv[0];
    tri->uv[1] = uv[1];
    tri->uv[2] = uv[2];

    // Insert edges
    for(int i = 0; i < 3; i++) {
	int j = (i + 1) % 3;
	EdgeKey key = EdgeKey(t->vertex[i],t->vertex[j]);
	Edge* edge = edgeMap[key];
	if (edge == NULL) {
	    edge = new Edge(t->vertex[i],t->vertex[j]);
	    edge->triangle[0] = tri;
	    edgeMap[key] = edge;
	} else {
	    edge->triangle[1] = tri;
	}
	tri->edge[i] = edge;
    }
    
    // Add to tris-list at each vertex
    for(int i = 0; i < 3; i++) {
	vertices[t->vertex[i]].tris.push_back(tri);
    }
    
}

void Mesh::computeAdjacentTris() {
    for(unsigned int i = 0; i < tris.size(); i++) {
	Tri* tri = tris[i];
	for(unsigned int e = 0; e < 3; e++) {
	    Tri* adj;
	    Edge* edge = tri->edge[e];
	    if (edge->triangle[0] == tri) {
		adj = edge->triangle[1];
	    } else {
		adj = edge->triangle[0];
	    }
	    tri->triangle[e] = adj;
	}
    }
}

void Mesh::computeInterpolatedNormals() {
    for(unsigned int i = 0; i < tris.size(); i++) {
	Tri* tri = tris[i];
	Vector normal = normals[tri->normal_idx];
	for(unsigned int j = 0; j < 3; j++) {
	    Vertex vertex = vertices[tri->vertex[j]];
	    int num = 1;
	    Vector interpolated_normal = normal;
	    for(unsigned int v = 0; v < vertex.tris.size(); v++) {
		Tri* other_tri = vertex.tris[v];
		Vector other_normal = normals[other_tri->normal_idx];
		if (other_tri != tri &&
			other_normal * normal > PHONG_ANGLETHRESHOLD) {
		    interpolated_normal = interpolated_normal + other_normal;
		    num++;
		}
	    }
	    if (num > 1) {
		interpolated_normal.normalize();
		normals.push_back(interpolated_normal);
		tri->interpolated_normal[j] = normals.size() -1;
	    } else {
		tri->interpolated_normal[j] = tri->normal_idx;
	    }
	}
    }
}

void Mesh::computeTriAreas() {
    for(unsigned int i = 0; i < tris.size(); i++) {
	Tri* tri = tris[i];
	tri->area = Vector::area(corners[tri->vertex[0]],corners[tri->vertex[1]],corners[tri->vertex[2]]);
    }
}

int Mesh::findExistingCorner(const Vector* c) const {
    unsigned int size = corners.size();
    for(unsigned int i = 0; i < size; i++) {
	//if ((corners[i] - *c).norm() < 0.5) return i;
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
    for (vector<Vector>::iterator p = corners.begin(); p != corners.end(); p++) {
	(*p) = M * (*p);
    }
    Matrix rot = M.extractRotation();
    for (vector<Vector>::iterator p = normals.begin(); p != normals.end(); p++) {
	(*p) =  rot * (*p);
    }
}

Vector2 Mesh::getUV(const Intersection &i) const {
    const Triangle* triangle = i.local_triangle;
    Tri* tri = tris[triangle->getTri()];
    Vector weight = getInterpolationWeights(triangle->getTri(), i.point);
    return tri->uv[0] * weight[0] +
	tri->uv[1] * weight[1] +
	tri->uv[2] * weight[2];
}

// ----------------------------------------------------------------------------
Vector Mesh::normal(const Intersection &i) const {
    return phong_normal(i);
    //   return normals[i.local_triangle->normali];
}

Vector Mesh::phong_normal(const Intersection &i) const {
    const Triangle* triangle = i.local_triangle;
    Tri* tri = tris[triangle->getTri()];
    Vector result = Vector(0,0,0);
    Vector weight = getInterpolationWeights(triangle->getTri(), i.point);
    for(unsigned int j = 0; j < 3; j++) {
	result = result + normals[tri->interpolated_normal[j]] * weight[j];
    }
    result.normalize();
    return result;
}

Vector Mesh::getInterpolationWeights(unsigned int triIdx, Vector p) const {
    // Hvert hjørnes vægt er den modsatte trekants areal.
    Tri* tri = tris[triIdx];
    Vector result = Vector(0,0,0);
    unsigned int j,j2,j3;
    for(j = 0; j < 3; j++) {
	j2 = (j + 1) % 3;
	j3 = (j + 2) % 3;
	result[j] =  Vector::area(p,corners[tri->vertex[j2]],corners[tri->vertex[j3]]) / tri->area;
    }
    return result;
}

// ----------------------------------------------------------------------------
bool Mesh::intersects(const BoundingBox& box) const {
    // Quick hackish implementation: wrap mesh in a sphere and check that for intersection
    if (box.inside(boundingBoundingBox()))
	return true;

    Vector center;
    for (vector<Vector>::const_iterator p = corners.begin(); p != corners.end(); p++) {
	center = center + (*p);
    }
    center = center / corners.size();
    double radius = 0;
    for (vector<Vector>::const_iterator p = corners.begin(); p != corners.end(); p++) {
       Vector v = ((*p) - center);
       double l = v.length();
       if (l > radius) radius = l;
    }
    assert(radius > 0);
    Sphere s = Sphere(center,radius,material);
    return s.intersects(box);
    
}

Material Mesh::getMaterial() const {
    return material;
}

// ----------------------------------------------------------------------------
BoundingBox Mesh::boundingBoundingBox() const {
    if (_boundingBoundingBox != NULL)
	return *_boundingBoundingBox;

    Vector mini = Vector(HUGE_DOUBLE,HUGE_DOUBLE,HUGE_DOUBLE);
    Vector maxi = Vector(-HUGE_DOUBLE,-HUGE_DOUBLE,-HUGE_DOUBLE);
    for (vector<Vector>::const_iterator p = corners.begin(); p != corners.end(); p++) {
	Vector v = (*p);
	for (int i = 0; i < 3; i++) {
	    mini[i] = min(mini[i],v[i]);
	    maxi[i] = max(maxi[i],v[i]);
	}
    }
    _boundingBoundingBox = new BoundingBox(mini,maxi);
    return *_boundingBoundingBox;
}

std::vector<Vector>* Mesh::getVertices() {
    std::vector<Vector>* result = new std::vector<Vector>;
    for(unsigned int i = 0; i < corners.size(); i++) {
	result->push_back(corners[i]);
    }
    return result;
}

/**
 * Returns a vector of all unique edges in the mesh. 
 * An edge is represented as a Linesegment.
 */
std::vector<Linesegment>* Mesh::getEdges() {
    
    std::vector<Linesegment>* result = new std::vector<Linesegment>;
    for(EdgeMapType::iterator h = edgeMap.begin(); h != edgeMap.end(); h++) {
	Edge* edge = h->second;
	Linesegment s = Linesegment(corners[edge->vertex[0]],corners[edge->vertex[1]]);
	result->push_back(s);
    }
    return result;
}


// ----------------------------------------------------------------------------
Mesh::Edge::Edge(int iV0, int iV1) {
    vertex[0] = iV0;
    vertex[1] = iV1;
    triangle[0] = NULL;
    triangle[1] = NULL;
}

// ----------------------------------------------------------------------------
Mesh::Tri::Tri(int iV0, int iV1, int iV2) {
    vertex[0] = iV0;
    vertex[1] = iV1;
    vertex[2] = iV2;
    normal_idx = -1;
    area = -1.0;
    for(unsigned int i = 0; i < 3; i++) {
	interpolated_normal[i] = -1;
    }
}

Mesh::Vertex::Vertex(int iV) {
    index = iV;
}

// ----------------------------------------------------------------------------
void Mesh::test() {
    BSP bsp = BSP();

    Material mat = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Mesh mesh = Mesh(MESH_FLAT,mat);
    mesh.addTriangle(Vector(-1,1,1),Vector(1,1,1),Vector(0,-1,-1));
    mesh.prepare();
    mesh.addParts(&bsp);
    bsp.prepare();

    assert(mesh.edgeMap.size() == 3);

    // Test intersection
    Ray ray = Ray(Vector(0,0,100),Vector(0,0,-1),0.0);
    Intersection i = bsp.intersect(ray);
    assert(i.point == Vector(0,0,0));
    assert(i.intersected);
    assert(i.t == 100.0);

    ray = Ray(Vector(0,0,100),Vector(0,0,1),0.0);
    i = bsp.intersect(ray);
    assert(!i.intersected);

    ray = Ray(Vector(0,0,-100),Vector(0,0,-1),0.0);
    i = bsp.intersect(ray);
    assert(!i.intersected);

    // Test torus
    Circle circle1 = Circle(Vector(0,75,0),200,Vector(0,1,0));
    Cylinder torus = Cylinder(circle1,100,16,10,Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.20,30));
    torus.prepare();

    assert(torus.corners.size() == 16*10);
    assert(torus.vertices.size() == 16*10);

    for(unsigned int i = 0; i < torus.tris.size(); i++) {
	Tri* tri = torus.tris[i];
	assert(tri->normal_idx != -1);
	assert(tri->area != -1.0);
	for(unsigned int j = 0; j < 3; j++) {
	   assert(tri->interpolated_normal[j] != -1);
	   assert(tri->vertex[j] != -1);
	   for (unsigned int k = 0; k < 3; k++) {
	       if (k != j) {
		   assert(tri->interpolated_normal[k] != tri->interpolated_normal[j]);
	       }
	   }
	}
    }

    for(unsigned int i = 0; i < torus.vertices.size(); i++) {
	Vertex v = torus.vertices[i];
	assert(v.tris.size() == 6);
    }

    assert(torus.normals.size() == 4 * torus.tris.size());
   
    for(unsigned int i = 0; i < torus.normals.size(); i++) {
	Vector normal = torus.normals[i];
	assert(IS_EQUAL(normal.norm(),double(1.0)));
    }


    cout << "Mesh::test() done." << endl;
}
