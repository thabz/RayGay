
#include <iostream>
#include <vector>
#include <cassert>
#include <map>

#include "mesh.h"
#include "boundingbox.h"
#include "constants.h"
#include "rgb.h"
#include "vector.h"
#include "matrix.h"
#include "intersection.h"
#include "ray.h"
#include "sphere.h"
#include "triangle.h"
#include "hierarchy.h"
#include "bsp.h"
#include "circle.h"
#include "cylinder.h"
#include "linesegment.h"

#define PHONG_ANGLETHRESHOLD 0.4226f // Smoothing threshold approx. 65 Deegrees :) 

using namespace std;

// ----------------------------------------------------------------------------
Mesh::Mesh(MeshType type, const Material& mat) {
    meshType = type;
    material = mat;
    _boundingBoundingBox = NULL;
    prepared = false;
    hierarchy = NULL;
}

// ----------------------------------------------------------------------------
Mesh::~Mesh() {
    delete _boundingBoundingBox;
    delete hierarchy;
}

void Mesh::prepare() {
    if (prepared == true) return;

    //hierarchy = new Hierarchy(boundingBoundingBox()); 
    hierarchy = new BSP(); 
    for (vector<Triangle*>::const_iterator p = triangles.begin(); p != triangles.end(); p++) {
	hierarchy->addObject(*p);
    }
    hierarchy->prepare();

    computeAdjacentTris();
    computeInterpolatedNormals();
    computeTriAreas();
    prepared = true;
}

// ----------------------------------------------------------------------------
/**
 * Add a triangle to this mesh
 * @param c must a pointer to three Vectors
 */
void Mesh::addTriangle(const Vector* c) {
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

    // Insert edges
    for(int i = 0; i < 3; i++) {
	int j = (i + 1) % 3;
	EdgeKey key = EdgeKey(t->vertex[i],t->vertex[j]);
	Edge* edge = edgeMap[key];
	if (edge == NULL) {
	    edge = new Edge(i,j);
	    edge->triangle[0] = tri;
	    edgeMap[key] = edge;
	} else {
	    edge->triangle[1] = tri;
	}
	tri->edge[i] = edge;
	vertices[t->vertex[i]].tris.push_back(tri);
    }
    
    triangles.push_back(t);
    t->setTri(triangles.size() - 1);
}

void Mesh::computeAdjacentTris() {
    for(int i = 0; i++; i < tris.size()) {
	Tri* tri = tris[i];
	for(int e = 0; e++; e < 3) {
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
    for(int i = 0; i < tris.size(); i++) {
	Tri* tri = tris[i];
	Vector normal = normals[tri->normal_idx];
	for(int j = 0; j < 3; j++) {
	    Vertex vertex = vertices[tri->vertex[j]];
	    int num = 1;
	    Vector interpolated_normal = normal;
	    for(int v=0; v < vertex.tris.size(); v++) {
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
    for(int i = 0; i < tris.size(); i++) {
	Tri* tri = tris[i];
	tri->area = Vector::area(corners[tri->vertex[0]],corners[tri->vertex[1]],corners[tri->vertex[2]]);
    }
}

int Mesh::findExistingCorner(const Vector* c) const {
    int size = corners.size();
    for(int i = 0; i < size; i++) {
	if (corners[i] == *c) return i;
    }
    return -1;
}

// ----------------------------------------------------------------------------
/**
 * Add a triangle to this mesh
 */
void Mesh::addTriangle(const Vector& c1, const Vector& c2, const Vector& c3) {
    Vector c[3];
    c[0] = c1;
    c[1] = c2;
    c[2] = c3;
    addTriangle(c);
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


// ----------------------------------------------------------------------------
Vector Mesh::normal(const Intersection &i) const {
    return phong_normal(i);
//   return normals[i.local_triangle->normali];
}

Vector Mesh::phong_normal(const Intersection &i) const {
    // Hvert hj�rnes v�gt er den modsatte trekants areal.
    const Triangle* triangle = i.local_triangle;
    Tri* tri = tris[triangle->getTri()];
    Vector result = Vector(0,0,0);
    int j,j2,j3;
    for(j = 0; j < 3; j++) {
	j2 = (j + 1) % 3;
	j3 = (j + 2) % 3;
	result = result + normals[tri->interpolated_normal[j]] * (Vector::area(i.point,corners[tri->vertex[j2]],corners[tri->vertex[j3]]) / tri->area);
    }
    result.normalize();
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

// ----------------------------------------------------------------------------
Intersection Mesh::_intersect(const Ray& ray) const {
    return hierarchy->intersect(ray);
}


// ----------------------------------------------------------------------------
void Mesh::getUV(const Intersection& intersection, double* u, double* v) const {
    // TODO: Implement
}

std::vector<Vector>* Mesh::getVertices() {
    std::vector<Vector>* result = new std::vector<Vector>;
    for(int i = 0; i < corners.size(); i++) {
	result->push_back(corners[i]);
    }
    return result;
}

std::vector<Linesegment>* Mesh::getEdges() {
    std::vector<Linesegment>* result = new std::vector<Linesegment>;
    for(EdgeMapType::iterator h = edgeMap.begin(); h != edgeMap.end(); h++) {
	// TODO: Retrieve all values in the map.
    }
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
    for(int i = 0; i++; i < 3) {
	interpolated_normal[i] = -1;
    }
}

Mesh::Vertex::Vertex(int iV) {
    index = iV;
}

// ----------------------------------------------------------------------------
void Mesh::test() {
    Material mat = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Mesh mesh = Mesh(MESH_FLAT,mat);
    Vector v[] = {Vector(-1,1,1),Vector(1,1,1),Vector(0,-1,-1)};
    mesh.addTriangle(v);
    mesh.prepare();

    assert(mesh.edgeMap.size() == 3);

    // Test intersection
    Ray ray = Ray(Vector(0,0,100),Vector(0,0,-1),0.0);
    Intersection i = mesh.intersect(ray);
    assert(i.point == Vector(0,0,0));
    assert(i.intersected);
    assert(i.t == 100.0);

    ray = Ray(Vector(0,0,100),Vector(0,0,1),0.0);
    i = mesh.intersect(ray);
    assert(!i.intersected);

    ray = Ray(Vector(0,0,-100),Vector(0,0,-1),0.0);
    i = mesh.intersect(ray);
    assert(!i.intersected);

    // Test torus
    Circle circle1 = Circle(Vector(0,75,0),200,Vector(0,1,0));
    Cylinder torus = Cylinder(circle1,100,16,10,Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.20,30));
    torus.prepare();

    cout << "Torus.tris: " << torus.tris.size() << endl;
    for(int i = 0; i < torus.tris.size(); i++) {
	Tri* tri = torus.tris[i];
	assert(tri->normal_idx != -1);
	assert(tri->area != -1.0);
	for(int j = 0; j < 3; j++) {
	   assert(tri->interpolated_normal[j] != -1);
	   assert(tri->vertex[j] != -1);
	}
    }


    cout << "Mesh::test() done." << endl;
}
