
#include <iostream>
#include <vector>
#include <cassert>

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

void Mesh::prepare() const {
    hierarchy = new Hierarchy(boundingBoundingBox()); 
    //hierarchy = new BSP(); 
    for (vector<Triangle*>::const_iterator p = triangles.begin(); p != triangles.end(); p++) {
	hierarchy->addObject(*p);
    }
    hierarchy->prepare();
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
	corners.push_back(c[i]);
	t->vertex[i] = corners.size() - 1;
    }
    triangles.push_back(t);
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
    return normals[i.local_triangle->normali];
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
    if (!prepared) prepare();

    return hierarchy->intersect(ray);

    /*
    hierarchy
    Intersection result;
    Intersection tmp;
    for (int i = 0; i < triangles.size(); i++) {
	const Triangle *p =  triangles[i];
	tmp = p->intersect(ray);

	if (tmp.intersected && (tmp.t < result.t || !result.intersected)) {
	    result = tmp;
	    result.local_triangle = p;
	}
    }
    return result;
    */
}


// ----------------------------------------------------------------------------
void Mesh::getUV(const Intersection& intersection, double* u, double* v) const {
    // TODO: Implement
}

// ----------------------------------------------------------------------------
void Mesh::test() {
    Material mat = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Mesh mesh = Mesh(MESH_FLAT,mat);
    Vector v[] = {Vector(-1,1,1),Vector(1,1,1),Vector(0,-1,-1)};
    mesh.addTriangle(v);
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

    cout << "Mesh::test() done." << endl;

}
