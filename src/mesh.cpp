
#include <iostream>
#include <vector>

#include "mesh.h"
#include "boundingbox.h"
#include "constants.h"
#include "rgb.h"
#include "vector.h"
#include "matrix.h"
#include "intersection.h"
#include "ray.h"
#include "sphere.h"

using namespace std;

Mesh::Mesh(MeshType type, Material mat) {
    meshType = type;
    material = mat;
    _boundingBoundingBox = NULL;
}

Mesh::~Mesh() {
    delete _boundingBoundingBox;
}

/**
 * Add a triangle to this mesh
 * @param c must a pointer to three Vectors
 */
void Mesh::addTriangle(const Vector* c) {
    Vector normal = Vector::xProduct(c[1] - c[0], c[2] - c[0]);
    normal.normalize();
    normals.push_back(normal);
    Triangle t;
    t.normal = normals.size() - 1;

    for(int i = 0; i < 3; i++) {
	corners.push_back(c[i]);
	t.vertex[i] = corners.size() - 1;
    }
    triangles.push_back(t);
}

void Mesh::addTriangle(const Vector& c1, const Vector& c2, const Vector& c3) {
    Vector c[3];
    c[0] = c1;
    c[1] = c2;
    c[2] = c3;
    addTriangle(c);
}

void Mesh::transform(const Matrix& M) {
    for (vector<Vector>::iterator p = corners.begin(); p != corners.end(); p++) {
	(*p) = M * (*p);
    }
    Matrix rot = M.extractRotation();
    for (vector<Vector>::iterator p = normals.begin(); p != normals.end(); p++) {
	(*p) =  rot * (*p);
    }
}


Vector Mesh::normal(const Intersection &i) {
    return normals[i.local_triangle->normal];
}

bool Mesh::onEdge(const Vector &p) {
    // TODO: implement
}

bool Mesh::inside(const Vector &p) {
    // TODO: implement
}

bool Mesh::intersects(const BoundingBox& box) {
    // Quick hackish implementation: wrap mesh in a sphere and check that for intersection
    Vector center;
    for (vector<Vector>::iterator p = corners.begin(); p != corners.end(); p++) {
	center = center + (*p);
    }
    center = center / corners.size();
    double radius = 0;
    for (vector<Vector>::iterator p = corners.begin(); p != corners.end(); p++) {
       Vector v = ((*p) - center);
       double l = v.length();
       if (l > radius) radius = l;
    }
    Sphere s = Sphere(center,radius,material);
    return s.intersects(box);
    
}

Material Mesh::getMaterial() {
    return material;
}

BoundingBox Mesh::boundingBoundingBox() {
    if (_boundingBoundingBox != NULL)
	return *_boundingBoundingBox;

    Vector mini = Vector(HUGE_DOUBLE,HUGE_DOUBLE,HUGE_DOUBLE);
    Vector maxi = Vector(-HUGE_DOUBLE,-HUGE_DOUBLE,-HUGE_DOUBLE);
    for (vector<Vector>::iterator p = corners.begin(); p != corners.end(); p++) {
	Vector v = (*p);
	for (int i = 0; i < 3; i++) {
	    mini[i] = min(mini[i],v[i]);
	    maxi[i] = max(maxi[i],v[i]);
	}
    }
    _boundingBoundingBox = new BoundingBox(mini,maxi);
    return *_boundingBoundingBox;
}

RGB Mesh::getDiffuseColor(const Vector& p) {
    RGB col = material.getDiffuseColor();
    return col;
};

Intersection Mesh::_intersect(const Ray& ray) {
    Intersection result;
    Intersection tmp;
    for (int i = 0; i < triangles.size(); i++) {
	Triangle *p =  &triangles[i];

	tmp = intersect_triangle(ray,
		corners[p->vertex[0]],
		corners[p->vertex[1]],
		corners[p->vertex[2]]);
	if (tmp.intersected && (tmp.t < result.t || !result.intersected)) {
	    result = tmp;
	    result.local_triangle = p;
	}
    }
    return result;
}

/* Fast code from http://www.ce.chalmers.se/staff/tomasm/code/ */
Intersection Mesh::intersect_triangle(const Ray& ray,
                   Vector vert0, Vector vert1, Vector vert2) {
   Vector edge1, edge2, tvec, pvec, qvec;
   double det,inv_det;
   double u,v;
   double t;

   Intersection intersection;
   Vector orig = ray.origin;
   Vector dir = ray.direction;

   /* find vectors for two edges sharing vert0 */
   edge1 = vert1 - vert0;
   edge2 = vert2 - vert0;

   /* begin calculating determinant - also used to calculate U parameter */
   pvec = Vector::xProduct(dir, edge2);

   /* if determinant is near zero, ray lies in plane of triangle */
   det = edge1 * pvec;

   if (IS_ZERO(det))
     return intersection;
   inv_det = 1.0 / det;

   /* calculate distance from vert0 to ray origin */
   tvec =  orig - vert0;

   /* calculate U parameter and test bounds */
   u = (tvec * pvec) * inv_det;
   if (u < 0.0 || u > 1.0)
     return intersection;

   /* prepare to test V parameter */
   qvec = Vector::xProduct(tvec, edge1);

   /* calculate V parameter and test bounds */
   v = (dir * qvec) * inv_det;
   if (v < 0.0 || u + v > 1.0)
     return intersection;

   /* calculate t, ray intersects triangle */
   t = (edge2 * qvec) * inv_det;

   if (t < EPSILON)
       return intersection;

   intersection = Intersection(orig + t*dir,t);
   intersection.u = u;
   intersection.v = v;
   return intersection;
}

void Mesh::getUV(const Intersection& intersection, double* u, double* v) {
    // TODO: Implement
}

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
