
#include "triangle.h"
#include "intersection.h"
#include "ray.h"
#include "mesh.h"
#include "math/vector2.h"


Triangle::Triangle(Mesh* m) {
    mesh = m;
}

Material Triangle::getMaterial() const { 
    return mesh->getMaterial(); 
}

void Triangle::setTri(int tri_idx) {
    _tri_idx = tri_idx; 
}

int Triangle::getTri() const{
    return _tri_idx;
}

// ----------------------------------------------------------------------------
Intersection Triangle::_intersect(const Ray& ray) const {
   /* Fast code from http://www.ce.chalmers.se/staff/tomasm/code/ */
   Vector vert0 = mesh->cornerAt(vertex[0]);
   Vector vert1 = mesh->cornerAt(vertex[1]);
   Vector vert2 = mesh->cornerAt(vertex[2]);

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
 //  intersection.u = u;
 //  intersection.v = v;
   intersection.local_object = this;
   return intersection;
}
    
Vector Triangle::normal(const Intersection &i) const {
    return mesh->normal(i);
};

bool Triangle::intersects(const BoundingBox& bb) const {
    // TODO: Implement an algorithm from http://www.realtimerendering.com/int/
    Vector* c = boundingBoundingBox().getCorners();
    bool result = false;
    for(int i = 0; i < 8; i++) {
	if (bb.inside(c[i]) || bb.onEdge(c[i]))
	    result = true;
    }
    delete [] c;
    return result;
}

BoundingBox Triangle::boundingBoundingBox() const {
    Vector tri[3];
    tri[0] = mesh->cornerAt(vertex[0]);
    tri[1] = mesh->cornerAt(vertex[1]);
    tri[2] = mesh->cornerAt(vertex[2]);
    return BoundingBox::enclosure(tri,3);
}
	
Vector2 Triangle::getUV(const Intersection& intersection) const {
    return mesh->getUV(intersection);
}

