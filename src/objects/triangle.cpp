
#include "triangle.h"
#include "intersection.h"
#include "ray.h"
#include "mesh.h"
#include "math/vector2.h"


Triangle::Triangle(Mesh* m) : Object(NULL) {
    mesh = m;
}

const Material* Triangle::getMaterial() const { 
    return mesh->getMaterial(); 
}

void Triangle::setTri(int tri_idx) {
    _tri_idx = tri_idx; 
}

int Triangle::getTri() const{
    return _tri_idx;
}

#define CROSS(dest,v1,v2) \
          dest[0]=v1[1]*v2[2]-v1[2]*v2[1]; \
          dest[1]=v1[2]*v2[0]-v1[0]*v2[2]; \
          dest[2]=v1[0]*v2[1]-v1[1]*v2[0];
#define DOT(v1,v2) (v1[0]*v2[0]+v1[1]*v2[1]+v1[2]*v2[2])
#define SUB(dest,v1,v2) \
          dest[0]=v1[0]-v2[0]; \
          dest[1]=v1[1]-v2[1]; \
          dest[2]=v1[2]-v2[2]; 

void Triangle::prepare() {
   const Vector& v0 = mesh->cornerAt(vertex[0]);
   vert0[0] = v0[0];
   vert0[1] = v0[1];
   vert0[2] = v0[2];
   const Vector& vert1 = mesh->cornerAt(vertex[1]);
   const Vector& vert2 = mesh->cornerAt(vertex[2]);

   SUB(edge1,vert1,vert0);
   SUB(edge2,vert2,vert0);
}

// ----------------------------------------------------------------------------
Intersection Triangle::_fullIntersect(const Ray& ray, const double t2) const {
    
   // Fast code from http://www.ce.chalmers.se/staff/tomasm/code/
   const Vector& vert0 = mesh->cornerAt(vertex[0]);
   const Vector& vert1 = mesh->cornerAt(vertex[1]);
   const Vector& vert2 = mesh->cornerAt(vertex[2]);
   Vector edge1 = vert1 - vert0;
   Vector edge2 = vert2 - vert0;

   Vector tvec, pvec, qvec;
   double det,inv_det;
   double u,v;
   double t;


   // begin calculating determinant - also used to calculate U parameter 
   CROSS(pvec, ray.getDirection(), edge2);

   // if determinant is near zero, ray lies in plane of triangle 
   det = edge1 * pvec;

   if (IS_ZERO(det))
     return Intersection();
   inv_det = 1.0 / det;

   // calculate distance from vert0 to ray origin 
   tvec =  ray.getOrigin() - vert0;

   // calculate U parameter and test bounds 
   u = (tvec * pvec) * inv_det;
   if (u < 0.0 || u > 1.0)
     return Intersection();

   // prepare to test V parameter 
   CROSS(qvec,tvec,edge1);

   // calculate V parameter and test bounds 
   v = (ray.getDirection() * qvec) * inv_det;
   if (v < 0.0 || u + v > 1.0)
     return Intersection();

   // calculate t, ray intersects triangle 
   t = (edge2 * qvec) * inv_det;

   if (t < EPSILON)
       return Intersection();
   
   Vector2 uv = mesh->getUV(this,u,v);
   Vector normal = mesh->normal(this,u,v);
   return Intersection(ray.getPoint(t),t,normal,uv);
}

// ----------------------------------------------------------------------------
double Triangle::_fastIntersect(const Ray& ray) const {
   /* Fast code from http://www.ce.chalmers.se/staff/tomasm/code/ */
   //const Vector& vert0 = mesh->cornerAt(vertex[0]);

   double tvec[3], pvec[3], qvec[3];

   double det;
   double u,v;

   /* begin calculating determinant - also used to calculate U parameter */
   CROSS(pvec,ray.getDirection(),edge2);

   /* if determinant is near zero, ray lies in plane of triangle */
   det = DOT(edge1,pvec);

   if (det < EPSILON)
   {
       return -1.0;
   } else {
      /* calculate distance from vert0 to ray origin */
      SUB(tvec,ray.getOrigin(),vert0)
      
      /* calculate U parameter and test bounds */
      u = DOT(tvec,pvec);

      if (u < 0.0 || u > det)
	 return -1.0;
      
      /* prepare to test V parameter */
      CROSS(qvec,tvec,edge1);
      
      /* calculate V parameter and test bounds */
      v = DOT(ray.getDirection(),qvec);
      if (v < 0.0 || u + v > det)
	 return -1.0;
   }
#if 0  
   // Backface culling
   else if (det < -EPSILON)
   {
      // calculate distance from vert0 to ray origin
      tvec = ray.getOrigin() - vert0;
      
      // calculate U parameter and test bounds
      u = tvec * pvec;
      if (u > 0.0 || u < det)
	 return -1;
      
      // prepare to test V parameter
      CROSS(qvec,tvec,edge1);
      
      // calculate V parameter and test bounds
      v = ray.getDirection() * qvec;
      if (v > 0.0 || u + v < det)
	 return -1;
   } 
   else
   {
       return -1;  /* ray is parallell to the plane of the triangle */
   }
#endif   

   /* calculate t, ray intersects triangle */
   return DOT(edge2,qvec) / det;
}
    
BoundingBox Triangle::boundingBoundingBox() const {
    Vector tri[3];
    tri[0] = mesh->cornerAt(vertex[0]);
    tri[1] = mesh->cornerAt(vertex[1]);
    tri[2] = mesh->cornerAt(vertex[2]);
    BoundingBox b = BoundingBox::enclosure(tri,3);
    b.grow(20.0 * EPSILON);
    return b;
}

double Triangle::area() const {
    Vector v[3];
    v[0] = mesh->cornerAt(vertex[0]);
    v[1] = mesh->cornerAt(vertex[1]);
    v[2] = mesh->cornerAt(vertex[2]);
    return 2.0 * Vector::area(v[0],v[1],v[2]);
}
