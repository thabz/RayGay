
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

Intersection Triangle::_intersect(const Ray& ray) const {
    // Never called
    exit(0);
    return Intersection();
}

void Triangle::prepare() {
   vert0 = mesh->cornerAt(vertex[0]);
   const Vector& vert1 = mesh->cornerAt(vertex[1]);
   const Vector& vert2 = mesh->cornerAt(vertex[2]);

   edge1 = vert1 - vert0;
   edge2 = vert2 - vert0;
}

#define CROSS(dest,v1,v2) \
          dest[0]=v1[1]*v2[2]-v1[2]*v2[1]; \
          dest[1]=v1[2]*v2[0]-v1[0]*v2[2]; \
          dest[2]=v1[0]*v2[1]-v1[1]*v2[0];
// ----------------------------------------------------------------------------
Intersection Triangle::_fullIntersect(const Ray& ray, const double t2) const {
    
   // Fast code from http://www.ce.chalmers.se/staff/tomasm/code/

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
   
   Intersection intersection = Intersection(ray.getOrigin() + t*ray.getDirection(),t);
   //intersection.setLocalObject(this);
   intersection.u = u;
   intersection.v = v;
   return intersection;
}

// ----------------------------------------------------------------------------
double Triangle::_fastIntersect(const Ray& ray) const {
   /* Fast code from http://www.ce.chalmers.se/staff/tomasm/code/ */
   //const Vector& vert0 = mesh->cornerAt(vertex[0]);

   Vector tvec, pvec, qvec;
   double det;
   double u,v;

   /* begin calculating determinant - also used to calculate U parameter */
   CROSS(pvec,ray.getDirection(),edge2);

   /* if determinant is near zero, ray lies in plane of triangle */
   det = edge1 * pvec;

   if (det < EPSILON)
   {
       return -1;
   } else {
      /* calculate distance from vert0 to ray origin */
      tvec = ray.getOrigin() - vert0;
      
      /* calculate U parameter and test bounds */
      u = tvec * pvec;
      if (u < 0.0 || u > det)
	 return -1;
      
      /* prepare to test V parameter */
      CROSS(qvec,tvec,edge1);
      
      /* calculate V parameter and test bounds */
      v = ray.getDirection() * qvec;
      if (v < 0.0 || u + v > det)
	 return -1;
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
   double inv_det = 1.0 / det;
   return (edge2 * qvec) * inv_det;
}
    
Vector Triangle::normal(const Intersection &i) const {
    return mesh->normal(this,i);
};

BoundingBox Triangle::boundingBoundingBox() const {
    Vector tri[3];
    tri[0] = mesh->cornerAt(vertex[0]);
    tri[1] = mesh->cornerAt(vertex[1]);
    tri[2] = mesh->cornerAt(vertex[2]);
    BoundingBox b = BoundingBox::enclosure(tri,3);
    b.grow(5*EPSILON);
    return b;
}
	
Vector2 Triangle::getUV(const Intersection& intersection) const {
    return mesh->getUV(this,intersection);
}

