
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
   const Vector& vert0 = mesh->cornerAt(vertex[0]);
   const Vector& vert1 = mesh->cornerAt(vertex[1]);
   const Vector& vert2 = mesh->cornerAt(vertex[2]);

   edge1 = vert1 - vert0;
   edge2 = vert2 - vert0;
}

// ----------------------------------------------------------------------------
Intersection Triangle::_fullIntersect(const Ray& ray, const double t2) const {
    
   // Fast code from http://www.ce.chalmers.se/staff/tomasm/code/
   const Vector& vert0 = mesh->cornerAt(vertex[0]);
   const Vector& vert1 = mesh->cornerAt(vertex[1]);
   const Vector& vert2 = mesh->cornerAt(vertex[2]);

   Vector edge1, edge2, tvec, pvec, qvec;
   double det,inv_det;
   double u,v;
   double t;

   // find vectors for two edges sharing vert0 
   edge1 = vert1 - vert0;
   edge2 = vert2 - vert0;

   // begin calculating determinant - also used to calculate U parameter 
   pvec = Vector::xProduct(ray.getDirection(), edge2);

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
   qvec = Vector::xProduct(tvec, edge1);

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
   const Vector& vert0 = mesh->cornerAt(vertex[0]);

   Vector tvec, pvec, qvec;
   double det;
   double u,v;
   double t;

#if 0   
   // Dette er den original kode 
   double inv_det;

   /* begin calculating determinant - also used to calculate U parameter */
   pvec = Vector::xProduct(ray.getDirection(), edge2);

   /* if determinant is near zero, ray lies in plane of triangle */
   det = edge1 * pvec;

   if (IS_ZERO(det)) {
     return -1.0;
   }

   inv_det = 1.0 / det;

   /* calculate distance from vert0 to ray origin */
   tvec =  ray.getOrigin() - vert0;

   /* calculate U parameter and test bounds */
   u = (tvec * pvec) * inv_det;
   if (u < 0.0 || u > 1.0)
     return -1.0;

   /* prepare to test V parameter */
   qvec = Vector::xProduct(tvec, edge1);

   /* calculate V parameter and test bounds */
   v = (ray.getDirection() * qvec) * inv_det;
   if (v < 0.0 || u + v > 1.0)
     return -1.0;

   /* calculate t, ray intersects triangle */
   t = (edge2 * qvec) * inv_det;
#else 
   // Denne version udskyder divisionen til sidst.

   /* begin calculating determinant - also used to calculate U parameter */
   pvec = Vector::xProduct(ray.getDirection(), edge2);

   /* if determinant is near zero, ray lies in plane of triangle */
   det = edge1 * pvec;

   if (det > EPSILON)
   {
      /* calculate distance from vert0 to ray origin */
      //SUB(tvec, orig, vert0);
      tvec = ray.getOrigin() - vert0;
      
      /* calculate U parameter and test bounds */
      u = tvec * pvec;
      if (u < 0.0 || u > det)
	 return -1;
      
      /* prepare to test V parameter */
      qvec = Vector::xProduct(tvec,edge1);
      
      /* calculate V parameter and test bounds */
      v = ray.getDirection() * qvec;
      if (v < 0.0 || u + v > det)
	 return -1;
    /*  
   
   }
   else if (det < -EPSILON)
   {
      // calculate distance from vert0 to ray origin
      tvec = ray.getOrigin() - vert0;
      
      // calculate U parameter and test bounds
      u = tvec * pvec;
      if (u > 0.0 || u < det)
	 return -1;
      
      // prepare to test V parameter
      qvec = Vector::xProduct(tvec,edge1);
      
      // calculate V parameter and test bounds
      v = ray.getDirection() * qvec;
      if (v > 0.0 || u + v < det)
	 return -1;
*/
   } else {
       return -1;  /* ray is parallell to the plane of the triangle */
   }

   /* calculate t, ray intersects triangle */
   t = (edge2 * qvec) / det;
#endif

   if (t < EPSILON) {
       return -1.0;
   } else {
       return t;
   }
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

