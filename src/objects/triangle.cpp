
#include "triangle.h"
#include "intersection.h"
#include "ray.h"
#include "mesh.h"
#include "math/vector2.h"
#include <math.h>
#include <stdio.h>
#include "boundingbox.h"



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

   if (det > EPSILON) {
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
      SUB(tvec,ray.getOrigin(),vert0)
      
      // calculate U parameter and test bounds
      u = DOT(tvec,pvec);
      if (u > 0.0 || u < det)
	 return -1;
      
      // prepare to test V parameter
      CROSS(qvec,tvec,edge1);
      
      // calculate V parameter and test bounds
      v = DOT(ray.getDirection(),qvec);
      if (v > 0.0 || u + v < det)
	 return -1;
   } 
#endif   
   else
   {
       return -1;  /* ray is parallell to the plane of the triangle */
   }

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


/********************************************************/
/* AABB-triangle overlap test code                      */
/* by Tomas Akenine-Möller                              */
/* Function: int triBoxOverlap(float boxcenter[3],      */
/*          float boxhalfsize[3],float triverts[3][3]); */
/* History:                                             */
/*   2001-03-05: released the code in its first version */
/*   2001-06-18: changed the order of the tests, faster */
/*                                                      */
/* Acknowledgement: Many thanks to Pierre Terdiman for  */
/* suggestions and discussions on how to optimize code. */
/* Thanks to David Hunt for finding a ">="-bug!         */
/********************************************************/

#define X 0
#define Y 1
#define Z 2

#define FINDMINMAX(x0,x1,x2,min,max) \
  min = max = x0;   \
  if(x1<min) min=x1;\
  if(x1>max) max=x1;\
  if(x2<min) min=x2;\
  if(x2>max) max=x2;

int planeBoxOverlap(float normal[3],float d, Vector maxbox)
{
  int q;
  float vmin[3],vmax[3];
  for(q=X;q<=Z;q++)
  {
    if(normal[q]>0.0f)
    {
      vmin[q]=-maxbox[q];
      vmax[q]=maxbox[q];
    }
    else
    {
      vmin[q]=maxbox[q];
      vmax[q]=-maxbox[q];
    }
  }
  if(DOT(normal,vmin)+d>0.0f) return 0;
  if(DOT(normal,vmax)+d>=0.0f) return 1;

  return 0;
}


/*======================== X-tests ========================*/
#define AXISTEST_X01(a, b, fa, fb)             \
    p0 = a*v0[Y] - b*v0[Z];                    \
    p2 = a*v2[Y] - b*v2[Z];                    \
        if(p0<p2) {min=p0; max=p2;} else {min=p2; max=p0;} \
    rad = fa * boxhalfsize[Y] + fb * boxhalfsize[Z];   \
    if(min>rad || max<-rad) return 0;

#define AXISTEST_X2(a, b, fa, fb)              \
    p0 = a*v0[Y] - b*v0[Z];                    \
    p1 = a*v1[Y] - b*v1[Z];                    \
        if(p0<p1) {min=p0; max=p1;} else {min=p1; max=p0;} \
    rad = fa * boxhalfsize[Y] + fb * boxhalfsize[Z];   \
    if(min>rad || max<-rad) return 0;

/*======================== Y-tests ========================*/
#define AXISTEST_Y02(a, b, fa, fb)             \
    p0 = -a*v0[X] + b*v0[Z];                   \
    p2 = -a*v2[X] + b*v2[Z];                       \
        if(p0<p2) {min=p0; max=p2;} else {min=p2; max=p0;} \
    rad = fa * boxhalfsize[X] + fb * boxhalfsize[Z];   \
    if(min>rad || max<-rad) return 0;

#define AXISTEST_Y1(a, b, fa, fb)              \
    p0 = -a*v0[X] + b*v0[Z];                   \
    p1 = -a*v1[X] + b*v1[Z];                       \
        if(p0<p1) {min=p0; max=p1;} else {min=p1; max=p0;} \
    rad = fa * boxhalfsize[X] + fb * boxhalfsize[Z];   \
    if(min>rad || max<-rad) return 0;

/*======================== Z-tests ========================*/

#define AXISTEST_Z12(a, b, fa, fb)             \
    p1 = a*v1[X] - b*v1[Y];                    \
    p2 = a*v2[X] - b*v2[Y];                    \
        if(p2<p1) {min=p2; max=p1;} else {min=p1; max=p2;} \
    rad = fa * boxhalfsize[X] + fb * boxhalfsize[Y];   \
    if(min>rad || max<-rad) return 0;

#define AXISTEST_Z0(a, b, fa, fb)              \
    p0 = a*v0[X] - b*v0[Y];                \
    p1 = a*v1[X] - b*v1[Y];                    \
        if(p0<p1) {min=p0; max=p1;} else {min=p1; max=p0;} \
    rad = fa * boxhalfsize[X] + fb * boxhalfsize[Y];   \
    if(min>rad || max<-rad) return 0;

int triBoxOverlap(Vector boxcenter,Vector boxhalfsize,Vector triverts[3])
{

  /*    use separating axis theorem to test overlap between triangle and box */
  /*    need to test for overlap in these directions: */
  /*    1) the {x,y,z}-directions (actually, since we use the AABB of the triangle */
  /*       we do not even need to test these) */
  /*    2) normal of the triangle */
  /*    3) crossproduct(edge from tri, {x,y,z}-directin) */
  /*       this gives 3x3=9 more tests */
   float v0[3],v1[3],v2[3];
   float min,max,d,p0,p1,p2,rad,fex,fey,fez;
   float normal[3],e0[3],e1[3],e2[3];

   /* This is the fastest branch on Sun */
   /* move everything so that the boxcenter is in (0,0,0) */
   SUB(v0,triverts[0],boxcenter);
   SUB(v1,triverts[1],boxcenter);
   SUB(v2,triverts[2],boxcenter);

   /* compute triangle edges */
   SUB(e0,v1,v0);      /* tri edge 0 */
   SUB(e1,v2,v1);      /* tri edge 1 */
   SUB(e2,v0,v2);      /* tri edge 2 */

   /* Bullet 3:  */
   /*  test the 9 tests first (this was faster) */
   fex = fabs(e0[X]);
   fey = fabs(e0[Y]);
   fez = fabs(e0[Z]);
   AXISTEST_X01(e0[Z], e0[Y], fez, fey);
   AXISTEST_Y02(e0[Z], e0[X], fez, fex);
   AXISTEST_Z12(e0[Y], e0[X], fey, fex);

   fex = fabs(e1[X]);
   fey = fabs(e1[Y]);
   fez = fabs(e1[Z]);
   AXISTEST_X01(e1[Z], e1[Y], fez, fey);
   AXISTEST_Y02(e1[Z], e1[X], fez, fex);
   AXISTEST_Z0(e1[Y], e1[X], fey, fex);

   fex = fabs(e2[X]);
   fey = fabs(e2[Y]);
   fez = fabs(e2[Z]);
   AXISTEST_X2(e2[Z], e2[Y], fez, fey);
   AXISTEST_Y1(e2[Z], e2[X], fez, fex);
   AXISTEST_Z12(e2[Y], e2[X], fey, fex);

   /* Bullet 1: */
   /*  first test overlap in the {x,y,z}-directions */
   /*  find min, max of the triangle each direction, and test for overlap in */
   /*  that direction -- this is equivalent to testing a minimal AABB around */
   /*  the triangle against the AABB */

   /* test in X-direction */
   FINDMINMAX(v0[X],v1[X],v2[X],min,max);
   if(min>boxhalfsize[X] || max<-boxhalfsize[X]) return -1;

   /* test in Y-direction */
   FINDMINMAX(v0[Y],v1[Y],v2[Y],min,max);
   if(min>boxhalfsize[Y] || max<-boxhalfsize[Y]) return -1;

   /* test in Z-direction */
   FINDMINMAX(v0[Z],v1[Z],v2[Z],min,max);
   if(min>boxhalfsize[Z] || max<-boxhalfsize[Z]) return -1;

   /* Bullet 2: */
   /*  test if the box intersects the plane of the triangle */
   /*  compute plane equation of triangle: normal*x+d=0 */
   CROSS(normal,e0,e1);
   d=-DOT(normal,v0);  /* plane eq: normal.x+d=0 */
   if(!planeBoxOverlap(normal,d,boxhalfsize)) return -1;

   return 1;   /* box and triangle overlaps */
}

int Triangle::intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const {
    Vector triverts[3];
    triverts[0] = mesh->cornerAt(vertex[0]);
    triverts[1] = mesh->cornerAt(vertex[1]);
    triverts[2] = mesh->cornerAt(vertex[2]);
    Vector boxcenter = voxel_bbox.center();
    Vector boxhalfsize = 0.5 * (voxel_bbox.maximum() - voxel_bbox.minimum());
    return triBoxOverlap(boxcenter,boxhalfsize,triverts);
}

