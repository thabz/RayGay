#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <cassert>
#include <iostream>

#include "boundingbox.h"
#include "objects/sphere.h"
#include "objects/solidbox.h"
#include "objects/cone.h"
#include "objects/csg.h"
#include "objects/cylinder.h"
#include "objects/mesh.h"
#include "objects/extrusion.h"
#include "objects/ellipsoid.h"
#include "objects/superellipsoid.h"
#include "objects/box.h"
#include "objects/necklace.h"
#include "objects/transformedinstance.h"
#include "objects/torus.h"
#include "objects/3ds.h"
#include "objects/tetrahedron.h"
#include "objects/tessalation.h"
#include "space/kdtree.h"
#include "paths/linesegment.h"
#include "paths/circle.h"

bool intersects(Object* o, const Ray& ray) {
    return o->fastIntersect(ray) > 0;
}
bool intersects(Object* o, const Vector& origin, const Vector& dir) {
    Ray ray = Ray(origin,dir,1);
    return intersects(o,ray);
}

Vector iPoint(Object* o, const Ray& ray) {
    double t = o->fastIntersect(ray);
    assert(t > 0);
    return o->fullIntersect(ray,t).getPoint();
}

Vector iPoint(Object* o, const Vector& origin, const Vector& dir) {
    Ray ray = Ray(origin,dir,1);
    return iPoint(o,ray);
}

Vector iNormal(Object* o, const Ray& ray) {
    double t = o->fastIntersect(ray);
    assert(t > 0);
    Intersection i = o->fullIntersect(ray,t);
    return i.getNormal();
}

Vector iNormal(Object* o, const Vector& origin, const Vector& dir) {
    Ray ray = Ray(origin,dir,1);
    return iNormal(o,ray);
}


void sphere_test() {
    Material* m = new Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Sphere s = Sphere(Vector(0,0,0),10.0,m);

    s = Sphere(Vector(0,0,0),60.0,m);

    /* Test intersection(ray) */
    //assert(intersects(&s,Vector(0,0,1000),Vector(0,0,-1)));
    //assert(IS_EQUAL(s.getLastIntersection()->getPoint()[2],60.0));
    
    assert(intersects(&s,Vector(0,0,1000),Vector(0,0,-1)));
    assert(IS_EQUAL(iPoint(&s,Vector(0,0,1000),Vector(0,0,-1))[2],60.0));

    //r = Ray(Vector(0,0,0),Vector(0,0,-1),1);
   // assert(s.intersect(r));
 //   assert(IS_EQUAL( s.getLastIntersection()->getPoint()[2], -60.0));

    assert(intersects(&s,Vector(0,0,0),Vector(0,0,-1)));
    assert(IS_EQUAL(iPoint(&s,Vector(0,0,0),Vector(0,0,-1))[2],-60.0));

    assert(intersects(&s,Vector(0,0,-1000),Vector(0,0,1)));
    assert(IS_EQUAL(iPoint(&s,Vector(0,0,-1000),Vector(0,0,1))[2],-60.0));

    assert(intersects(&s,Vector(0,0,-100),Vector(0,0,-1)) == false);

    assert(intersects(&s,Vector(0,0,-60),Vector(0,0,-1)) == false);

    /* Test boundingBoundingBox() */
    s = Sphere(Vector(0,0,0),20.0,m);
    assert(s.boundingBoundingBox().inside(BoundingBox(Vector(-20,-20,-20),Vector(20,20,20))));

    /* Test clone */
    Object* s1 = new Sphere(Vector(0,0,0),20,m);
    Object* s2 = dynamic_cast<Object*>(s1->clone());
    s1->transform(Matrix::matrixTranslate(Vector(0,-100,0)));
    s2->transform(Matrix::matrixTranslate(Vector(0,100,0)));
    assert(intersects(s1,Vector(0,-100,-1000),Vector(0,0,1)) == true);
    assert(intersects(s2,Vector(0,-100,-1000),Vector(0,0,1)) == false);
    assert(intersects(s1,Vector(0,100,-1000),Vector(0,0,1)) == false);
    assert(intersects(s2,Vector(0,100,-1000),Vector(0,0,1)) == true);

    /* Test AllIntersections */
    s = Sphere(Vector(0,0,0),20.0,m);
    vector<Intersection> result;
    Ray ray = Ray(Vector(0,0,100),Vector(0,0,-1),-1);
    s.allIntersections(ray,result);
    assert(result.size() == 2);
    assert(result[0].getPoint() == Vector(0,0,20));
    assert(result[0].isEntering() == true);
    assert(result[1].getPoint() == Vector(0,0,-20));
    assert(result[1].isEntering() == false);

    ray = Ray(Vector(0,0,-100),Vector(0,0,1),-1);
    result.clear();
    s.allIntersections(ray,result);
    assert(result.size() == 2);
    assert(result[0].getPoint() == Vector(0,0,-20));
    assert(result[0].isEntering() == true);
    assert(result[1].getPoint() == Vector(0,0,20));
    assert(result[1].isEntering() == false);

    ray = Ray(Vector(0,0,0),Vector(0,0,1),-1);
    result.clear();
    s.allIntersections(ray,result);
    assert(result.size() == 1);
    assert(result[0].getPoint() == Vector(0,0,20));
    assert(result[0].isEntering() == false);

    ray = Ray(Vector(0,0,0),Vector(0,0,-1),-1);
    result.clear();
    s.allIntersections(ray,result);
    assert(result.size() == 1);
    assert(result[0].getPoint() == Vector(0,0,-20));
    assert(result[0].isEntering() == false);

}

void box_test() {
    Material* m = new Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Box* b = new Box(Vector(-1,-1,-1),Vector(1,1,1),m);
    b->prepare();
    assert(b->getVertices()->size() == 8);

    KdTree* bsp = new KdTree();
    b->addSelf(bsp);
    bsp->prepare();
    Intersection* inter = new Intersection();
    Ray r = Ray(Vector(0,0,100),Vector(0,0,-1),1);
    assert(bsp->intersect(r,inter));
    assert(inter->getPoint() == Vector(0,0,1));
    assert(inter->getUV() == Vector2(0.5,0.5));

    r = Ray(Vector(0,-100,0),Vector(0,1,0),1);
    assert(bsp->intersect(r,inter));
    assert(inter->getPoint() == Vector(0,-1,0));

    /* Test second constructor */
    b = new Box(Vector(0,0,0),2,2,2,m);
    b->prepare();
    assert(b->getVertices()->size() == 8);

    bsp = new KdTree();
    b->addSelf(bsp);
    bsp->prepare();

    r = Ray(Vector(0,0,100),Vector(0,0,-1),1);
    assert(bsp->intersect(r,inter));
    assert(inter->getPoint() == Vector(0,0,1));

    r = Ray(Vector(0,-100,0),Vector(0,1,0),1);
    assert(bsp->intersect(r,inter));
    assert(inter->getPoint() == Vector(0,-1,0));

    r = Ray(Vector(0,-100,1.5),Vector(0,1,0),1);
    assert(bsp->intersect(r,inter) == false);

    /* test clone() */
    b = new Box(Vector(-1,-1,-1),Vector(1,1,1),m);
    SceneObject* b2 = b->clone();
    assert(b2 != NULL);
    b->transform(Matrix::matrixTranslate(Vector(0,10,0)));
    b->prepare();
    b2->prepare();
    bsp = new KdTree();
    b->addSelf(bsp);
    b2->addSelf(bsp);
    bsp->prepare();

    Intersection i;
    r = Ray(Vector(0,0,100),Vector(0,0,-1),1);
    assert(bsp->intersect(r,&i));
    r = Ray(Vector(0,10,100),Vector(0,0,-1),1);
    assert(bsp->intersect(r,&i));
    r = Ray(Vector(0,5,100),Vector(0,0,-1),1);
    assert(!bsp->intersect(r,&i));
}

void mesh_test() {
}

void tetrahedron_test() {
    Material* mat = new Material(RGB(.0,.0,.0),RGB(.0,.0,.0));
    Tetrahedron t = Tetrahedron(Vector(0,0,0),100,mat);
    assert(t.getEdges()->size() == 6);
    assert(t.getVertices()->size() == 4);
}

void tesselation_test() {
    Material* mat = new Material(RGB(.0,.0,.0),RGB(.0,.0,.0));

    // 4 triangles
    Tessalation* t = new Tessalation(Vector(0,0,0),100,0,mat);
    assert(t->getEdges()->size() == 6);
    assert(t->getVertices()->size() == 4);

    // 12 triangles
    t = new Tessalation(Vector(0,0,0),100,1,mat);
    assert(t->getVertices()->size() == 8);
    //cout << t->getEdges()->size() << endl;
  //  assert(t->getEdges()->size() == 4 * 3);

    // 36 triangles
    t = new Tessalation(Vector(0,0,0),100,2,mat);
    assert(t->getVertices()->size() == 20);

    // 108 triangles
    t = new Tessalation(Vector(0,0,0),100,3,mat);
    assert(t->getVertices()->size() == 56);
}

void extrusion_test() {

    Material* m = new Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    // Check bounds 
    Vector o = Vector(0,0,0);
    Vector top = Vector(10,0,0);
    BoundingBox b = BoundingBox(Vector(-1,-10,-10),Vector(11,10,10));
	    
    Extrusion* c = new Extrusion(o,top,9.0,5,m);

    c = new Extrusion(top,o,9.0,5,m);

    top = Vector(0,10,0);
    b = BoundingBox(Vector(-10,-1,-10),Vector(10,11,10));
    c = new Extrusion(o,top,5.0,5,m);

    // Check intersection 
    c = new Extrusion(Vector(0,0,0),Vector(0,0,-10),5.0,3,m);
    c->prepare();
    KdTree* bsp = new KdTree();
    c->addSelf(bsp);
    bsp->prepare();
    Intersection i;
    Ray r = Ray(Vector(0.5,0.5,100),Vector(0,0,-1),1);
    assert(bsp->intersect(r,&i));

    // Check generated mesh 
    c = new Extrusion(Vector(0,0,0),Vector(0,0,-10),2.0,5,m);
    c->prepare();
    assert(c->getVertices()->size() == 5*2 + 2);

    Circle circle1 = Circle(Vector(0,75,0),200,Vector(0,1,0));
    Extrusion torus = Extrusion(circle1,100,16,10,new Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.20,30));
    torus.prepare();

    assert(torus.getVertices()->size() == 16*10);

}

void cylinder_test() {
    Material* m = new Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Cylinder* cyl = new Cylinder(Vector(0,0,0),Vector(0,0,10),10,false,m);

    /* Test intersection() of z-axis aligned cylinder */
    
    assert(intersects(cyl,Vector(0,-1000,5),Vector(0,1,0)));
    assert(iPoint(cyl,Vector(0,-1000,5),Vector(0,1,0)) == Vector(0,-10,5));
    assert(iNormal(cyl,Vector(0,-1000,5),Vector(0,1,0)) == Vector(0,-1,0));

    assert(intersects(cyl,Vector(0,1000,5),Vector(0,-1,0)));
    assert(iPoint(cyl,Vector(0,1000,5),Vector(0,-1,0)) == Vector(0,10,5));
    assert(iNormal(cyl,Vector(0,1000,5),Vector(0,-1,0)) == Vector(0,1,0));

    delete cyl;

    // Test a cylinder translated along the z-axis
    cyl = new Cylinder(Vector(0,0,2),Vector(0,0,10),10,false,m);
    delete cyl;
    
    // Test an x-axis aligned cylinder
    cyl = new Cylinder(Vector(2,0,0),Vector(10,0,0),10,false,m);
    assert(intersects(cyl,Vector(3,0,1000),Vector(0,0,-1)));
    assert(iPoint(cyl,Vector(3,0,1000),Vector(0,0,-1)) == Vector(3,0,10));
    assert(iNormal(cyl,Vector(3,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    
    delete cyl;
    
    // Test an y-axis aligned cylinder
    cyl = new Cylinder(Vector(0,2,0),Vector(0,10,0),10,false,m);
    assert(intersects(cyl,Vector(0,3,1000),Vector(0,0,-1)));
    assert(iPoint(cyl,Vector(0,3,1000),Vector(0,0,-1)) == Vector(0,3,10));
    assert(iNormal(cyl,Vector(0,3,1000),Vector(0,0,-1)) == Vector(0,0,1));

    assert(intersects(cyl,Vector(0,0,100),Vector(0,0,-1)) == false);
    delete cyl;

    // test allIntersections
    cyl = new Cylinder(Vector(0,2,0),Vector(0,10,0),10,true,m);
    vector<Intersection> result;
    Ray ray = Ray(Vector(0,5,1000),Vector(0,0,-1),-1);
    cyl->allIntersections(ray,result);
    assert(result.size() == 2);
    assert(result[0].getPoint() == Vector(0,5,10));
    assert(result[1].getPoint() == Vector(0,5,-10));
    
    // Intersect with caps (z-axis aligned)
    cyl = new Cylinder(Vector(0,0,2),Vector(0,0,10),10,true,m);
    ray = Ray(Vector(0,0,50),Vector(0,0,-1),-1);
    result.clear();
    cyl->allIntersections(ray,result);
    assert(result.size() == 2);
    assert(result[0].getPoint() == Vector(0,0,10));
    assert(result[0].getNormal() == Vector(0,0,1));
    assert(result[1].getPoint() == Vector(0,0,2));
    assert(result[1].getNormal() == Vector(0,0,-1));

    // Intersect with caps (y-axis aligned)
    cyl = new Cylinder(Vector(0,2,0),Vector(0,10,0),10,true,m);
    ray = Ray(Vector(0,50,1),Vector(0,-1,0),-1);
    result.clear();
    cyl->allIntersections(ray,result);
    assert(result.size() == 2);
    assert(result[0].getPoint() == Vector(0,10,1));
    assert(result[0].getNormal() == Vector(0,1,0));
    assert(result[1].getPoint() == Vector(0,2,1));
    assert(result[1].getNormal() == Vector(0,-1,0));
    assert(iPoint(cyl,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,10,0));
    assert(iNormal(cyl,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,1,0));
    assert(iPoint(cyl,Vector(1,1000,1),Vector(0,-1,0)) == Vector(1,10,1));
    assert(iNormal(cyl,Vector(1,1000,1),Vector(0,-1,0)) == Vector(0,1,0));

    // Test clone()
    Object* s1 = new Cylinder(Vector(2,0,0),Vector(10,0,0),10,true,m);
    Object* s2 = dynamic_cast<Object*>(s1->clone());
    s1->transform(Matrix::matrixTranslate(Vector(0,-100,0)));
    s2->transform(Matrix::matrixTranslate(Vector(0,100,0)));
    assert(intersects(s1,Vector(5,-100,-1000),Vector(0,0,1)) == true);
    assert(intersects(s2,Vector(5,-100,-1000),Vector(0,0,1)) == false);
    assert(intersects(s1,Vector(5,100,-1000),Vector(0,0,1)) == false);
    assert(intersects(s2,Vector(5,100,-1000),Vector(0,0,1)) == true);

    // Rays with origin on surface
    cyl = new Cylinder(Vector(0,2,0),Vector(0,10,0),10,true,m);
    assert(!intersects(cyl,Vector(0,10,0),Vector(0,1,0)));
    assert(intersects(cyl,Vector(0,10,0),Vector(0,-1,0)));
    assert(iPoint(cyl,Vector(0,10,0),Vector(0,-1,0)) == Vector(0,2,0));
    assert(iPoint(cyl,Vector(0,2,0),Vector(0,1,0)) == Vector(0,10,0));
    delete cyl;

    // Ray with origin inside cylinder
    cyl = new Cylinder(Vector(0,0,2),Vector(0,0,10),10,true,m);
    assert(intersects(cyl,Vector(0,0,5),Vector(0,0,1)));
    assert(iPoint(cyl,Vector(0,0,5),Vector(0,0,1)) == Vector(0,0,10));
    assert(iNormal(cyl,Vector(0,0,5),Vector(0,0,1)) == Vector(0,0,1));
    assert(intersects(cyl,Vector(0,0,5),Vector(0,1,0)));
    assert(iPoint(cyl,Vector(0,0,5),Vector(0,1,0)) == Vector(0,10,5));
    assert(iNormal(cyl,Vector(0,0,5),Vector(0,1,0)) == Vector(0,1,0));

    cyl = new Cylinder(Vector(0,2,0),Vector(0,10,0),10,true,m);
    assert(intersects(cyl,Vector(0,5,0),Vector(0,1,0)));
    assert(iPoint(cyl,Vector(0,5,0),Vector(0,1,0)) == Vector(0,10,0));
    assert(iNormal(cyl,Vector(0,5,0),Vector(0,1,0)) == Vector(0,1,0));

    // Scaled cylinder
    cyl = new Cylinder(Vector(0,0,0),Vector(0,1,0),1,true,m);
    cyl->transform(Matrix::matrixScale(Vector(3,10,2)));
    assert(intersects(cyl,Vector(0,5,100),Vector(0,0,-1)));
    assert(iPoint(cyl,Vector(0,5,100),Vector(0,0,-1)) == Vector(0,5,2));
    assert(iNormal(cyl,Vector(0,5,100),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iPoint(cyl,Vector(1000,5,0),Vector(-1,0,0)) == Vector(3,5,0));
    assert(iNormal(cyl,Vector(1000,5,0),Vector(-1,0,0)) == Vector(1,0,0));
}

void test_3ds() {
    Material* m = new Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    ThreeDS* chair = new ThreeDS("../3ds/egg-chair.3ds",1.0,m);
    assert(chair != NULL);
}

void objectgroup_test() {
    // Test clone()
    Material* m = new Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Linesegment segment = Linesegment(Vector(0,0,0),Vector(0,0,100)); 
    Necklace* g1 = new Necklace(&segment,10,10,m);
    SceneObject* g2 = g1->clone();
    g1->transform(Matrix::matrixTranslate(Vector(0,-100,0)));
    g2->transform(Matrix::matrixTranslate(Vector(0,100,0)));

    KdTree* bsp = new KdTree();
    g1->addSelf(bsp);
    g2->addSelf(bsp);
    bsp->prepare();

    Intersection i;
    Ray r = Ray(Vector(0,100,1000),Vector(0,0,-1),1);
    assert(bsp->intersect(r,&i));
    r = Ray(Vector(0,-100,1000),Vector(0,0,-1),1);
    assert(bsp->intersect(r,&i));
    r = Ray(Vector(0,0,1000),Vector(0,0,-1),1);
    assert(!bsp->intersect(r,&i));
}

void torus_test() {
    Material* m = new Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Torus* t = new Torus(10,1,m);

    // intersect
    Intersection i;
    assert(intersects(t,Vector(0,0,1000),Vector(0,0,-1)));
    assert(iPoint(t,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,11));
    assert(iNormal(t,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));

    Ray ray = Ray(Vector(0,0,-1000),Vector(0,0,1),1);
    assert(intersects(t,ray));
    assert(iPoint(t,ray) == Vector(0,0,-11));
    assert(iNormal(t,ray) == Vector(0,0,-1));

    ray = Ray(Vector(0,1000,0),Vector(0,-1,0),1); // In middle
    assert(!intersects(t,ray));
    ray = Ray(Vector(0,1000,11.1),Vector(0,-1,0),1); // Close by
    assert(!intersects(t,ray));
    ray = Ray(Vector(1000,11.1,0),Vector(-1,0,0),1); // Close by
    assert(!intersects(t,ray));
    delete t;

    // Test allIntersections()
    t = new Torus(10,1,m);
    ray = Ray(Vector(1000,0,0),Vector(-1,0,0),1);
    vector<Intersection> all;
    t->allIntersections(ray,all);
    assert(all.size() == 4);
    assert(all[0].getPoint() == Vector(11,0,0));
    assert(all[0].isEntering() == true);
    assert(all[1].getPoint() == Vector(9,0,0));
    assert(all[1].isEntering() == false);
    assert(all[2].getPoint() == Vector(-9,0,0));
    assert(all[2].isEntering() == true);
    assert(all[3].getPoint() == Vector(-11,0,0));
    assert(all[3].isEntering() == false);

    ray = Ray(Vector(0,1000,0),Vector(0,-1,0),1);
    all.clear();
    t->allIntersections(ray,all);
    assert(all.size() == 0);

    // Test scaled torus
    t = new Torus(10,1,m);
    t->transform(Matrix::matrixScale(Vector(2,3,4)));
    assert(iPoint(t,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,44));
    assert(iNormal(t,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iPoint(t,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-44));
    assert(iNormal(t,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-1));
    assert(iPoint(t,Vector(1000,0,0),Vector(-1,0,0)) == Vector(22,0,0));
    assert(iNormal(t,Vector(1000,0,0),Vector(-1,0,0)) == Vector(1,0,0));
    assert(iPoint(t,Vector(-1000,0,0),Vector(1,0,0)) == Vector(-22,0,0));
    assert(iNormal(t,Vector(-1000,0,0),Vector(1,0,0)) == Vector(-1,0,0));
}

void transformed_instance_test() {
    Sphere* s = new Sphere(Vector(0,0,0),10.0,NULL);
    TransformedInstance* t1 = new TransformedInstance(s);
    t1->transform(Matrix::matrixTranslate(Vector(100,0,0)));
    TransformedInstance* t2 = new TransformedInstance(s);
    t2->transform(Matrix::matrixTranslate(Vector(-100,0,0)));
    assert(intersects(t1,Vector(100,0,1000),Vector(0,0,-1)));
    assert(!intersects(t1,Vector(0,0,1000),Vector(0,0,-1)));

    assert(intersects(t2,Vector(-100,0,1000),Vector(0,0,-1)));
    assert(!intersects(t2,Vector(0,0,1000),Vector(0,0,-1)));
    
    //assert(t1->intersect(Ray,0)));
    //assert(!t1->intersect(Ray(Vector(0,0,1000),Vector(0,0,-1),0)));
    //assert(t2->intersect(Ray(Vector(-100,0,1000),Vector(0,0,-1),0)));
    //assert(!t2->intersect(Ray(Vector(0,0,1000),Vector(0,0,-1),0)));
}

void csg_test() {
    ///////////////////////////////////////////////////////////////
    // Union 
    ///////////////////////////////////////////////////////////////
    // Ray from outside
    Sphere* s1 = new Sphere(Vector(0,0,10),15,NULL);
    Sphere* s2 = new Sphere(Vector(0,0,-10),15,NULL);
    Sphere* s3 = new Sphere(Vector(0,0,0),15,NULL);
    Solid* csg = new CSGUnion(s1,s2,NULL);
    Ray ray = Ray(Vector(0,0,100),Vector(0,0,-1),-1);
    vector<Intersection> all;
    csg->allIntersections(ray,all);
    assert(all.size() == 2);
    assert(all[0].getPoint() == Vector(0,0,25));
    assert(all[1].getPoint() == Vector(0,0,-25));
    CSGUnion* csg2 = new CSGUnion(csg,s3,NULL);
    all.clear();
    csg2->allIntersections(ray,all);
    assert(all.size() == 2);
    assert(all[0].getPoint() == Vector(0,0,25));
    assert(all[1].getPoint() == Vector(0,0,-25));
    Sphere* s4 = new Sphere(Vector(0,0,-50),10,NULL);
    CSGUnion* csg3 = new CSGUnion(csg2,s4,NULL);
    all.clear();
    csg3->allIntersections(ray,all);
    assert(all.size() == 4);
    assert(all[0].getPoint() == Vector(0,0,25));
    assert(all[1].getPoint() == Vector(0,0,-25));
    assert(all[2].getPoint() == Vector(0,0,-40));
    assert(all[3].getPoint() == Vector(0,0,-60));
    // Ray from inside
    ray = Ray(Vector(0,0,0),Vector(0,0,-1),-1);
    all.clear();
    csg3->allIntersections(ray,all);
    assert(all.size() == 3);
    assert(all[0].getPoint() == Vector(0,0,-25));
    assert(all[1].getPoint() == Vector(0,0,-40));
    assert(all[2].getPoint() == Vector(0,0,-60));

    s1 = new Sphere(Vector(0,0,10),5,NULL);
    s2 = new Sphere(Vector(0,0,-10),5,NULL);
    csg = new CSGUnion(s1,s2,NULL);
    
    assert(iPoint(csg,Vector(0,0,0),Vector(0,0,1)) == Vector(0,0,5));
    assert(iNormal(csg,Vector(0,0,0),Vector(0,0,1)) == Vector(0,0,-1));
    assert(iPoint(csg,Vector(0,0,0),Vector(0,0,-1)) == Vector(0,0,-5));
    assert(iNormal(csg,Vector(0,0,0),Vector(0,0,-1)) == Vector(0,0,1));

    s1 = new Sphere(Vector(0,0,10),15,NULL);
    s2 = new Sphere(Vector(0,0,-10),15,NULL);
    csg = new CSGUnion(s1,s2,NULL);
    assert(iPoint(csg,Vector(0,0,25),Vector(0,0,-1)) == Vector(0,0,-25));
    assert(iNormal(csg,Vector(0,0,25),Vector(0,0,-1)) == Vector(0,0,-1));
    assert(iPoint(csg,Vector(0,0,5),Vector(0,0,-1)) == Vector(0,0,-25));
    assert(iNormal(csg,Vector(0,0,5),Vector(0,0,-1)) == Vector(0,0,-1));
    assert(iPoint(csg,Vector(0,0,-25),Vector(0,0,1)) == Vector(0,0,25));
    assert(iNormal(csg,Vector(0,0,-25),Vector(0,0,1)) == Vector(0,0,1));

    // Testing the constructor taking a vector<Solid*>
    vector<Solid*> solids;

    solids.push_back(s1);
    solids.push_back(s2);
    csg = new CSGUnion(&solids,NULL);
    assert(iPoint(csg,Vector(0,0,25),Vector(0,0,-1)) == Vector(0,0,-25));
    assert(iNormal(csg,Vector(0,0,25),Vector(0,0,-1)) == Vector(0,0,-1));
    assert(iPoint(csg,Vector(0,0,5),Vector(0,0,-1)) == Vector(0,0,-25));
    assert(iNormal(csg,Vector(0,0,5),Vector(0,0,-1)) == Vector(0,0,-1));
    assert(iPoint(csg,Vector(0,0,-25),Vector(0,0,1)) == Vector(0,0,25));
    assert(iNormal(csg,Vector(0,0,-25),Vector(0,0,1)) == Vector(0,0,1));
    
    solids.clear();
    // A group of ball that all intersect
    for(int i = 0; i <= 100; i++) {
	solids.push_back(new Sphere(Vector(0,0,i),2,NULL));
    }
    csg = new CSGUnion(&solids,NULL);
    assert(iPoint(csg,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,102));
    assert(iNormal(csg,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iPoint(csg,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-2));
    assert(iNormal(csg,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-1));
    assert(intersects(csg,Vector(0,0,50),Vector(0,0,-1)));

    assert(iPoint(csg,Vector(0,0,50),Vector(0,0,1)) == Vector(0,0,102));
    assert(iNormal(csg,Vector(0,0,50),Vector(0,0,1)) == Vector(0,0,1));

    all.clear();
    ray = Ray(Vector(0,0,1000),Vector(0,0,-1),-1);
    csg->allIntersections(ray,all);
    assert(all.size() == 2);
    assert(all[0].getPoint() == Vector(0,0,102));
    assert(all[1].getPoint() == Vector(0,0,-2));

    solids.clear();
    // A group that doesn't intersect
    for(int i = 0; i <= 100; i++) {
	solids.push_back(new Sphere(Vector(0,0,i),0.25,NULL));
    }
    csg = new CSGUnion(&solids,NULL);
    assert(iPoint(csg,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,100.25));
    assert(iNormal(csg,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iPoint(csg,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-0.25));
    assert(iNormal(csg,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-1));
    all.clear();
    ray = Ray(Vector(0,0,1000),Vector(0,0,-1),-1);
    csg->allIntersections(ray,all);
    assert(all.size() == 2*101);
    assert(all[0].getPoint() == Vector(0,0,100.25));
    assert(all[1].getPoint() == Vector(0,0,99.75));
    
    ///////////////////////////////////////////////////////////////
    // Intersection 
    ///////////////////////////////////////////////////////////////
    s1 = new Sphere(Vector(0,0,10),15,NULL);
    s2 = new Sphere(Vector(0,0,-10),15,NULL);
    s3 = new Sphere(Vector(0,0,0),15,NULL);
    csg = new CSGIntersection(s1,s3,NULL);
    ray = Ray(Vector(0,0,100),Vector(0,0,-1),-1);
    all.clear();
    csg->allIntersections(ray,all);
    assert(all.size() == 2);
    assert(all[0].getPoint() == Vector(0,0,15));
    assert(all[0].getNormal() == Vector(0,0,1));
    assert(all[1].getPoint() == Vector(0,0,-5));
    assert(all[1].getNormal() == Vector(0,0,-1));
    assert(iPoint(csg,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,15));
    assert(iNormal(csg,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iPoint(csg,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-5));
    assert(iNormal(csg,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-1));

    // Ray that misses
    ray = Ray(Vector(0,1000,20),Vector(0,-1,0),-1);
    all.clear();
    csg->allIntersections(ray,all);
    assert(all.size() == 0);
    assert(!intersects(csg,Vector(0,1000,20),Vector(0,-1,0)));
    assert(intersects(csg,Vector(0,1000,14),Vector(0,-1,0)));

    // Ray that misses
    ray = Ray(Vector(0,1000,-10),Vector(0,-1,0),-1);
    all.clear();
    csg->allIntersections(ray,all);
    assert(all.size() == 0);
    assert(!intersects(csg,Vector(0,1000,-10),Vector(0,-1,0)));
    assert(intersects(csg,Vector(0,1000,-3),Vector(0,-1,0)));

    // Void intersection
    s1 = new Sphere(Vector(0,0,10),5,NULL);
    s2 = new Sphere(Vector(0,0,-10),5,NULL);
    csg = new CSGIntersection(s1,s2,NULL);
    assert(!intersects(csg,Vector(0,0,1000),Vector(0,0,-1)));
    ray = Ray(Vector(0,0,1000),Vector(0,0,-1),-1);
    all.clear();
    csg->allIntersections(ray,all);
    assert(all.size() == 0);

    ///////////////////////////////////////////////////////////////
    // Difference 
    ///////////////////////////////////////////////////////////////
    s1 = new Sphere(Vector(0,0,10),15,NULL);
    s2 = new Sphere(Vector(0,0,0),15,NULL);
    csg = new CSGDifference(s1,s2,NULL);
    ray = Ray(Vector(0,0,100),Vector(0,0,-1),-1);
    all.clear();
    csg->allIntersections(ray,all);
    assert(all.size() == 2);
    assert(all[0].getPoint() == Vector(0,0,25));
    assert(all[0].getNormal() == Vector(0,0,1));
    assert(all[1].getPoint() == Vector(0,0,15));
    assert(all[1].getNormal() == Vector(0,0,-1));
    assert(iPoint(csg,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,25));
    assert(iNormal(csg,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iPoint(csg,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,15));
    assert(iNormal(csg,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-1));
    assert(!intersects(csg,Vector(0,0,25.1),Vector(0,0,1)));
    assert(!intersects(csg,Vector(0,0,25),Vector(0,0,1)));
    assert(!intersects(csg,Vector(0,0,14),Vector(0,0,-1)));
    assert(!intersects(csg,Vector(0,0,14.99),Vector(0,0,-1)));
    assert(!intersects(csg,Vector(0,0,15),Vector(0,0,-1)));
    
    // Test a sphere with three other spheres subtracted from its middle,
    // front and back, so that the resulting object is hollow along the z-axis.

    s1 = new Sphere(Vector(0,0,0),200.0,NULL);
    s2 = new Sphere(Vector(0,0,0),180.0,NULL);
    Solid* s = new CSGDifference(s1,s2,NULL); // Make it hollow
    s3 = new Sphere(Vector(0,0,200),100.0,NULL); 
    Solid* b4 = new CSGDifference(s,s3,NULL); // Cut front
    s4 = new Sphere(Vector(0,0,-200),100.0,NULL); 
    Solid* b5 = new CSGDifference(b4,s4,NULL); // Cut back
    
    Ray r = Ray(Vector(0,0,1000),Vector(0,0,-1),1);
    assert(intersects(s,r));
    assert(intersects(s3,r));
    assert(intersects(s4,r));
    assert(intersects(b4,r));
    r = Ray(Vector(0,0,-1000),Vector(0,0,1),1);
    assert(! intersects(b5,r));
    r = Ray(Vector(0,0,0),Vector(0,0,1),1);
    assert(! intersects(b5,r));

    // Test a hollow sphere
    s1 = new Sphere(Vector(0,0,0),30,NULL);
    s2 = new Sphere(Vector(0,0,0),29,NULL);
    csg = new CSGDifference(s1,s2,NULL); // Make it hollow
    ray = Ray(Vector(0,0,1000),Vector(0,0,-1),-1);
    all.clear();
    csg->allIntersections(ray,all);
    assert(all.size() == 4);
    assert(all[0].getPoint() == Vector(0,0,30));
    assert(all[1].getPoint() == Vector(0,0,29));
    assert(all[2].getPoint() == Vector(0,0,-29));
    assert(all[3].getPoint() == Vector(0,0,-30));
    assert(all[0].isEntering() == true);
    assert(all[1].isEntering() == false);
    assert(all[2].isEntering() == true);
    assert(all[3].isEntering() == false);
    assert(all[0].getNormal() == Vector(0,0,1));
    assert(all[1].getNormal() == Vector(0,0,-1));
    assert(all[2].getNormal() == Vector(0,0,1));
    assert(all[3].getNormal() == Vector(0,0,-1));


    // Test a hollow ellipsoid
    Ellipsoid* e1 = new Ellipsoid(Vector(0,0,0),Vector(10,20,30),NULL);
    Ellipsoid* e2 = new Ellipsoid(Vector(0,0,0),Vector(9,19,29),NULL);
    csg = new CSGDifference(e1,e2,NULL);
    ray = Ray(Vector(0,0,1000),Vector(0,0,-1),-1);
    all.clear();
    csg->allIntersections(ray,all);
    assert(all.size() == 4);
    assert(all[0].getPoint() == Vector(0,0,30));
    assert(all[1].getPoint() == Vector(0,0,29));
    assert(all[2].getPoint() == Vector(0,0,-29));
    assert(all[3].getPoint() == Vector(0,0,-30));
    assert(all[0].getNormal() == Vector(0,0,1));
    assert(all[1].getNormal() == Vector(0,0,-1));
    assert(all[2].getNormal() == Vector(0,0,1));
    assert(all[3].getNormal() == Vector(0,0,-1));
    assert(all[0].isEntering() == true);
    assert(all[1].isEntering() == false);
    assert(all[2].isEntering() == true);
    assert(all[3].isEntering() == false);
    // Rays with origin on edges
    assert(iPoint(csg,Vector(0,0,30),Vector(0,0,-1)) == Vector(0,0,29));
    assert(iNormal(csg,Vector(0,0,30),Vector(0,0,-1)) == Vector(0,0,-1));
    assert(iPoint(csg,Vector(0,0,29),Vector(0,0,-1)) == Vector(0,0,-29));
    assert(iNormal(csg,Vector(0,0,29),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iPoint(csg,Vector(0,0,-29),Vector(0,0,-1)) == Vector(0,0,-30));
    assert(iNormal(csg,Vector(0,0,-29),Vector(0,0,-1)) == Vector(0,0,-1));
    assert(!intersects(csg,Vector(0,0,-30),Vector(0,0,-1)));
    
    // Test a hollow ellipsoid with top cut off
    SolidBox* box = new SolidBox(Vector(-100,5,-100),Vector(100,50,100),NULL);
    Solid* cup = new CSGDifference(csg,box,NULL);
    ray = Ray(Vector(0,0,1000),Vector(0,0,-1),-1);
    all.clear();
    cup->allIntersections(ray,all);
    assert(all.size() == 4);
    assert(all[0].getPoint() == Vector(0,0,30));
    assert(all[1].getPoint() == Vector(0,0,29));
    assert(all[2].getPoint() == Vector(0,0,-29));
    assert(all[3].getPoint() == Vector(0,0,-30));
    assert(all[0].getNormal() == Vector(0,0,1));
    assert(all[1].getNormal() == Vector(0,0,-1));
    assert(all[2].getNormal() == Vector(0,0,1));
    assert(all[3].getNormal() == Vector(0,0,-1));
    ray = Ray(Vector(0,1000,0),Vector(0,-1,0),-1);
    all.clear();
    cup->allIntersections(ray,all);
    assert(all.size() == 2);
    assert(all[0].getPoint() == Vector(0,-19,0));
    assert(all[1].getPoint() == Vector(0,-20,0));
    assert(all[0].getNormal() == Vector(0,1,0));
    assert(all[1].getNormal() == Vector(0,-1,0));
    assert(iPoint(cup,Vector(0,200,0),Vector(0,-1,0)) == Vector(0,-19,0));
    assert(iNormal(cup,Vector(0,200,0),Vector(0,-1,0)) == Vector(0,1,0));
    assert(iPoint(cup,Vector(0,-19,0),Vector(0,-1,0)) == Vector(0,-20,0));
    assert(iNormal(cup,Vector(0,-19,0),Vector(0,-1,0)) == Vector(0,-1,0));
    assert(!intersects(csg,Vector(0,-20,0),Vector(0,-1,0)));
}

void solidbox_test() {
    Ray ray;
    vector<Intersection> all;

    SolidBox* b = new SolidBox(Vector(-10,-10,-10),Vector(10,10,10),NULL);
    assert(intersects(b,Vector(20,0,0),Vector(-1,0,0)));
    assert(iPoint(b,Vector(20,0,0),Vector(-1,0,0)) == Vector(10,0,0));
    assert(iNormal(b,Vector(20,0,0),Vector(-1,0,0)) == Vector(1,0,0));
    assert(iPoint(b,Vector(-20,0,0),Vector(1,0,0)) == Vector(-10,0,0));
    assert(iNormal(b,Vector(-20,0,0),Vector(1,0,0)) == Vector(-1,0,0));
    assert(iPoint(b,Vector(0,0,0),Vector(1,0,0)) == Vector(10,0,0));
    assert(iNormal(b,Vector(0,0,0),Vector(1,0,0)) == Vector(1,0,0));

    // Ray from outside
    ray = Ray(Vector(0,0,100),Vector(0,0,-1),-1);
    all.clear();
    b->allIntersections(ray,all);
    assert(all.size() == 2);
    assert(all[0].getPoint() == Vector(0,0,10));
    assert(all[0].getNormal() == Vector(0,0,1));
    assert(all[0].isEntering());
    assert(all[1].getPoint() == Vector(0,0,-10));
    assert(all[1].getNormal() == Vector(0,0,-1));
    assert(!all[1].isEntering());

    // Ray from inside
    ray = Ray(Vector(0,0,0),Vector(0,-1,0),-1);
    all.clear();
    b->allIntersections(ray,all);
    assert(all.size() == 1);
    assert(all[0].getPoint() == Vector(0,-10,0));
    assert(all[0].getNormal() == Vector(0,-1,0));
    assert(!all[0].isEntering());
    
    // Ray that misses
    ray = Ray(Vector(0,0,20),Vector(0,-1,0),-1);
    all.clear();
    b->allIntersections(ray,all);
    assert(all.size() == 0);

    // Outwards ray with origin on edge of box
    ray = Ray(Vector(0,0,10),Vector(0,0,1),-1);
    all.clear();
    b->allIntersections(ray,all);
    assert(all.size() == 0);
    assert(intersects(b,ray) == false);
    
    // Inwards ray with origin on edge of box
    ray = Ray(Vector(0,0,10),Vector(0,0,-1),-1);
    all.clear();
    b->allIntersections(ray,all);
    assert(all.size() == 1);
    assert(all[0].getPoint() == Vector(0,0,-10));
    assert(all[0].getNormal() == Vector(0,0,-1));
    assert(!all[0].isEntering());
    assert(intersects(b,ray) == true);

    // Scaled box
    b = new SolidBox(Vector(-1,-1,-1),Vector(1,1,1),NULL);
    b->transform(Matrix::matrixScale(Vector(10,20,30)));
    assert(intersects(b,Vector(9,19,100),Vector(0,0,-1)));
    assert(iPoint(b,Vector(9,19,100),Vector(0,0,-1)) == Vector(9,19,30));
    assert(iNormal(b,Vector(9,19,100),Vector(0,0,-1)) == Vector(0,0,1));
    assert(intersects(b,Vector(-9,-19,-100),Vector(0,0,1)));
    assert(iPoint(b,Vector(-9,-19,-100),Vector(0,0,1)) == Vector(-9,-19,-30));
    assert(iNormal(b,Vector(-9,-19,-100),Vector(0,0,1)) == Vector(0,0,-1));

    // Solidbox in CSG difference
    SolidBox* b1 = new SolidBox(Vector(-20,350,-20),Vector(20,400,20),NULL);
    SolidBox* b2 = new SolidBox(Vector(-15,395,-15),Vector(15,405,15),NULL);
    Solid* csg = new CSGDifference(b1,b2,NULL);
    assert(iPoint(csg,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,395,0));
    assert(iNormal(csg,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,1,0));

}

void ellipsoid_test() {
    Ellipsoid* e = new Ellipsoid(Vector(0,0,0),Vector(10,20,30),NULL);
    assert(intersects(e,Vector(0,0,1000),Vector(0,0,-1)));
    assert(iPoint(e,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,30));
    assert(iNormal(e,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iPoint(e,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,20,0));
    assert(iNormal(e,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,1,0));
    assert(iPoint(e,Vector(1000,0,0),Vector(-1,0,0)) == Vector(10,0,0));
    assert(iNormal(e,Vector(1000,0,0),Vector(-1,0,0)) == Vector(1,0,0));

    e = new Ellipsoid(Vector(0,0,0),Vector(2,5,6),NULL);
    e->transform(Matrix::matrixScale(Vector(5,4,5)));
    assert(intersects(e,Vector(0,0,1000),Vector(0,0,-1)));
    assert(iPoint(e,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,30));
    assert(iNormal(e,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iPoint(e,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,20,0));
    assert(iNormal(e,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,1,0));
    assert(iPoint(e,Vector(1000,0,0),Vector(-1,0,0)) == Vector(10,0,0));
    assert(iNormal(e,Vector(1000,0,0),Vector(-1,0,0)) == Vector(1,0,0));

    Ray ray = Ray(Vector(0,0,1000),Vector(0,0,-1),-1);
    vector<Intersection> all;
    e->allIntersections(ray,all);
    assert(all.size() == 2);
    assert(all[0].getPoint() == Vector(0,0,30));
    assert(all[0].getNormal() == Vector(0,0,1));
    assert(all[1].getPoint() == Vector(0,0,-30));
    assert(all[1].getNormal() == Vector(0,0,-1));
    assert(all[0].isEntering());
    assert(!all[1].isEntering());
}

void cone_test() {
    Ray ray;
    vector<Intersection> all;
    Cone* c;
    Vector n;

    // Cone along z-axis
    c = new Cone(Vector(0,0,0),Vector(0,0,1),1,0,true,NULL);
    assert(intersects(c,Vector(0,1000,0.5),Vector(0,-1,0)));
    assert(iPoint(c,Vector(0,1000,0.5),Vector(0,-1,0)) == Vector(0,0.5,0.5));
    n = Vector(0,1,1);
    n.normalize();
    assert(iNormal(c,Vector(0,1000,0.5),Vector(0,-1,0)) == n);
    assert(!intersects(c,Vector(1,1000,0.5),Vector(0,-1,0)));
    assert(!intersects(c,Vector(0,1000,-0.1),Vector(0,-1,0)));
    assert(!intersects(c,Vector(0,1000,1.1),Vector(0,-1,0)));

    c = new Cone(Vector(0,0,0),Vector(0,0,1),4,2,true,NULL);
    assert(intersects(c,Vector(0,1000,0.5),Vector(0,-1,0)));
    assert(iPoint(c,Vector(0,1000,0.5),Vector(0,-1,0)) == Vector(0,3,0.5));

    c = new Cone(Vector(0,0,-1),Vector(0,0,1),4,2,true,NULL);
    assert(intersects(c,Vector(0,1000,0),Vector(0,-1,0)));
    assert(iPoint(c,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,3,0));
    assert(intersects(c,Vector(0,1000,-0.9),Vector(0,-1,0)));
    assert(intersects(c,Vector(0,1000,0.9),Vector(0,-1,0)));
    assert(intersects(c,Vector(0.01,1000,-0.9),Vector(0,-1,0)));
    assert(intersects(c,Vector(0.01,1000,0.9),Vector(0,-1,0)));

    c = new Cone(Vector(0,0,0),Vector(0,0,10),500,10,true,NULL);
    assert(intersects(c,Vector(0,1000,0.1),Vector(0,-1,0)));
    assert(intersects(c,Vector(0,1000,9.9),Vector(0,-1,0)));
    assert(intersects(c,Vector(0.01,1000,9.9),Vector(0,-1,0)));
    assert(intersects(c,Vector(0.01,1000,9.9),Vector(0,-1,0)));
    assert(intersects(c,Vector(0,0,1000),Vector(0,0,-1)));
    assert(iPoint(c,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,10));
    assert(iNormal(c,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iPoint(c,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,0));
    assert(iNormal(c,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-1));
    
    // Cone along y-axis
    c = new Cone(Vector(0,0,0),Vector(0,1,0),2,4,true,NULL);
    assert(intersects(c,Vector(0,0.5,100),Vector(0,0,-1)));
    assert(iPoint(c,Vector(0,0.5,1000),Vector(0,0,-1)) == Vector(0,0.5,3));

    c = new Cone(Vector(0,0,0),Vector(0,10,0),500,10,true,NULL);
    assert(intersects(c,Vector(1000,0.1,0),Vector(-1,0,0)));
    assert(intersects(c,Vector(1000,9.9,0),Vector(-1,0,0)));
    assert(intersects(c,Vector(1000,9.9,10),Vector(-1,0,0)));
    assert(intersects(c,Vector(1000,0.1,400),Vector(-1,0,0)));
    assert(!intersects(c,Vector(1000,0.1,500),Vector(-1,0,0)));
    assert(iPoint(c,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,10,0));
    assert(iNormal(c,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,1,0));
    assert(iPoint(c,Vector(0,-1000,0),Vector(0,1,0)) == Vector(0,0,0));
    assert(iNormal(c,Vector(0,-1000,0),Vector(0,1,0)) == Vector(0,-1,0));

    // Cone along y-axis
    c = new Cone(Vector(0,-1,0),Vector(0,1,0),2,4,true,NULL);
    assert(intersects(c,Vector(0,0,100),Vector(0,0,-1)));
    assert(iPoint(c,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,3));
    
    // Cone along x-axis
    c = new Cone(Vector(-1,0,0),Vector(1,0,0),2,4,true,NULL);
    assert(intersects(c,Vector(0,1000,0),Vector(0,-1,0)));
    assert(iPoint(c,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,3,0));

    // Intersection with caps (cone along z-axis)
    c = new Cone(Vector(0,0,0),Vector(0,0,1),1,0,true,NULL);
    assert(iPoint(c,Vector(0,0,-100),Vector(0,0,1)) == Vector(0,0,0));
    c = new Cone(Vector(0,0,-1),Vector(0,0,1),2,1,true,NULL);
    assert(iPoint(c,Vector(0,0,-100),Vector(0,0,1)) == Vector(0,0,-1));
    assert(iPoint(c,Vector(0,0,100),Vector(0,0,-1)) == Vector(0,0,1));

    // All intersections with caps (cone along z-axis)
    c = new Cone(Vector(0,0,-2),Vector(0,0,3),2,1,true,NULL);
    ray = Ray(Vector(0,0,10),Vector(0,0,-1),-1);
    all.clear();
    c->allIntersections(ray,all);
    assert(all.size() == 2);
    assert(all[0].getPoint() == Vector(0,0,3));
    assert(all[0].getNormal() == Vector(0,0,1));
    assert(all[0].isEntering());
    assert(all[1].getPoint() == Vector(0,0,-2));
    assert(all[1].getNormal() == Vector(0,0,-1));
    assert(!all[1].isEntering());
    
    // All intersections with caps (cone along x-axis)
    c = new Cone(Vector(-2,0,0),Vector(3,0,0),2,1,true,NULL);
    ray = Ray(Vector(-10,0,0),Vector(1,0,0),-1);
    all.clear();
    c->allIntersections(ray,all);
    assert(all.size() == 2);
    assert(all[0].getPoint() == Vector(-2,0,0));
    assert(all[0].getNormal() == Vector(-1,0,0));
    assert(all[0].isEntering());
    assert(all[1].getPoint() == Vector(3,0,0));
    assert(all[1].getNormal() == Vector(1,0,0));
    assert(!all[1].isEntering());
}

void superellipsoid_test() {
    // Unscaled instance at origin
    SuperEllipsoid* s = new SuperEllipsoid(0.2,0.2,100,0.0001,NULL);
    assert(intersects(s,Vector(0,0,1000),Vector(0,0,-1)));
    assert(!intersects(s,Vector(2,0,1000),Vector(0,0,-1)));
    assert(!intersects(s,Vector(0,2,1000),Vector(0,0,-1)));
    assert(iPoint(s,Vector(0,0,1000),Vector(0,0,-1)).z() < 1.001);
    assert(iPoint(s,Vector(0,0,1000),Vector(0,0,-1)).z() > 0.999);
    assert(iNormal(s,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iNormal(s,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-1));
    assert(iNormal(s,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,1,0));
    assert(iNormal(s,Vector(0,-1000,0),Vector(0,1,0)) == Vector(0,-1,0));
    assert(intersects(s,Vector(0,0,0),Vector(0,0,-1)));
    assert(intersects(s,Vector(0,0,0),Vector(0,0,1)));
    
    
    // Scaled instance at origin
    s = new SuperEllipsoid(0.2,0.2,100,0.0001,NULL);
    s->transform(Matrix::matrixScale(Vector(10,10,10)));
    assert(intersects(s,Vector(0,0,1000),Vector(0,0,-1)));
    assert(intersects(s,Vector(9,0,1000),Vector(0,0,-1)));
    assert(!intersects(s,Vector(11,0,1000),Vector(0,0,-1)));
    assert(iPoint(s,Vector(0,0,1000),Vector(0,0,-1)).z() < 10.001);
    assert(iPoint(s,Vector(0,0,1000),Vector(0,0,-1)).z() > 9.999);
    assert(iNormal(s,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iNormal(s,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-1));
    assert(iNormal(s,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,1,0));
    assert(intersects(s,Vector(0,0,0),Vector(0,0,-1)));
    assert(intersects(s,Vector(0,0,0),Vector(0,0,1)));

    // Scaled and translated instance
    s->transform(Matrix::matrixTranslate(Vector(100,300,400)));
    assert(intersects(s,Vector(100,300,1000),Vector(0,0,-1)));
    assert(intersects(s,Vector(109,300,1000),Vector(0,0,-1)));
    assert(!intersects(s,Vector(111,300,1000),Vector(0,0,-1)));
    assert(!intersects(s,Vector(0,0,1000),Vector(0,0,-1)));
    assert(iPoint(s,Vector(100,300,1000),Vector(0,0,-1)).z() < 410.01);
    assert(iPoint(s,Vector(100,300,1000),Vector(0,0,-1)).z() > 409.99);
    assert(iNormal(s,Vector(100,300,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iNormal(s,Vector(100,300,-1000),Vector(0,0,1)) == Vector(0,0,-1));
    //assert(intersects(s,Vector(100,300,400),Vector(0,0,-1)));
    //assert(intersects(s,Vector(100,300,400),Vector(0,0,1)));

    // Test a superellipsoid with other n1 and n2 values
    s = new SuperEllipsoid(0.2,3.0,100,0.0001,NULL);
    assert(intersects(s,Vector(0,0,1000),Vector(0,0,-1)));
    assert(!intersects(s,Vector(2,0,1000),Vector(0,0,-1)));
    assert(!intersects(s,Vector(0,2,1000),Vector(0,0,-1)));
    assert(iPoint(s,Vector(0,0,1000),Vector(0,0,-1)).z() < 1.001);
    assert(iPoint(s,Vector(0,0,1000),Vector(0,0,-1)).z() > 0.999);
    assert(iNormal(s,Vector(0,0,1000),Vector(0,0,-1)) == Vector(0,0,1));
    assert(iNormal(s,Vector(0,0,-1000),Vector(0,0,1)) == Vector(0,0,-1));
    assert(iNormal(s,Vector(0,1000,0),Vector(0,-1,0)) == Vector(0,1,0));
    assert(iNormal(s,Vector(0,-1000,0),Vector(0,1,0)) == Vector(0,-1,0));
    
}

int main(int argc, char *argv[]) {
    transformed_instance_test();
    sphere_test();
    box_test();
    cylinder_test();
    torus_test();
    mesh_test();
    tetrahedron_test();
    tesselation_test();
    extrusion_test();
    objectgroup_test();

    Mesh::test();
    test_3ds();
    solidbox_test();
    ellipsoid_test();
    csg_test();
    cone_test();
    superellipsoid_test();
    return EXIT_SUCCESS;
}


