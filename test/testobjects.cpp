#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <cassert>
#include <iostream>

#include "boundingbox.h"
#include "sphere.h"
#include "cylinder.h"
#include "boolean.h"
#include "mesh.h"
#include "extrusion.h"
#include "box.h"
#include "necklace.h"
#include "3ds.h"
#include "tetrahedron.h"
#include "tessalation.h"
#include "booleanoperand.h"
#include "bsp.h"
#include "paths/linesegment.h"
#include "paths/circle.h"

void sphere_test() {
    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Sphere s = Sphere(Vector(0,0,0),10.0,m);
    assert(s.inside(Vector(0,0,0)));
    assert(s.inside(Vector(9,0,0)));
    assert(!s.inside(Vector(10,0,0)));
    assert(!s.inside(Vector(11,0,0)));
    assert(s.onEdge(Vector(10,0,0)));
    assert(s.onEdge(Vector(0,10,0)));
    assert(!s.onEdge(Vector(0,0,0)));

    s = Sphere(Vector(0,0,0),60.0,m);

    /* Test intersection(ray) */
    Ray r = Ray(Vector(0,0,1000),Vector(0,0,-1),1);
    assert(s.intersect(r));
    assert(IS_EQUAL(s.getLastIntersection()->getPoint()[2],60.0));

    r = Ray(Vector(0,0,0),Vector(0,0,-1),1);
    assert(s.intersect(r));
    assert(IS_EQUAL( s.getLastIntersection()->getPoint()[2], -60.0));

    r = Ray(Vector(0,0,-1000),Vector(0,0,1),1);
    assert(s.intersect(r));
    assert(IS_EQUAL( s.getLastIntersection()->getPoint()[2], -60.0));

    r = Ray(Vector(0,0,-100),Vector(0,0,-1),1);
    assert(s.intersect(r) == false);

    r = Ray(Vector(0,0,-60),Vector(0,0,-1),1);
    assert(s.intersect(r) == false);

    /* Test intersects(BoundingBox) */
    s = Sphere(Vector(0,0,0),10.0,m);
    BoundingBox b1 = BoundingBox(Vector(-20,-20,-20),Vector(0,0,0));
    assert(s.intersects(b1));
    b1 = BoundingBox(Vector(-20,-20,-20),Vector(-15,-15,-15));
    assert(!s.intersects(b1));
    b1 = BoundingBox(Vector(-20,-20,-20),Vector(20,20,20));
    assert(s.intersects(b1));
    b1 = BoundingBox(Vector(-5,-5,-5),Vector(5,5,5));
    assert(s.intersects(b1));

    /* Test boundingBoundingBox() */
    s = Sphere(Vector(0,0,0),20.0,m);
    assert(s.boundingBoundingBox() == BoundingBox(Vector(-20,-20,-20),Vector(20,20,20)));

    /* Test clone */
    Object* s1 = new Sphere(Vector(0,0,0),20,m);
    Object* s2 = dynamic_cast<Object*>(s1->clone());
    s1->transform(Matrix::matrixTranslate(Vector(0,-100,0)));
    s2->transform(Matrix::matrixTranslate(Vector(0,100,0)));

    r = Ray(Vector(0,-100,-1000),Vector(0,0,1),1);
    assert(s1->intersect(r));
    assert(!s2->intersect(r));
    r = Ray(Vector(0,100,-1000),Vector(0,0,1),1);
    assert(!s1->intersect(r));
    assert(s2->intersect(r));
}

void boolean_test() {
    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Sphere* s1 = new Sphere(Vector(-10,0,0),30.0,m);
    Sphere* s2 = new Sphere(Vector(10,0,0),30.0,m);

    /* Test BOOLEAN_UNION */
    Boolean* b = new Boolean(s1,Boolean::BOOLEAN_UNION,s2,m);
    assert(b->inside(Vector(0,0,0)));
    assert(b->inside(Vector(39,0,0)));
    assert(b->onEdge(Vector(40,0,0)));
    assert(!b->inside(Vector(40,0,0)));
    assert(b->inside(Vector(-20,0,0)));

    assert(b->onEdge(Vector(-40,0,0)));
    assert(b->onEdge(Vector(10,30,0)));

    assert(!b->onEdge(Vector(-20,0,0)));
    assert(!b->onEdge(Vector(20,0,0)));
    assert(!b->onEdge(Vector(0,0,0)));

    /* Test BOOLEAN_INTERSECTION */
    b = new Boolean(s1,Boolean::BOOLEAN_INTERSECTION,s2,m);
    assert(b->inside(Vector(0,0,0)));
    assert(b->inside(Vector(19,0,0)));
    assert(b->inside(Vector(-19,0,0)));
    assert(!b->inside(Vector(39,0,0)));
    assert(!b->inside(Vector(-39,0,0)));
    assert(!b->inside(Vector(20,0,0)));
    assert(!b->inside(Vector(-20,0,0)));

    assert(!b->onEdge(Vector(-40,0,0)));
    assert(!b->onEdge(Vector(40,0,0)));
    assert(!b->onEdge(Vector(0,0,0)));
    assert(b->onEdge(Vector(-20,0,0)));
    assert(b->onEdge(Vector(20,0,0)));
    
    /* Test BOOLEAN_DIFFERENCE */
    b = new Boolean(s1,Boolean::BOOLEAN_DIFFERENCE,s2,m);
    assert(!b->inside(Vector(-40,0,0)));
    assert(b->inside(Vector(-39,0,0)));
    assert(b->inside(Vector(-21,0,0)));
    assert(!b->inside(Vector(-20,0,0)));
    assert(!b->inside(Vector(0,0,0)));
    assert(!b->inside(Vector(40,0,0)));
    assert(!b->inside(Vector(39,0,0)));

    assert(!b->onEdge(Vector(-41,0,0)));
    assert(b->onEdge(Vector(-40,0,0)));
    assert(!b->onEdge(Vector(-39,0,0)));
    assert(!b->onEdge(Vector(-21,0,0)));
    assert(b->onEdge(Vector(-20,0,0)));
    assert(!b->onEdge(Vector(-19,0,0)));
    assert(!b->onEdge(Vector(0,0,0)));
    assert(!b->onEdge(Vector(20,0,0)));
    assert(!b->onEdge(Vector(40,0,0)));

    Sphere* s3 = new Sphere(Vector(-10,0,0),10.0,m);
    Sphere* s4 = new Sphere(Vector(10,0,0),10.0,m);
    b = new Boolean(s3,Boolean::BOOLEAN_INTERSECTION,s4,m);
    assert(b->onEdge(Vector(0,0,0)));

    /* Intersection test */
    s1 = new Sphere(Vector(0,0,0),60.0,m);
    s2 = new Sphere(Vector(0,0,60),40.0,m);

    b = new Boolean(s1,Boolean::BOOLEAN_DIFFERENCE,s2,m);

    Ray r = Ray(Vector(0,0,1000),Vector(0,0,-1),1);
    assert(b->intersect(r));
    Intersection* i = b->getLastIntersection();

    assert(i->getPoint() == Vector(0,0,20));
    assert(b->normal(*i) == Vector(0,0,1));

    r = Ray(Vector(0,0,-1000),Vector(0,0,1),1);
    assert(b->intersect(r));
    i = b->getLastIntersection();
    assert(i->getPoint() == Vector(0,0,-60));
    assert(b->normal(*i) == Vector(0,0,-1));

    // Test a sphere with three other spheres subtracted from its middle,
    // front and back, so that the resulting object is hollow along the z-axis.

    s1 = new Sphere(Vector(0,0,0),200.0,m);
    s2 = new Sphere(Vector(0,0,0),180.0,m);
    Boolean* s = new Boolean(s1,Boolean::BOOLEAN_DIFFERENCE,s2,m); // Make it hollow
    assert(!s->inside(Vector(0,0,0)));
    assert(!s->onEdge(Vector(0,0,0)));
    assert(s->inside(Vector(0,0,190)));
    assert(!s->onEdge(Vector(0,0,190)));
    assert(s->onEdge(Vector(0,0,180)));
    assert(!s->inside(Vector(0,0,180)));
    assert(s->onEdge(Vector(0,0,200)));
    assert(!s->inside(Vector(0,0,200)));
    assert(s->inside(Vector(0,0,-190)));
    assert(s->inside(Vector(0,190,0)));
    assert(s->inside(Vector(190,0,0)));

    s3 = new Sphere(Vector(0,0,200),100.0,m); // Cut front
    Boolean* b4 = new Boolean(s,Boolean::BOOLEAN_DIFFERENCE,s3,m);
    assert(s->inside(Vector(0,0,190)));
    assert(s->inside(Vector(0,0,-190)));
    assert(!b4->inside(Vector(0,0,190)));
    assert(!b4->onEdge(Vector(0,0,190)));
    assert(b4->inside(Vector(0,0,-190)));
    assert(b4->inside(Vector(0,190,0)));
    
    s4 = new Sphere(Vector(0,0,-200),100.0,m); // Cut back
    Boolean* b5 = new Boolean(b4,Boolean::BOOLEAN_DIFFERENCE,s4,m);
    assert(!b5->inside(Vector(0,0,190)));
    assert(!b5->onEdge(Vector(0,0,190)));
    assert(!b5->inside(Vector(0,0,-190)));
    assert(!b5->inside(Vector(0,0,-250)));
    assert(!b5->onEdge(Vector(0,0,-190)));
    assert(!b5->onEdge(Vector(0,0,-200)));
    assert(!b5->onEdge(Vector(0,0,-300)));
    assert(b5->inside(Vector(0,190,0)));
    assert(b5->inside(Vector(0,-190,0)));
    assert(b5->onEdge(Vector(0,200,0)));
    assert(b5->onEdge(Vector(0,-200,0)));
    assert(!b5->inside(Vector(0,0,0)));
    assert(!b5->onEdge(Vector(0,0,0)));
    
    r = Ray(Vector(0,0,1000),Vector(0,0,-1),1);
    assert(s->intersect(r));
    assert(s3->intersect(r));
    assert(s4->intersect(r));
    assert(b4->intersect(r));
    r = Ray(Vector(0,0,-1000),Vector(0,0,1),1);
    assert(!b5->intersect(r));
 /*   i = b5->getLastIntersection();
    cout << i->getPoint() << "  ...." << endl;
    assert(!b5->inside(Vector(0,0,0)));
    assert(!b5->onEdge(Vector(0,0,0)));*/
}

void box_test() {
    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Box* b = new Box(Vector(-1,-1,-1),Vector(1,1,1),m);
    b->prepare();
    assert(b->getVertices()->size() == 8);

    BSP* bsp = new BSP();
    b->addSelf(bsp);
    bsp->prepare();
    Ray r = Ray(Vector(0,0,100),Vector(0,0,-1),1);
    assert(bsp->intersect(r));
    assert(bsp->getLastIntersection()->getPoint() == Vector(0,0,1));

    r = Ray(Vector(0,-100,0),Vector(0,1,0),1);
    assert(bsp->intersect(r));
    assert(bsp->getLastIntersection()->getPoint() == Vector(0,-1,0));

    /* Test second constructor */
    b = new Box(Vector(0,0,0),2,2,2,m);
    b->prepare();
    assert(b->getVertices()->size() == 8);

    bsp = new BSP();
    b->addSelf(bsp);
    bsp->prepare();

    r = Ray(Vector(0,0,100),Vector(0,0,-1),1);
    assert(bsp->intersect(r));
    assert(bsp->getLastIntersection()->getPoint() == Vector(0,0,1));

    r = Ray(Vector(0,-100,0),Vector(0,1,0),1);
    assert(bsp->intersect(r));
    assert(bsp->getLastIntersection()->getPoint() == Vector(0,-1,0));

    r = Ray(Vector(0,-100,1.5),Vector(0,1,0),1);
    assert(bsp->intersect(r) == false);

    /* test clone() */
    b = new Box(Vector(-1,-1,-1),Vector(1,1,1),m);
    SceneObject* b2 = b->clone();
    assert(b2 != NULL);
    b->transform(Matrix::matrixTranslate(Vector(0,10,0)));
    b->prepare();
    b2->prepare();
    bsp = new BSP();
    b->addSelf(bsp);
    b2->addSelf(bsp);
    bsp->prepare();

    r = Ray(Vector(0,0,100),Vector(0,0,-1),1);
    assert(bsp->intersect(r));
    r = Ray(Vector(0,10,100),Vector(0,0,-1),1);
    assert(bsp->intersect(r));
    r = Ray(Vector(0,5,100),Vector(0,0,-1),1);
    assert(!bsp->intersect(r));
}

void mesh_test() {
}

void tetrahedron_test() {
    Material mat = Material(RGB(.0,.0,.0),RGB(.0,.0,.0));
    Tetrahedron t = Tetrahedron(Vector(0,0,0),100,mat);
    assert(t.getEdges()->size() == 6);
    assert(t.getVertices()->size() == 4);
}

void tesselation_test() {
    Material mat = Material(RGB(.0,.0,.0),RGB(.0,.0,.0));

    // 4 triangles
    Tessalation* t = new Tessalation(Vector(0,0,0),100,0,mat);
    assert(t->getEdges()->size() == 6);
    assert(t->getVertices()->size() == 4);

    // 12 triangles
    t = new Tessalation(Vector(0,0,0),100,1,mat);
    assert(t->getVertices()->size() == 8);
    cout << t->getEdges()->size() << endl;
  //  assert(t->getEdges()->size() == 4 * 3);

    // 36 triangles
    t = new Tessalation(Vector(0,0,0),100,2,mat);
    assert(t->getVertices()->size() == 20);

    // 108 triangles
    t = new Tessalation(Vector(0,0,0),100,3,mat);
    assert(t->getVertices()->size() == 56);
}

void extrusion_test() {

    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    // Check bounds 
    Vector o = Vector(0,0,0);
    Vector top = Vector(10,0,0);
    BoundingBox b = BoundingBox(Vector(-1,-10,-10),Vector(11,10,10));
	    
    Extrusion* c = new Extrusion(o,top,9.0,5,m);
    assert(b.inside(c->boundingBoundingBox()));

    c = new Extrusion(top,o,9.0,5,m);
    assert(b.inside(c->boundingBoundingBox()));

    top = Vector(0,10,0);
    b = BoundingBox(Vector(-10,-1,-10),Vector(10,11,10));
    c = new Extrusion(o,top,5.0,5,m);
    assert(b.inside(c->boundingBoundingBox()));

    // Check intersection 
    c = new Extrusion(Vector(0,0,0),Vector(0,0,-10),5.0,3,m);
    c->prepare();
    BSP bsp = BSP();
    c->addSelf(&bsp);
    bsp.prepare();
    Ray r = Ray(Vector(0.5,0.5,100),Vector(0,0,-1),1);
    assert(bsp.intersect(r));

    // Check generated mesh 
    c = new Extrusion(Vector(0,0,0),Vector(0,0,-10),2.0,5,m);
    c->prepare();
    assert(c->getVertices()->size() == 5*2 + 2);

    Circle circle1 = Circle(Vector(0,75,0),200,Vector(0,1,0));
    Extrusion torus = Extrusion(circle1,100,16,10,Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.20,30));
    torus.prepare();

    assert(torus.getVertices()->size() == 16*10);

}

void cylinder_test() {
    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Cylinder* cyl = new Cylinder(Vector(0,0,0),Vector(0,0,10),10,m);

    /* Test inside() and onEdge() */
    assert(cyl->inside(Vector(0,0,5)));
    assert(cyl->inside(Vector(0,0,9)));
    assert(!cyl->inside(Vector(0,0,10)));
    
    assert(cyl->onEdge(Vector(0,0,10)));
    assert(cyl->onEdge(Vector(10,0,5)));
    assert(cyl->onEdge(Vector(-10,0,5)));
    assert(!cyl->inside(Vector(10,0,5)));
    assert(!cyl->inside(Vector(0,0,11)));
    assert(!cyl->inside(Vector(1,0,-1)));

    /* Test intersection() of z-axis aligned cylinder */
    
    Ray r = Ray(Vector(0,-1000,5),Vector(0,1,0),1);
    assert(cyl->intersect(r));
    assert(IS_EQUAL(cyl->getLastIntersection()->getPoint()[0], 0.0));
    assert(IS_EQUAL(cyl->getLastIntersection()->getPoint()[1], -10.0));
    assert(IS_EQUAL(cyl->getLastIntersection()->getPoint()[2], 5.0));

    r = Ray(Vector(0,1000,5),Vector(0,-1,0),1);
    assert(cyl->intersect(r));
    assert(IS_EQUAL(cyl->getLastIntersection()->getPoint()[0], 0.0));
    assert(IS_EQUAL(cyl->getLastIntersection()->getPoint()[1], 10.0));
    assert(IS_EQUAL(cyl->getLastIntersection()->getPoint()[2], 5.0));

    delete cyl;

    // Test a cylinder translated along the z-axis
    cyl = new Cylinder(Vector(0,0,2),Vector(0,0,10),10,m);

    assert(cyl->inside(Vector(0,0,3)));
    assert(cyl->inside(Vector(0,0,5)));
    assert(cyl->inside(Vector(0,0,9)));
    assert(!cyl->inside(Vector(0,0,10)));
    assert(!cyl->inside(Vector(0,0,11)));
    assert(!cyl->inside(Vector(0,0,2)));
    
    assert(cyl->onEdge(Vector(0,0,2)));
    assert(cyl->onEdge(Vector(0,0,10)));
    assert(cyl->onEdge(Vector(10,0,5)));
    assert(cyl->onEdge(Vector(-10,0,5)));
    assert(!cyl->inside(Vector(10,0,5)));
    assert(!cyl->inside(Vector(0,0,11)));
    assert(!cyl->inside(Vector(0,0,1)));
    assert(!cyl->onEdge(Vector(0,0,1)));
    delete cyl;
    
    // Test an x-axis aligned cylinder
    cyl = new Cylinder(Vector(2,0,0),Vector(10,0,0),10,m);
    assert(cyl->inside(Vector(5,0,0)));
    assert(cyl->inside(Vector(9,0,0)));
    assert(!cyl->inside(Vector(0,0,0)));
    assert(!cyl->inside(Vector(-1,0,0)));
    assert(cyl->onEdge(Vector(2,0,0)));
    assert(cyl->onEdge(Vector(10,0,0)));
    assert(cyl->onEdge(Vector(5,10,0)));
    assert(cyl->onEdge(Vector(5,-10,0)));
    r = Ray(Vector(3,0,1000),Vector(0,0,-1),1);
    assert(cyl->intersect(r));
    
    // Test an y-axis aligned cylinder
    cyl = new Cylinder(Vector(0,2,0),Vector(0,10,0),10,m);
    assert(cyl->inside(Vector(0,5,0)));
    delete cyl;

    cyl = new Cylinder(Vector(1,1,1),Vector(10,10,10),10,m);
    assert(cyl->inside(Vector(2,2,2)));
    assert(cyl->inside(Vector(5,5,5)));
    assert(cyl->inside(Vector(7,7,7)));
    assert(cyl->inside(Vector(9,9,9)));
    assert(!cyl->onEdge(Vector(2,2,2)));
    assert(cyl->onEdge(Vector(1,1,1)));
    assert(cyl->onEdge(Vector(10,10,10)));

}

void test_3ds() {
    ThreeDS* chair = new ThreeDS("../3ds/egg-chair.3ds",1.0);
}

void objectgroup_test() {
    // Test clone()
    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Linesegment segment = Linesegment(Vector(0,0,0),Vector(0,0,100)); 
    Necklace* g1 = new Necklace(segment,10,10,m);
    SceneObject* g2 = g1->clone();
    g1->transform(Matrix::matrixTranslate(Vector(0,-100,0)));
    g2->transform(Matrix::matrixTranslate(Vector(0,100,0)));

    BSP* bsp = new BSP();
    g1->addSelf(bsp);
    g2->addSelf(bsp);
    bsp->prepare();

    Ray r = Ray(Vector(0,100,1000),Vector(0,0,-1),1);
    assert(bsp->intersect(r));
    r = Ray(Vector(0,-100,1000),Vector(0,0,-1),1);
    assert(bsp->intersect(r));
    r = Ray(Vector(0,0,1000),Vector(0,0,-1),1);
    assert(!bsp->intersect(r));
}

int main(int argc, char *argv[]) {
    sphere_test();
    box_test();
    cylinder_test();
    test_3ds();
    mesh_test();
    tetrahedron_test();
    tesselation_test();
    extrusion_test();
    objectgroup_test();

    Mesh::test();
    boolean_test();
    return EXIT_SUCCESS;
}


