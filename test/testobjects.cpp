#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <cassert>
#include <iostream>

#include "boundingbox.h"
#include "sphere.h"
#include "boolean.h"
#include "mesh.h"
#include "extrusion.h"
#include "box.h"
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
}

void boolean_test() {
    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Sphere s1 = Sphere(Vector(-10,0,0),30.0,m);
    Sphere s2 = Sphere(Vector(10,0,0),30.0,m);

    /* Test BOOLEAN_UNION */
    Boolean b = Boolean(&s1,Boolean::BOOLEAN_UNION,&s2,m);
    assert(b.inside(Vector(0,0,0)));
    assert(b.inside(Vector(39,0,0)));
    assert(b.onEdge(Vector(40,0,0)));
    assert(!b.inside(Vector(40,0,0)));
    assert(b.inside(Vector(-20,0,0)));

    assert(b.onEdge(Vector(-40,0,0)));
    assert(b.onEdge(Vector(10,30,0)));

    assert(!b.onEdge(Vector(-20,0,0)));
    assert(!b.onEdge(Vector(20,0,0)));
    assert(!b.onEdge(Vector(0,0,0)));

    /* Test BOOLEAN_INTERSECTION */
    b = Boolean(&s1,Boolean::BOOLEAN_INTERSECTION,&s2,m);
    assert(b.inside(Vector(0,0,0)));
    assert(b.inside(Vector(19,0,0)));
    assert(b.inside(Vector(-19,0,0)));
    assert(!b.inside(Vector(39,0,0)));
    assert(!b.inside(Vector(-39,0,0)));
    assert(!b.inside(Vector(20,0,0)));
    assert(!b.inside(Vector(-20,0,0)));

    assert(!b.onEdge(Vector(-40,0,0)));
    assert(!b.onEdge(Vector(40,0,0)));
    assert(!b.onEdge(Vector(0,0,0)));
    assert(b.onEdge(Vector(-20,0,0)));
    assert(b.onEdge(Vector(20,0,0)));
    
    /* Test BOOLEAN_DIFFERENCE */
    b = Boolean(&s1,Boolean::BOOLEAN_DIFFERENCE,&s2,m);
    assert(!b.inside(Vector(-40,0,0)));
    assert(b.inside(Vector(-39,0,0)));
    assert(b.inside(Vector(-21,0,0)));
    assert(!b.inside(Vector(-20,0,0)));
    assert(!b.inside(Vector(0,0,0)));
    assert(!b.inside(Vector(40,0,0)));
    assert(!b.inside(Vector(39,0,0)));

    assert(!b.onEdge(Vector(-41,0,0)));
    assert(b.onEdge(Vector(-40,0,0)));
    assert(!b.onEdge(Vector(-39,0,0)));
    assert(!b.onEdge(Vector(-21,0,0)));
    assert(b.onEdge(Vector(-20,0,0)));
    assert(!b.onEdge(Vector(-19,0,0)));
    assert(!b.onEdge(Vector(0,0,0)));
    assert(!b.onEdge(Vector(20,0,0)));
    assert(!b.onEdge(Vector(40,0,0)));

    Sphere s3 = Sphere(Vector(-10,0,0),10.0,m);
    Sphere s4 = Sphere(Vector(10,0,0),10.0,m);
    b = Boolean(&s3,Boolean::BOOLEAN_INTERSECTION,&s4,m);
    assert(b.onEdge(Vector(0,0,0)));

    /* Intersection test */
    s1 = Sphere(Vector(0,0,0),60.0,m);
    s2 = Sphere(Vector(0,0,60),40.0,m);

    b = Boolean(&s1,Boolean::BOOLEAN_DIFFERENCE,&s2,m);

    Ray r = Ray(Vector(0,0,1000),Vector(0,0,-1),1);
    assert(b.intersect(r));
    Intersection* i = b.getLastIntersection();

    assert(IS_EQUAL(i->getPoint()[2],20.0));
    assert(Vector(0,0,1) == b.normal(*i));
}

void box_test() {
    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Box* b = new Box(Vector(-1,-1,-1),Vector(1,1,1),m);
    b->prepare();
    assert(b->getVertices()->size() == 8);

    BSP bsp = BSP();
    b->addParts(&bsp);
    bsp.prepare();
    Ray r = Ray(Vector(0,0,100),Vector(0,0,-1),1);
    assert(bsp.intersect(r));
    assert(bsp.getLastIntersection()->getPoint() == Vector(0,0,1));

    r = Ray(Vector(0,-100,0),Vector(0,1,0),1);
    assert(bsp.intersect(r));
    assert(bsp.getLastIntersection()->getPoint() == Vector(0,-1,0));
    delete b;

    /* Test second constructor */
    b = new Box(Vector(0,0,0),2,2,2,m);
    b->prepare();
    assert(b->getVertices()->size() == 8);

    bsp = BSP();
    b->addParts(&bsp);
    bsp.prepare();

    r = Ray(Vector(0,0,100),Vector(0,0,-1),1);
    assert(bsp.intersect(r));
    assert(bsp.getLastIntersection()->getPoint() == Vector(0,0,1));

    r = Ray(Vector(0,-100,0),Vector(0,1,0),1);
    assert(bsp.intersect(r));
    assert(bsp.getLastIntersection()->getPoint() == Vector(0,-1,0));

    r = Ray(Vector(0,-100,1.5),Vector(0,1,0),1);
    assert(bsp.intersect(r) == false);
}

void mesh_test() {

}

void tetrahedron_test() {
    Material mat = Material(RGB(0,0,0),RGB(0,0,0));
    Tetrahedron t = Tetrahedron(Vector(0,0,0),100,mat);
    assert(t.getEdges()->size() == 6);
    assert(t.getVertices()->size() == 4);
}

void tesselation_test() {
    Material mat = Material(RGB(0,0,0),RGB(0,0,0));

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
    c->addParts(&bsp);
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

int main(int argc, char *argv[]) {
    sphere_test();
    boolean_test();
    box_test();
    mesh_test();
    tetrahedron_test();
    tesselation_test();
    extrusion_test();

    Mesh::test();
    Box::test();
    return EXIT_SUCCESS;
}


