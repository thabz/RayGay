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
#include "cylinder.h"
#include "box.h"
#include "tetrahedron.h"
#include "tessalation.h"
#include "booleanoperand.h"
#include "bsp.h"

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

    Ray r = Ray(Vector(0,0,1000),Vector(0,0,-1),1);
    assert(IS_ZERO(s.intersect(r).getPoint()[2] - 60.0));

    r = Ray(Vector(0,0,0),Vector(0,0,-1),1);
    double z = s.intersect(r).getPoint()[2];
    assert(IS_ZERO( z + 60.0));

    r = Ray(Vector(0,0,-1000),Vector(0,0,1),1);
    z = s.intersect(r).getPoint()[2];
    assert(IS_ZERO( z + 60.0));

    r = Ray(Vector(0,0,-100),Vector(0,0,-1),1);
    assert(!s.intersect(r).isIntersected());

    r = Ray(Vector(0,0,-60),Vector(0,0,-1),1);
    assert(!s.intersect(r).isIntersected());

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
    Intersection i = b.intersect(r);

    assert(IS_EQUAL(i.getPoint()[2],20.0));
    assert(IS_EQUAL(b.normal(i)[0],0.0));
    assert(IS_EQUAL(b.normal(i)[1],0.0));
    assert(IS_EQUAL(b.normal(i)[2],1.0));
}

void box_test() {
    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Box b = Box(Vector(-1,-1,-1),Vector(1,1,1),m);
    b.prepare();
    BSP bsp = BSP();
    b.addParts(&bsp);
    bsp.prepare();
    Ray r = Ray(Vector(0,0,100),Vector(0,0,-1),1);
    assert(bsp.intersect(r).isIntersected());
    assert(bsp.intersect(r).getPoint() == Vector(0,0,1));

    r = Ray(Vector(0,-100,0),Vector(0,1,0),1);
    assert(bsp.intersect(r).isIntersected());
    assert(bsp.intersect(r).getPoint() == Vector(0,-1,0));
}

int main(int argc, char *argv[]) {
    sphere_test();
    boolean_test();
    box_test();

    Mesh::test();
    Box::test();
    Cylinder::test();
    Tetrahedron::test();
    Tessalation::test();
    return EXIT_SUCCESS;
}


