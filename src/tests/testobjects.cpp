#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <cassert>
#include <iostream>

#include "boundingbox.h"
#include "sphere.h"

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
    assert(IS_ZERO(s.intersect(r).point[2] - 60.0));

    r = Ray(Vector(0,0,0),Vector(0,0,-1),1);
    double z = s.intersect(r).point[2];
    assert(IS_ZERO( z + 60.0));

    r = Ray(Vector(0,0,-1000),Vector(0,0,1),1);
    z = s.intersect(r).point[2];
    assert(IS_ZERO( z + 60.0));

    r = Ray(Vector(0,0,-100),Vector(0,0,-1),1);
    assert(!s.intersect(r).intersected);

    r = Ray(Vector(0,0,-60),Vector(0,0,-1),1);
    assert(!s.intersect(r).intersected);

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

int main(int argc, char *argv[]) {

    sphere_test();
    return EXIT_SUCCESS;
}


