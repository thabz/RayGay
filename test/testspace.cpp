#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <cassert>
#include <iostream>

#include "boundingbox.h"
#include "intersection.h"
#include "ray.h"

void boundingbox_test() {
    BoundingBox b;
    /* Test normal() */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    assert(b.normal(Vector(1,0,0))[0] = 1);
    assert(b.normal(Vector(-1,0,0))[0] = -1);
    assert(b.normal(Vector(0,1,0))[1] = 1);
    assert(b.normal(Vector(0,-1,0))[1] = -1);
    assert(b.normal(Vector(0.5,0.2,1))[2] = 1);
    assert(b.normal(Vector(0,0,-1))[2] = -1);

    b = BoundingBox(Vector(-100,-10,-100),Vector(100,10,100));
    assert(b.normal(Vector(-100,0,-100))[0] = -1);


    /* Test onEdge() */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    assert(b.onEdge(Vector(-1,-1,-1)));
    assert(b.onEdge(Vector(-1,0,0)));
    assert(b.onEdge(Vector(1,0,0)));
    assert(b.onEdge(Vector(-1,0,1)));
    assert(b.onEdge(Vector(1,0.5,0.5)));
    assert(!b.onEdge(Vector(0.5,0.5,0.5)));
    assert(!b.onEdge(Vector(0,0,0.5)));
    assert(!b.onEdge(Vector(0,0,0)));
    assert(!b.onEdge(Vector(10,0,0)));

    /* Test inside() */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    assert(b.inside(Vector(0,0,0)));
    assert(b.inside(Vector(0.5,0.2,-0.3)));
    assert(!b.inside(Vector(1,0.2,-0.3)));
    assert(!b.inside(Vector(0,-1,-0.3)));
    assert(!b.inside(Vector(0,-1,1)));
    assert(!b.inside(Vector(0,-10,12)));

    /* Test intersection */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    Ray r;
    Intersection i;
    r = Ray(Vector(0.5,0.5,60),Vector(0,0,-1),1);
    i = b.intersect(r);
    assert(i.isIntersected());
    assert(IS_EQUAL(i.getPoint()[2],1.0));

    r = Ray(Vector(0.5,-50,0),Vector(0,1,0),1);
    i = b.intersect(r);
    assert(i.isIntersected());
    assert(IS_EQUAL(i.getPoint()[1],-1.0));
    
    r = Ray(Vector(2,2,60),Vector(0,0,-1),1);
    assert(!b.intersect(r).isIntersected());

    r = Ray(Vector(100,100,0),Vector(-1,-1,0),1);
    assert(b.intersect(r).isIntersected());

    r = Ray(Vector(-100,-100,-100),Vector(1,1,1),1);
    i = b.intersect(r);
    assert(i.isIntersected());
    assert(IS_EQUAL(i.getPoint()[0],-1.0));
    assert(IS_EQUAL(i.getPoint()[1],-1.0));
    assert(IS_EQUAL(i.getPoint()[2],-1.0));

    /* Test doUnion */
    BoundingBox b1 = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    BoundingBox b2 = BoundingBox(Vector(-2,-2,-2),Vector(0,0,0));
    BoundingBox bu = BoundingBox::doUnion(b1,b2);
    assert(BoundingBox(Vector(-2,-2,-2),Vector(1,1,1)) == bu);

    BoundingBox bi = BoundingBox::doIntersection(b1,b2);
    assert(BoundingBox(Vector(-1,-1,-1),Vector(0,0,0)) == bi);

    /* Test cutByPlane */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    assert(b.cutByPlane(0,10) == -1);
    assert(b.cutByPlane(1,10) == -1);
    assert(b.cutByPlane(2,10) == -1);
    assert(b.cutByPlane(0,-2) == 1);
    assert(b.cutByPlane(1,-3) == 1);
    assert(b.cutByPlane(2,-4) == 1);
    assert(b.cutByPlane(0,0) == 0);
    assert(b.cutByPlane(1,0.5) == 0);
    assert(b.cutByPlane(2,-0.5) == 0);
}

int main(int argc, char *argv[]) {

    boundingbox_test();
    return EXIT_SUCCESS;
}

