#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <cassert>
#include <iostream>

#include "paths/linesegment.h"
#include "paths/spiral.h"
#include "paths/circle.h"
#include "boundingbox.h"

void circle_test() {
    int num = 100;
    Vector points[100];
    BoundingBox b;

    /* Test at origin */
    Circle c = Circle(Vector(0,0,0),10,Vector(0,0,1));
    assert(c.isClosed());
    c.getPoints(num,points);
    b = BoundingBox(Vector(-11,-11,-1),Vector(11,11,1));
    assert(b.inside(points,num));
    

    /* Test translated */
    c = Circle(Vector(10,10,0),10,Vector(0,0,1));
    assert(c.isClosed());
    c.getPoints(num,points);
    b = BoundingBox(Vector(-1,-1,-1),Vector(21,21,1));
    assert(b.inside(points,num));

    /* Test direction along x */
    c = Circle(Vector(0,0,0),10,Vector(1,0,0));
    assert(c.isClosed());
    c.getPoints(num,points);
    b = BoundingBox(Vector(-1,-11,-11),Vector(1,11,11));
    assert(b.inside(points,num));

    /* Test direction along y */
    c = Circle(Vector(0,0,0),10,Vector(0,1,0));
    assert(c.isClosed());
    c.getPoints(num,points);
    b = BoundingBox(Vector(-11,-1,-11),Vector(11,1,11));
    assert(b.inside(points,num));

    /* Test tangent */
    c = Circle(Vector(0,0,0),10,Vector(0,1,0));
    assert(c.getPoint(0) == Vector(-10,0,0));
    assert(c.getTangent(0) == Vector(0,0,1));
    assert(c.getPoint(0.25) == Vector(0,0,10));
    assert(c.getTangent(0.25) == Vector(1,0,0));
    assert(c.getPoint(0.5) == Vector(10,0,0));
    assert(c.getTangent(0.5) == Vector(0,0,-1));
    assert(c.getPoint(0.75) == Vector(0,0,-10));
    assert(c.getTangent(0.75) == Vector(-1,0,0));

    c = Circle(Vector(0,10,0),10,Vector(0,1,0));
    assert(c.getPoint(0) == Vector(-10,10,0));
    assert(c.getTangent(0) == Vector(0,0,1));
    assert(c.getPoint(0.25) == Vector(0,10,10));
    assert(c.getTangent(0.25) == Vector(1,0,0));
    assert(c.getPoint(0.5) == Vector(10,10,0));
    assert(c.getTangent(0.5) == Vector(0,0,-1));
    assert(c.getPoint(0.75) == Vector(0,10,-10));
    assert(c.getTangent(0.75) == Vector(-1,0,0));


    /* Done */
}

void linesegment_test() {
    Vector b1 = Vector(-10,-10,-10);
    Vector b2 = Vector(10,10,10);
    Linesegment l = Linesegment(b1,b2);
    assert(b1 == l.getPoint(0));
    assert(b2 == l.getPoint(1));

    Vector p[3];
    l.getPoints(3,p);
    assert(p[0] == b1);

    l.getTangents(3,p);
    assert(p[0] == p[1]);
    Vector n = Vector(1,1,1);
    n.normalize();
    assert(p[0] == n);

    assert(!l.isClosed());
}

int main(int argc, char *argv[]) {

    linesegment_test();
    circle_test();
    return EXIT_SUCCESS;
}

