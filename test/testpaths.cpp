#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <cassert>
#include <iostream>

#include "paths/linesegment.h"
#include "paths/spiral.h"
#include "paths/circle.h"
#include "paths/line.h"
#include "paths/bezierspline.h"
#include "paths/catmullromspline.h"
#include "boundingbox.h"

using namespace std;

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

void line_test() {
    Line l1 = Line(Vector(1,1,1),Vector(2,2,2));
    Line l2 = Line(Vector(2,2,2),Vector(1,1,1));
    assert(l1 == l2);

    Line l3 = Line(Vector(3,3,3),Vector(4,4,4));
    assert(l1.isParallel(l3));
    assert(l2.isParallel(l3));
    assert(l3.isParallel(l3));
    assert(l3.contains(Vector(2,2,2)));
    assert(l3.contains(Vector(-2,-2,-2)));

    l1 = Line(Vector(5,0,0),Vector(10,0,0));
    assert(IS_EQUAL(l1.distance(Vector(0,1,0)),1.0));
    assert(IS_EQUAL(l1.distance(Vector(0,-2,0)),2.0));
    assert(IS_EQUAL(l1.distance(Vector(1,0,0)),0.0));
    assert(l1.contains(Vector(-10000,0,0)));
    assert(!l1.contains(Vector(0,0,10000)));
    assert(!l1.contains(Vector(1,1,0)));

}

void bezierspline_test() {
    int num = 5;
    Vector p[5] = { Vector(1,1,1),Vector(10,10,2),Vector(50,20,10),Vector(3,4,6),Vector(5,2,5) };
    BezierSpline* spline = new BezierSpline(p,num);

    // Check that controlpoints are correct
    assert(spline->getControlPoint(0) == p[0]);
    assert(spline->getControlPoint(1) == p[1]);
    assert(spline->getControlPoint(2) == p[2]);
    assert(spline->getControlPoint(num-1) == p[num-1]);

    // A Bezierspline passes through first and last controlpoint
    assert(spline->getPoint(0) == p[0]);
    assert(spline->getPoint(1) == p[num-1]);

    // The curve is tangent to p[1]-p[0] and p[n]-p[n-1] at the endpoints
    Vector t1 = spline->getTangent(0);
    t1.normalize();
    Vector t2 = p[1]-p[0];
    t2.normalize();
    assert(t1 == t2);

    t1 = spline->getTangent(1);
    t1.normalize();
    t2 = p[num-1]-p[3];
    t2.normalize();
    assert(t1 == t2);

    delete spline;
}

/// \todo Finish implementation
void arc_test() {
}

void catmullromspline_test() {
    int num = 5;
    Vector p[5] = { Vector(1,1,1),Vector(10,10,2),Vector(50,20,10),Vector(3,4,6),Vector(5,2,5) };
    CatmullRomSpline* spline = new CatmullRomSpline(p,num);
    
    // A Catmull-Rom spline passes through all but first and last control points
    //cout << "f(0) = " << spline->getPoint(0) << endl;
    assert(spline->getPoint(0) == p[1]);
    //cout << "f(0.5) = " << spline->getPoint(0.5) << endl;
    assert(spline->getPoint(0.5) == p[2]);
    //cout << "f(1) = " << spline->getPoint(1) << endl;
    assert(spline->getPoint(1) == p[3]);

    // The curve is tangent to p[2]-p[0] and p[n]-p[n-2] at the endpoints
    Vector t1 = spline->getTangent(0);
    //cout << "f'(0) = " << spline->getTangent(0) << endl;
    t1.normalize();
    Vector t2 = p[2]-p[0];
    t2.normalize();
    assert(t1 == t2);

    //cout << "f'(1) = " << spline->getTangent(1) << endl;
    t1 = spline->getTangent(1);
    t1.normalize();
    t2 = p[num-1]-p[2];
    t2.normalize();
    assert(t1 == t2);

    delete spline;

    
}

int main(int argc, char *argv[]) {
    linesegment_test();
    circle_test();
    line_test();
    bezierspline_test();
    arc_test();
    catmullromspline_test();

    return EXIT_SUCCESS;
}

