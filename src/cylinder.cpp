

#include <iostream>
#include <cassert>

#include "cylinder.h"
#include "circle.h"

/**
 * Construct a cylinder object
 *
 * @param begin The bottom point
 * @param end	The top point
 * @param radius The radius of the cylinder
 * @param segments The number of segments to use
 * @param m Material
 */
Cylinder::Cylinder(const Vector& begin, const Vector& end, double radius, int segments, Material m) : Mesh(Mesh::MESH_FLAT,m) {

    segments++;
    Vector direction = end - begin;
    direction.normalize();
    Circle b = Circle(begin,radius,direction);
    Circle e = Circle(end,radius,direction);
    Vector* bp = new Vector[segments];  // Points on begin circle
    Vector* ep = new Vector[segments];  // Points on end circle
    b.getPoints(segments,bp);
    e.getPoints(segments,ep);
    
    for(int i = 0; i < segments - 1; i++) {
	int j = i + 1;
	// Discs
	addTriangle(begin,bp[j],bp[i]);
	addTriangle(end,ep[i],ep[j]);
	// Stem
	addTriangle(ep[j],bp[j],bp[i]);
	addTriangle(ep[i],ep[j],bp[i]);
    }
    delete [] bp;
    delete [] ep;
}

Cylinder::~Cylinder() {
}

void Cylinder::test() {

    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    /* Check bounds */
    Vector o = Vector(0,0,0);
    Vector top = Vector(10,0,0);
    BoundingBox b = BoundingBox(Vector(-1,-10,-10),Vector(11,10,10));
	    
    Cylinder c = Cylinder(o,top,9.0,5,m);
    assert(b.inside(c.boundingBoundingBox()));

    c = Cylinder(top,o,9.0,5,m);
    assert(b.inside(c.boundingBoundingBox()));

    top = Vector(0,10,0);
    b = BoundingBox(Vector(-10,-1,-10),Vector(10,11,10));
    c = Cylinder(o,top,5.0,5,m);
    assert(b.inside(c.boundingBoundingBox()));

    return;
    /* Check intersection */
    c = Cylinder(Vector(0,0,0),Vector(0,0,-10),5.0,3,m);
    Ray r = Ray(Vector(0.5,0.5,100),Vector(0,0,-1),1);
    Intersection i = c.intersect(r);
    assert(i.intersected);
    assert(i.point == Vector(0,0,0));
}
