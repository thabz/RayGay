

#include <cassert>

#include "cylinder.h"
#include "paths/circle.h"

/**
 * Construct a cylinder object
 *
 * @param begin The bottom point
 * @param end	The top point
 * @param radius The radius of the cylinder
 * @param segments The number of segments to use
 * @param m Material
 */
Cylinder::Cylinder(const Vector& begin, const Vector& end, double radius, unsigned int segments, Material m) : Mesh(Mesh::MESH_FLAT,m) {

    Vector direction = end - begin;
    direction.normalize();
    Circle b = Circle(begin,radius,direction);
    Circle e = Circle(end,radius,direction);
    Vector* bp = new Vector[segments];  // Points on begin circle
    Vector* ep = new Vector[segments];  // Points on end circle
    b.getPoints(segments,bp);
    e.getPoints(segments,ep);
    
    for(unsigned int i = 0; i < segments; i++) {
	unsigned int j = (i + 1) % segments;
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

Cylinder::Cylinder(const Path& path, double radius, unsigned int segments, unsigned int pieces, Material m) : Mesh(Mesh::MESH_FLAT,m) {
    assert(pieces > 2);

    Vector* bp = new Vector[segments];  // Points on begin circle
    Vector* cp = new Vector[segments];  // Points on current circle
    Vector* pp = new Vector[segments];  // Points on previous circle
    double last_t = 0;
    for (unsigned int p = 0; p < pieces; p++) {
	double t = double(p) / double(pieces);
	Vector c = path.getPoint(t);
	Vector n = path.getTangent(t);
	Circle circle = Circle(c,radius,n);
	if (p == 0) {
	    circle.getPoints(segments,bp);
	    circle.getPoints(segments,pp);
	    last_t = t;
	} else {
	    circle.getPoints(segments,cp);
	    for(unsigned int i = 0; i < segments; i++) {
		unsigned int j = (i + 1) % segments;
		double ti = double(i) / double(segments);
		double tj = double(j) / double(segments);
		addTriangle(pp[j],cp[j],cp[i],
			    Vector2(last_t,tj),Vector2(t,tj),Vector2(t,ti));
		addTriangle(pp[i],pp[j],cp[i],
			    Vector2(last_t,ti),Vector2(last_t,tj),Vector2(t,ti));
	    }
	    circle.getPoints(segments,pp);
	}
	last_t = t;
    }
    if (path.isClosed()) {
	for(unsigned int i = 0; i < segments; i++) {
	    unsigned int j = (i + 1) % segments;
	    double ti = double(i) / double(segments);
	    double tj = double(j) / double(segments);
	    addTriangle(pp[j],bp[j],bp[i],
		    Vector2(last_t,tj),Vector2(1,tj),Vector2(1,ti));
	    addTriangle(pp[i],pp[j],bp[i],
		    Vector2(last_t,ti),Vector2(last_t,tj),Vector2(1,ti));
	}
    } else {
	// TODO: Add begin (bp) and end (pp) discs

    }
    delete [] cp;
    delete [] pp;
    delete [] bp;
}



