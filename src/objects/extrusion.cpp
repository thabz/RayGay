
#include <cassert>

#include "objects/extrusion.h"
#include "paths/circle.h"

/**
 * Construct a extrusion object
 *
 * @param begin The bottom point
 * @param end	The top point
 * @param radius The radius of the extrusion
 * @param segments The number of segments to use
 * @param m Material
 */
Extrusion::Extrusion(const Vector& begin, const Vector& end, double radius, uint segments, Material* m) : Mesh(Mesh::MESH_PHONG,m) {

    Vector direction = end - begin;
    direction.normalize();
    Circle b = Circle(begin,radius,direction);
    Circle e = Circle(end,radius,direction);
    Vector* bp = new Vector[segments];  // Points on begin circle
    Vector* ep = new Vector[segments];  // Points on end circle
    b.getPoints(segments,bp);
    e.getPoints(segments,ep);
    
    for(uint i = 0; i < segments; i++) {
	uint j = (i + 1) % segments;
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

Extrusion::Extrusion(const Path& path, double radius, uint segments, uint pieces, Material* m) : Mesh(Mesh::MESH_PHONG,m) {
    assert(pieces > 2);

    Vector* cp = new Vector[segments]; 
    double last_t = 0;
    for (uint p = 0; p < pieces; p++) {
	double t = double(p) / double(pieces);
	Vector c = path.getPoint(t);
	Vector n = path.getTangent(t);

	Circle circle = Circle(c,radius,n);
	circle.getPoints(segments,cp);
	
	for(uint i = 0; i < segments; i++) {
	    uint k = addVertex(cp[i]);
	    assert(k == p*segments + i);
	}

	if (p > 0) {
	    uint p1 = p - 1;
	    for(uint i = 0; i < segments; i++) {
		uint j = (i + 1) % segments;
		double ti = double(i) / double(segments);
		double tj = double(j) / double(segments);

		addTriangle(p1*segments+j, p*segments+j, p*segments+i,
		    Vector2(last_t,tj),Vector2(t,tj),Vector2(t,ti));
		addTriangle(p1*segments+i, p1*segments+j, p*segments+i,
		    Vector2(last_t,ti),Vector2(last_t,tj),Vector2(t,ti));
	    }
	}
	last_t = t;
    }
    if (path.isClosed()) {
	int p1 = pieces-1;
	for(uint i = 0; i < segments; i++) {
	    uint j = (i + 1) % segments;
	    double ti = double(i) / double(segments);
	    double tj = double(j) / double(segments);

	    addTriangle(p1*segments+j,j,i,
		    Vector2(last_t,tj),Vector2(1,tj),Vector2(1,ti));
	    addTriangle(p1*segments+i,p1*segments+j,i,
		    Vector2(last_t,ti),Vector2(last_t,tj),Vector2(1,ti));
	}
    } else {
	// TODO: Add begin and end discs

    }
    delete [] cp;
}



