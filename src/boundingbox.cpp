
#include "boundingbox.h"

#include <iosfwd>
#include <iostream>
#include <cassert>

#include "ray.h"
#include "intersection.h"
#include "math/matrix.h"
#include "rgb.h"

BoundingBox::BoundingBox() {
    corners = NULL;
}

BoundingBox::BoundingBox(const Vector c1, const Vector c2) {
    _c1 = Vector(min(c1[0],c2[0]),min(c1[1],c2[1]),min(c1[2],c2[2]));
    _c2 = Vector(max(c1[0],c2[0]),max(c1[1],c2[1]),max(c1[2],c2[2]));
    corners = NULL;
}

BoundingBox::~BoundingBox() {
    //delete [] corners;
}

bool BoundingBox::inside(const Vector &p) const {
    return p[0] > _c1[0] &&
	   p[0] < _c2[0] &&
	   p[1] > _c1[1] &&
	   p[1] < _c2[1] &&
           p[2] > _c1[2] &&
	   p[2] < _c2[2];
}

bool BoundingBox::inside(const Vector* points, int num) const {
    assert(num > 0);
    for(int i = 0; i < num; i++) {
	if (!inside(points[i])) return false;
    }
    return true;

}

bool BoundingBox::inside(const BoundingBox& b) const {
    return inside(b.minimum()) && inside(b.maximum());
}

bool BoundingBox::onEdge(const Vector &p) const {
    bool p0bounded = p[0] >= _c1[0] && p[0] <= _c2[0];
    bool p1bounded = p[1] >= _c1[1] && p[1] <= _c2[1];
    bool p2bounded = p[2] >= _c1[2] && p[2] <= _c2[2];

    return ((IS_EQUAL(p[0],_c1[0]) || IS_EQUAL(p[0],_c2[0])) &&
	       p1bounded && p2bounded)
           ||
           ((p[1] == _c1[1] || p[1] == _c2[1]) &&
	       p0bounded && p2bounded)
           ||
           ((p[2] == _c1[2] || p[2] == _c2[2]) &&
	       p1bounded && p0bounded);
}


// Fast algorithm from http://www.cs.utah.edu/~awilliam/box/
Intersection BoundingBox::intersect(const Ray& ray) const {
    Intersection result;
    Vector B = ray.origin;
    Vector v_inv = ray.inv_direction;
    Vector v = ray.direction;

    double t,tmin, tmax, tymin, tymax, tzmin, tzmax; 

    if (v[0] >= 0) { 
	tmin = (_c1[0] - B[0]) * v_inv[0]; 
	tmax = (_c2[0] - B[0]) * v_inv[0]; 
    } else { 
	tmin = (_c2[0] - B[0]) * v_inv[0]; 
	tmax = (_c1[0] - B[0]) * v_inv[0]; 
    } 
    if (v[1] >= 0) { 
	tymin = (_c1[1] - B[1]) * v_inv[1]; 
	tymax = (_c2[1] - B[1]) * v_inv[1];
    } else {
	tymin = (_c2[1] - B[1]) * v_inv[1]; 
	tymax = (_c1[1] - B[1]) * v_inv[1];
    } 
    if ( (tmin > tymax) || (tymin > tmax) ) 
	return result;   // No intersection 
    if (tymin > tmin) 
	tmin = tymin; 
    if (tymax < tmax) 
	tmax = tymax; 
    if (v[2] >= 0) { 
	tzmin = (_c1[2] - B[2]) * v_inv[2]; 
	tzmax = (_c2[2] - B[2]) * v_inv[2]; 
    } else { 
	tzmin = (_c2[2] - B[2]) * v_inv[2]; 
	tzmax = (_c1[2] - B[2]) * v_inv[2]; 
    } 
    if ( (tmin > tzmax) || (tzmin > tmax) ) 
	return result; 	// No intersection
    if (tzmin > tmin)
	tmin = tzmin; 
    if (tzmax < tmax)
	tmax = tzmax; 
    
    t = tmin;
    if (t > 0 && !IS_ZERO(t) && !IS_EQUAL(t,HUGE_DOUBLE)) {
	Vector p = t*v + B;
	result = Intersection(p,t);
    }
    return result;
}

bool BoundingBox::checkIntersect(const Ray& ray) const {
    Vector B = ray.origin;
    Vector v_inv = ray.inv_direction;
    Vector v = ray.direction;

    double tmin, tmax, tymin, tymax, tzmin, tzmax; 

    if (v[0] >= 0) { 
	tmin = (_c1[0] - B[0]) * v_inv[0]; 
	tmax = (_c2[0] - B[0]) * v_inv[0]; 
    } else { 
	tmin = (_c2[0] - B[0]) * v_inv[0]; 
	tmax = (_c1[0] - B[0]) * v_inv[0]; 
    } 
    if (v[1] >= 0) { 
	tymin = (_c1[1] - B[1]) * v_inv[1]; 
	tymax = (_c2[1] - B[1]) * v_inv[1];
    } else {
	tymin = (_c2[1] - B[1]) * v_inv[1]; 
	tymax = (_c1[1] - B[1]) * v_inv[1];
    } 
    if ( (tmin > tymax) || (tymin > tmax) ) 
	return false;   // No intersection 
    if (tymin > tmin) 
	tmin = tymin; 
    if (tymax < tmax) 
	tmax = tymax; 
    if (v[2] >= 0) { 
	tzmin = (_c1[2] - B[2]) * v_inv[2]; 
	tzmax = (_c2[2] - B[2]) * v_inv[2]; 
    } else { 
	tzmin = (_c2[2] - B[2]) * v_inv[2]; 
	tzmax = (_c1[2] - B[2]) * v_inv[2]; 
    } 
    if ( (tmin > tzmax) || (tzmin > tmax) ) 
	return false; 	// No intersection
    if (tzmin > tmin)
	tmin = tzmin; 
    if (tzmax < tmax)
	tmax = tzmax; 
    
    return ( (tmax < HUGE_DOUBLE) && (tmax > -1 ) );
}

Vector BoundingBox::normal(const Vector& p) const {
    if (IS_EQUAL(p[0],_c1[0])) {
	    return Vector(-1,0,0);
    } else if (IS_EQUAL(p[0],_c2[0])) {
	    return Vector(1,0,0);
    } if (IS_EQUAL(p[1],_c1[1])) {
	    return Vector(0,-1,0);
    } else if (IS_EQUAL(p[1],_c2[1])) {
	    return Vector(0,1,0);
    } if (IS_EQUAL(p[2],_c1[2])) {
	    return Vector(0,0,-1);
    } else if (IS_EQUAL(p[2],_c2[2])) {
	    return Vector(0,0,-1);
    } else {
	assert(false);
    }
}


/**
 * The array must be deleted after use.
 */
Vector* BoundingBox::getCorners() const {
    if (corners != NULL) return corners;
    corners = new Vector[8];
    Vector* c = corners;
    if (c == NULL) exit(1);
    c[0] = Vector(_c1[0],_c1[1],_c1[2]);
    c[1] = Vector(_c1[0],_c1[1],_c2[2]);
    c[2] = Vector(_c1[0],_c2[1],_c1[2]);
    c[3] = Vector(_c1[0],_c2[1],_c2[2]);
    c[4] = Vector(_c2[0],_c1[1],_c1[2]);
    c[5] = Vector(_c2[0],_c1[1],_c2[2]);
    c[6] = Vector(_c2[0],_c2[1],_c1[2]);
    c[7] = Vector(_c2[0],_c2[1],_c2[2]);
    return c;
}

BoundingBox BoundingBox::doUnion(const BoundingBox& b1, const BoundingBox& b2) {
    Vector mini = Vector(min(b1._c1[0],b2._c1[0]),
	                 min(b1._c1[1],b2._c1[1]),
			 min(b1._c1[2],b2._c1[2]));
    Vector maxi = Vector(max(b1._c2[0],b2._c2[0]),
	                 max(b1._c2[1],b2._c2[1]),
			 max(b1._c2[2],b2._c2[2]));
    return BoundingBox(mini,maxi);
}

// This should return NULL is they don't intersect...!
BoundingBox BoundingBox::doIntersection(const BoundingBox& b1, const BoundingBox& b2) {
    Vector mini = Vector(max(b1._c1[0],b2._c1[0]),
	                 max(b1._c1[1],b2._c1[1]),
			 max(b1._c1[2],b2._c1[2]));
    Vector maxi = Vector(min(b1._c2[0],b2._c2[0]),
	                 min(b1._c2[1],b2._c2[1]),
			 min(b1._c2[2],b2._c2[2]));
    return BoundingBox(mini,maxi);
}

BoundingBox BoundingBox::enclosure(Vector* points, int num) {
    Vector mini = Vector(HUGE_DOUBLE,HUGE_DOUBLE,HUGE_DOUBLE);
    Vector maxi = Vector(-HUGE_DOUBLE,-HUGE_DOUBLE,-HUGE_DOUBLE);
    for(int i = 0; i < num; i++) {
	for (int j = 0; j < 3; j++) {
	    mini[j] = min(mini[j],points[i][j]);
	    maxi[j] = max(maxi[j],points[i][j]);
	}
    }
    return BoundingBox(mini,maxi);
}



bool BoundingBox::operator==(const BoundingBox &b) const {
    return b._c1 == _c1 && b._c2 == _c2;
}

/**
 * The surfacearea of all six faces.
 */
double BoundingBox::area() const {
    double w = _c2[0] - _c1[0];
    double h = _c2[1] - _c1[1];
    double d = _c2[2] - _c1[2];
    return 2.0*w*h + 2.0*w*d + 2.0*h*d;
}

/**
 * Says whether this boundingbox is on the upper or lower side of an axis-aligned plane
 *
 * @param cutplane_dimension The axis the plane cuts (0,1,2)
 * @param cutplane_value The axis-value where the plane cuts.
 * 
 * @return 1 (higher), -1 (lower) or 0 (intersects)
 */
int BoundingBox::cutByPlane(int cutplane_dimension, double cutplane_value) const {
    assert(cutplane_dimension == 0 || cutplane_dimension == 1 || cutplane_dimension == 2);
    double min = minimum()[cutplane_dimension];
    double max = maximum()[cutplane_dimension];
    if (cutplane_value > max) {
	return -1;
    } else if (cutplane_value < min) {
        return 1;
    } else {
        return 0;
    }
}

ostream & operator<<(ostream &os, const BoundingBox &b) {
    os << "Boundingbox:(" << b.minimum() << ',' << b.maximum() << ')';
    return os;
}

void BoundingBox::test() {
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
    assert(i.intersected);
    assert(IS_EQUAL(i.point[2],1.0));

    r = Ray(Vector(0.5,-50,0),Vector(0,1,0),1);
    i = b.intersect(r);
    assert(i.intersected);
    assert(IS_EQUAL(i.point[1],-1.0));
    
    r = Ray(Vector(2,2,60),Vector(0,0,-1),1);
    assert(!b.intersect(r).intersected);

    r = Ray(Vector(100,100,0),Vector(-1,-1,0),1);
    assert(b.intersect(r).intersected);

    r = Ray(Vector(-100,-100,-100),Vector(1,1,1),1);
    i = b.intersect(r);
    assert(i.intersected);
    assert(IS_EQUAL(i.point[0],-1.0));
    assert(IS_EQUAL(i.point[1],-1.0));
    assert(IS_EQUAL(i.point[2],-1.0));

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

    cout << "BoundingBox::test() done." << endl;
}

