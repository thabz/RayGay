
#include "boundingbox.h"

#include <iosfwd>
#include <iostream>
#include <cassert>

#include "ray.h"
#include "intersection.h"
#include "exception.h"
#include "math/constants.h"
#include "math/matrix.h"
#include "math/vector2.h"
#include "image/rgb.h"


BoundingBox::BoundingBox() {
}

BoundingBox::BoundingBox(const Vector c1, const Vector c2) {
    _c1[0] = MIN(c1[0],c2[0]);
    _c1[1] = MIN(c1[1],c2[1]);
    _c1[2] = MIN(c1[2],c2[2]);
    _c2[0] = MAX(c1[0],c2[0]);
    _c2[1] = MAX(c1[1],c2[1]);
    _c2[2] = MAX(c1[2],c2[2]);
}

BoundingBox::BoundingBox(const std::vector<Vector>& swarm) {
    int num = swarm.size();
    if (num < 2) throw_exception("At least two Vectors are needed");
    for(int i = 0; i < num; i++) {
	Vector c = swarm[i];
	_c1[0] = MIN(_c1[0],c[0]);
	_c1[1] = MIN(_c1[1],c[1]);
	_c1[2] = MIN(_c1[2],c[2]);
  	_c2[0] = MAX(c[0],_c2[0]);
	_c2[1] = MAX(c[1],_c2[1]);
	_c2[2] = MAX(c[2],_c2[2]);
    }
}

bool BoundingBox::inside(const Vector &p) const {
    return p[0] > _c1[0] &&
	   p[0] < _c2[0] &&
	   p[1] > _c1[1] &&
	   p[1] < _c2[1] &&
           p[2] > _c1[2] &&
	   p[2] < _c2[2];
}

bool BoundingBox::insideOrTouching(const Vector &p) const {
    return p[0] >= _c1[0] &&
	   p[0] <= _c2[0] &&
	   p[1] >= _c1[1] &&
	   p[1] <= _c2[1] &&
           p[2] >= _c1[2] &&
	   p[2] <= _c2[2];
}

Vector BoundingBox::center() const {
    return 0.5 * (Vector(_c1[0],_c1[1],_c1[2]) + Vector(_c2[0],_c2[1],_c2[2]));
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


/**
 * Finds the two distances \f$(t_{min},t_{max})\f$ where a ray intersect this 
 * boundingbox. The values are returned as an ordered pair 
 * \f$(t_{min},t_{max})\f$
 * in a Vector2 so that \f$ t_{min} \leq t_{max} \f$.
 *
 * The method will return a pair where \f$ t_{min} > t_{max} \f$
 * only in the case where no intersection was found.
 * 
 * Fast algorithm from http://www.cs.utah.edu/~awilliam/box/
 */
Vector2 BoundingBox::intersect(const Ray& ray) const {
    const Vector& B = ray.getOrigin();
    const Vector& v_inv = ray.getInverseDirection();
    const Vector& v = ray.getDirection();

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
	return Vector2(2,1);   // No intersection 
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
	return Vector2(2,1);   // No intersection 
    if (tzmin > tmin)
	tmin = tzmin; 
    if (tzmax < tmax)
	tmax = tzmax; 

    return Vector2(tmin,tmax);
}

bool BoundingBox::checkIntersect(const Ray& ray) const {
    const Vector& B = ray.getOrigin();
    const Vector& v_inv = ray.getInverseDirection();
    const Vector& v = ray.getDirection();

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
    Vector* corners = new Vector[8];
    Vector* c = corners;
    assert(c != NULL);
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
    Vector mini = Vector(MIN(b1._c1[0],b2._c1[0]),
	                 MIN(b1._c1[1],b2._c1[1]),
			 MIN(b1._c1[2],b2._c1[2]));
    Vector maxi = Vector(MAX(b1._c2[0],b2._c2[0]),
	                 MAX(b1._c2[1],b2._c2[1]),
			 MAX(b1._c2[2],b2._c2[2]));
    return BoundingBox(mini,maxi);
}

// This should return NULL is they don't intersect...!
BoundingBox BoundingBox::doIntersection(const BoundingBox& b1, const BoundingBox& b2) {
    Vector mini = Vector(MAX(b1._c1[0],b2._c1[0]),
	                 MAX(b1._c1[1],b2._c1[1]),
			 MAX(b1._c1[2],b2._c1[2]));
    Vector maxi = Vector(MIN(b1._c2[0],b2._c2[0]),
	                 MIN(b1._c2[1],b2._c2[1]),
			 MIN(b1._c2[2],b2._c2[2]));
    return BoundingBox(mini,maxi);
}

BoundingBox BoundingBox::enclosure(Vector* points, int num) {
    Vector mini = Vector(HUGE_DOUBLE,HUGE_DOUBLE,HUGE_DOUBLE);
    Vector maxi = Vector(-HUGE_DOUBLE,-HUGE_DOUBLE,-HUGE_DOUBLE);
    for(int i = 0; i < num; i++) {
	for (int j = 0; j < 3; j++) {
	    mini[j] = MIN(mini[j],points[i][j]);
	    maxi[j] = MAX(maxi[j],points[i][j]);
	}
    }
    return BoundingBox(mini,maxi);
}



bool BoundingBox::operator==(const BoundingBox &b) const {
    return b.minimum() == minimum() && b.maximum() == maximum();
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
    const double min = minimum(cutplane_dimension);
    const double max = maximum(cutplane_dimension);
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

void BoundingBox::grow(double nudge) {
    assert(nudge >= 0.0);
    _c1[0] -= nudge;
    _c1[1] -= nudge;
    _c1[2] -= nudge;
    _c2[0] += nudge;
    _c2[1] += nudge;
    _c2[2] += nudge;
}

Vector BoundingBox::lengths() const {
    return Vector(_c2[0]-_c1[0],_c2[1]-_c1[1],_c2[2]-_c1[2]);
}

/**
 * @param percent where 0.01 is 1%
 */
void BoundingBox::growPercentage(double percent) {
     Vector l = lengths();
     _c1[0] -= percent * l[0];
     _c1[1] -= percent * l[1];
     _c1[2] -= percent * l[2];
     _c2[0] += percent * l[0];
     _c2[1] += percent * l[1];
     _c2[2] += percent * l[2];
}

// Stolen from http://www.gamasutra.com/features/19991018/Gomez_4.htm
bool BoundingBox::intersectSphere(const Vector& center, double squared_radius) const {
    double s, d = 0;

    for (int i = 0; i < 3; i++) {
	if (center[i] < minimum(i)) {
	    s = center[i] - minimum(i);
	    d += s*s;
	} else if (center[i] > maximum(i)) {
	    s = center[i] - maximum(i);
	    d += s*s;
	}
    }
    return d <= squared_radius;
}

bool BoundingBox::split(BoundingBox& left, BoundingBox& right, const unsigned int dim, const double axis) const {
    if (axis > maximum(dim) || axis < minimum(dim)) {
	return false;
    }

    Vector split_max = maximum();
    split_max[dim] = axis;
    Vector split_min = minimum();
    split_min[dim] = axis;

    left = BoundingBox(minimum(),split_max);
    right = BoundingBox(split_min,maximum());
    return true;
}

