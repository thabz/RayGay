
#include "boolean.h"
#include <iostream>
#include <cassert>
#include "sphere.h" /* For test only */
#include "boundingbox.h"
#include "booleanoperand.h"
#include "object.h"

Boolean::Boolean(BooleanOperand* lhs, BooleanOp op, BooleanOperand* rhs, Material m) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
    _material = m;
}

Intersection Boolean::_intersect(const Ray& ray) const {
    Intersection il = _lhs->intersect(ray);
    Intersection ir = _rhs->intersect(ray);

    Intersection* closest;
    Intersection empty = Intersection();
    bool el, er;

    switch(_op) {
	case BOOLEAN_INTERSECTION: /* Same as next */
	case BOOLEAN_UNION: 
	    el = onEdge(il.point);
	    er = onEdge(ir.point);
	    if (il.intersected && ir.intersected) {
		if (el && er) {
		    return il.t < ir.t ? il : ir;
		} else if (el) {
		    return il;
		} else if (er) {
		    return ir;
		} else {
		    return empty;
		}
	    } else if (il.intersected && el) {
		return il;
	    } else if (ir.intersected && er) {
		return ir;
	    } else {
		return empty;
	    }
	    break;

	case BOOLEAN_DIFFERENCE:
	    if ((!il.intersected) && (!ir.intersected)) {
		return empty; // No intersection
	    }
	    if (onEdge(il.point)) {
		return il;
	    }
	    if (onEdge(ir.point)) {
		return ir;
	    }
	    if (!il.intersected) {
		return empty; // No intersection 
	    }
	    if (!ir.intersected) {
		return il; // Trivial intersection
	    }
	    closest = il.t < ir.t ? &il : &ir;
	    if (onEdge(closest->point)) {
		return *closest;
	    } else {
		Ray new_ray = Ray(closest->point, ray.direction, ray.indice_of_refraction);
		Intersection next_intersection = intersect(new_ray);
		next_intersection.t += closest->t; 
		return next_intersection;
	    }
	    break;
    }
    throw unknownOp(_op);
}

void Boolean::transform(const Matrix& m) {
    _lhs->transform(m);
    _rhs->transform(m);
}

// FIXME: We're not sure that i is from lhs or rhs
Vector Boolean::normal(const Intersection& i) const {
    Vector p = i.point;
    switch(_op) {
	case BOOLEAN_INTERSECTION: /* Same as next */
	case BOOLEAN_UNION:
	    if (_rhs->onEdge(p)) {
		return _rhs->normal(i);
	    } else {
		return _lhs->normal(i);
	    }
	case BOOLEAN_DIFFERENCE:
	    if (_lhs->onEdge(p) && !_rhs->inside(p)) {
		return _lhs->normal(i);
	    } else {
		return -1 * _rhs->normal(i);
	    }
    }
    throw unknownOp(_op);
}

Material Boolean::getMaterial() const {
    return _material;
}

bool Boolean::onEdge(const Vector &p) const {
    switch(_op) {
	case BOOLEAN_UNION: 
	    return (_rhs->onEdge(p) && !_lhs->inside(p)) ||
	  	   (_lhs->onEdge(p) && !_rhs->inside(p)); 
	case BOOLEAN_DIFFERENCE:
            return (_lhs->onEdge(p) && !_rhs->inside(p)) ||
		   (_rhs->onEdge(p) && _lhs->inside(p));
	case BOOLEAN_INTERSECTION:
	    return (_rhs->onEdge(p) && _lhs->inside(p)) ||
                   (_lhs->onEdge(p) && _rhs->inside(p)) ||
		   (_rhs->onEdge(p) && _lhs->onEdge(p));
    }
    throw unknownOp(_op);
}

bool Boolean::inside(const Vector &p) const {
    switch(_op) {
	case BOOLEAN_UNION: 
	    return _lhs->inside(p) || _rhs->inside(p);
	case BOOLEAN_DIFFERENCE:
	    return _lhs->inside(p) && !_rhs->inside(p) && !_rhs->onEdge(p);
	case BOOLEAN_INTERSECTION:
	    return _lhs->inside(p) && _rhs->inside(p);
    }
    throw unknownOp(_op);
}

bool Boolean::intersects(const BoundingBox& box) const {
    switch(_op) {
	case BOOLEAN_UNION:
	    return _lhs->intersects(box) || _lhs->intersects(box);
        case BOOLEAN_INTERSECTION:
	    return _lhs->intersects(box) && _lhs->intersects(box);
	case BOOLEAN_DIFFERENCE:
	    return _lhs->intersects(box);
    }
    throw unknownOp(_op);
}

BoundingBox Boolean::boundingBoundingBox() const {
    switch(_op) {
	case BOOLEAN_UNION:
	    return BoundingBox::doUnion(_rhs->boundingBoundingBox(),_lhs->boundingBoundingBox());
        case BOOLEAN_INTERSECTION:
	    return BoundingBox::doIntersection(_rhs->boundingBoundingBox(),_lhs->boundingBoundingBox());
	case BOOLEAN_DIFFERENCE:
	    return _lhs->boundingBoundingBox(); // TODO: Could be smaller
    }
    throw unknownOp(_op);
}

void Boolean::getUV(const Intersection& intersection, double* u, double* v) const {
    // TODO: Implement
}

void Boolean::test() {
    Material m = Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Sphere s1 = Sphere(Vector(-10,0,0),30.0,m);
    Sphere s2 = Sphere(Vector(10,0,0),30.0,m);

    /* Test BOOLEAN_UNION */
    Boolean b = Boolean(&s1,BOOLEAN_UNION,&s2,m);
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
    b = Boolean(&s1,BOOLEAN_INTERSECTION,&s2,m);
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
    b = Boolean(&s1,BOOLEAN_DIFFERENCE,&s2,m);
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
    b = Boolean(&s3,BOOLEAN_INTERSECTION,&s4,m);
    assert(b.onEdge(Vector(0,0,0)));

    /* Intersection test */
    s1 = Sphere(Vector(0,0,0),60.0,m);
    s2 = Sphere(Vector(0,0,60),40.0,m);

    b = Boolean(&s1,BOOLEAN_DIFFERENCE,&s2,m);

    Ray r = Ray(Vector(0,0,1000),Vector(0,0,-1),1);
    Intersection i = b.intersect(r);

    assert(IS_EQUAL(i.point[2],20.0));
    assert(IS_EQUAL(b.normal(i)[0],0.0));
    assert(IS_EQUAL(b.normal(i)[1],0.0));
    assert(IS_EQUAL(b.normal(i)[2],1.0));

    cout << "Boolean::test() done." << endl;
}
