
#include "boolean.h"
#include "boundingbox.h"
#include "booleanoperand.h"
#include "math/vector2.h"

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
	    el = onEdge(il.getPoint());
	    er = onEdge(ir.getPoint());
	    if (il.isIntersected() && ir.isIntersected()) {
		if (el && er) {
		    return il.getT() < ir.getT() ? il : ir;
		} else if (el) {
		    return il;
		} else if (er) {
		    return ir;
		} else {
		    return empty;
		}
	    } else if (il.isIntersected() && el) {
		return il;
	    } else if (ir.isIntersected() && er) {
		return ir;
	    } else {
		return empty;
	    }
	    break;

	case BOOLEAN_DIFFERENCE:
	    if ((!il.isIntersected()) && (!ir.isIntersected())) {
		return empty; // No intersection
	    }
	    if (onEdge(il.getPoint())) {
		return il;
	    }
	    if (onEdge(ir.getPoint())) {
		return ir;
	    }
	    if (!il.isIntersected()) {
		return empty; // No intersection 
	    }
	    if (!ir.isIntersected()) {
		return il; // Trivial intersection
	    }
	    closest = il.getT() < ir.getT() ? &il : &ir;
	    if (onEdge(closest->getPoint())) {
		return *closest;
	    } else {
		Ray new_ray = Ray(closest->getPoint(), ray.getDirection(), ray.getIndiceOfRefraction());
		Intersection next_intersection = intersect(new_ray);
		next_intersection.setT( closest->getT() + next_intersection.getT());; 
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
    Vector p = i.getPoint();
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

const Material& Boolean::getMaterial() const {
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

Vector2 Boolean::getUV(const Intersection& intersection) const {
    // TODO: Implement
}

