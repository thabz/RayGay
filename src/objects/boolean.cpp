
#include "boolean.h"
#include "boundingbox.h"
#include "booleanoperand.h"
#include "math/vector2.h"

Boolean::Boolean(BooleanOperand* lhs, BooleanOp op, BooleanOperand* rhs, const Material* m) : BooleanOperand(m) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
}

double Boolean::_fastIntersect(const Ray& ray) const {
    return _intersect(ray).getT();
}

Intersection Boolean::_fullIntersect(const Ray& ray, const double t) const {
    return _intersect(ray);
}

Intersection Boolean::_intersect(const Ray& ray) const {
    Intersection il;
    Intersection ir;
    double i_tl = _lhs->fastIntersect(ray);
    double i_tr = _rhs->fastIntersect(ray);
    if (i_tl > 0) {
	il = _lhs->fullIntersect(ray,i_tl);
    }
    if (i_tr > 0) {
	ir = _rhs->fullIntersect(ray,i_tr);
    }

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
		double t = fastIntersect(new_ray);
		Intersection next_intersection = fullIntersect(new_ray,t);
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
	    //return _lhs->inside(p) && (!_rhs->inside(p)) && (!_rhs->onEdge(p));
	    return _lhs->inside(p) && !_rhs->inside(p) && !_rhs->onEdge(p);
	case BOOLEAN_INTERSECTION:
	    return _lhs->inside(p) && _rhs->inside(p);
	default: 
	    throw unknownOp(_op);
    }
    
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

SceneObject* Boolean::clone() const {
    BooleanOperand* lhs = dynamic_cast<BooleanOperand*>(this->_lhs->clone());
    BooleanOperand* rhs = dynamic_cast<BooleanOperand*>(this->_rhs->clone());
    return new Boolean(lhs,this->_op,rhs,this->getMaterial());
}


