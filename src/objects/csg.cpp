
#include <cassert>
#include "objects/csg.h"
#include "boundingbox.h"
#include "exception.h"

CSG::CSG(Solid* left, CSGOperation op, Solid* right, const Material* mat) : Solid(mat) {
    this->left = left;
    this->right = right;
    this->op = op;
}

SceneObject* CSG::clone() const {
    Solid* lhs = dynamic_cast<Solid*>(this->left->clone());
    Solid* rhs = dynamic_cast<Solid*>(this->right->clone());
    return new CSG(lhs, this->op, rhs, this->getMaterial());
}

BoundingBox CSG::boundingBoundingBox() const {
    BoundingBox rb = right->boundingBoundingBox();
    BoundingBox lb = left->boundingBoundingBox();
    switch(op) {
	case UNION:
	    return BoundingBox::doUnion(rb,lb);
	case INTERSECTION:
	    return BoundingBox::doIntersection(rb,lb);
	case DIFFERENCE:
	    return lb;
	default:
	    throw_exception("Unknown operator");
    }
}

void CSG::transform(const Matrix& m) {
    right->transform(m);
    left->transform(m);
}

vector<Intersection> CSG::allIntersections(const Ray& ray) const {
    vector<Intersection> result;
    vector<Intersection> left_int = left->allIntersections(ray);
    vector<Intersection> right_int = right->allIntersections(ray);
    unsigned int l = 0;
    unsigned int r = 0;
    bool left_inside = false;
    bool right_inside = false;
    if (left_int.size() > 0) {
	left_inside = !left_int[0].isEntering();
    }
    if (right_int.size() > 0) {
	right_inside = !right_int[0].isEntering();
    }
    switch(op) {
	case UNION:
	    // Bail out quickly if possible
	    if (left_int.empty()) return right_int;
	    if (right_int.empty()) return left_int;
	    // Merge intersections while preserving order
	    while (l < left_int.size() && r < right_int.size()) {
		if (left_int[l].getT() < right_int[r].getT()) {
		    Intersection i = left_int[l];
		    left_inside = i.isEntering();
		    l++;
		    if (!right_inside) {
			result.push_back(i);
		    }
		} else {
		    Intersection i = right_int[r];
		    right_inside = i.isEntering();
		    r++;
		    if (!left_inside) {
			result.push_back(i);
		    }
		}
	    }
	    // Copy remaining 
	    while (l < left_int.size()) result.push_back(left_int[l++]);
	    while (r < right_int.size()) result.push_back(right_int[r++]);
	    return result;
	case DIFFERENCE:
	    if (left_int.empty() && !left_inside) return result;
	    // Invert all directions of right
	    for(unsigned int i = 0; i < right_int.size(); i++) {
		right_int[i].isEntering(!right_int[i].isEntering());
	    }
	    right_inside = !right_inside;
	    if (right_int.empty() && !right_inside) return result;
	    // Merge intersections while preserving order
	    while (l < left_int.size() && r < right_int.size()) {
		if (left_int[l].getT() < right_int[r].getT()) {
		    Intersection i = left_int[l];
		    left_inside = i.isEntering();
		    l++;
		    if (right_inside) {
			result.push_back(i);
		    }
		} else {
		    Intersection i = right_int[r];
		    right_inside = i.isEntering();
		    r++;
		    if (left_inside) {
			result.push_back(i);
		    }
		}
	    }
	    // Copy remaining if still inside other 
	    while (l < left_int.size() && right_inside) 
		result.push_back(left_int[l++]);
	    while (r < right_int.size() && left_inside) 
		result.push_back(right_int[r++]);
	    return result;
	case INTERSECTION:
	    // Bail out quickly if possible
	    if (left_int.empty() && !left_inside) return result;
	    if (right_int.empty() && !right_inside) return result;
	    // Merge intersections while preserving order
	    while (l < left_int.size() && r < right_int.size()) {
		if (left_int[l].getT() < right_int[r].getT()) {
		    Intersection i = left_int[l];
		    left_inside = i.isEntering();
		    l++;
		    if (right_inside) {
			result.push_back(i);
		    }
		} else {
		    Intersection i = right_int[r];
		    right_inside = i.isEntering();
		    r++;
		    if (left_inside) {
			result.push_back(i);
		    }
		}
	    }
	    // Copy remaining if still inside other 
	    while (l < left_int.size() && right_inside) 
		result.push_back(left_int[l++]);
	    while (r < right_int.size() && left_inside) 
		result.push_back(right_int[r++]);
	    return result;
	default:
	    throw_exception("Unknown operator");
    }
}

double CSG::_fastIntersect(const Ray& ray) const {
    double left_t, right_t;
    vector<Intersection> all;
    switch(op) {
	case UNION:
	    right_t = right->fastIntersect(ray);
	    left_t = left->fastIntersect(ray);
	    if (right_t > 0 && left_t > 0) {
		return right_t < left_t ? right_t : left_t;
	    } else if (right_t < 0 && left_t > 0) {
		return left_t;
	    } else if (left_t < 0 && right_t > 0) {
		return right_t;
	    } else {
		return -1;
	    }
	case DIFFERENCE:
	case INTERSECTION:
	    all = allIntersections(ray);
	    return all.empty() ? -1 : all.front().getT();
	default:
	    throw_exception("Unknown operator");
    };
}

Intersection CSG::_fullIntersect(const Ray& ray, const double t) const {
    vector<Intersection> all = allIntersections(ray);
    Intersection result;
    switch(op) {
	case UNION:
	case DIFFERENCE:
	case INTERSECTION:
	    if (!all.empty()) {
		result = all.front();
		//assert(IS_EQUAL(t,result.getT()));
		if (result.getNormal() * ray.getDirection() > 0) {
		    result.flipNormal();
		}
	    } else {
		throw_exception("This shouldn't happen...");
	    }
	    return result;
	default:
	    throw_exception("Unknown operator");
    }
}

