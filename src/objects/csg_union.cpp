
#include <cassert>
#include "objects/csg.h"
#include "boundingbox.h"
#include "exception.h"

CSGUnion::CSGUnion(Solid* left, Solid* right, const Material* mat) : Solid(mat) {
    this->left = left;
    this->right = right;
}

SceneObject* CSGUnion::clone() const {
    Solid* lhs = dynamic_cast<Solid*>(this->left->clone());
    Solid* rhs = dynamic_cast<Solid*>(this->right->clone());
    return new CSGUnion(lhs, rhs, this->getMaterial());
}

BoundingBox CSGUnion::boundingBoundingBox() const {
    BoundingBox rb = right->boundingBoundingBox();
    BoundingBox lb = left->boundingBoundingBox();
    return BoundingBox::doUnion(rb,lb);
}

void CSGUnion::transform(const Matrix& m) {
    right->transform(m);
    left->transform(m);
}

void CSGUnion::allIntersections(const Ray& ray, vector<Intersection>& result) const {
    vector<Intersection> left_int;
    left->allIntersections(ray,left_int);
    vector<Intersection> right_int;
    right->allIntersections(ray,right_int);
    result.reserve(left_int.size() + right_int.size());
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
    // Bail out quickly if possible
    if (left_int.empty()) {
	result = right_int;
	return;
    }
    if (right_int.empty()) {
	result = left_int;
	return;	
    }
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
}

double CSGUnion::_fastIntersect(const Ray& ray) const {
    double left_t, right_t;
    vector<Intersection> all;
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
}

Intersection CSGUnion::_fullIntersect(const Ray& ray, const double t) const {
    vector<Intersection> all;
    allIntersections(ray,all);
    Intersection result;
    if (!all.empty()) {
	result = all.front();
    } else {
	throw_exception("This shouldn't happen...");
    }
    return result;
}
