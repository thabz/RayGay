
#include <cassert>
#include "objects/csg.h"
#include "aabox.h"
#include "exception.h"

CSGIntersection::CSGIntersection(Solid* left, Solid* right, const Material* mat) : Solid(mat) {
    this->left = left;
    this->right = right;
}
CSGDifference::CSGDifference(Solid* left, Solid* right, const Material* mat) : Solid(mat) {
    this->left = left;
    this->right = right;
}

SceneObject* CSGIntersection::clone() const {
    Solid* lhs = dynamic_cast<Solid*>(this->left->clone());
    Solid* rhs = dynamic_cast<Solid*>(this->right->clone());
    return new CSGIntersection(lhs, rhs, this->getMaterial());
}
SceneObject* CSGDifference::clone() const {
    Solid* lhs = dynamic_cast<Solid*>(this->left->clone());
    Solid* rhs = dynamic_cast<Solid*>(this->right->clone());
    return new CSGDifference(lhs, rhs, this->getMaterial());
}

AABox CSGDifference::getBoundingBox() const {
    AABox rb = right->getContainedBox();
    AABox lb = left->getBoundingBox();
    return AABox::doDifference(lb, rb);
}
AABox CSGIntersection::getBoundingBox() const {
    AABox rb = right->getBoundingBox();
    AABox lb = left->getBoundingBox();
    return AABox::doIntersection(rb,lb);
}

void CSGDifference::transform(const Matrix& m) {
    right->transform(m);
    left->transform(m);
}

void CSGIntersection::transform(const Matrix& m) {
    right->transform(m);
    left->transform(m);
}

/**
 * Find all intersections.
 *
 * This is done by inverting the right side hit list and merging
 * the two lists by using the intersection-rule.
 * 
 * \f[ R - L = R  \cap \neg L     \f]
 */
void CSGDifference::allIntersections(const Ray& ray, vector<Intersection>& result) const {
    vector<Intersection> left_int;
    bool left_inside = false;
    left->allIntersections(ray,left_int);
    if (left_int.empty()) return;
    if (left_int.size() > 0) {
	left_inside = !left_int.front().isEntering();
    }

    vector<Intersection> right_int;
    bool right_inside = false;
    right->allIntersections(ray,right_int);
    if (right_int.size() > 0) {
	right_inside = !right_int.front().isEntering();
    }

    // Invert all directions of right
    for(uint i = 0; i < right_int.size(); i++) {
	right_int[i].isEntering(!right_int[i].isEntering());
	right_int[i].flipNormal();
    }
    right_inside = !right_inside;
    if (right_int.empty() && !right_inside) return;
    
    // Merge intersections while preserving order
    uint l = 0;
    uint r = 0;
    result.reserve(left_int.size() + right_int.size());
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
}

void CSGIntersection::allIntersections(const Ray& ray, vector<Intersection>& result) const {
    vector<Intersection> left_int;
    left->allIntersections(ray,left_int);
    if (left_int.empty()) return;
    bool left_inside = false;
    if (left_int.size() > 0) {
	left_inside = !left_int[0].isEntering();
    }

    vector<Intersection> right_int;
    right->allIntersections(ray,right_int);
    if (right_int.empty()) return;
    bool right_inside = false;
    if (right_int.size() > 0) {
	right_inside = !right_int[0].isEntering();
    }
    
    // Merge intersections while preserving order
    result.reserve(left_int.size() + right_int.size());
    uint l = 0;
    uint r = 0;
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
}

double CSGDifference::_fastIntersect(const Ray& ray) const {
    vector<Intersection> all;
    allIntersections(ray,all);
    return all.empty() ? -1 : all.front().getT();
}

double CSGIntersection::_fastIntersect(const Ray& ray) const {
    vector<Intersection> all;
    allIntersections(ray,all);
    return all.empty() ? -1 : all.front().getT();
}


void CSGDifference::_fullIntersect(const Ray& ray, const double t, Intersection& result) const {
    vector<Intersection> all;
    allIntersections(ray,all);
    if (!all.empty()) {
	result = all.front();
    } else {
	throw_exception("This shouldn't happen...");
    }
}

void CSGIntersection::_fullIntersect(const Ray& ray, const double t, Intersection& result) const {
    vector<Intersection> all;
    allIntersections(ray,all);
    if (!all.empty()) {
	result = all.front();
    } else {
	throw_exception("This shouldn't happen...");
    }
}

