
#include <cassert>
#include "objects/csg.h"
#include "boundingbox.h"
#include "exception.h"
#include <algorithm>

CSGUnion::CSGUnion(Solid* left, Solid* right, const Material* mat) : Solid(mat) {
    this->left = left;
    this->right = right;
}

CSGUnion::CSGUnion(vector<Solid*>* solids, const Material* mat) : Solid(mat) {
    unsigned int size = solids->size();
    if (size < 2) throw_exception("At least two solids are needed.");
    if (size == 2) {
	this->left = solids->front();
	this->right = solids->operator[](1);
    } else {
	this->left = solids->operator[](0);
	this->right = solids->operator[](1);
	for(unsigned int i = 2; i < size; i++) {
	    this->right = new CSGUnion(this->right,solids->operator[](i),NULL);
	}
    }
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

class compareIntersectionsAsc {
    public:
	bool operator()(const Intersection& i1, const Intersection &i2) {
	    return i1.getT() < i2.getT();
	}
};

void CSGUnion::allIntersections(const Ray& ray, vector<Intersection>& result) const {
    vector<Intersection> left_int;
    left->allIntersections(ray,left_int);
    vector<Intersection> right_int;
    right->allIntersections(ray,right_int);
    result.reserve(left_int.size() + right_int.size());

    /*
    vector<Intersection> intersections;
    unsigned int i = 0;
    while (i < left_int.size()) intersections.push_back(left_int[i++]);
    i = 0;
    while (i < right_int.size()) intersections.push_back(right_int[i++]);
    sort(intersections.begin(),intersections.end(),compareIntersectionsAsc());

    if (intersections.size() == 0) return;

    bool inside = !intersections.front().isEntering();

    i = 0;
    while (i < intersections.size()) {
	Intersection& intersection = intersections[i];
	if ((intersection.isEntering() && !inside) ||
            (!intersection.isEntering() && inside)) {
	    result.push_back(intersection);
	    inside = !inside;
	}
	i++;
    }
    return;
    */

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
