
#include <cassert>
#include "objects/csg.h"
#include "aabox.h"
#include "exception.h"
#include <algorithm>

CSGUnion::CSGUnion(Solid* left, Solid* right, const Material* mat) : Solid(mat) {
    this->left = left;
    this->right = right;
    this->max_intersections = left->maxIntersections() + right->maxIntersections();
}

CSGUnion::CSGUnion(vector<Solid*>* solids, const Material* mat) : Solid(mat) {
    uint32_t size = solids->size();
    if (size < 2) {
	throw_exception("At least two solids are needed.");
    } else if (size == 2) {
	this->left = solids->operator[](0);
	this->right = solids->operator[](1);
    } else {
	vector<Solid*>* lefts = new vector<Solid*>(); 
	vector<Solid*>* rights = new vector<Solid*>(); 
	for(uint32_t i = 0; i < size/2; i++) {
	    lefts->push_back(solids->operator[](i));
	}
	for(uint32_t i = size/2; i < size; i++) {
	    rights->push_back(solids->operator[](i));
	}
	if (lefts->size() == 1) {
	    left = lefts->operator[](0);
	} else {
	    left = new CSGUnion(lefts,mat);
	}
	if (rights->size() == 1) {
	    right = rights->operator[](0);
	} else {
	    right = new CSGUnion(rights,mat);
	}
	delete lefts;
	delete rights;
    }
    this->max_intersections = left->maxIntersections() + right->maxIntersections();
}

SceneObject* CSGUnion::clone() const {
    Solid* lhs = dynamic_cast<Solid*>(this->left->clone());
    Solid* rhs = dynamic_cast<Solid*>(this->right->clone());
    return new CSGUnion(lhs, rhs, this->getMaterial());
}

AABox CSGUnion::getBoundingBox() const {
    AABox rb = right->getBoundingBox();
    AABox lb = left->getBoundingBox();
    return AABox::doUnion(rb,lb);
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

uint32_t CSGUnion::maxIntersections() const {
    if (max_intersections == 0) throw_exception("What!");
    return max_intersections;
}

uint32_t CSGUnion::allIntersections(const Ray& ray, Intersection* result) const {
    Intersection* left_int = (Intersection*)::alloca(sizeof(Intersection)*left->maxIntersections());
    Intersection* right_int = (Intersection*)::alloca(sizeof(Intersection)*right->maxIntersections());

    uint32_t left_num = left->allIntersections(ray,left_int);
    uint32_t right_num = right->allIntersections(ray,right_int);

    if (left_num + right_num > max_intersections) {
	cout << "Argh!" << endl;
    }

    uint32_t l = 0;
    uint32_t r = 0;
    uint32_t j = 0;
    bool left_inside = false;
    bool right_inside = false;

    if (left_num > 0) {
	left_inside = !left_int[0].isEntering();
    }
    if (right_num > 0) {
	right_inside = !right_int[0].isEntering();
    }


    // Merge intersections while preserving order
    while (l < left_num && r < right_num) {
	if (left_int[l].getT() < right_int[r].getT()) {
	    Intersection& i = left_int[l++];
	    left_inside = i.isEntering();
	    if (!right_inside) {
		result[j++] = i;
	    }
	} else {
	    Intersection& i = right_int[r++];
	    right_inside = i.isEntering();
	    if (!left_inside) {
		result[j++] = i;
	    }
	}
    }
    // Copy remaining 
    while (l < left_num) result[j++] = left_int[l++];
    while (r < right_num) result[j++] = right_int[r++];

    return j;
}

double CSGUnion::_fastIntersect(const Ray& ray) const {
    double left_t, right_t;
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

void CSGUnion::_fullIntersect(const Ray& ray, const double t, Intersection& result) const {
    Intersection* all = (Intersection*)::alloca(sizeof(Intersection)*maxIntersections());
    uint32_t num = allIntersections(ray, all);
    if (num > 0) {
	result = all[0];
    } else {
	throw_exception("This shouldn't happen...");
    }
}

bool CSGUnion::inside(const Vector& p) const {
    return left->inside(p) || right->inside(p);        
}

