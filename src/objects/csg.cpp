
#include "objects/csg.h"
#include "boundingbox.h"

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

bool CSG::inside(const Vector& p) const {
    switch(op) {
	case UNION:
	    return left->inside(p) || right->inside(p);
	case DIFFERENCE:
	    return left->inside(p) && !right->inside(p);
	case INTERSECTION:
	    return left->inside(p) && right->inside(p);
    }
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
    bool left_inside = left->inside(ray.getOrigin());
    bool right_inside = right->inside(ray.getOrigin());
    switch(op) {
	case UNION:
	    if (left_int.empty()) return right_int;
	    if (right_int.empty()) return left_int;
	case DIFFERENCE:
	case INTERSECTION:
	    return result;
    }
}

double CSG::_fastIntersect(const Ray& ray) const {
    double left_t, right_t;
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
	    vector<Intersection> all = allIntersections(ray);
	    return all.empty() ? -1 : all.front().getT();
    };
}

Intersection CSG::_fullIntersect(const Ray& ray, const double t) const {
    vector<Intersection> all = allIntersections(ray);
    switch(op) {
	case UNION:
	case DIFFERENCE:
	case INTERSECTION:
	    return all.empty() ? Intersection() : all.front();
    }
}

