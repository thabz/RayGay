
#include "hierarchy.h"
#include <vector>
#include <cassert>
#include <iostream>
#include "object.h"
#include "boundingbox.h"
#include "intersection.h"
#include "ray.h"

using namespace std;

Hierarchy::Hierarchy() {
    _box = BoundingBox(Vector(-HUGE_DOUBLE,-HUGE_DOUBLE,-HUGE_DOUBLE),Vector(HUGE_DOUBLE,HUGE_DOUBLE,HUGE_DOUBLE));
    _parent = NULL;
    _depth = 1;
}

Hierarchy::Hierarchy(BoundingBox bbox) {
    _box = bbox;
    _parent = NULL;
    _depth = 1;
}

Hierarchy::Hierarchy(BoundingBox bbox, Hierarchy* parent) {
    _box = bbox;
    _parent = parent;
    _depth = parent->_depth + 1;
}

Hierarchy::~Hierarchy() {
    /*
   for (vector<Hierarchy*>::iterator h = children.begin(); h != children.end(); h++) {
       delete *h;
   }
   */
}

void Hierarchy::addObject(object* obj) {
    assert(obj->intersects(_box));
    if (hasChildren()) {
        for (vector<Hierarchy*>::iterator h = children.begin(); h != children.end(); h++) {
	    if (obj->intersects((*h)->getBoundingBox())) {
		(*h)->addObject(obj);
	    }
	}
    } else {
	objects.push_back(obj);
	if (objects.size() > HIERARCHY_MAX_OBJECTS_PER_LEAF && _depth < HIERARCHY_MAX_DEPTH) {
	    split();
	}
    }
}

bool Hierarchy::hasChildren() const {
    return !children.empty();
}

bool Hierarchy::hasObjects() const {
    return !objects.empty();
}

void Hierarchy::split() {
    assert(children.size() == 0);
    
    Vector mini = _box.minimum();
    Vector maxi = _box.maximum();

    double midx = (maxi[0] + mini[0]) / 2.0;
    double midy = (maxi[1] + mini[1]) / 2.0;
    double midz = (maxi[2] + mini[2]) / 2.0;
    Vector mid = Vector(midx,midy,midz);
    
    Vector* corners = _box.getCorners();
    for (int i = 0; i < 8; i++) {
	Hierarchy* h = new Hierarchy(BoundingBox(corners[i],mid),this);
	h->_depth = _depth + 1;
	children.push_back(h);
    }

    // Distribute my objects into the new sub-hierarchies
    for (vector<Hierarchy*>::iterator h = children.begin(); h != children.end(); h++) {
	for (vector<object*>::iterator p = objects.begin(); p != objects.end(); p++) {
	    if ((*p)->intersects((*h)->getBoundingBox())) {
		(*h)->addObject(*p);
	    }
	}
    }
    objects.clear();
    assert(objects.empty());
}

/**
 * Cleans op hierarchy.
 * Prunes all empty children recursively.
 * Optimizes paths by bypassing nodes with only one child.
 */
void Hierarchy::prepare() {
    pruneChildren();
    optimizePaths();
    shrinkBoundingBoxes();
}

void Hierarchy::pruneChildren() {
    vector<Hierarchy*> new_children;

    for (vector<Hierarchy*>::iterator h = children.begin(); h != children.end(); h++) {
	Hierarchy* hierarchy = (*h);
	if (hierarchy->hasChildren()) {
	    hierarchy->pruneChildren();
	    new_children.push_back(hierarchy);
	} else if (!hierarchy->hasObjects()) {
	    delete hierarchy;
	} else {
	    new_children.push_back(hierarchy);
	}
    }
    children = new_children;
}

 /**
  * Optimizes paths by bypassing nodes with only one child.
  */
void Hierarchy::optimizePaths() {
    vector<Hierarchy*> new_children;
    bool skipping;
    Hierarchy* child;

    for (vector<Hierarchy*>::iterator h = children.begin(); h != children.end(); h++) {
	child = *h;
	skipping = true;
	while (skipping) {
	    if (child->children.size() == 1) {
		child = child->children.front();
		child->_parent = this;
	    } else {
		skipping = false;
	    }
	}
	child->optimizePaths();
	new_children.push_back(child);
    }
    children = new_children;
}

/**
 * Shrink bounding boxes
 */
void Hierarchy::shrinkBoundingBoxes() {
    BoundingBox boundingBoundingBox;
    bool boxEmpty = true;
    
    for (vector<Hierarchy*>::iterator p = children.begin(); p != children.end(); p++) {
	(*p)->shrinkBoundingBoxes();
    }

    if (hasChildren()) {
	for (vector<Hierarchy*>::iterator p = children.begin(); p != children.end(); p++) {
	    if (boxEmpty) {
		boundingBoundingBox = (*p)->_box;
		boxEmpty = false;
	    } else {
		boundingBoundingBox = BoundingBox::doUnion(boundingBoundingBox,(*p)->_box);
	    }
	}
    } else if (hasObjects()) {
	for (vector<object*>::iterator p = objects.begin(); p != objects.end(); p++) {
	    if (boxEmpty) {
		boundingBoundingBox = (*p)->boundingBoundingBox();
		boxEmpty = false;
	    } else {
		boundingBoundingBox = BoundingBox::doUnion(boundingBoundingBox,(*p)->boundingBoundingBox());
	    }
	}
    }
    assert(!boxEmpty);
    _box = BoundingBox::doIntersection(_box,boundingBoundingBox);
}

double Hierarchy::area() {
    double area = _box.area();
    if (_parent == NULL)
	area = 0.0;
    for (vector<Hierarchy*>::iterator p = children.begin(); p != children.end(); p++) {
	area += (*p)->_box.area();
    }
    return area;
}

Intersection Hierarchy::intersect(const Ray& ray) const {
    Intersection result = Intersection(); 
    Intersection tmp;

    if (_parent != NULL) { // Skip boundscheck on root-box.
	if (!_box.checkIntersect(ray)) {
	    return result;
	}
    }

    if (hasChildren()) {
	for (vector<Hierarchy*>::const_iterator p = children.begin(); p != children.end(); p++) {
	    tmp = (*p)->intersect(ray);
	    if (tmp.isIntersected()) {
		if (!result.isIntersected() || tmp.getT() < result.getT()) {
		    result = tmp;
		}
	    }
	}
    } else if (hasObjects()) {
	for (vector<object*>::const_iterator p = objects.begin(); p != objects.end(); p++) {
	    (*p)->intersect(ray);
	    tmp = *((*p)->getLastIntersection());
	    if (tmp.isIntersected()) {
		if (!result.isIntersected() || tmp.getT() < result.getT()) {
		    result = tmp;
		}
	    }
	}
    }
    return result;
}

Intersection Hierarchy::intersectForShadow(const Ray& ray) const {
    Intersection result = Intersection(); 
    Intersection tmp;

    if (_parent != NULL) { // Skip boundscheck on root-box.
	if (!_box.checkIntersect(ray)) {
	    return result;
	}
    }

    if (hasChildren()) {
	for (vector<Hierarchy*>::const_iterator p = children.begin(); p != children.end(); p++) {
	    tmp = (*p)->intersectForShadow(ray);
	    if (tmp.isIntersected()) {
		return tmp;
	    }
	}
    } else if (hasObjects()) {
	for (vector<object*>::const_iterator p = objects.begin(); p != objects.end(); p++) {
	    (*p)->intersect(ray);
	    tmp = *((*p)->getLastIntersection());
	    if (tmp.isIntersected()) {
		return tmp;
	    }
	}
    }
    return result;
}

Intersection Hierarchy::intersectForShadow(const Ray& ray, const object* hint) const {
    if (hint != NULL) {
	hint->intersect(ray);
	Intersection i = *(hint->getLastIntersection());
	if (i.isIntersected()) {
	    return i;
	}
    }
    return intersectForShadow(ray);
}

ostream & operator<<(ostream &os, Hierarchy &x) {
	if (x.hasObjects()) {
	os << "Objects: (";
	for (vector<object*>::iterator p = x.objects.begin(); p != x.objects.end(); p++) {
	    os << "o";
	}
	os << ")";
    }

    if (x.hasChildren()) {
	os << "Children: (";
	for (vector<Hierarchy*>::iterator p = x.children.begin(); p != x.children.end(); p++) {
	    os << *(*p);
	}
	os << ")";
    }
    return os;
}


