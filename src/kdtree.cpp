
#include "kdtree.h"
#include "intersection.h"
#include "ray.h"
#include "object.h"
#include "ray.h"
#include "stats.h"

Object* KdTree::last_primary_intersected_object = NULL;

KdTree::KdTree() {
    prepared = false;
}

KdTree::~KdTree() {
    if (prepared) {
	delete[] stack;
    }
}

void KdTree::addObject(Object* obj) {
    Stats::getUniqueInstance()->inc("KdTree: Objects added");
    objects.push_back(obj);
}

inline
bool KdTree::intersect(const Ray& ray) const {
    ray.lowest_t = HUGE_DOUBLE;
    return intersect(ray,double(0),HUGE_DOUBLE);
}

inline
bool KdTree::intersectPrimary(const Ray& ray) const {
    return intersect(ray,double(0),HUGE_DOUBLE);
}

inline
bool KdTree::intersectForShadow(const Ray& ray) const {
    return intersectForShadow(ray,double(0),HUGE_DOUBLE);
}

bool KdTree::intersectForShadow(const Ray& ray, const Object* hint) const {
    if (hint != NULL && hint->intersect(ray)) {
	last_intersection = hint->getLastIntersection();
	return true;
    } else {
	return intersectForShadow(ray);
    }
}

// TODO: Write a faster intersect for shadows
bool KdTree::intersectForShadow(const Ray& ray,double a, double b) const {
    return intersect(ray,a,b);
}

void KdTree::prepare() {
    // TODO: Find max depth
    stack = new StackElem[10000];
}

/**
 * Implementation of the recursive $f[ TA_rec^B $f] algorithm.
 */
bool KdTree::intersect(const Ray& ray, double a, double b) const {

    double t;
    KdNode *farChild, *curNode;
    curNode = nodes;
    int enPt = 0;
    stack[enPt].t = a;

    if (a >= 0.0) 
	stack[enPt].pb = ray.getOrigin() + ray.getDirection() * a;
    else 
	stack[enPt].pb = ray.getOrigin();

    int exPt = 1;
    stack[exPt].t = b;
    stack[exPt].pb = ray.getOrigin() + ray.getDirection() * b;
    stack[exPt].node = NULL;

    while (curNode != NULL) {
	while (curNode->axis >= 0) {
	    /* Current node is not a leaf */
	    double splitVal = curNode->splitPlane;
	    int axis = curNode->axis; // ?

	    if (stack[enPt].pb[axis] <= splitVal) {
		if (stack[exPt].pb[axis] <= splitVal) {
		    curNode = curNode->left;
		    continue;
		}
		if (stack[exPt].pb[axis] == splitVal) {
		    curNode = curNode->right;
		    continue;
		}
		farChild = curNode->right;
		curNode = curNode->left;
	    } else {
		if (splitVal < stack[exPt].pb[axis]) {
		    curNode = curNode->right;
		    continue;
		}
		farChild = curNode->left;
		curNode = curNode->right;
	    }

	    t = (splitVal - ray.getOrigin()[axis]) / ray.getDirection()[axis];

	    int tmp = exPt;
	    Increment(exPt);

	    if (exPt == enPt)
		Increment(exPt);

	    stack[exPt].prev = tmp;
	    stack[exPt].t = t;
	    stack[exPt].node = farChild;
	    stack[exPt].pb[axis] = splitVal;
	    stack[exPt].pb[nextAxis] = ray.getOrigin()[nextAxis] + 
		                       t * ray.getDirection()[nextAxis];
	    stack[exPt].pb[prevAxis] = ray.getOrigin()[prevAxis] +
		                       t * ray.getDirection()[prevAxis];
	} /* while curNode not a leaf */

	// TODO: Intersect with all objects in list, discarding
	// those lying before stack[enPt].t or farther than stack[exPt].t
	bool object_hit = false;
	if (curNode->objects->size() > 0) {
	    const vector<Object*> objects = *(curNode->objects);
	    double smallest_t = HUGE_DOUBLE;
	    for (unsigned int i = 0; i < objects.size(); i++) {
		if (objects[i]->intersect(ray) && 
			objects[i]->getLastIntersection()->getT() < smallest_t) {
		    last_intersection = objects[i]->getLastIntersection();
		    smallest_t = last_intersection->getT();
		    object_hit = true;
		}
	    }
	    ray.lowest_t = min(ray.lowest_t,smallest_t);
	}
	if (object_hit) return true;
	
	enPt = exPt;

	curNode = stack[exPt].node;
	exPt = stack[enPt].prev;
    } /* while curNode != end of nodes */
    return false;
}
