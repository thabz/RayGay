
#include <cassert>
#include <list>

#include "kdtree.h"
#include "intersection.h"
#include "ray.h"
#include "object.h"
#include "ray.h"
#include "stats.h"
#include "boundingbox.h"

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
    added_objects.push_back(obj);
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
    tmp_nodes.push_back(KdNode());
    tmp_nodes[0].objects = &added_objects;
    max_depth = 0;
    prepare(&(tmp_nodes[0]),1);

    int nodes_num = tmp_nodes.size();
    nodes = new KdNode[nodes_num];
    stack = new StackElem[max_depth];
    for(int i = 0; i < nodes_num; i++) {
	nodes[i] = tmp_nodes[i];
    }
}

void KdTree::prepare(KdNode* curNode,int depth) {
    if (depth > max_depth)
	max_depth = depth;

    if (curNode->objects->size() <= KD_TREE_MAX) {
	curNode->axis = -1;
    } else {
	// Find the cutplane_dimension and cutplane_value
	BoundingBox bbox = enclosure(curNode->objects);
	Vector best_measure = Vector(0,HUGE_DOUBLE,0);
	int best_dim = -1;
	double best_val = 1;
	for(int i = 0; i < 3; i++) {
	    double val = median(curNode->objects,i);
	    Vector measure = measureSplit(curNode->objects,i,val);
	    if (measure[1] < best_measure[1] &&
		measure[0] <  curNode->objects->size() &&
		measure[2] <  curNode->objects->size()) {
		best_dim = i;
		best_val = val;
		best_measure = measure;
	    }
	}
	if (best_dim != -1) {
	    curNode->axis = best_dim;
	    curNode->splitPlane = best_val;
	} else {
	    curNode->axis = largestDimension(bbox);
	    curNode->splitPlane = median(curNode->objects,curNode->axis);
	}

	tmp_nodes.push_back(KdNode());
	tmp_nodes.push_back(KdNode());
	KdNode* lower = &(tmp_nodes[tmp_nodes.size() - 1]);
	KdNode* higher = &(tmp_nodes[tmp_nodes.size() - 2]);
	curNode->left = lower;
	curNode->right = higher;

	unsigned int size = curNode->objects->size();

	// Put all objects into lower- or higher_objects
	int l = 0; int m = 0; int h = 0;
	for(vector<Object*>::iterator p = curNode->objects->begin(); p != curNode->objects->end(); p++) {
	    Object* obj = *p;
	    BoundingBox bbox = obj->boundingBoundingBox();
	    int cut_val = bbox.cutByPlane(curNode->axis, curNode->splitPlane);
	    if (cut_val == -1) {
		lower->objects->push_back(obj);
		l++;
	    } else if (cut_val == 1) {
		higher->objects->push_back(obj);
		h++;
	    } else {
		lower->objects->push_back(obj);
		higher->objects->push_back(obj);
		m++;
	    }
	}
	return; // TODO: Remove
	
	if (lower->objects->size() == size || higher->objects->size() == size) {
	    // Objects couldn't be subdivided
	    curNode->axis = -1;
	    delete lower->objects;
	    delete higher->objects;
	    tmp_nodes.pop_back();
	    tmp_nodes.pop_back();
	} else {
	    delete curNode->objects;
	    // Recursive prepare()
	    prepare(lower,depth+1);
	    prepare(higher,depth+1);
	}
    } 
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
	    exPt++;
	  //  Increment(exPt);

	    if (exPt == enPt)
		exPt++;
	//	Increment(exPt);

	    stack[exPt].prev = tmp;
	    stack[exPt].t = t;
	    stack[exPt].node = farChild;
	    stack[exPt].pb[axis] = splitVal;
	    int nextAxis = (axis+1) & 3;
	    int prevAxis = (axis-1) & 3;
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

KdTree::KdNode::KdNode() {
    objects = new std::vector<Object*>;
}

int KdTree::largestDimension(const BoundingBox& box) {
    double x = box.maximum()[0] - box.minimum()[0];
    double y = box.maximum()[1] - box.minimum()[1];
    double z = box.maximum()[2] - box.minimum()[2];
    double max = MAX(x,MAX(y,z));
    if (IS_EQUAL(x,max)) {
	    return 0;
    } else if (IS_EQUAL(y,max)) {
	    return 1;
    } else if (IS_EQUAL(z,max)) {
	    return 2;
    } else {
	return -1;
       // Throw an exception
    }
}

BoundingBox KdTree::enclosure(std::vector<Object*>* objects) const {
    assert(objects->size() > 0);
    BoundingBox result = (*objects)[0]->boundingBoundingBox(); 
    for(unsigned int i = 1; i < objects->size(); i++) {
        result = BoundingBox::doUnion(result,(*objects)[i]->boundingBoundingBox());
    }
    return result;
}

double KdTree::median(std::vector<Object*>* objects, int d) const {
    std::list<double> L;
    for(unsigned int i = 0; i < objects->size(); i++) {
	    Object* obj = (*objects)[i];
	    BoundingBox bbox = obj->boundingBoundingBox();
	    double c = (bbox.maximum()[d] + bbox.minimum()[d]) / 2.0;
	    L.push_back(c);
    }
    L.sort();
    unsigned int size = L.size();
    assert(size == objects->size());
    // Return L[size/2]
    unsigned int i = 0;
    for (std::list<double>::iterator h = L.begin(); h != L.end(); h++) {
	if (i++ > size/2) return *h;
    }
    exit(0);
}

Vector KdTree::measureSplit(std::vector<Object*>* objects, int dim, double val) const {
    Vector result = Vector(0,0,0);
    for(unsigned int i = 0; i < objects->size(); i++) {
	Object* obj = (*objects)[i];
	BoundingBox bbox = obj->boundingBoundingBox();
	int cut_val = bbox.cutByPlane(dim, val);
	if (cut_val == -1) {
	    result[0]++;
	} else if (cut_val == 1) {
	    result[2]++;
	} else {
	    result[1]++;
	}
    }
    return result;
}
