
#include <cassert>
#include <list>

#include "kdtree.h"
#include "intersection.h"
#include "ray.h"
#include "object.h"
#include "ray.h"
#include "stats.h"
#include "boundingbox.h"
#include "math/vector2.h"

Object* KdTree::last_primary_intersected_object = NULL;

KdTree::KdTree() {
    added_objects = new vector<Object*>;
    prepared = false;
}

KdTree::~KdTree() {
    if (prepared) {
	delete[] stack;
    }
}

void KdTree::addObject(Object* obj) {
    Stats::getUniqueInstance()->inc("KdTree: Objects added");
    added_objects->push_back(obj);
}

inline
bool KdTree::intersect(const Ray& ray) const {
    //Vector2 h = world_bbox.intersect(ray);
    Vector2 h = Vector2(0,HUGE_DOUBLE);
    return intersect(ray,h[0],h[1]);
}

inline
bool KdTree::intersectPrimary(const Ray& ray) const {
    //Vector2 h = world_bbox.intersect(ray);
    Vector2 h = Vector2(0,HUGE_DOUBLE);
    return intersect(ray,h[0],h[1]);
}

inline
bool KdTree::intersectForShadow(const Ray& ray, double max_t) const {
    return intersectForShadow(ray,double(0),max_t);
}

bool KdTree::intersectForShadow(const Ray& ray, const Object* hint, double max_t) const {
    if (hint != NULL && hint->intersect(ray) && hint->getLastIntersection()->getT() < max_t) {
	last_intersection = hint->getLastIntersection();
	return true;
    } else {
	return intersectForShadow(ray,max_t);
    }
}

// TODO: Write a faster intersect for shadows
bool KdTree::intersectForShadow(const Ray& ray,double a, double b) const {
    return intersect(ray,a,b);
}

void KdTree::prepare() {
    world_bbox = enclosure(added_objects);
    tmp_nodes.push_back(KdNodeTmp());
    tmp_nodes[0].objects = added_objects;
    tmp_nodes[0].axis = 0;
    assert(tmp_nodes.size() == 1);
    max_depth = 0;
    prepare(0,1);
    int nodes_num = tmp_nodes.size();
    nodes = new KdNode[nodes_num];
    stack = new StackElem[max_depth*10];
    cout << "Prepared..." << endl;
    cout << "Max depth: " << max_depth << endl;
    cout << "Nodes: " << nodes_num << endl;

    for(int i = 0; i < nodes_num; i++) {
	KdNode node = KdNode();
	KdNodeTmp old = tmp_nodes[i];
	node.objects = old.objects;
	node.splitPlane = old.splitPlane;
	node.axis = old.axis;
	node.left = &(nodes[old.left]);
	node.right = &(nodes[old.right]);
	nodes[i] = node;
    }
    tmp_nodes.clear();
}

void KdTree::prepare(int curNode_idx,int depth) {
    KdNodeTmp* curNode = &(tmp_nodes[curNode_idx]);

    if (depth > max_depth) 
	max_depth = depth;

    if (curNode->objects->size() <= KD_TREE_MAX) {
	curNode->axis = -1;
	return;
    }

    // Find the splitplane value
    curNode->splitPlane = float(objectMedian(curNode->objects,curNode->axis));

    tmp_nodes.push_back(KdNodeTmp());
    tmp_nodes.push_back(KdNodeTmp());
    curNode = &(tmp_nodes[curNode_idx]); // Reloading since it might have changed
    int left_idx = tmp_nodes.size() - 1;
    int right_idx = tmp_nodes.size() - 2;
    curNode->left = left_idx;
    curNode->right = right_idx;
    KdNodeTmp* lower = &tmp_nodes[curNode->left];
    KdNodeTmp* higher = &tmp_nodes[curNode->right];
    lower->axis = (curNode->axis + 1) % 3;
    higher->axis = (curNode->axis + 1) % 3;

    unsigned int size = curNode->objects->size();

    // Put all objects into lower- or higher_objects
    int l = 0; int m = 0; int h = 0;
    vector<Object*>::iterator p = curNode->objects->begin();
    while (p != curNode->objects->end()) {
	Object* obj = *p;
	assert(obj != NULL);
	const BoundingBox bbox = obj->boundingBoundingBox();
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
	p++;
    }

    if (lower->objects->size() == size || higher->objects->size() == size) {
	// Objects couldn't be subdivided
	curNode->axis = -1;
	lower->objects->clear();
	higher->objects->clear();
	//delete [] lower->objects;
	//delete [] higher->objects;
	tmp_nodes.pop_back();
	tmp_nodes.pop_back();
    } else {
	curNode->objects->clear();
	//delete curNode->objects;
	// Recursive prepare()
	prepare(left_idx,depth+1);
	prepare(right_idx,depth+1);
    }
}

/**
 * Implementation of the recursive $f[ TA_rec^B $f] algorithm.
 * See http://sgi.felk.cvut.cz/~havran/phdthesis.html
 * See http://www.acm.org/jgt/papers/HavranKopalBittnerZara97/TA-B.html
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
		
		/*
		if (stack[exPt].pb[axis] == splitVal) { //TODO: Wierd!
		    curNode = curNode->right;
		    continue;
		}
		*/
		
		farChild = curNode->right;
		curNode = curNode->left;
	    } else {
		if (splitVal <= stack[exPt].pb[axis]) {
		    curNode = curNode->right;
		    continue;
		}
		farChild = curNode->left;
		curNode = curNode->right;
	    }

	    t = (splitVal - ray.getOrigin()[axis]) / ray.getDirection()[axis];

	    int tmp = exPt;
	    exPt++;

	    if (exPt == enPt)
		exPt++;

	    stack[exPt].prev = tmp;
	    stack[exPt].t = t;
	    stack[exPt].node = farChild;
	    stack[exPt].pb[axis] = splitVal;
	    int nextAxis = (axis+1) % 3;
	    int prevAxis = (axis+2) % 3;
	    stack[exPt].pb[nextAxis] = ray.getOrigin()[nextAxis] + 
		                       t * ray.getDirection()[nextAxis];
	    stack[exPt].pb[prevAxis] = ray.getOrigin()[prevAxis] +
		                       t * ray.getDirection()[prevAxis];
	} /* while curNode not a leaf */

	// Intersect with all objects in list, discarding
	// those lying before stack[enPt].t or farther than stack[exPt].t
	Object* object_hit = NULL;
	if (curNode->objects->size() > 0) {
	    const vector<Object*> &objects = *(curNode->objects);
	    double smallest_t = HUGE_DOUBLE;
	    for (unsigned int i = 0; i < objects.size(); i++) {
		if (objects[i]->intersect(ray)) {
		    double i_t =  objects[i]->getLastIntersection()->getT();
		    if (i_t < smallest_t && i_t > stack[enPt].t && i_t < stack[exPt].t) {
			smallest_t = objects[i]->getLastIntersection()->getT();
			object_hit = objects[i];
		    }
		}
	    }
	}
	if (object_hit != NULL) {
	    last_intersection = object_hit->getLastIntersection();
	    return true;
	}

	enPt = exPt;

	curNode = stack[exPt].node;
	exPt = stack[enPt].prev;
    } /* while curNode != end of nodes */
    return false;
}

KdTree::KdNode::KdNode() {
}

KdTree::KdNodeTmp::KdNodeTmp() {
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

double KdTree::objectMedian(std::vector<Object*>* objects, int d) const {
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

double KdTree::spacialMedian(std::vector<Object*>* objects, int d) const {
    BoundingBox box = enclosure(objects);
    return (box.minimum()[d] + box.maximum()[d]) / 2.0;
}
