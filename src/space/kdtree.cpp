#include <cassert>
#include <list>

#include "kdtree.h"
#include "intersection.h"
#include "ray.h"
#include "objects/object.h"
#include "ray.h"
#include "stats.h"
#include "boundingbox.h"
#include "math/vector2.h"

#undef VERBOSE 

KdTree::KdTree() {
    added_objects = new vector<Object*>;
    prepared = false;
}

KdTree::~KdTree() {
}

void KdTree::addObject(Object* obj) {
    //Stats::getUniqueInstance()->inc(STATS_KDTREE_OBJECTS_ADDED);
    added_objects->push_back(obj);
}

bool KdTree::intersect(const Ray& ray, Intersection* result) const {
    Vector2 h = world_bbox.intersect(ray);
    if (h[1] < h[0]) {
	return false;
    } else {
	return intersect(ray,result,0.0,h[1]);
    }
}

bool KdTree::intersectPrimary(const Ray& ray, Intersection* result) const {
    Vector2 h = world_bbox.intersect(ray);
    if (h[1] < h[0]) {
	return false;
    } else {
	return intersect(ray,result,max(h[0],0.0),h[1]);
    }
}

Object* KdTree::intersectForShadow(const Ray& ray, double max_t) const {
    Vector2 h = world_bbox.intersect(ray);
    if (h[1] < h[0]) {
	return NULL;
    } else {
	return intersectForShadow(ray,double(0),min(max_t,h[1]));
    }
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
#ifdef VERBOSE    
    cout << "Prepared..." << endl;
    cout << "Max depth: " << max_depth << endl;
    cout << "Nodes: " << nodes_num << endl;
#endif    
    for(int i = 0; i < nodes_num; i++) {
	KdNode node = KdNode();
	KdNodeTmp old = tmp_nodes[i];
	node.splitPlane = old.splitPlane;
	node.axis = old.axis;
	if (node.axis >= 0) {
	    node.left = &(nodes[old.left]);
	    node.right = &(nodes[old.right]);
	} else {
	    node.objects = old.objects;
	}
	nodes[i] = node;
    }
    tmp_nodes.clear();
}

void KdTree::prepare(int curNode_idx,int depth) {
    KdNodeTmp* curNode = &(tmp_nodes[curNode_idx]);

    if (depth > max_depth) 
	max_depth = depth;

    std::vector<Object*>* objects = curNode->objects;

    if (objects->size() <= KD_TREE_MAX) {
	curNode->axis = -1;
	return;
    }
    if (depth > KD_TREE_MAX_DEPTH) {
	curNode->axis = -1;
	return;
    }
    // Find the best axis to split
    Vector best_measure = Vector(0,HUGE_DOUBLE,0);
    int best_dim = -1;
    double best_val = 1;
    for(int i = 0; i < 3; i++) {
	double val = objectMedian(objects,i);
	Vector measure = measureSplit(objects,i,val);
	if (measure[1] < best_measure[1] &&
		measure[0] <  objects->size() &&
		measure[2] <  objects->size()) {
	    best_dim = i;
	    best_val = val;
	    best_measure = measure;
	}
    }
#ifdef VERBOSE    
    for(int i = 0; i < depth; i++) {
        cout << "* ";
    }
    cout << objects->size() << " -> " << best_measure;
#endif
    if (best_measure[1] > best_measure[0] + best_measure[2]) {
#ifdef VERBOSE	
	cout << " Leaf" << endl;
#endif	
	curNode->axis = -1;
	return;
    }
    
    if (best_dim != -1) {
	curNode->axis = best_dim;
	curNode->splitPlane = best_val;
    } else {
	curNode->axis = largestDimension(enclosure(objects));
	curNode->splitPlane = objectMedian(objects,curNode->axis);
    }


    // Find the splitplane value
    //curNode->splitPlane = float(objectMedian(curNode->objects,curNode->axis));

    tmp_nodes.push_back(KdNodeTmp());
    tmp_nodes.push_back(KdNodeTmp());
    curNode = &(tmp_nodes[curNode_idx]); // Reloading since it might have changed
    int left_idx = tmp_nodes.size() - 1;
    int right_idx = tmp_nodes.size() - 2;
    curNode->left = left_idx;
    curNode->right = right_idx;
    KdNodeTmp* lower = &tmp_nodes[curNode->left];
    KdNodeTmp* higher = &tmp_nodes[curNode->right];
    //lower->axis = 2;//(curNode->axis + 1) % 3;
    //higher->axis = 1;//(curNode->axis + 1) % 3;

    unsigned int size = objects->size();

    // Put all objects into lower- or higher_objects
    int l = 0; int m = 0; int h = 0;
    vector<Object*>::iterator p = objects->begin();
    while (p != objects->end()) {
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
#ifdef VERBOSE	
	cout << " Leaf" << endl;
#endif	
	// Objects couldn't be subdivided
	curNode->axis = -1;
	lower->objects->clear();
	higher->objects->clear();
	//delete [] lower->objects;
	//delete [] higher->objects;
	tmp_nodes.pop_back();
	tmp_nodes.pop_back();
    } else {
#ifdef VERBOSE	
	cout << endl;
#endif	
	objects->clear();
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
bool KdTree::intersect(const Ray& ray, Intersection* result, const double a, const double b) const {

    StackElem* stack = (StackElem*)alloca(sizeof(StackElem)*(max_depth*2));

    double t;
    KdNode *farChild, *curNode;
    curNode = nodes;
    int enPt = 0;
    stack[enPt].t = a;

    if (a >= 0.0) {
	stack[enPt].pb[0] = ray.getOrigin()[0] + ray.getDirection()[0] * a;
	stack[enPt].pb[1] = ray.getOrigin()[1] + ray.getDirection()[1] * a;
	stack[enPt].pb[2] = ray.getOrigin()[2] + ray.getDirection()[2] * a;
    } else {
	stack[enPt].pb[0] = ray.getOrigin()[0];
	stack[enPt].pb[1] = ray.getOrigin()[1];
	stack[enPt].pb[2] = ray.getOrigin()[2];    
    }
    int exPt = 1;
    stack[exPt].t = b;
    stack[exPt].pb[0] = ray.getOrigin()[0] + ray.getDirection()[0] * b;
    stack[exPt].pb[1] = ray.getOrigin()[1] + ray.getDirection()[1] * b;
    stack[exPt].pb[2] = ray.getOrigin()[2] + ray.getDirection()[2] * b;
    stack[exPt].node = NULL;

    while (curNode != NULL) {
	while (curNode->axis >= 0) {
	    /* Current node is not a leaf */
	    double splitVal = curNode->splitPlane;
	    int axis = curNode->axis; // ?
	    switch(axis) {
		case 0:
		    {
			if (stack[enPt].pb[0] <= splitVal) {
			    if (stack[exPt].pb[0] <= splitVal) {
				curNode = curNode->left;
				continue;
			    }
			    farChild = curNode->right;
			    curNode = curNode->left;
			} else {
			    if (splitVal <= stack[exPt].pb[0]) {
				curNode = curNode->right;
				continue;
			    }
			    farChild = curNode->left;
			    curNode = curNode->right;
			}

			t = (splitVal - ray.getOrigin()[0]) / ray.getDirection()[0];

			int tmp = exPt;
			exPt++;

			if (exPt == enPt)
			    exPt++;

			stack[exPt].prev = tmp;
			stack[exPt].t = t;
			stack[exPt].node = farChild;
			stack[exPt].pb[0] = splitVal;
			stack[exPt].pb[1] = ray.getOrigin()[1] + t*ray.getDirection()[1];
			stack[exPt].pb[2] = ray.getOrigin()[2] + t*ray.getDirection()[2];
			continue;
		    }
		case 1:
		    {
			if (stack[enPt].pb[1] <= splitVal) {
			    if (stack[exPt].pb[1] <= splitVal) {
				curNode = curNode->left;
				continue;
			    }
			    farChild = curNode->right;
			    curNode = curNode->left;
			} else {
			    if (splitVal <= stack[exPt].pb[1]) {
				curNode = curNode->right;
				continue;
			    }
			    farChild = curNode->left;
			    curNode = curNode->right;
			}

			t = (splitVal - ray.getOrigin()[1]) / ray.getDirection()[1];

			int tmp = exPt;
			exPt++;

			if (exPt == enPt)
			    exPt++;

			stack[exPt].prev = tmp;
			stack[exPt].t = t;
			stack[exPt].node = farChild;
			stack[exPt].pb[0] = ray.getOrigin()[0] + t*ray.getDirection()[0];
			stack[exPt].pb[1] = splitVal;
			stack[exPt].pb[2] = ray.getOrigin()[2] + t*ray.getDirection()[2];
			continue;

		    }
		case 2:
		    {
			if (stack[enPt].pb[2] <= splitVal) {
			    if (stack[exPt].pb[2] <= splitVal) {
				curNode = curNode->left;
				continue;
			    }
			    farChild = curNode->right;
			    curNode = curNode->left;
			} else {
			    if (splitVal <= stack[exPt].pb[2]) {
				curNode = curNode->right;
				continue;
			    }
			    farChild = curNode->left;
			    curNode = curNode->right;
			}

			t = (splitVal - ray.getOrigin()[2]) / ray.getDirection()[2];

			int tmp = exPt;
			exPt++;

			if (exPt == enPt)
			    exPt++;

			stack[exPt].prev = tmp;
			stack[exPt].t = t;
			stack[exPt].node = farChild;
			stack[exPt].pb[0] = ray.getOrigin()[0] + t*ray.getDirection()[0];
			stack[exPt].pb[1] = ray.getOrigin()[1] + t*ray.getDirection()[1];
			stack[exPt].pb[2] = splitVal;
			continue;

		    }
	    }
	}/* while curNode not a leaf */

	// Intersect with all objects in list, discarding
	// those lying before stack[enPt].t or farther than stack[exPt].t
	Object* object_hit = NULL;
	double smallest_t = HUGE_DOUBLE;
	if (!curNode->objects->empty()) {
	    vector<Object*>* objects = curNode->objects;
	    unsigned int objects_size = objects->size();
	    for (unsigned int i = 0; i < objects_size; i++) {
		double i_t = (*objects)[i]->fastIntersect(ray);
		if (i_t > 0 && i_t < smallest_t && i_t > stack[enPt].t && i_t < stack[exPt].t) {
		    smallest_t = i_t;
		    object_hit = (*objects)[i];
		}
	    }
	}
	if (object_hit != NULL) {
	    *(result) = object_hit->fullIntersect(ray,smallest_t);
	    return true;
	}

	enPt = exPt;

	curNode = stack[exPt].node;
	exPt = stack[enPt].prev;
    } /* while curNode != end of nodes */
    return false;
}

Object* KdTree::intersectForShadow(const Ray& ray, const double a, const double b) const {

    StackElem* stack = (StackElem*)alloca(sizeof(StackElem)*(max_depth*2));

    double t;
    KdNode *farChild, *curNode;
    curNode = nodes;
    int enPt = 0;
    stack[enPt].t = a;

    if (a >= 0.0) {
	stack[enPt].pb[0] = ray.getOrigin()[0] + ray.getDirection()[0] * a;
	stack[enPt].pb[1] = ray.getOrigin()[1] + ray.getDirection()[1] * a;
	stack[enPt].pb[2] = ray.getOrigin()[2] + ray.getDirection()[2] * a;
    } else {
	stack[enPt].pb[0] = ray.getOrigin()[0];
	stack[enPt].pb[1] = ray.getOrigin()[1];
	stack[enPt].pb[2] = ray.getOrigin()[2];    
    }
    int exPt = 1;
    stack[exPt].t = b;
    stack[exPt].pb[0] = ray.getOrigin()[0] + ray.getDirection()[0] * b;
    stack[exPt].pb[1] = ray.getOrigin()[1] + ray.getDirection()[1] * b;
    stack[exPt].pb[2] = ray.getOrigin()[2] + ray.getDirection()[2] * b;
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
	if (!curNode->objects->empty()) {
	    const vector<Object*> &objects = *(curNode->objects);
	    unsigned int objects_size = objects.size();
	    for (unsigned int i = 0; i < objects_size; i++) {
		double i_t = objects[i]->fastIntersect(ray);
		if (i_t > 0 && i_t > stack[enPt].t && i_t < stack[exPt].t) {
		    return objects[i];
		}
	    }
	}

	enPt = exPt;

	curNode = stack[exPt].node;
	exPt = stack[enPt].prev;
    } /* while curNode != end of nodes */
    return NULL;
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
    double split = 0;
    for (std::list<double>::iterator h = L.begin(); h != L.end(); h++) {
	if (i++ > size/2) {
	    split = *h;
	    goto KAJ;
	}
    }
    KAJ:

    double best_split = split;
    if (objects->size() < 500) {
	// Try to refine best_split value
	double best_count = objects->size() * 10;
	int min_side = objects->size() / 3;
	for(unsigned int i = 0; i < objects->size(); i++) {
	    Object* obj = (*objects)[i];
	    BoundingBox bbox = obj->boundingBoundingBox();
	    if (bbox.cutByPlane(d,split) == 0) {
		double v;
		Vector measure;
		v = bbox.minimum()[d];
		measure = measureSplit(objects,d,v);
		if (measure[1] < best_count && measure[0] > min_side && measure[2] > min_side) {
		    best_split = v;
		    best_count = measure[1];
		}
		v = bbox.maximum()[d];
		measure = measureSplit(objects,d,v);
		if (measure[1] < best_count && measure[0] > min_side && measure[2] > min_side) {
		    best_split = v;
		    best_count = measure[1];
		}
	    }
	}
    }
    return best_split;
}

double KdTree::spacialMedian(std::vector<Object*>* objects, int d) const {
    BoundingBox box = enclosure(objects);
	return (box.minimum()[d] + box.maximum()[d]) / 2.0;
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

/*
   For constructor:
hitcache = new HitCache(8);

bool KdTree::intersect(const Ray& ray, Intersection* inter, void* fromObject) {
    if (fromObject == NULL) {
	return intersect(ray,inter);
    } else {
	Object* object = hitcache->findEntry(fromObject);
	bool result;
	if (object == NULL) {
	    result = intersect(ray,inter);
	} else {
	    double t_obj = object->fastIntersect(ray);
	    if (t_obj > 0) {
		result = intersect(ray,inter,double(0),t_obj + 0.5);
	    } else {
		result = intersect(ray,inter);
		if (!result) {
		    hitcache->removeEntry(fromObject);
		}
	    }
	}
	if (result) {
	    Object* hitObject = inter->getObject();
	    double t = inter->getT();
	    hitcache->addEntry(fromObject,hitObject,t);
	}
	return result;
    }
}
*/
