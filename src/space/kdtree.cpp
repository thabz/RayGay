#include <cassert>
#include <list>

#include "objects/object.h"
#include "space/kdtree.h"
#include "intersection.h"
#include "ray.h"
#include "objects/object.h"
#include "ray.h"
#include "stats.h"
#include "exception.h"
#include "boundingbox.h"
#include "math/vector2.h"

#define VERBOSE
#define SANITY_CHECK

#define KD_TREE_MAX 2
#define KD_TREE_MAX_DEPTH 50
// Don't check for best split in all 3 dimensions when number
// of objects in a node exceeds this value. Then just use largest
// dimension of bbox as best dimension.
#define KD_TREE_MAX_ELEMENTS_IN_FULL_SPLIT_CHECK 25

#define NO_STATS

/*
 * Macros for accessing the packed KdNode.
 */
#define leftNode(node) (node->left == 0 ? NULL : &(nodes[node->left]))
#define rightNode(node) (node->left == 0 ? NULL : &(nodes[node->left+1]))
#define isLeafNode(node) (node->axis == -1)
#define getNodeAxis(node) (node->axis)
#define getNodeObjectNum(node) (node->num)
#define getNodeSplitValue(node) (node->splitPlane)
#define getTopNode() (&(nodes[0]))


KdTree::KdTree() {
    prepared = false;
    added_objects = new vector<Object*>;
}

KdTree::~KdTree() {
    for(unsigned int i = 0; i < nodes.size(); i++) {
	if (nodes[i].axis == -1 && nodes[i].num > 0) {
	    delete [] nodes[i].objects;
	}
    }
    nodes.clear();
}

void KdTree::addObject(Object* obj) {
#ifndef NO_STATS    
    Stats::getUniqueInstance()->inc(STATS_KDTREE_OBJECTS_ADDED);
#endif    
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
	return intersectForShadow_real(ray,min(max_t,h[1]));
    }
}

class compareAreaDesc {
    public:
	bool operator()(Object const* p1, Object const* p2)
	{
	    return p1->area() > p2->area();
	}
};

void KdTree::prepare() {
    assert(added_objects->size() > 0);
    if (prepared) throw_exception("Already prepared.");

    unsigned int num = added_objects->size();
    
    BoundedObject* bobs = new (BoundedObject)[num];
    left_bobs = new (BoundedObject*)[num];
    right_bobs = new (BoundedObject*)[num];

    for(unsigned int i = 0; i < num; i++) {
	bobs[i].object = added_objects->operator[](i);
	bobs[i].bbox = added_objects->operator[](i)->boundingBoundingBox();
	left_bobs[i] = &(bobs[i]);
    }

    delete added_objects;
    added_objects = NULL;

    world_bbox = enclosure(left_bobs,num);
    max_depth = 0;

    nodes.push_back(KdNode());
    assert(nodes.size() == 1);

    prepare(num, world_bbox, 1, 0);

    delete [] bobs;
    delete [] left_bobs;
    delete [] right_bobs;
    
#ifndef NO_STATS
    Stats::getUniqueInstance()->put(STATS_KDTREE_DEPTH,max_depth);
    Stats::getUniqueInstance()->put(STATS_KDTREE_NODES,nodes.size());
#endif    
    cout << "Nodes in kd-tree: " << nodes.size() << endl;
    cout << "Depth: " << max_depth << endl;
    cout << "Waste: " << (nodes.capacity() - nodes.size())*sizeof(KdNode) << endl;
    prepared = true;
}

void KdTree::prepare(unsigned int num, const BoundingBox& bbox, unsigned int depth, const unsigned int dest_idx) {

    assert(dest_idx < nodes.size());

    // Mark curNode as a leaf until a suitable split-plane is found
    int axis = -1;
    double splitPlane = 0;

    unsigned int new_left_idx = 0;
    unsigned int new_right_idx = 0;

    // Keep within max depth or minimum node size
    if (depth <= KD_TREE_MAX_DEPTH && num > KD_TREE_MAX) {

	if (depth > max_depth) {
	    max_depth = depth;
	}

	// Make an extra copy of the bobject pointer list for this node
	//BoundedObject** left_bobs = bobs;
	// TODO: Use alloca when num is small
	//BoundedObject** right_bobs = new (BoundedObject*)[num];
	memcpy(right_bobs, left_bobs, num*sizeof(BoundedObject*));

	CostResult splitResult;

	// Find the best axis to split node at
	if (findBestSplitPlane(num, bbox,splitResult)) {

	    // The current node will be split 
	    axis = splitResult.dim;
	    splitPlane = splitResult.axis;

	    // Allocate childnodes
	    nodes.push_back(KdNode());
	    nodes.push_back(KdNode());
	    new_left_idx = nodes.size() - 2;
	    new_right_idx = nodes.size() - 1;

	    // Find bounding boxes for the two children
	    BoundingBox lower_bbox;
	    BoundingBox higher_bbox;
	    if (!bbox.split(lower_bbox, higher_bbox, axis, splitPlane)) {
		throw_exception("Split plane outside bbox of node");
	    }

	    BoundedObject* tmp;

	    // Move into lower
	    unsigned int j = 0;
	    for(unsigned int i = 0; i < splitResult.left_index; i++) {
		BoundedObject* bobject = left_bobs[i];
		if (bobject->bbox.minimum(axis) < splitPlane) {
		    assert(bobject->object != NULL);
		    if (bobject->object->intersects(lower_bbox,bobject->bbox) >= 0) {
			tmp = left_bobs[j];
			left_bobs[j] = bobject;
			left_bobs[i] = tmp;
			j++;
		    }
		}
	    }
	    assert(j <= splitResult.left_index);
	    // Recurse into left subtree
	    prepare(j, lower_bbox, depth+1, new_left_idx);

	    // Move into higher
	    j = 0;
	    for(unsigned int i = 0; i < num; i++) {
		BoundedObject* bob = left_bobs[i];
		if (bob->bbox.maximum(axis) > splitPlane) {
		    if (bob->object->intersects(higher_bbox,bob->bbox) >= 0) {
			tmp = left_bobs[j];
			left_bobs[j] = bob;
			left_bobs[i] = tmp;
			j++;
		    }
		}
	    }
	    assert(j <= num - splitResult.right_index );
	    // Recurse into right subtree
	    prepare(j, higher_bbox, depth+1, new_right_idx);


	    /*
	       j = 0;
	       for(unsigned int i = splitResult.right_index; i < num; i++) {
	       BoundedObject* bobject = right_bobs[i];
	       if (bobject->object->intersects(bbox,bobject->bbox) >= 0) 
	       right_bobs[j++] = bobject;
	       }
	    // Recurse into right subtree
	    prepare(right_bobs, j, higher_bbox, depth+1, new_right_idx);
	    */
	}
    } 

    // Build the real KdNode
    KdNode& new_node = nodes[dest_idx];

    if (axis == -1) {
	new_node.axis = -1;
	new_node.num = num;
	if (num > 0) {
	    new_node.objects = new (Object*)[num];
	    for(unsigned int j = 0; j < num; j++) {
		new_node.objects[j] = left_bobs[j]->object;
	    }
	} else {
	    new_node.objects = NULL;
	}
    } else {
	new_node.axis = axis;
	new_node.splitPlane = splitPlane;
	assert(new_left_idx != 0 && new_right_idx != 0);
	assert(new_right_idx == new_left_idx + 1);
	new_node.left = new_left_idx;
    }
}

/**
 * Implementation of the recursive $f[ TA_rec^B $f] algorithm.
 * See http://sgi.felk.cvut.cz/~havran/phdthesis.html
 * See http://www.acm.org/jgt/papers/HavranKopalBittnerZara97/TA-B.html
 */
bool KdTree::intersect(const Ray& ray, Intersection* result, const double a, const double b) const {

    StackElem* stack = (StackElem*)alloca(sizeof(StackElem)*(max_depth+2));

    double t;
    const KdNode* curNode = getTopNode();
    const KdNode* farChild = NULL;
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
	while (!isLeafNode(curNode)) {
	    /* Current node is not a leaf */
	    double splitVal = getNodeSplitValue(curNode);
	    int axis = getNodeAxis(curNode); // ?
	    switch(axis) {
		case 0:
		    {
			if (stack[enPt].pb[0] <= splitVal) {
			    if (stack[exPt].pb[0] <= splitVal) {
				curNode = leftNode(curNode);
				continue;
			    }
			    farChild = rightNode(curNode);
			    curNode = leftNode(curNode);
			} else {
			    if (splitVal <= stack[exPt].pb[0]) {
				curNode = rightNode(curNode);
				continue;
			    }
			    farChild = leftNode(curNode);
			    curNode = rightNode(curNode);
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
				curNode = leftNode(curNode);
				continue;
			    }
			    farChild = rightNode(curNode);
			    curNode = leftNode(curNode);
			} else {
			    if (splitVal <= stack[exPt].pb[1]) {
				curNode = rightNode(curNode);
				continue;
			    }
			    farChild = leftNode(curNode);
			    curNode = rightNode(curNode);
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
				curNode = leftNode(curNode);
				continue;
			    }
			    farChild = rightNode(curNode);
			    curNode = leftNode(curNode);
			} else {
			    if (splitVal <= stack[exPt].pb[2]) {
				curNode = rightNode(curNode);
				continue;
			    }
			    farChild = leftNode(curNode);
			    curNode = rightNode(curNode);
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
	if (getNodeObjectNum(curNode) > 0) {
	    Object* object_hit = NULL;
	    double smallest_t = stack[exPt].t;
	    const double s_min_t = MAX(0.0,stack[enPt].t);
	    for (unsigned int i = 0; i < getNodeObjectNum(curNode); i++) {
		double i_t = curNode->objects[i]->fastIntersect(ray);
		if (i_t > s_min_t && i_t < smallest_t) {
		    smallest_t = i_t;
		    object_hit = curNode->objects[i];
		}
	    }
	    if (object_hit != NULL) {
		*(result) = object_hit->fullIntersect(ray,smallest_t);
		return true;
	    }
	}

	enPt = exPt;

	curNode = stack[exPt].node;
	exPt = stack[enPt].prev;
    } /* while curNode != end of nodes */
    return false;
}

Object* KdTree::intersectForShadow_real(const Ray& ray, const double b) const {

    StackElem* stack = (StackElem*)alloca(sizeof(StackElem)*(max_depth+2));

    double t;
    const KdNode *farChild, *curNode;
    curNode = getTopNode();
    int enPt = 0;
    stack[enPt].t = 0.0;

    stack[enPt].pb[0] = ray.getOrigin()[0];
    stack[enPt].pb[1] = ray.getOrigin()[1];
    stack[enPt].pb[2] = ray.getOrigin()[2];    

    int exPt = 1;
    stack[exPt].t = b;
    stack[exPt].pb[0] = ray.getOrigin()[0] + ray.getDirection()[0] * b;
    stack[exPt].pb[1] = ray.getOrigin()[1] + ray.getDirection()[1] * b;
    stack[exPt].pb[2] = ray.getOrigin()[2] + ray.getDirection()[2] * b;
    stack[exPt].node = NULL;

    while (curNode != NULL) {
	while (!isLeafNode(curNode)) {
	    /* Current node is not a leaf */
	    double splitVal = getNodeSplitValue(curNode);
	    int axis = getNodeAxis(curNode);

	    if (stack[enPt].pb[axis] <= splitVal) {
		if (stack[exPt].pb[axis] <= splitVal) {
		    curNode = leftNode(curNode);
		    continue;
		}
		farChild = rightNode(curNode);
		curNode = leftNode(curNode);
	    } else {
		if (splitVal <= stack[exPt].pb[axis]) {
		    curNode = rightNode(curNode);
		    continue;
		}
		farChild = leftNode(curNode);
		curNode = rightNode(curNode);
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
	if (getNodeObjectNum(curNode) > 0) {
	    const double min_t = MAX(0.0,stack[enPt].t);
	    for (unsigned int i = 0; i < getNodeObjectNum(curNode); i++) {
		double i_t = curNode->objects[i]->fastIntersect(ray);
		if (i_t > min_t && i_t < stack[exPt].t) {
		    return curNode->objects[i];
		}
	    }
	}

	enPt = exPt;

	curNode = stack[exPt].node;
	exPt = stack[enPt].prev;
    } /* while curNode != end of nodes */
    return NULL;
}

BoundingBox KdTree::enclosure(BoundedObject** bobs, unsigned int num) const {
    assert(num > 0);
    BoundingBox result = bobs[0]->bbox; 
    for(unsigned int i = 1; i < num; i++) {
	result = BoundingBox::doUnion(result,bobs[i]->bbox);
    }
    return result;
}

class cmpL {
    public:
	cmpL(unsigned int d) { this->d = d; } 
	bool operator()(const BoundedObject* p1, const BoundedObject* p2) const {
	    return p1->bbox.minimum(d) < p2->bbox.minimum(d);
	}
    private:
	unsigned int d;
};

class cmpR {
    public:
	cmpR(unsigned int d) { this->d = d; }
	bool operator()(const BoundedObject* p1, const BoundedObject* p2) const {
	    return p1->bbox.maximum(d) < p2->bbox.maximum(d);
	}
    private:
	unsigned int d;
};

void KdTree::findBestSplitPlane(unsigned int size, const BoundingBox& bbox, CostResult& result, int d) const {
    assert(d == 0 || d == 1 || d == 2);

    double split;
    Vector bbox_lenghts = bbox.maximum() - bbox.minimum();
    double lowest_cost = 0.9*size*bbox.area();

    double cap_a = 2 * bbox_lenghts[(d+1)%3] * bbox_lenghts[(d+2)%3];
    double cap_p = 2 * bbox_lenghts[(d+1)%3] + 2 * bbox_lenghts[(d+2)%3];

    sort(left_bobs, left_bobs + size, cmpL(d));
    sort(right_bobs, right_bobs + size, cmpR(d));
    result.current_sort_dim = d;

    unsigned int l = 0;
    unsigned int r = 0;
    while (l < size || r < size) {
	bool used_right;
	if (l < size && r < size) {
	    double rsplit = right_bobs[r]->bbox.maximum(d);
	    double lsplit = left_bobs[l]->bbox.minimum(d);
	    if (rsplit < lsplit) {
		split = rsplit;
		used_right = true;
	    } else {
		split = lsplit;
		used_right = false;
	    }
	} else {
	    if (l == size) {
		split = right_bobs[r]->bbox.maximum(d);
		used_right = true;
	    } else {
		split = left_bobs[l]->bbox.minimum(d);
		used_right = false;
	    }
	}

	if (used_right) r++;

	if (split < bbox.maximum()[d] && split > bbox.minimum()[d]) {
	    double left_area = cap_a + (split - bbox.minimum()[d])*cap_p;
	    double right_area = cap_a + (bbox.maximum()[d] - split)*cap_p;
	    double cost = left_area * l + right_area * (size -r);
	    if (cost < lowest_cost) {  
		result.dim = d;
		result.axis = split;
		result.left_index = l;
		result.right_index = r;
		lowest_cost = cost;
	    } 
	}

	if (!used_right) l++;
    } /* while more edges to check */
}

bool KdTree::findBestSplitPlane(unsigned int num, const BoundingBox& bbox, CostResult& result) const {
    result.dim = -1;
    result.left_index = 0;
    result.right_index = 0;

    if (num == 0) 
	return false;

    if (num < KD_TREE_MAX_ELEMENTS_IN_FULL_SPLIT_CHECK) {
	// Find best split in all 3 dimensions
	for(int d = 0; d < 3; d++) {
	    findBestSplitPlane(num, bbox, result, d);
	}
    } else {
	// Find best split in largest dimension
	Vector bbox_lenghts = bbox.maximum() - bbox.minimum();
	int d = bbox_lenghts.largestDimension();
	findBestSplitPlane(num, bbox,result, d);
    }

    if (result.dim == -1) {
	// Not splitting has best cost
	return false;
    } else {
	if (result.current_sort_dim != result.dim) {
	    // Sort objects again
	    sort(left_bobs, left_bobs + num, cmpL(result.dim));
	    //sort(right_bobs, right_bobs + num, cmpR(result.dim));
	    result.current_sort_dim = result.dim;
	}
	return true;
    }
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
