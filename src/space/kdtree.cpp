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
#define KD_TREE_MAX_DEPTH 100

#undef NO_STATS

KdTree::KdTree() {
    prepared = false;
    added_objects = new vector<Object*>;
}

KdTree::~KdTree() {
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

    unsigned int objects_num = added_objects->size();
    
    vector<BoundedObject*>*  bounded_objects = new vector<BoundedObject*>;
    bounded_objects->reserve(objects_num);

    BoundedObject* bobs = new BoundedObject[objects_num];

    for(unsigned int i = 0; i < objects_num; i++) {
	bobs[i].object = added_objects->operator[](i);
	bobs[i].bbox = added_objects->operator[](i)->boundingBoundingBox();
	bounded_objects->push_back(&bobs[i]);
    }
    delete added_objects;
    added_objects = NULL;

    world_bbox = enclosure(bounded_objects);
    max_depth = 0;
    nodes_count = 0;

    KdNodeTmp node;
    node.bbox = world_bbox;
    node.bobjects = bounded_objects;

    top_node = prepare(&node,1);
    delete [] bobs;
    
#ifndef NO_STATS
    Stats::getUniqueInstance()->put(STATS_KDTREE_DEPTH,max_depth);
    Stats::getUniqueInstance()->put(STATS_KDTREE_NODES,nodes_count);
#endif    
    prepared = true;
}

KdTree::KdNode* KdTree::prepare(KdNodeTmp* curNode, unsigned int depth) {

    KdNode* left_node_ptr = NULL;
    KdNode* right_node_ptr = NULL;
    // Mark curNode as a leaf until a suitable split-plane is found
    curNode->axis = -1;

    // Keep with in max depth or minimum node size
    if (depth <= KD_TREE_MAX_DEPTH && curNode->bobjects->size() > KD_TREE_MAX) {

	if (depth > max_depth) {
	    max_depth = depth;
	}

	// Make an extra copy of the bobject indice list for this node
	CostResult splitResult;
	splitResult.left_bobjects = curNode->bobjects;
	splitResult.right_bobjects = new vector<BoundedObject*>;
	*(splitResult.right_bobjects) = *(splitResult.left_bobjects);
	assert(splitResult.right_bobjects->size() == splitResult.left_bobjects->size());
	assert(splitResult.right_bobjects != splitResult.left_bobjects);
	assert(splitResult.right_bobjects->front()->bbox == splitResult.left_bobjects->front()->bbox);

	// Find the best axis to split node at
	if (findBestSplitPlane(curNode->bbox,splitResult)) {
	    // curNode will be split 
	    curNode->axis = splitResult.dim;
	    curNode->splitPlane = splitResult.axis;

	    KdNodeTmp lower;
	    KdNodeTmp higher;
	    lower.bobjects = new vector<BoundedObject*>;
	    higher.bobjects = new vector<BoundedObject*>;

	    // Find bounding boxes for the two children
	    if (!curNode->bbox.split(&(lower.bbox), &(higher.bbox), curNode->axis, curNode->splitPlane)) {
		throw_exception("Split plane outside bbox of node");
	    }

	    // Move into lower
	    lower.bobjects->reserve(splitResult.left_index);
	    for(unsigned int i = 0; i < splitResult.left_index; i++) {
		BoundedObject* bobject = splitResult.left_bobjects->operator[](i);
		assert(bobject->object != NULL);
		if (bobject->object->intersects(curNode->bbox,bobject->bbox) >= 0) 
		    lower.bobjects->push_back(bobject);
	    }
	    delete splitResult.left_bobjects;

	    // Move into higher
	    higher.bobjects->reserve(splitResult.right_bobjects->size() - splitResult.right_index);
	    for(unsigned int i = splitResult.right_index; i < splitResult.right_bobjects->size(); i++) {
		BoundedObject* bobject = splitResult.right_bobjects->operator[](i);
		if (bobject->object->intersects(curNode->bbox,bobject->bbox) >= 0) 
		    higher.bobjects->push_back(bobject);
	    }
	    delete splitResult.right_bobjects;

	    // Recurse into child nodes
	    left_node_ptr = prepare(&lower,depth+1);
	    right_node_ptr = prepare(&higher,depth+1);
	}
    }

    KdNode* new_node = new KdNode();
    new_node->axis = curNode->axis;
    new_node->splitPlane = curNode->splitPlane;
    nodes_count++;
    if (curNode->axis == -1) {
	unsigned int num = curNode->bobjects->size();
	new_node->objects = new (Object*)[num];
	new_node->num = num;
	//new_node->objects->reserve(curNode->bobjects->size());
	for(unsigned int j = 0; j < num; j++) {
	    new_node->objects[j] = curNode->bobjects->operator[](j)->object;
	}
	delete curNode->bobjects;
    } else {
	assert(left_node_ptr != NULL && right_node_ptr != NULL);
	new_node->left = left_node_ptr;
	new_node->right = right_node_ptr;
    }
    return new_node;
}

/**
 * Implementation of the recursive $f[ TA_rec^B $f] algorithm.
 * See http://sgi.felk.cvut.cz/~havran/phdthesis.html
 * See http://www.acm.org/jgt/papers/HavranKopalBittnerZara97/TA-B.html
 */
bool KdTree::intersect(const Ray& ray, Intersection* result, const double a, const double b) const {

    StackElem* stack = (StackElem*)alloca(sizeof(StackElem)*(max_depth+2));

    double t;
    KdNode *farChild, *curNode;
    curNode = top_node;
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
	if (curNode->num > 0) {
	    Object* object_hit = NULL;
	    double smallest_t = stack[exPt].t;
	    const double s_min_t = MAX(0.0,stack[enPt].t);
	    for (unsigned int i = 0; i < curNode->num; i++) {
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
    KdNode *farChild, *curNode;
    curNode = top_node;
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
	while (curNode->axis >= 0) {
	    /* Current node is not a leaf */
	    double splitVal = curNode->splitPlane;
	    int axis = curNode->axis; 

	    if (stack[enPt].pb[axis] <= splitVal) {
		if (stack[exPt].pb[axis] <= splitVal) {
		    curNode = curNode->left;
		    continue;
		}
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
	if (curNode->num > 0) {
	    const double min_t = MAX(0.0,stack[enPt].t);
	    for (unsigned int i = 0; i < curNode->num; i++) {
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

KdTree::CostResult::CostResult() {
    left_bobjects = NULL;
    right_bobjects = NULL;
}

BoundingBox KdTree::enclosure(vector<BoundedObject*>* bs) const {
    unsigned int num = bs->size();
    assert(num > 0);
    BoundingBox result = bs->front()->bbox; 
    for(unsigned int i = 1; i < num; i++) {
	result = BoundingBox::doUnion(result,bs->operator[](i)->bbox);
    }
    return result;
}

int g_d;

class cmpL {
    public:
	bool operator()(const BoundedObject* p1, const BoundedObject* p2) {
	    assert(g_d == 0 || g_d == 1 || g_d == 2);
	    return p1->bbox.minimum()[g_d] < p2->bbox.minimum()[g_d];
	}
};

class cmpR {
    public:
	bool operator()(const BoundedObject* p1, const BoundedObject* p2) {
	    assert(g_d == 0 || g_d == 1 || g_d == 2);
	    return p1->bbox.maximum()[g_d] < p2->bbox.maximum()[g_d];
	}
};

bool KdTree::findBestSplitPlane(const BoundingBox& bbox, CostResult& result) const {
    result.dim = -1;
    result.left_index = 0;
    result.right_index = 0;

    double split;
    unsigned int size = result.left_bobjects->size();
    if (size == 0) 
	return false;

    assert(result.left_bobjects->size() == result.right_bobjects->size());
    Vector bbox_lenghts = bbox.maximum() - bbox.minimum();

    double lowest_cost = 0.9*size*bbox.area();
    for(int d = 0; d < 3; d++) {
	g_d = d;

	double cap_a = 2 * bbox_lenghts[(d+1)%3] * bbox_lenghts[(d+2)%3];
	double cap_p = 2 * bbox_lenghts[(d+1)%3] + 2 * bbox_lenghts[(d+2)%3];
	vector<BoundedObject*>* left_bobjects = result.left_bobjects;
	vector<BoundedObject*>* right_bobjects = result.right_bobjects;

	sort(left_bobjects->begin(), left_bobjects->end(), cmpL());
	sort(right_bobjects->begin(), right_bobjects->end(), cmpR());

	unsigned int l = 0;
	unsigned int r = 0;
	while (l < size || r < size) {
	    bool used_right;
	    if (l < size && r < size) {
		double rsplit = right_bobjects->operator[](r)->bbox.maximum()[d];
		double lsplit = left_bobjects->operator[](l)->bbox.minimum()[d];
		if (rsplit < lsplit) {
		    split = rsplit;
		    used_right = true;
		} else {
		    split = lsplit;
		    used_right = false;
		}
	    } else {
		if (l == size) {
		    split = right_bobjects->operator[](r)->bbox.maximum()[d];
		    used_right = true;
		} else {
		    split = left_bobjects->operator[](l)->bbox.minimum()[d];
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
    } /* while more dimensions to try */
    if (result.dim == -1) {
	return false;
    } else {
	//cout << "Split " << size << " into " << result.left_index << "," << size - result.right_index << endl;
	g_d = result.dim;
	sort(result.left_bobjects->begin(), result.left_bobjects->end(), cmpL());
	sort(result.right_bobjects->begin(), result.right_bobjects->end(), cmpR());
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
