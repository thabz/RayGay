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
    tmp_nodes.push_back(KdNodeTmp());
    for(unsigned int i = 0; i < added_objects->size(); i++) {
	BoundedObject bobj;
	bobj.object = (*added_objects)[i]; // TODO: Slow!
	bobj.bbox = bobj.object->boundingBoundingBox();
        tmp_nodes[0].bobjects.push_back(bobj);
    }
    world_bbox = enclosure(&(tmp_nodes[0].bobjects));
    tmp_nodes[0].axis = 0;
    tmp_nodes[0].bbox = world_bbox;
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
	KdNodeTmp& old = tmp_nodes[i];
	node.splitPlane = old.splitPlane;
	node.axis = old.axis;
	if (node.axis >= 0) {
	    node.left = &(nodes[old.left]);
	    node.right = &(nodes[old.right]);
	} else {
	    node.objects = new vector<Object*>;
	    for(unsigned int j = 0; j < old.bobjects.size(); j++) {
		node.objects->push_back(old.bobjects[j].object);
	    }
	    sort(node.objects->begin(),node.objects->end(),compareAreaDesc());
	    old.bobjects.clear();
	}
	nodes[i] = node;
    }
    tmp_nodes.clear();
}

void KdTree::prepare(int curNode_idx,int depth) {

    KdNodeTmp* curNode = &(tmp_nodes[curNode_idx]);

    if (depth > KD_TREE_MAX_DEPTH) {
	curNode->axis = -1;
	return;
    }

    if (depth > max_depth) 
	max_depth = depth;

    if (curNode->bobjects.size() <= KD_TREE_MAX) {
	curNode->axis = -1;
	return;
    }
    
    // Find the best axis to split node at
    int best_dim = -1;
    double best_val;
    findBestSplitPlane(curNode->bbox,curNode->bobjects,&best_dim,&best_val);
    curNode->axis = best_dim;
    curNode->splitPlane = best_val;
    if (best_dim == -1) {
	// Leaf
	return;
    }

    tmp_nodes.push_back(KdNodeTmp());
    tmp_nodes.push_back(KdNodeTmp());
    curNode = &(tmp_nodes[curNode_idx]); // Reloading since it might have been realloc'ed
    int left_idx = tmp_nodes.size() - 1;
    int right_idx = tmp_nodes.size() - 2;
    curNode->left = left_idx;
    curNode->right = right_idx;
    KdNodeTmp* lower = &tmp_nodes[curNode->left];
    KdNodeTmp* higher = &tmp_nodes[curNode->right];

    curNode->bbox.split(&(lower->bbox), &(higher->bbox), best_dim, best_val);

    // Put all objects into lower- or higher_objects
    int l = 0; int m = 0; int h = 0;
    vector<BoundedObject>::iterator p = curNode->bobjects.begin();
    while (p != curNode->bobjects.end()) {
	BoundedObject obj = *p;
	if (obj.object == NULL)
	    continue;
	const BoundingBox bbox = obj.bbox;
	int cut_val = bbox.cutByPlane(curNode->axis, curNode->splitPlane);
	if (cut_val == -1) {
	    lower->bobjects.push_back(obj);
	    l++;
	} else if (cut_val == 1) {
	    higher->bobjects.push_back(obj);
	    h++;
	} else {
	    lower->bobjects.push_back(obj);
	    higher->bobjects.push_back(obj);
	    m++;
	}
	p++;
    }
    curNode->bobjects.clear();

    // Recurse into child nodes
    prepare(left_idx,depth+1);
    prepare(right_idx,depth+1);
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
		double i_t = (*objects)[i]->fastIntersect(ray); // TODO: Slow!
		if (i_t > 0 && i_t < smallest_t && i_t > stack[enPt].t && i_t < stack[exPt].t) {
		    smallest_t = i_t;
		    object_hit = (*objects)[i]; // TODO: Slow!
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

Object* KdTree::intersectForShadow_real(const Ray& ray, const double b) const {

    StackElem* stack = (StackElem*)alloca(sizeof(StackElem)*(max_depth+2));

    double t;
    KdNode *farChild, *curNode;
    curNode = nodes;
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
}

int KdTree::largestDimension(const BoundingBox& box) const {
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

BoundingBox KdTree::enclosure(std::vector<BoundedObject>* objects) const {
    assert(objects->size() > 0);
    BoundingBox result = (*objects)[0].bbox; 
    for(unsigned int i = 1; i < objects->size(); i++) {
	result = BoundingBox::doUnion(result,(*objects)[i].bbox);
    }
    return result;
}

Vector KdTree::measureSplit(const std::vector<BoundedObject>& bobjects, int dim, double val) const {
    Vector result = Vector(0,0,0);
    for(unsigned int i = 0; i < bobjects.size(); i++) {
	int cut_val = bobjects[i].bbox.cutByPlane(dim, val);
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

double KdTree::evaluateCost(const BoundingBox& bbox, const std::vector<BoundedObject>& bobjects, int dim, double val, Vector* measure) const {
    Vector c = measureSplit(bobjects,dim,val);
    (*measure) = c;
    BoundingBox left;
    BoundingBox right;
    if (bbox.split(&left,&right,dim,val)) {
	//cout << "Split OK" << endl;
	return left.area()*(c[0]+c[1]) + right.area()*(c[2]+c[1]);
    } else {
	//cout << "Can't split" << endl;
	return HUGE_DOUBLE;
    }
}

void KdTree::findBestSplitPlane(const BoundingBox& bbox, const std::vector<BoundedObject>& bobjects,int* best_dim, double* best_axis) const {
    (*best_dim) = -1;
    double lowest_cost = HUGE_DOUBLE;
    double axis;
    unsigned int size = bobjects.size();
    double max_cost = 1.0*size*bbox.area();
    for(int d = 0; d < 3; d++) {
	double from = bbox.minimum()[d];
	double to = bbox.maximum()[d];
	double from_tole = from + ((to-from)/10.0);
	double to_tole = to - ((to-from)/10.0);
	// Eval cost at boundaries of all objects
	for(unsigned int i = 0; i < size; i++) {
	    BoundingBox o_bbox = bobjects[i].bbox;
	    Vector mea;

	    axis = o_bbox.minimum()[d];
	    if (axis > from && axis < to) {
		double cost = evaluateCost(bbox,bobjects,d,axis,&mea);
#if 0		
		if (cost < lowest_cost && (mea[0]+mea[1] < size || axis > from_tole) && (mea[2]+mea[1] < size || axis < to_tole)) {  
#else 
		if (cost < lowest_cost && cost < max_cost) {  

#endif			
			(*best_dim) = d;
			(*best_axis) = axis;
			lowest_cost = cost;
		} 
	    }
	    axis = o_bbox.maximum()[d];
	    if (axis > from && axis < to) {
		double cost = evaluateCost(bbox,bobjects,d,axis,&mea);
		if (cost < lowest_cost && cost < max_cost) {  
		    (*best_dim) = d;
		    (*best_axis) = axis;
		    lowest_cost = cost;
		}
	    }
	}
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
