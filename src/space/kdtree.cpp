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

#define KD_TREE_MAX 2
#define KD_TREE_MAX_DEPTH 50

KdTree::KdTree() : GenericKdTree<Object>(KD_TREE_MAX_DEPTH, KD_TREE_MAX) {
}

bool KdTree::intersect(const Ray& ray, Intersection* result) const {
    Vector2 h = world_bbox.intersect(ray);
    if (h[1] < h[0]) {
	return false;
    } else {
	bool res = intersect(ray,result,0.0,h[1]);
	if (res && ray.getDirection() * result->getNormal() > 0) {
	    result->flipNormal();
	}
	return res;
    }
}

bool KdTree::intersectPrimary(const Ray& ray, Intersection* result) const {
    Vector2 h = world_bbox.intersect(ray);
    if (h[1] < h[0]) {
	return false;
    } else {
	bool res = intersect(ray,result,max(h[0],0.0),h[1]);
	if (res && ray.getDirection() * result->getNormal() > 0) {
	    result->flipNormal();
	}
	return res;
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


/**
 * Implementation of the recursive $f[ TA_rec^B $f] algorithm.
 * See http://sgi.felk.cvut.cz/~havran/phdthesis.html
 * See http://www.acm.org/jgt/papers/HavranKopalBittnerZara97/TA-B.html
 */
bool KdTree::intersect(const Ray& ray, Intersection* result, const double a, const double b) const {

    StackElem* stack = (StackElem*)alloca(sizeof(StackElem)*(max_depth+2));

    double t;
    const KdNode<Object>* curNode = getTopNode();
    const KdNode<Object>* farChild = NULL;
    int enPt = 0;
    stack[enPt].t = a;

    if (a >= 0.0) {
	stack[enPt].pb[0] = ray.getOrigin()[0] + ray.getDirection()[0] * a;
	stack[enPt].pb[1] = ray.getOrigin()[1] + ray.getDirection()[1] * a;
	stack[enPt].pb[2] = ray.getOrigin()[2] + ray.getDirection()[2] * a;
    } else {
	ray.getOrigin().toArray(stack[enPt].pb);
    }
    int exPt = 1;
    stack[exPt].t = b;
    stack[exPt].pb[0] = ray.getOrigin()[0] + ray.getDirection()[0] * b;
    stack[exPt].pb[1] = ray.getOrigin()[1] + ray.getDirection()[1] * b;
    stack[exPt].pb[2] = ray.getOrigin()[2] + ray.getDirection()[2] * b;
    stack[exPt].node = NULL;

    while (curNode != NULL) {
	while (!curNode->isLeafNode()) {
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
	uint object_num = getNodeObjectNum(curNode);
	if (object_num > 0) {
	    Object* object_hit = NULL;
	    double smallest_t = stack[exPt].t;
	    const double s_min_t = MAX(0.0,stack[enPt].t);
	    for (uint i = 0; i < object_num; i++) {
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
    const KdNode<Object> *farChild, *curNode;
    curNode = getTopNode();
    int enPt = 0;
    stack[enPt].t = 0.0;

    ray.getOrigin().toArray(stack[enPt].pb);

    int exPt = 1;
    stack[exPt].t = b;
    stack[exPt].pb[0] = ray.getOrigin()[0] + ray.getDirection()[0] * b;
    stack[exPt].pb[1] = ray.getOrigin()[1] + ray.getDirection()[1] * b;
    stack[exPt].pb[2] = ray.getOrigin()[2] + ray.getDirection()[2] * b;
    stack[exPt].node = NULL;

    while (curNode != NULL) {
	while (!curNode->isLeafNode()) {
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
	uint object_num = getNodeObjectNum(curNode);
	if (object_num > 0) {
	    const double min_t = MAX(0.0,stack[enPt].t);
	    for (uint i = 0; i < object_num; i++) {
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
