
#ifndef SPACE_KD_TREE_H
#define SPACE_KD_TREE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "types.h"
#include "math/vector.h"
#include "boundingbox.h"
#include <vector>
#include "space/generickdtree.h"
#include "objects/object.h"

/**
 * k-dimensional tree.
 *
 * Implementerer http://www.acm.org/jgt/papers/HavranKopalBittnerZara97/ som er den bedste BSP-traversal algorithme.
 *
 * TODO: Make a destructor that delete nodes recursively beginning at top_node.
 */
class KdTree : public GenericKdTree<Object> {

    public:
	/// Constructor
	KdTree();
	
	/// Returns the nearest intersection
	bool intersect(const Ray& ray, Intersection* result) const; 
	
	/// Returns the nearest intersection
	bool intersectPrimary(const Ray& ray, Intersection* result) const; 

	/// Returns any intersection
	Object* intersectForShadow(const Ray& ray, double max_t) const;  

    private:
	struct StackElem {
	    double pb[3];   // coordinates of entry/exit point
	    const KdNode<Object>* node;   // pointer to far child
	    float t;        // the entry/exit signed distance
	    int prev;       // pointer to previus stack item
	};

	bool intersect(const Ray& ray, Intersection* result, const double a, const double b) const;

	Object* intersectForShadow_real(const Ray&,const double) const;


};


#endif

