
#ifndef SPACE_KD_TREE_H
#define SPACE_KD_TREE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "math/vector.h"
#include "aabox.h"
#include <vector>
#include "space/generickdtree.h"
#include "objects/object.h"

/**
 * k-dimensional tree.
 *
 * Implementerer http://www.acm.org/jgt/papers/HavranKopalBittnerZara97/ som er den bedste BSP-traversal algoritme.
 *
 * TODO: Make a destructor that delete nodes recursively beginning at top_node.
 */
class KdTree : public GenericKdTree<Object> {

    public:
	/// Constructor
	KdTree();
	
	/// Returns the nearest intersection
	bool intersect(const Ray& ray, Intersection& result) const; 
	
	/// Returns the distance to nearest intersection
	double intersect(const Ray& ray) const;
	
	/// Returns the nearest intersection
	bool intersectPrimary(const Ray& ray, Intersection& result) const; 

	/// Returns any intersection
	Object* intersectForShadow(const Ray& ray, double max_t, const Object* ignore = NULL) const;  

    private:
	struct StackElem {
	    double pb[3];   // coordinates of entry/exit point
	    double t;        // the entry/exit signed distance
	    const KdNode<Object>* node;   // pointer to far child
	    int prev;       // pointer to previus stack item
	};

	bool intersect(const Ray& ray, Intersection& result, const double a, const double b) const;
	double intersect(const Ray& ray, const double a, const double b) const;
	Object* intersectForShadow_real(const Ray&,const double, const Object* ignore) const;
};


#endif

