
#ifndef BSP_H
#define BSP_H

#include "spacesubdivider.h"
#include "intersection.h"
#include "object.h"
#include <vector>

#define BSP_MAX 4

class Ray;

/** A binary space partitioning (BSP) tree is a binary tree 
 *  for multidimensional points where successive levels are 
 *  split by arbitrary hyperplanes.
 */
class BSP : public SpaceSubdivider {

    public:
	BSP();
	void addObject(object* obj); ///< Place a object in the BSP tree 

	Intersection intersect(const Ray& ray) const; ///< Calculate an intersection with the BSP tree
	Intersection intersectForShadow(const Ray& ray) const; ///< Calculate an intersection with the BSP tree 
	Intersection intersectForShadow(const Ray& ray, const object* hint) const; ///< Calculate an intersection with the BSP tree

	/// This gets called after all objects are added and before any intersection methods are called.
	void prepare();

    private:
	int cutplane_dimension;   ///< [0..2] for x, y or z.
	int cutplane_value;       ///< The value of x, y or z the cut plane cuts 

	std::vector<object*> objects;
	BSP* lower;
	BSP* higher;
};

#endif
