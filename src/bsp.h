
#ifndef BSP_H
#define BSP_H

#include "spacesubdivider.h"
#include "intersection.h"
#include "object.h"
#include <vector>

#define BSP_MAX 2

class Ray;

/** A binary space partitioning (BSP) tree is a binary tree 
 *  for multidimensional points where successive levels are 
 *  split by arbitrary hyperplanes.
 */
class BSP : public SpaceSubdivider {

    public:
	BSP();
	virtual ~BSP() {};
	void addObject(object* obj); ///< Place a object in the BSP tree 

	Intersection intersect(const Ray& ray) const; ///< Returns the nearest intersection
	Intersection intersectForShadow(const Ray& ray) const; ///< Returns any intersection 
	Intersection intersectForShadow(const Ray& ray, const object* hint) const; ///< Returns any intersection but hint-object is checked for intersection first.

	/// This gets called after all objects are added and before any intersection methods are called.
	void prepare();
	
        /// Internal test
	static void test();

    private:
	int cutplane_dimension;   ///< [0..2] for x, y or z.
        double cutplane_value;       ///< The value of x, y or z the cut plane cuts 
	std::vector<object*> objects;
	BSP* lower;
	BSP* higher;

	/// Returns the smallest bbox containing all objects of this node
	BoundingBox enclosure() const;

	/// Returns [0..2] for x, y or z being the widest side of the box.
	static int BSP::largestDimension(const BoundingBox& box);
	Intersection intersect(const Ray&,double,double) const;
	Intersection intersect_recurse(const Ray&,double,double) const;
};

#endif
