
#ifndef BSP_H
#define BSP_H

#include "spacesubdivider.h"
#include "intersection.h"
#include "object.h"
#include "ray.h"
#include <vector>

#define BSP_MAX 2

/**
 * A binary space partitioning tree.
 *
 * A BSP tree is a binary tree 
 * for multidimensional points where successive levels are 
 * split by arbitrary hyperplanes.
 *
 * TODO: Implementer http://www.acm.org/jgt/papers/HavranKopalBittnerZara97/ som er den bedste BSP-traversal algorithme.
 */
class BSP : public SpaceSubdivider {

    public:
	BSP();
	virtual ~BSP() {};
	void addObject(Object* obj); ///< Place a object in the BSP tree 

	bool intersect(const Ray& ray) const; ///< Returns the nearest intersection
	bool intersectPrimary(const Ray& ray) const; ///< Returns the nearest intersection
	bool intersectForShadow(const Ray& ray) const; ///< Returns any intersection 
	bool intersectForShadow(const Ray& ray, const Object* hint) const; ///< Returns any intersection but hint-object is checked for intersection first.

	/// This gets called after all objects are added and before any intersection methods are called.
	void prepare();

	Intersection* getLastIntersection() const { return last_intersection; };

	
        /// Internal test
	static void test();

    private:
	int cutplane_dimension;   ///< [0..2] for x, y or z.
        double cutplane_value;       ///< The value of x, y or z the cut plane cuts 
	std::vector<Object*> objects;
	BSP* lower;
	BSP* higher;

	mutable Intersection* last_intersection;
	static Object* last_primary_intersected_object;

	/// Returns the smallest bbox containing all objects of this node
	BoundingBox enclosure() const;

	/// Returns [0..2] for x, y or z being the widest side of the box.
	static int BSP::largestDimension(const BoundingBox& box);
	double median(int dimension) const;
	Vector measureSplit(const int dim, const double val) const;

	bool intersect(const Ray&,const double,const double) const;
	bool intersect_recurse(const Ray&,const double,const double) const;
	bool intersectForShadow(const Ray&,const double,const double) const;
	bool intersectForShadow_recurse(const Ray&,const double,const double) const;

};



#endif
