
#ifndef HIERARCHY_H
#define HIERARCHY_H

#include "object.h"
#include <vector>
#include <iosfwd>
#include "boundingbox.h"
#include "intersection.h"
class Ray;

#define HIERARCHY_MAX 8

/// Implementation of a bounding volume hierarchy.

// This is basically a tree of BoundingBox at the joins and object as leafs.
class Hierarchy {
    friend std::ostream & operator<< (std::ostream &os, Hierarchy &x);

    public:
        /// Constructor.
	Hierarchy(BoundingBox bbox);
	
	~Hierarchy();

	void addObject(object* obj); ///< Place a object in the hierarchy
	Intersection intersect(const Ray& ray); ///< Calculate an intersection with the hierarchy
	Intersection intersectForShadow(const Ray& ray); ///< Calculate an intersection with the hierarchy
	Intersection intersectForShadow(const Ray& ray, const object* hint); ///< Calculate an intersection with the hierarchy

	void optimize(); ///< Clean up

    private:
	Hierarchy(BoundingBox bbox, Hierarchy* parent);
	Hierarchy* _parent;
	bool hasChildren() const;
	bool hasObjects() const;
	void split();
	BoundingBox& getBoundingBox() { return _box; };
	void pruneChildren();
	void optimizePaths();
	void shrinkBoundingBoxes();
	double area(); ///< The surfacearea of all boundingboxes in this hierarchy
	
	BoundingBox _box;
	std::vector<object*> objects;
	std::vector<Hierarchy*> children;
};

#endif
