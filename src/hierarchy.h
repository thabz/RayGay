
#ifndef HIERARCHY_H
#define HIERARCHY_H

#include "object.h"
#include <vector>
#include <iosfwd>
#include "boundingbox.h"
#include "intersection.h"
#include "spacesubdivider.h"
class Ray;

#define HIERARCHY_MAX 8

/// Implementation of a bounding volume hierarchy.

// This is basically a tree of BoundingBox at the joins and object as leafs.
class Hierarchy : public SpaceSubdivider {
    friend std::ostream & operator<< (std::ostream &os, Hierarchy &x);

    public:
        /// Constructor.
	Hierarchy();

        /// Constructor.
	Hierarchy(BoundingBox bbox);

	/// Destructor
	~Hierarchy();

	void addObject(object* obj); ///< Place a object in the hierarchy
	Intersection intersect(const Ray& ray) const; ///< Calculate an intersection with the hierarchy
	Intersection intersectForShadow(const Ray& ray) const; ///< Calculate an intersection with the hierarchy
	Intersection intersectForShadow(const Ray& ray, const object* hint) const; ///< Calculate an intersection with the hierarchy

	/// This gets called after all objects are added and before any intersection methods are called.
	void prepare();

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
