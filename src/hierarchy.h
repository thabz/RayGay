
#ifndef HIERARCHY_H
#define HIERARCHY_H

#include "object.h"
#include <vector>
#include <iosfwd>
#include "box.h"
#include "intersection.h"
class Ray;

#define HIERARCHY_MAX 8

/// Implementation of a bounding volume hierarchy.

// This is basically a tree of Box at the joins and object as leafs.
class Hierarchy {
    friend std::ostream & operator<< (std::ostream &os, Hierarchy &x);

    public:
	Hierarchy(Box bbox, Hierarchy* parent);
	~Hierarchy();

	void addObject(object* obj); ///< Place a object in the hierarchy
	Intersection intersect(const Ray& ray); ///< Calculate an intersection with the hierarchy

	void optimize(); ///< Clean up

    private:
	Hierarchy* _parent;
	bool hasChildren() const;
	bool hasObjects() const;
	void split();
	Box& getBox() { return _box; };
	void pruneChildren();
	void optimizePaths();
	void shrinkBoxes();
	double area(); ///< The surfacearea of all boundingboxes in this hierarchy
	
	Box _box;
	std::vector<object*> objects;
	std::vector<Hierarchy*> children;
};

#endif
