
#ifndef HIERARCHY_H
#define HIERARCHY_H

#include "object.h"
#include <vector>
#include <iosfwd>
#include "boundingbox.h"
#include "intersection.h"
#include "spacesubdivider.h"
class Ray;

#define HIERARCHY_MAX_OBJECTS_PER_LEAF 8
#define HIERARCHY_MAX_DEPTH 100

/// 

/**
 * Implementation of a bounding volume hierarchy by octrees.
 * This is basically a tree of BoundingBox at the joins and object as leafs.
 *
 * A method proposed by Glassner in 1984.
 *
 * TODO: Implementer følgende optimering:
 *
 * I. Gargantini and H.H. Atkinson: {Ray Tracing and Octree: Numerical Evaluation of the First Intersection}, CGF, Vol. 12, No. 4, pp. 199-210, 1993.

In this paper the authors propose the recursive traversal algorithm, that has special encoding of octants that have to be traversed. Authors claim the improvement of execution speed from 32% to 62% over the Samet traversal code [9], when the number of voxels is huge. Since we implemented this traversal algorithm, we outline it briefly. We assume the signed distance to the entry point and exit point of the octant is known. First we compute the signed distances with all three subdivision planes. We sort these three signed distances in an ascending order. Then we disregard the signed distances outside the interval given by the entry point and the exit point. Next we identify the octants by the positioning of the intersection points with subdivision planes with regard to the other planes coordinates. The traversal is based on the knowledge that in an octree at most four octants can be pierced. These are induced by four intervals given by three intersection points inside the current octant plus entry and exit point (five points along the ray path gives four intervals). The authors also discuss the robustness of the traversal algorithm when the ray pierces the neighborhood of intersection of more than one splitting plane, at worst the middle point of the octree node.
 */
// TODO: Fix så nedarvning kan lade sig gøre
class Hierarchy { //: public SpaceSubdivider {
    friend std::ostream & operator<< (std::ostream &os, Hierarchy &x);

    public:
        /// Constructor.
	Hierarchy();

        /// Constructor (used by Mesh for now... this really should be private)
	Hierarchy(BoundingBox bbox);

	/// Destructor
	virtual ~Hierarchy();

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
	int _depth;
	std::vector<object*> objects;
	std::vector<Hierarchy*> children;
};

#endif
