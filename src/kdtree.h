
#ifndef KD_TREE
#define KD_TREE

#include "math/vector.h"
#include "spacesubdivider.h"
#include <vector>

class Ray;
class Intersection;
class Object;

/**
 * k-dimensional tree
 */
class KdTree : public SpaceSubdivider {

    public:
	KdTree();
	virtual ~KdTree();
	bool intersect(const Ray& ray) const; ///< Returns the nearest intersection
	bool intersectPrimary(const Ray& ray) const; ///< Returns the nearest intersection
	bool intersectForShadow(const Ray& ray) const; ///< Returns any intersection 
	bool intersectForShadow(const Ray& ray, const Object* hint) const; ///< Returns any intersection but hint-object is checked for intersection first.

	Intersection* getLastIntersection() const { return last_intersection; };
	void addObject(Object* obj); ///< Place a object in the kd-tree 
	void prepare();

    private:
	struct KdNode {
	    std::vector<Object*>* objects;  // Enclosed objects when this is a leaf
	    KdNode* left;     // Left child
	    KdNode* right;    // Right child
	    float splitPlane; // Position of splitting plane
	    int axis;         // Orientation where x,y,z is 0,1,2 and -1 denotes a leaf
	};

	struct StackElem {
	    KdNode* node;   // pointer to far child
	    float t;        // the entry/exit signed distance
	    Vector pb;      // coordinates of entry/exit point
	    int prev;       // pointer to previus stack item
	};

	bool intersect(const Ray& ray, double a, double b) const;
	bool intersectForShadow(const Ray&,double,double) const;

	KdNode* nodes;
	mutable StackElem* stack;

	std::vector<Object*> objects;
	bool prepared;
	mutable Intersection* last_intersection;
	static Object* last_primary_intersected_object;

};

#endif

