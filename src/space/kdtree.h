
#ifndef SPACE_KD_TREE_H
#define SPACE_KD_TREE_H

#include "math/vector.h"
#include "boundingbox.h"
#include "objects/object.h"
#include <vector>

/*
class Ray;
class Intersection;
class Object;
*/

	class BoundedObject {
	    public:
		Object* object;
		BoundingBox bbox;
	};

/**
 * k-dimensional tree.
 *
 * Implementerer http://www.acm.org/jgt/papers/HavranKopalBittnerZara97/ som er den bedste BSP-traversal algorithme.
 */
class KdTree {

    public:
	/// Constructor
	KdTree();
	/// Destructor
	virtual ~KdTree();
	bool intersect(const Ray& ray, Intersection* result) const; 
	//bool intersect(const Ray& ray) const; ///< Returns the nearest intersection

	bool intersectPrimary(const Ray& ray, Intersection* result) const; ///< Returns the nearest intersection
	Object* intersectForShadow(const Ray& ray, double max_t) const; ///< Returns any intersection 

	void addObject(Object* obj); ///< Place a object in the kd-tree 
	void prepare();

	/// The BoundingBox around all objects added to the tree
	BoundingBox boundingBox() const { return world_bbox; };


    private:
	class KdNode {
	    public:
		KdNode* left;     // Left child
		union {
		    std::vector<Object*>* objects;  // Enclosed objects when this is a leaf
		    KdNode* right;    // Right child
		};
		float splitPlane; // Position of splitting plane
		int axis;         // Orientation where x,y,z is 0,1,2 and -1 denotes a leaf
	    public:
		KdNode();
	};
	
	
	class KdNodeTmp {
	    public:
		std::vector<BoundedObject>* bobjects;  // Enclosed objects when this is a leaf
		BoundingBox bbox; // Bounding box of voxel
		int left;     // Left child
		int right;    // Right child
		float splitPlane; // Position of splitting plane
		int axis;         // Orientation where x,y,z is 0,1,2 and -1 denotes a leaf
	    public:
		KdNodeTmp();
	};

	struct StackElem {
	    KdNode* node;   // pointer to far child
	    float t;        // the entry/exit signed distance
	    Vector pb;      // coordinates of entry/exit point
	    int prev;       // pointer to previus stack item
	};

	// The I/O data for the findBestSplitPlane method
	class CostResult {
	    public:
		CostResult();
		int dim; //> Output
		double axis; //> Output
		vector<BoundedObject>* left_bobjects; //> Input
		vector<BoundedObject>* right_bobjects; //> Input
		unsigned int left_index; //> Output
		unsigned int right_index; //> Output
	};

	bool intersect(const Ray& ray, Intersection* result, const double a, const double b) const;
	Object* intersectForShadow_real(const Ray&,const double) const;
	int largestDimension(const BoundingBox& box) const;
	BoundingBox enclosure(const std::vector<BoundedObject>& objects) const;
	BoundingBox world_bbox;
	bool findBestSplitPlane(const BoundingBox& bbox, CostResult& result) const;

	// The recursive prepare method
	void prepare(int curNode_idx, int depth);

	// The kd-tree nodes
	KdNode* nodes;
	
	// Temporary nodes used when building the three
	std::vector<KdNodeTmp> tmp_nodes;

	int max_depth;
	bool prepared;
};


#endif

