
#ifndef SPACE_KD_TREE_H
#define SPACE_KD_TREE_H

#include "math/vector.h"
#include "boundingbox.h"
#include <vector>

class Object;

struct BoundedObject {
	Object* object;
	BoundingBox bbox;
};

/**
 * k-dimensional tree.
 *
 * Implementerer http://www.acm.org/jgt/papers/HavranKopalBittnerZara97/ som er den bedste BSP-traversal algorithme.
 *
 * \todo Make a destructor that delete nodes recursively beginning at top_node.
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
		union {
		    // Left child when not a leaf. Right child is left + 1.
		    unsigned int left;  
		    // Enclosed objects when this is a leaf
		    Object** objects;
		};
		union {
		    // Number of objects when this is a leaf
		    unsigned int num;
		    // Position of splitting plane
		    float splitPlane;
		};
		// Orientation where x,y,z is 0,1,2 and -1 denotes a leaf
		short axis;
	};


	class KdNodeTmp {
	    public:
		std::vector<BoundedObject*>* bobjects;  // Enclosed objects when this is a leaf
		float splitPlane; // Position of splitting plane
		int axis;         // Orientation where x,y,z is 0,1,2 and -1 denotes a leaf
	};

	struct StackElem {
	    const KdNode* node;   // pointer to far child
	    float t;        // the entry/exit signed distance
	    int prev;       // pointer to previus stack item
	    double pb[3];   // coordinates of entry/exit point
	};

	// The I/O data for the findBestSplitPlane method
	class CostResult {
	    public:
		CostResult();
		int dim; //> Output
		double axis; //> Output
		int current_sort_dim;
		std::vector<BoundedObject*>* left_bobjects; //> Input
		std::vector<BoundedObject*>* right_bobjects; //> Input
		unsigned int left_index; //> Output
		unsigned int right_index; //> Output
		unsigned int left_size;
		unsigned int right_size;
	};

	bool intersect(const Ray& ray, Intersection* result, const double a, const double b) const;
	Object* intersectForShadow_real(const Ray&,const double) const;
	BoundingBox enclosure(std::vector<BoundedObject*>* bobs) const;
	BoundingBox world_bbox;
	bool findBestSplitPlane(const BoundingBox& bbox, CostResult& result) const;
	void findBestSplitPlane(const BoundingBox& bbox, CostResult& result, int split_dim) const;
	// The recursive prepare method
	void prepare(KdNodeTmp* tmp_node, const BoundingBox& bbox, unsigned int depth, const unsigned int dest_idx);

	// The kd-tree nodes
	vector<KdNode> nodes;

	unsigned int max_depth;
	bool prepared;

	std::vector<Object*>* added_objects;
};


#endif

