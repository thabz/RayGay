
#ifndef GENERIC_KD_TREE_H
#define GENERIC_KD_TREE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vector>
#include "boundingbox.h"

/*
 * Macros for accessing the packed KdNode.
 */
//#define leftNode(node) (node->left == 0 ? NULL : &(nodes[node->left]))
//#define rightNode(node) (node->left == 0 ? NULL : &(nodes[node->left+1]))
#define leftNode(node) (&(nodes[(((node)->left) >> 2)]))
#define rightNode(node) (&(nodes[(((node)->left) >> 2)+1]))
#define getTopNode() (&(nodes[0]))

template<class ObjectType>
class KdNode {
    public:
	union {
	    // Enclosed objects when this is a leaf
	    ObjectType** objects;
	    // Position of splitting plane when not a leaf
	    float splitPlane;
	};
	union {
	    // [30 bits left/num | 2 bits axis]
	    
	    // Left child when not a leaf. Right child is left + 1.
	    uint left;  
	    // Number of objects when this is a leaf
	    uint num;

	    // Axis where x,y,z is 0,1,2 and 3 denotes a leaf is 
	    // packed into left/num as first two bits.
	};

	void initLeafNode(uint num, ObjectType** objects);
	void initInteriorNode(uint axis, float plane, uint left);
	bool isLeafNode() const;
	float getSplitValue() const;
	uint getObjectNum() const;
	uint getAxis() const;
};

template<class ObjectType>
class BoundedObject {
    public:
	BoundingBox bbox;
	ObjectType* object;
};


template<class ObjectType>
class GenericKdTree {

    public:
	/// Destructor
	virtual ~GenericKdTree();

	/// Place a object in the kd-tree 
	void addObject(ObjectType* obj); 
	
	void prepare();
	
	/// The BoundingBox around all objects added to the tree
	BoundingBox boundingBox() const { return world_bbox; };

    protected:
	/// Constructor
	GenericKdTree(uint max_depth, uint max_objs);

	BoundingBox enclosure(BoundedObject<ObjectType>** bobs, uint num) const;

	BoundingBox world_bbox;
	uint max_depth;

	// The kd-tree nodes
	vector<KdNode<ObjectType> > nodes;

    private:
	bool prepared;
	uint opt_max_depth; 
	uint opt_max_objs;
	
	// The I/O data for the findBestSplitPlane method
	struct CostResult {
	    double axis; //> Output
		int dim; //> Output
		int current_sort_dim;
		uint left_index; //> Output
		uint right_index; //> Output
	};
	
	// The recursive prepare method
	void prepare(uint num, const BoundingBox& bbox, uint depth, const uint dest_idx);

	bool findBestSplitPlane(uint size, const BoundingBox& bbox, CostResult& result) const;
	void findBestSplitPlane(uint size, const BoundingBox& bbox, CostResult& result, int split_dim) const;


	BoundedObject<ObjectType>** left_bobs;
	BoundedObject<ObjectType>** right_bobs;

	std::vector<ObjectType*>* added_objects;
};

template<class ObjectType>
class cmpL {
    public:
	cmpL(uint d) { this->d = d; } 
	bool operator()(const BoundedObject<ObjectType>* const p1, const BoundedObject<ObjectType>* const p2) const {
	    return p1->bbox.minimum(d) < p2->bbox.minimum(d);
	}
    private:
	uint d;
};

template<class ObjectType>
class cmpR {
    public:
	cmpR(uint d) { this->d = d; }
	bool operator()(const BoundedObject<ObjectType>* const p1, const BoundedObject<ObjectType>* const p2) const {
	    return p1->bbox.maximum(d) < p2->bbox.maximum(d);
	}
    private:
	uint d;
};

#include "space/generickdtree.cpp"

#endif
