
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
#define KD_TREE_MAX 3
#define KD_TREE_MAX_DEPTH 100

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
		std::vector<BoundedObject> bobjects;  // Enclosed objects when this is a leaf
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

	class CostResult {
	    public:
		int dim;
		double axis;
		vector<BoundedObject> left_bobjects;
		vector<BoundedObject> right_bobjects;
		unsigned int left_index;
		unsigned int right_index;
	};

	bool intersect(const Ray& ray, Intersection* result, const double a, const double b) const;
	Object* intersectForShadow_real(const Ray&,const double) const;
	int largestDimension(const BoundingBox& box) const;
	BoundingBox enclosure(std::vector<BoundedObject>* objects) const;
	BoundingBox world_bbox;
	double objectMedian(std::vector<Object*>* objects, int d) const;
	double spacialMedian(std::vector<Object*>* objects, int d) const;
	Vector measureSplit(const std::vector<BoundedObject>& bobjects, int dim, double val) const;
	double evaluateCost(const BoundingBox& bbox, const std::vector<BoundedObject>& objects, int dim, double val, Vector* measure) const;
	void findBestSplitPlane(const BoundingBox& bbox, CostResult& result) const;

	void prepare(int curNode_idx, int depth);

	KdNode* nodes;
	std::vector<KdNodeTmp> tmp_nodes;
	int max_depth;

	std::vector<Object*>* added_objects;
	bool prepared;

};


#endif

