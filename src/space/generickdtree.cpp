
#include "space/generickdtree.h"
#include "stats.h"
#include "exception.h"
#include <cassert>

// Don't check for best split in all 3 dimensions when number
// of objects in a node exceeds this value. Then just use largest
// dimension of bbox as best dimension.
#define KD_TREE_MAX_ELEMENTS_IN_FULL_SPLIT_CHECK 25

template<class ObjectType>
GenericKdTree<ObjectType>::GenericKdTree<ObjectType>(uint max_depth, uint max_objs) {
    this->opt_max_depth = max_depth;
    this->opt_max_objs = max_objs;

    prepared = false;
    added_objects = new std::vector<ObjectType*>;
}

template<class ObjectType>
GenericKdTree<ObjectType>::~GenericKdTree<ObjectType>() {
    for(uint i = 0; i < nodes.size(); i++) {
	if (nodes[i].axis == -1 && nodes[i].num > 0) {
	    delete [] nodes[i].objects;
	}
    }
    nodes.clear();
}

template<class ObjectType>
void GenericKdTree<ObjectType>::addObject(ObjectType* obj) {
    Stats::getUniqueInstance()->inc(STATS_KDTREE_OBJECTS_ADDED);
    added_objects->push_back(obj);
}

template<class ObjectType>
BoundingBox GenericKdTree<ObjectType>::enclosure(BoundedObject<ObjectType>** bobs, uint num) const {
    assert(num > 0);
    BoundingBox result = bobs[0]->bbox; 
    for(uint i = 1; i < num; i++) {
	result = BoundingBox::doUnion(result,bobs[i]->bbox);
    }
    return result;
}

template<class ObjectType>
void GenericKdTree<ObjectType>::findBestSplitPlane(uint size, const BoundingBox& bbox, CostResult& result, int d) const {
    assert(d == 0 || d == 1 || d == 2);

    double split;
    Vector bbox_lenghts = bbox.lengths();
    double lowest_cost = 0.9*size*bbox.area();

    double cap_a = 2 * bbox_lenghts[(d+1)%3] * bbox_lenghts[(d+2)%3];
    double cap_p = 2 * bbox_lenghts[(d+1)%3] + 2 * bbox_lenghts[(d+2)%3];

    sort(left_bobs, left_bobs + size, cmpL<ObjectType>(d));
    sort(right_bobs, right_bobs + size, cmpR<ObjectType>(d));
    result.current_sort_dim = d;

    uint l = 0;
    uint r = 0;
    bool used_right;
    double rsplit, lsplit;
    while (l < size || r < size) {
	if (l < size && r < size) {
	    rsplit = right_bobs[r]->bbox.maximum(d);
	    lsplit = left_bobs[l]->bbox.minimum(d);
	    if (rsplit < lsplit) {
		split = rsplit;
		used_right = true;
	    } else {
		split = lsplit;
		used_right = false;
	    }
	} else {
	    if (l == size) {
		split = right_bobs[r]->bbox.maximum(d);
		used_right = true;
	    } else {
		split = left_bobs[l]->bbox.minimum(d);
		used_right = false;
	    }
	}

	if (used_right) r++;

	if (split < bbox.maximum(d) && split > bbox.minimum(d)) {
	    double left_area = cap_a + (split - bbox.minimum(d))*cap_p;
	    double right_area = cap_a + (bbox.maximum(d) - split)*cap_p;
	    double cost = left_area * l + right_area * (size -r);
	    if (cost < lowest_cost) {  
		result.dim = d;
		result.axis = split;
		result.left_index = l;
		result.right_index = r;
		lowest_cost = cost;
	    } 
	}

	if (!used_right) l++;
    } /* while more edges to check */
}

template<class ObjectType>
bool GenericKdTree<ObjectType>::findBestSplitPlane(uint num, const BoundingBox& bbox, CostResult& result) const {
    result.dim = -1;
    result.left_index = 0;
    result.right_index = 0;

    if (num == 0) 
	return false;

    // Make a copy of the left bobjects pointer list for this node
    memcpy(right_bobs, left_bobs, num*sizeof(BoundedObject<ObjectType>*));


    if (num < KD_TREE_MAX_ELEMENTS_IN_FULL_SPLIT_CHECK) {
	// Find best split in all 3 dimensions
	for(int d = 0; d < 3; d++) {
	    findBestSplitPlane(num, bbox, result, d);
	}
    } else {
	// Find best split in largest dimension
	int d = bbox.lengths().largestDimension();
	findBestSplitPlane(num, bbox,result, d);
    }

    if (result.dim == -1) {
	// Not splitting has best cost
	return false;
    } else {
	if (result.current_sort_dim != result.dim) {
	    // Sort objects again
	    sort(left_bobs, left_bobs + num, cmpL<ObjectType>(result.dim));
	    //sort(right_bobs, right_bobs + num, cmpR(result.dim));
	    result.current_sort_dim = result.dim;
	}
	return true;
    }
}

template<class ObjectType>
void GenericKdTree<ObjectType>::prepare() {
    if (prepared) throw_exception("Already prepared.");

    uint num = added_objects->size();
    assert(num > 0);
    
    BoundedObject<ObjectType>* bobs = new BoundedObject<ObjectType>[num];
    left_bobs = new BoundedObject<ObjectType>*[num];
    right_bobs = new BoundedObject<ObjectType>*[num];

    for(uint i = 0; i < num; i++) {
	bobs[i].object = added_objects->operator[](i);
	bobs[i].bbox = added_objects->operator[](i)->boundingBoundingBox();
	left_bobs[i] = &(bobs[i]);
    }

    delete added_objects;
    added_objects = NULL;

    world_bbox = enclosure(left_bobs,num);
    max_depth = 0;

    nodes.push_back(KdNode<ObjectType>());
    assert(nodes.size() == 1);

    prepare(num, world_bbox, 1, 0);

    delete [] bobs;
    delete [] left_bobs;
    delete [] right_bobs;
    
    Stats::getUniqueInstance()->put(STATS_KDTREE_DEPTH,max_depth);
    Stats::getUniqueInstance()->put(STATS_KDTREE_NODES,nodes.size());
    //cout << "Size of KdNode: " << sizeof(KdNode) << endl;
    //cout << "Waste: " << (nodes.capacity() - nodes.size())*sizeof(KdNode) << endl;
    prepared = true;
}

template<class ObjectType>
void GenericKdTree<ObjectType>::prepare(uint num, const BoundingBox& bbox, uint depth, const uint dest_idx) {

    assert(dest_idx < nodes.size());

    // Mark curNode as a leaf until a suitable split-plane is found
    int axis = -1;
    double splitPlane = 0;

    uint new_left_idx = 0;
    uint new_right_idx = 0;

    // Keep within max depth or minimum node size
    if (depth <= opt_max_depth && num > opt_max_objs) {

	if (depth > max_depth) {
	    max_depth = depth;
	}

	CostResult splitResult;

	// Find the best axis to split node at
	if (findBestSplitPlane(num, bbox,splitResult)) {

	    // The current node will be split 
	    axis = splitResult.dim;
	    splitPlane = splitResult.axis;

	    // Allocate childnodes
	    nodes.push_back(KdNode<ObjectType>());
	    nodes.push_back(KdNode<ObjectType>());
	    new_left_idx = nodes.size() - 2;
	    new_right_idx = nodes.size() - 1;

	    // Find bounding boxes for the two children
	    BoundingBox lower_bbox;
	    BoundingBox higher_bbox;
	    if (!bbox.split(lower_bbox, higher_bbox, axis, splitPlane)) {
		throw_exception("Split plane outside bbox of node");
	    }

	    BoundedObject<ObjectType>* tmp;

	    // Move into lower
	    uint j = 0;
	    for(uint i = 0; i < splitResult.left_index; i++) {
		BoundedObject<ObjectType>* bob = left_bobs[i];
		if (bob->bbox.minimum(axis) < splitPlane) {
		    if (bob->object->intersects(lower_bbox,bob->bbox) >= 0) {
			tmp = left_bobs[j];
			left_bobs[j] = bob;
			left_bobs[i] = tmp;
			j++;
		    }
		}
	    }
	    assert(j <= splitResult.left_index);
	    // Recurse into left subtree
	    prepare(j, lower_bbox, depth+1, new_left_idx);

	    // Move into higher
	    j = 0;
	    for(uint i = 0; i < num; i++) {
		BoundedObject<ObjectType>* bob = left_bobs[i];
		if (bob->bbox.maximum(axis) > splitPlane) {
		    if (bob->object->intersects(higher_bbox,bob->bbox) >= 0) {
			tmp = left_bobs[j];
			left_bobs[j] = bob;
			left_bobs[i] = tmp;
			j++;
		    }
		}
	    }
	    assert(j <= num - splitResult.right_index );
	    // Recurse into right subtree
	    prepare(j, higher_bbox, depth+1, new_right_idx);
	}
    } 

    // Build the real KdNode
    KdNode<ObjectType>& new_node = nodes[dest_idx];

    if (axis == -1) {
	new_node.axis = -1;
	new_node.num = num;
	if (num > 0) {
	    new_node.objects = new ObjectType*[num];
	    for(uint j = 0; j < num; j++) {
		new_node.objects[j] = left_bobs[j]->object;
	    }
	} else {
	    new_node.objects = NULL;
	}
    } else {
	new_node.axis = axis;
	new_node.splitPlane = splitPlane;
	assert(new_left_idx != 0 && new_right_idx != 0);
	assert(new_right_idx == new_left_idx + 1);
	new_node.left = new_left_idx;
    }
}

