

class KdTree : public SpaceSubdivider {

    private:
	struct KdNode {
	    Object* objlist;  // Enclosed objects when this is a leaf
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

	KdNode* nodes;
}

bool intersect_recurse(const Ray& ray, const double min_t, const double max_t) const {


}
