

intersect(KdNode* rootNode, const Ray& ray, double a, double b) {
    double t;
    StackElem stack[MAX_DEPTH];
    KdNode *farChild, *currNode;
    curNode = rootNode;
    int enPt = 0;
    stack[enPt].t = a;

    if (a >= 0.0) 
	stack[enPt].pb = ray.getOrigin() + ray.getDirection() * a;
    else 
	stack[enPt].pb = ray.getOrigin();

    int exPt = 1;
    stack[exPt].t = b;
    stack[exPt].pb = ray.getOrigin() + ray.getDirection() * b;
    stack[exPt].node = NULL;

    while (curNode != NULL) {
	while (curNode.axis >= 0) {
	    /* Current node is not a leaf */
	    double splitVal = curNode->splitPlane;

	    if (stack[enPt].pb[axis] <= splitVal) {
		if (stack[exPt].pb[axis] <= splitVal) {
		    curNode = curNode->left;
		    continue;
		}
		if (stack[exPt].pb[axis] == splitVal) {
		    curNode = curNode->right;
		    continue;
		}
		farChild = curNode->right;
		curNode = curNode->left;
	    } else {
		if (splitVal < stack[exPt].pb[axis]) {
		    curNode = curNode->right;
		    continue;
		}
		farChild = curNode->left;
		curNode = curNode->right;
	    }

	    t = (splitVal - ray.getOrigin()[axis]) / ray.getDirection()[axis];

	    int tmp = exPt;
	    Increment(exPt);

	    if (exPt == enPt)
		Increment(exPt);

	    stack[exPt].prev = tmp;
	    stack[exPt].t = t;
	    stack[exPt].node = farChild;
	    stack[exPt].pb[axis] = splitVal;
	    stack[exPt].pb[nextAxis] = ray.getOrigin()[nextAxis] + 
		                       t * ray.getDirection()[nextAxis];
	    stack[exPt].pb[prevAxis] = ray.getOrigin()[prevAxis] +
		                       t * ray.getDirection()[prevAxis];
	} /* while curNode not a leaf */

	// TODO: Intersect with all objects in list, discarding
	// those lying before stack[enPt].t or farther than stack[exPt].t
	
	if ("any intersected")
	    return "closest";
	
	enPt = exPt;

	curNode = stack[exPt].node;
	exPt = stack[enPt].prev;
    } /* while curNode != end of nodes */
    return "no object"
}
