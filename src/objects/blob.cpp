
#include "objects/blob.h"
#include "math/vector.h"
#include "math/matrix.h"

////////////////////////////////////////////////////////////////////////
// Blob surface code
////////////////////////////////////////////////////////////////////////

/**
 * @param iso the weight level defining the surface
 * @param steps the raymarching steps
 * @param accuracy the accuracy which is some low number
 * @param material material of the Blob
 */
Blob::Blob(double iso, unsigned int steps, double accuracy, Material* material) : IsoSurface(steps,accuracy,iso,material) {
    atoms_num = 0;
}

/**
 * Add an atom to the blob.
 * 
 * @param center center of the sphere
 * @param radius radius of the sphere
 * @param weight weight to use
 */
void Blob::addAtom(const Vector& center, double radius, double weight) {
    centers.push_back(center);
    radii.push_back(radius*radius);
    weights.push_back(weight);
    atoms_num++;

    Vector rrr = Vector(radius,radius,radius);
    BoundingBox box = BoundingBox(center-rrr,center+rrr);
    bbox = BoundingBox::doUnion(box,bbox);

    tree.addObject(new BlobAtom(radius, weight, center));
}

void Blob::prepare() {
    tree.prepare();
}

/**
 * Using the metaball distribution model.
 */
/*
double Blob::evaluateFunction(const Vector& point) const {
    double sum = 0;
    for(int i = 0; i < atoms_num; i++) {
	double r = (centers[i] - point).length();
	if (r >= radii[i]) {
	    continue;
	} else if (r <= radii[i] / 3.0) {
	    double p = r / radii[i];
	    sum += weights[i] * (1.0 - 3.0 * p * p);
	} else {
	    double p = 1.0 - (r / radii[i]);
	    sum += 1.5 * weights[i] * p * p;
	}
    }
    return sum;
}
*/
double Blob::evaluateFunction(const Vector& point) const {
#if 1
    return tree.eval(point);
#else
    double sum = 0;
    double one_ninth = 1.0 / 9.0;
    for(int i = 0; i < atoms_num; i++) {
	double rr = (centers[i] - point).norm();
	double RR = radii[i];
	if (rr >= RR) {
	    continue;
	} else {
	    double q = rr / RR;
	    //double p = -22.0 + 17.0*q - 4.0*q*q;
	    double p = ((-4.0 * q + 17.0) * q - 22.0) * q * one_ninth;
	    //p *= q * one_ninth;
	    sum += weights[i] * (1.0 + p);
	}
    }
    return sum;
#endif
}

SceneObject* Blob::clone() const {
    Blob* result = new Blob(*this);
    return result;
}

////////////////////////////////////////////////////////////////////////
// BlobAtom code
////////////////////////////////////////////////////////////////////////

BlobAtom::BlobAtom(double radius, double weight, const Vector& center) {
    this->radius = radius;
    this->weight = weight;
    this->center = center;
    this->radius_squared = radius * radius;
}

BoundingBox BlobAtom::boundingBoundingBox() const {
    Vector rrr = Vector(radius,radius,radius);
    return BoundingBox(center-rrr,center+rrr);
}


/**
 * Check to see if the sphere overlaps the voxel_bbox by
 * finding the squared distance from the sphere to the bbox.
 *
 * J. Arvo: "A simple method for box-sphere intersection testing" in: A. Glassner (ed.), <i>Graphics Gems</i>, pp. 335-339, Academic Press, Boston, MA, 1990.
 */
int BlobAtom::intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const {
    //return 0;
    double s;
    double d = 0.0;
    for(int i = 0; i < 3; i++) {
	if (center[i] < voxel_bbox.minimum()[i]) {
	    s = center[i] - voxel_bbox.minimum()[i];
	    d += s*s;
	} else if (center[i] > voxel_bbox.maximum()[i]) {
	    s = center[i] - voxel_bbox.maximum()[i];
	    d += s*s;
	}
    }
    return d <= radius*radius + EPSILON ? 1 : -1;
}

////////////////////////////////////////////////////////////////////////
// BlobTree code
////////////////////////////////////////////////////////////////////////

#define KD_TREE_MAX 15 
#define KD_TREE_MAX_DEPTH 10

BlobTree::BlobTree() : GenericKdTree<BlobAtom>(KD_TREE_MAX_DEPTH, KD_TREE_MAX) {
}

double BlobTree::eval(const Vector& point) const {
    int axis;
    double value;
    const KdNode<BlobAtom>* node = getTopNode();

    // Find the leaf node point is in
    while (!isLeafNode(node)) {
	axis = getNodeAxis(node);
	value = getNodeSplitValue(node);
	if (point[axis] < value) {
	    node = leftNode(node);
	} else {
	    node = rightNode(node);
	}
    }

    // Find the blob value
    double sum = 0.0;
    double one_ninth = 1.0 / 9.0;
    uint num = getNodeObjectNum(node);
    BlobAtom* b;
    for(uint i = 0; i < num; i++) {
	b = node->objects[i];
	double rr = (b->center - point).norm();
	double RR = b->radius * b->radius;
	if (rr >= RR) {
	    continue;
	} else {
	    double q = rr / RR;
	    double p = ((-4.0 * q + 17.0) * q - 22.0) * q * one_ninth;
	    sum += b->weight * (1.0 + p);
	}
    }

    return sum;
}

