
#include "objects/blob.h"
#include "math/vector.h"
#include "math/matrix.h"

////////////////////////////////////////////////////////////////////////
// BlobAtom code
////////////////////////////////////////////////////////////////////////

BlobAtom::BlobAtom(double radius, double weight) 
{
    this->radius = radius;
    this->weight = weight;
    this->radius_squared = radius * radius;
}

////////////////////////////////////////////////////////////////////////
// BlobAtomSphere code
////////////////////////////////////////////////////////////////////////

BlobAtomSphere::BlobAtomSphere(double radius, double weight, const Vector& center) : BlobAtom(radius, weight) 
{
    this->center = center;
}

BoundingBox BlobAtomSphere::boundingBoundingBox() const 
{
    Vector rrr = Vector(radius,radius,radius);
    return BoundingBox(center-rrr,center+rrr);
}

double BlobAtomSphere::squaredDistToPoint(const Vector& point) const 
{
    return (center - point).norm();
}

/**
 * Check to see if the sphere overlaps the voxel_bbox by
 * finding the squared distance from the sphere to the bbox.
 *
 * J. Arvo: "A simple method for box-sphere intersection testing" in: A. Glassner (ed.), <i>Graphics Gems</i>, pp. 335-339, Academic Press, Boston, MA, 1990.
 */
int BlobAtomSphere::intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const {
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
    return d <= radius_squared + EPSILON ? 1 : -1;
}

////////////////////////////////////////////////////////////////////////
// BlobAtomCylinder code
////////////////////////////////////////////////////////////////////////

BlobAtomCylinder::BlobAtomCylinder(double radius, double weight, const Vector& from, const Vector& to) : BlobAtom(radius, weight) 
{
    this->from = from;
    this->to = to;
}

BoundingBox BlobAtomCylinder::boundingBoundingBox() const 
{
    BoundingBox bbox = BoundingBox(from,to);
    bbox.grow(radius);
    return bbox;
}

double BlobAtomCylinder::squaredDistToPoint(const Vector& point) const 
{
    // TODO: Implement
    return 0;
}

int BlobAtomCylinder::intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const {
    return 0;
}

////////////////////////////////////////////////////////////////////////
// BlobTree code
////////////////////////////////////////////////////////////////////////

#define KD_TREE_MAX 10 
#define KD_TREE_MAX_DEPTH 20

BlobTree::BlobTree() : GenericKdTree<BlobAtom>(KD_TREE_MAX_DEPTH, KD_TREE_MAX) {
}

double BlobTree::eval(const Vector& point) const {
    uint axis;
    double value;
    const KdNode<BlobAtom>* node = getTopNode();

    // Find the leaf node point is in
    while (!node->isLeafNode()) {
	axis = node->getAxis();
	value = node->getSplitValue();
	if (point[axis] < value) {
	    node = leftNode(node);
	} else {
	    node = rightNode(node);
	}
    }

    // Find the blob value
    double sum = 0.0;
    const double one_ninth = 0.11111111111111111111111111111111111111; // 1 / 9
    uint num = node->getObjectNum();
    BlobAtom* b;
    for(uint i = 0; i < num; i++) {
	b = node->objects[i];
	double rr = b->squaredDistToPoint(point);
	double RR = b->radius_squared;
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
    tree = new BlobTree();
}

Blob::~Blob() {
    delete tree;
}

/**
 * Add an spherical atom to the blob.
 * 
 * @param center center of the sphere
 * @param radius radius of the sphere
 * @param weight weight to use
 */
void Blob::addAtom(const Vector& center, double radius, double weight) {
    tree->addObject(new BlobAtomSphere(radius, weight, center));
}

void Blob::prepare() {
    tree->prepare();
}

double Blob::evaluateFunction(const Vector& point) const {
    return tree->eval(point);
}

SceneObject* Blob::clone() const {
    Blob* result = new Blob(*this);
    return result;
}

BoundingBox Blob::_boundingBoundingBox() const 
{
    return tree->boundingBox();
}

