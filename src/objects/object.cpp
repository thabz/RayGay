

#include "intersection.h"
#include "ray.h"
#include "space/kdtree.h"

#include "object.h"

Object::Object(const Material* material) {
   this->material = material;
}

void Object::addSelf(KdTree* space) {
    space->addObject((Object*)this);
}

/**
 * Refine an intersection. This is used when pruning the Kd-Tree.
 * Many objects have empty space in their bounding boxes. If an
 * voxel only intersects empty space in an objects bounding box
 * the objects can safely be removed from said voxel during pruning.
 *
 * This methods allows for a more refined test of whether an object
 * lies in a voxel's bounding box.
 *
 * @param voxel_bbox The bounding box of the voxel
 * @param obj_bbox This object's own bounding box
 * @return 0 if this test should be ignored
 *         1 if intersection
 *         -1 if no intersection
 */
int Object::intersects(const AABox& voxel_bbox, const AABox& obj_bbox) const {
    return 0;
}

/**
 * This gets called after construction but before any
 * intersection methods are called.
 */
void Object::prepare() {
    // Default does nothing
}

double Object::area() const {
    return 1;
}

bool Object::canSelfshadow() const {
    return true;
}
