

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
 * This gets called after construction but before any
 * intersection methods are called.
 */
void Object::prepare() {
    // Default does nothing
}

double Object::area() const {
    return 1;
}
