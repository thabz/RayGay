

#include "intersection.h"
#include "ray.h"
#include "space/spacesubdivider.h"

#include "object.h"

Object::Object(const Material* material) {
   last_ray = -1;
   this->material = material;
}

void Object::addSelf(SpaceSubdivider* space) {
    space->addObject((Object*)this);
}

/**
 * This gets called after construction but before any
 * intersection methods are called.
 */
void Object::prepare() {
    // Default does nothing
}

