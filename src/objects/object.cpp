
#include "object.h"

#include "intersection.h"
#include "ray.h"

Object::Object(const Material* material) {
   last_ray = -1;
   this->material = material;
}


/**
 * This gets called after construction but before any
 * intersection methods are called.
 */
void Object::prepare() {
    // Default does nothing
}

