
#include "object.h"

#include "intersection.h"
#include "ray.h"

object::object() {
   last_ray = -1;
}


/**
 * This gets called after construction but before any
 * intersection methods are called.
 */
void object::prepare() {
    // Default does nothing
}

