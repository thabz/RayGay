

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

// Stand-in untill all subclasses have _fastIntersect and _fullIntersect methods instead of _intersect.
double Object::_fastIntersect(const Ray& ray) const {
    Intersection intersection = _intersect(ray);
    return intersection.getT();
}

