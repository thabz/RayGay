

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

double Object::fastIntersect(const Ray& ray) const {
    if (ray.getId() != last_ray) {
	last_t = _fastIntersect(ray);
	last_ray = ray.getId();
	return last_t;
    }
    return last_t;
}

double Object::_fastIntersect(const Ray& ray) const {
    Intersection intersection = _intersect(ray);
    return intersection.getT();
}

