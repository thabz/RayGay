
#include "object.h"

#include "intersection.h"
#include "ray.h"

object::object() {
   last_ray = -1;
}

/**
 *  A caching proxy around the private _intersect(Ray) method in subclasses.
 */

/*  Because an object can exist in several bounding boxes we end up shooting
 *  the same ray at the same object several times.
 */
Intersection object::intersect(const Ray& ray) const {
    if (ray.getId() != last_ray) {
	Intersection result = _intersect(ray);
	last_ray = ray.getId();
	result.setObject(const_cast<object*>(this));
	last_intersection = result;
    }
    return last_intersection;
}
