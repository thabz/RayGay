
#include "volume.h"

/**
 * Using the Marching Cubes algorithm.
 *
 * @see http://www.exaflop.org/docs/marchcubes/ind.html
 */
Intersection Volume::_intersect(const Ray& ray) const {
    const BoundingBox& bbox = this->boundingBoundingBox();
    Vector2 inout = bbox.intersect(ray);
    double t_begin = inout[0];
    double t_end = inout[1];
    double t_step = (t_end - t_begin) / double(steps);

    for(double t = t_begin; t <= t_end; t += t_step) {
	if (this->inside(ray.getPoint(t))) {

	}
    }

    // No intersection
    return Intersection();
}
