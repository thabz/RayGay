
#include "lights/pointlight.h"
#include "lights/lightinfo.h"
#include "math/matrix.h"
#include "intersection.h"
#include "space/kdtree.h"
#include "objects/object.h"

Pointlight::Pointlight(const Vector& pos) : Lightsource(pos) {
}

void Pointlight::getLightinfo(const Intersection& inter, KdTree* space, Lightinfo* info, unsigned int depth) const {
    // TODO: Move point ESPILON along normal to avoid selfshadowing.
    info->direction_to_light = position - inter.getPoint();
    double dist_to_light = info->direction_to_light.length();
    if (IS_ZERO(dist_to_light)) {
	info->cos = 0.0;
	info->intensity = 0.0;
	return;

    }
    info->direction_to_light = info->direction_to_light / dist_to_light;
    info->cos = info->direction_to_light * inter.getNormal();

    if (info->cos > 0.0) {
	Ray ray_to_light = Ray(inter.getPoint(),info->direction_to_light,-1.0);
	bool occluded = shadowcache.occluded(ray_to_light,dist_to_light,depth,space);
	info->intensity = occluded ? 0.0 : 1.0;
    }
}

