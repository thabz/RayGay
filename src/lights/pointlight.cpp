
#include "lights/pointlight.h"
#include "lights/lightinfo.h"
#include "intersection.h"

class KdTree;

Pointlight::Pointlight(const Vector& pos) : Lightsource(pos) {
}

void Pointlight::getLightinfo(const Intersection& inter, KdTree* space, Lightinfo* info, unsigned int depth) const {
    
    // Move intersection point ESPILON along surface normal to 
    // avoid selfshadowing.
    Vector surface_point = inter.getPoint() + 10*EPSILON * inter.getNormal();
    
    info->direction_to_light = position - surface_point;
    double dist_to_light = info->direction_to_light.length();
    if (IS_ZERO(dist_to_light)) {
	info->cos = 0.0;
	info->intensity = 0.0;
	return;

    }
    info->direction_to_light = info->direction_to_light / dist_to_light;
    info->cos = info->direction_to_light * inter.getNormal();

    if (info->cos > 0.0) {
	Ray ray_to_light = Ray(surface_point,info->direction_to_light,-1.0);
	bool occluded = shadowcache.occluded(ray_to_light,dist_to_light,depth,space);
	info->intensity = occluded ? 0.0 : 1.0;
    }
}

