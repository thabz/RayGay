
#include "lights/skylight.h"
#include "math/functions.h"
#include "math/halton.h"
#include "intersection.h"

class KdTree;

Skylight::Skylight (double radius, int num) : Lightsource(Vector(0,0,0)) {
    this->radius = radius;
    this->num = num;
    Halton qmc = Halton(2,2);
    for(int i = 0; i < num; i++) {
	    Vector pos = Math::perturbVector(Vector(0,1,0),DEG2RAD(89),&qmc);
	    positions.push_back(pos*radius);
	    shadowcaches.push_back(ShadowCache());
    }
}

void Skylight::getLightinfo(const Intersection& inter, KdTree* space, Lightinfo* info, uint32_t depth) const {
    int count = 0;
    double cos_total = 0;
    double cos_tmp;
    for(int i = 0; i < num; i++) {
	    Vector direction_to_light = positions[i] - inter.getPoint();
	    double dist_to_light = direction_to_light.length();
	    direction_to_light = direction_to_light / dist_to_light;
	    cos_tmp = direction_to_light * inter.getNormal();
	    if (cos_tmp > 0.0) {
	        Ray ray_to_light = Ray(inter.getPoint(),direction_to_light,-1.0);
	        bool occluded = shadowcaches[i].occluded(ray_to_light,dist_to_light,depth,space);
	        if (!occluded) { 
	    	    count++;
	    	    cos_total += cos_tmp;
	        }
	    }
    }
    info->cos = cos_total / double(count);
    info->intensity = double(count) / num;
    info->direction_to_light = Vector(0,1,0); // Should be undefined
}

void Skylight::getSingleLightinfo(const Intersection& inter, KdTree* space, Lightinfo* info, uint32_t depth) const {
    int sublight = int(RANDOM(0,num));

    Vector direction_to_light;
    info->direction_to_light = positions[sublight] - inter.getPoint();
    info->direction_to_light.normalize();
    info->cos = info->direction_to_light * inter.getNormal();
    double dist_to_light = info->direction_to_light.length();

    if (info->cos > 0.0) {
        Ray ray_to_light = Ray(inter.getPoint(),direction_to_light,-1.0);
        bool occluded = shadowcaches[num].occluded(ray_to_light,dist_to_light,depth,space);
	    info->intensity = occluded ? 0 : 1;
    }
}
