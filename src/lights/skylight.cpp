
#include "lights/skylight.h"
#include "math/functions.h"
#include "intersection.h"
#include "space/spacesubdivider.h"

Skylight::Skylight (double radius, int num) : Lightsource(Vector(0,0,0)) {
    this->radius = radius;
    this->num = num;
    for(int i = 0; i < num; i++) {
	Vector pos = Math::perturbVector(Vector(0,1,0),DEG2RAD(89));
	positions.push_back(pos*radius);
	shadowcaches.push_back(ShadowCache());
    }
}

Lightinfo Skylight::getLightinfo(const Intersection& inter, const Vector& normal, SpaceSubdivider* space, unsigned int depth) const {
    Lightinfo info;
    int count = 0;
    double cos_total = 0;
    double cos_tmp;
    for(int i = 0; i < num; i++) {
	Vector pos = positions[i];
	Vector direction_to_light = pos - inter.getPoint();
	double dist_to_light = direction_to_light.length();
	direction_to_light.normalize();
	cos_tmp = direction_to_light * normal;
	if (cos_tmp > 0.0) {
	    Ray ray_to_light = Ray(inter.getPoint(),direction_to_light,-1.0);
	    bool occluded = shadowcaches[i].occluded(ray_to_light,dist_to_light,depth,space);
	    if (!occluded) { 
		count++;
		cos_total += cos_tmp;
	    }
	}
    }
    info.cos = cos_total / double(count);
    info.intensity = double(count) / num;
    info.direction_to_light = Vector(0,1,0); // Should be undefined
    return info;
}
