
#include "skylight.h"
#include "math/functions.h"
#include "intersection.h"
#include "spacesubdivider.h"

Skylight::Skylight (double radius, int num) : Lightsource(Vector(0,0,0)) {
    this->radius = radius;
    this->num = num;
    for(int i = 0; i < num; i++) {
	Vector pos = Math::perturbVector(Vector(0,1,0),DEG2RAD(89));
	positions.push_back(pos*radius);
	hints.push_back(NULL);
    }
}

Lightinfo Skylight::getLightinfo(const Intersection& inter, const Vector& normal, SpaceSubdivider* space) const {
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
	    bool in = space->intersectForShadow(ray_to_light,hints[i],dist_to_light);
	    if (in) { 
		hints[i] = space->getLastIntersection()->getObject(); 
	    } else {
		count++;
		cos_total += cos_tmp;
		hints[i] = NULL;
	    }
	}
    }
    info.cos = cos_total / double(count);
    info.intensity = double(count) / num;
    info.direction_to_light = Vector(0,1,0); // Should be undefined
    return info;
}
