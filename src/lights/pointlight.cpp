
#include "lights/pointlight.h"
#include "lights/lightinfo.h"
#include "math/matrix.h"
#include "intersection.h"
#include "spacesubdivider.h"
#include "stats.h"

Pointlight::Pointlight(const Vector& pos) : Lightsource(pos) {
    hint = NULL;
}

Lightinfo Pointlight::getLightinfo(const Intersection& inter,const Vector& normal, SpaceSubdivider* space) const {
    // TODO: Move point ESPILON along normal to avoid selfshadowing.
    Lightinfo info;
    info.direction_to_light = position - inter.getPoint();
    double dist_to_light = info.direction_to_light.length();
    info.direction_to_light.normalize();
    info.cos = info.direction_to_light * normal;

    if (info.cos > 0.0) {
	Stats::getUniqueInstance()->inc("Shadow rays cast");
	Ray ray_to_light = Ray(inter.getPoint(),info.direction_to_light,-1.0);
	bool in = space->intersectForShadow(ray_to_light,hint,dist_to_light);

	if (in) {
	    hint = space->getLastIntersection()->getObject();
	    info.intensity = 0.0;
	} else {
	    hint = NULL;
	    info.intensity = 1.0;
	}
    }
    return info;
}

