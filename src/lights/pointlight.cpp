
#include "lights/pointlight.h"
#include "lights/lightinfo.h"
#include "math/matrix.h"
#include "intersection.h"
#include "space/spacesubdivider.h"
#include "objects/object.h"
#include "stats.h"

Pointlight::Pointlight(const Vector& pos) : Lightsource(pos) {
}

Lightinfo Pointlight::getLightinfo(const Intersection& inter,const Vector& normal, SpaceSubdivider* space, unsigned int depth) const {
    // TODO: Move point ESPILON along normal to avoid selfshadowing.
    Lightinfo info;
    info.direction_to_light = position - inter.getPoint();
    double dist_to_light = info.direction_to_light.length();
    info.direction_to_light = info.direction_to_light / dist_to_light;
    info.cos = info.direction_to_light * normal;

    if (info.cos > 0.0) {
	Stats::getUniqueInstance()->inc("Shadow rays cast");
	Ray ray_to_light = Ray(inter.getPoint(),info.direction_to_light,-1.0);
	// TODO: Move hint-checking away from space and into shadowcache code
	// TODO: Check other objects in hints voxel
	Object* hint = shadowcache.get(depth);

	if (hint != NULL) {
	    double t = hint->fastIntersect(ray_to_light);
	    if (t > 0 && t < dist_to_light) {
		info.intensity = 0.0;
		return info;
	    }
	} 
	bool in = space->intersectForShadow(ray_to_light,dist_to_light);
	if (in) {
	    shadowcache.put(depth,space->getLastIntersection()->getObject());
	    //shadowcache.put(depth,NULL);
	    info.intensity = 0.0;
	} else {
	    shadowcache.put(depth,NULL);
	    info.intensity = 1.0;
	}

    }
    return info;
}

