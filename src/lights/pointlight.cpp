
#include "lights/pointlight.h"
#include "lights/lightinfo.h"
#include "math/matrix.h"
#include "intersection.h"
#include "spacesubdivider.h"

Pointlight::Pointlight(const Vector& pos) {
    position = pos;
    hint = NULL;
}

void Pointlight::transform(const Matrix& m) {
    position = m * position;
}

Lightinfo Pointlight::getLightinfo(const Intersection& inter,const Vector& normal, SpaceSubdivider* space) const {
    Lightinfo info;
    info.direction_to_light = position - inter.getPoint();
    info.direction_to_light.normalize();
    info.cos = info.direction_to_light * normal;
    if (info.cos > 0.0) {
	Ray ray_to_light = Ray(inter.getPoint(),info.direction_to_light,-1.0);
	bool in = space->intersectForShadow(ray_to_light,hint);
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

RGB Pointlight::getDiffuseColor(const Vector& p) {
    return RGB(0.0,0.0,0.0);
};

