
#include "pointlight.h"
#include "math/matrix.h"
#include "image/rgb.h"
#include "intersection.h"
#include "lightinfo.h"
#include "spacesubdivider.h"

Pointlight::Pointlight(const Vector& pos) {
    position = pos;
}

void Pointlight::transform(const Matrix& m) {
    position = m * position;
}

Lightinfo Pointlight::getLightinfo(const Intersection& inter,const Vector& normal, const SpaceSubdivider& space) const {
    Lightinfo info;
    info.direction_to_light = position - inter.point;
    info.direction_to_light.normalize();
    info.cos = info.direction_to_light * normal;
    if (info.cos > 0.0) {
	Ray ray_to_light = Ray(inter.point,info.direction_to_light,-1.0);
	Intersection i2 = space.intersect(ray_to_light);
	info.intensity = i2.intersected ? 0.0 : 1.0;
    }
    return info;
}

RGB Pointlight::getDiffuseColor(const Vector& p) {
    return RGB(0.0,0.0,0.0);
};

