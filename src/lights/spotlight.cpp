
#include <math.h>
#include "spotlight.h"
#include "math/matrix.h"
#include "rgb.h"
#include <cassert>
#include <iostream>
#include "intersection.h"
#include "lightinfo.h"
#include "scene.h"
#include "spacesubdivider.h"

Spotlight::Spotlight(const Vector& pos, const Vector& dir, double angle, double cut_angle) {
    _pos = pos;
    _dir = dir;
    _dir.normalize();
    _angle = angle;
    _cut_angle = cut_angle;
}

void Spotlight::transform(const Matrix& m) {
    _pos = m * _pos;
    _dir = m.extractRotation() * _dir;
}

Lightinfo Spotlight::getLightinfo(const Intersection& inter, const Vector& normal, const SpaceSubdivider& space) const {
    Lightinfo info;
    info.direction_to_light = _pos - inter.point;
    info.direction_to_light.normalize();
    info.cos = info.direction_to_light * normal;
    if (info.cos > 0.0) {
	Ray ray_to_light = Ray(inter.point,info.direction_to_light,-1.0);
	Intersection i2 = space.intersect(ray_to_light);
	info.intensity = i2.intersected ? 0.0 : 1.0;
	
	// Angle between light-direction and direction from light to incident
	double b = (double(-1) * info.direction_to_light) * _dir;
	if (b >= 1.0 ) b = 1.0; // This fixes a rounding error in math.h
	double a = acos(b);  
	//double intensity;
	if (a <= _cut_angle) {
	    info.intensity = 1.0;
	} else if (a <= _angle) {
	    info.intensity = 1.0 - (a - _cut_angle) / (_angle - _cut_angle);
	} else {
	    info.intensity = 0.0;
	}
    }
    return info;
}

