
#include "lights/spotlight.h"
#include "math/matrix.h"
#include "intersection.h"
#include "spacesubdivider.h"
#include "stats.h"
#include "math/functions.h"

/**
 * @param pos The position of the spotlight
 * @param look_at What the light is directed to
 * @param angle Defines the lightcone
 * @param cut_angle Angle to cut at
 */
Spotlight::Spotlight(const Vector& pos, const Vector& look_at, double angle, double cut_angle) : Lightsource(pos) {
    _dir = look_at - pos;
    _dir.normalize();
    _angle = DEG2RAD(angle);
    _cut_angle = DEG2RAD(cut_angle);
}

void Spotlight::transform(const Matrix& m) {
    Lightsource::transform(m);
    _dir = m.extractRotation() * _dir;
}

Lightinfo Spotlight::getLightinfo(const Intersection& inter, const Vector& normal, SpaceSubdivider* space) const {
    Lightinfo info;
    info.direction_to_light = this->getPosition() - inter.getPoint();
    info.direction_to_light.normalize();
    info.cos = info.direction_to_light * normal;
    if (info.cos > 0.0) {
	Stats::getUniqueInstance()->inc("Shadow rays cast");
	Ray ray_to_light = Ray(inter.getPoint(),info.direction_to_light,-1.0);
	bool in = space->intersectForShadow(ray_to_light);
	info.intensity =  in ? 0.0 : 1.0;
	// TODO: This code sucks... Where is bool in used?
	// TODO: Check that shadowing object is in front of light

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

Ray Spotlight::getRandomPhotonRay() const {
    Vector dir = Math::perturbVector(_dir,_angle);
    return Ray(this->getPosition(),dir,0);
}
