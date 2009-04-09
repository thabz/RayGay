
#ifdef P_USE_PRAGMA
#pragma implementation "spotlight.h"
#endif

#include "lights/spotlight.h"
#include "math/matrix.h"
#include "intersection.h"
#include "space/kdtree.h"
#include "math/functions.h"
#include "math/halton.h"

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
    _qmc = new Halton(2,2);
}

Spotlight::~Spotlight() {
    delete _qmc;
}

void Spotlight::transform(const Matrix& m) {
    Lightsource::transform(m);
    _dir = m.extractRotation() * _dir;
}

void Spotlight::getLightinfo(const Intersection& inter, KdTree* space, Lightinfo* info, uint32_t depth) const {
    Vector surface_point = inter.getPoint() + 1000*EPSILON * inter.getOriginalNormal();
    info->direction_to_light = this->getPosition() - surface_point;
    double dist_to_light = info->direction_to_light.length();
    info->direction_to_light.normalize();
    info->cos = info->direction_to_light * inter.getNormal();
    if (info->cos > 0.0) {
	Ray ray_to_light = Ray(surface_point,info->direction_to_light,-1.0);
	bool in = space->intersectForShadow(ray_to_light,dist_to_light);
	info->intensity =  in ? 0.0 : 1.0;

	if (!in) {
	    // Angle between light-direction and direction from light to incident
	    double b = (double(-1) * info->direction_to_light) * _dir;
	    if (b >= 1.0 ) b = 1.0; // This fixes a rounding error in math.h
	    double a = acos(b);  
	    //double intensity;
	    if (a <= _cut_angle) {
		info->intensity = 1.0;
	    } else if (a <= _angle) {
		info->intensity = 1.0 - (a - _cut_angle) / (_angle - _cut_angle);
	    } else {
		info->intensity = 0.0;
	    }
	}
    }
}

Ray Spotlight::getRandomPhotonRay() const {
    Vector dir = Math::perturbVector(_dir,_angle,_qmc);
    return Ray(this->getPosition(),dir,0);
}
