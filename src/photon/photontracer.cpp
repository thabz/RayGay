
#include <iostream>
#include <vector>
#include <time.h>

#include "photontracer.h"
#include "causticsmap.h"
#include "globalphotonmap.h"
#include "spacesubdivider.h"
#include "scene.h"
#include "object.h"
#include "ray.h"
#include "intersection.h"
#include "lights/lightsource.h"
#include "materials/material.h"
#include "math/functions.h"
#include "math/matrix.h"
#include "math/halton.h"
#include "stats.h"

#define MAX_BOUNCES 50

using namespace std;

/**
 * Traces photons through the scene.
 *
 * This is responsible for getting photons from the lightsources,
 * tracing them through the scene and stored them in the photonmap
 */
PhotonTracer::PhotonTracer(Scene* scene, SpaceSubdivider* space, GlobalPhotonMap* globalphotonmap, CausticsMap* causticsmap) {
    this->scene = scene;
    this->space = space;
    this->causticsmap = causticsmap;
    this->globalphotonmap = globalphotonmap;
    this->qmcsequence = new Halton(2,2);
}

PhotonTracer::~PhotonTracer() {
    delete qmcsequence;
}

void PhotonTracer::trace(int max_photons) {
    time_t beginTime = time(NULL);
    Ray ray;
    const std::vector<Lightsource*>& lights = scene->getLightsources();
    int ligths_num = lights.size();
    int storedPhotons = 0;
    int i = 0;
    while (storedPhotons < max_photons) {
	i = (i + 1) % ligths_num;
	Lightsource* light = lights[i];
	Vector light_power = light->getPower();
	storedPhotons += trace(light->getRandomPhotonRay(),light_power,0);
    }
    Stats::getUniqueInstance()->put("Photons stored",max_photons);
    Stats::getUniqueInstance()->put("Photontracing time (seconds)",time(NULL)-beginTime);
}

int PhotonTracer::trace(const Ray& ray, RGB power, int bounces) {
    if (bounces > MAX_BOUNCES) 
	return 0;

    Stats::getUniqueInstance()->inc("Photon rays traced");
    if (!space->intersect(ray)) {
	Stats::getUniqueInstance()->inc("Photons lost in void");
	return 0;
    }
    Intersection* intersection = space->getLastIntersection();
    const Material& material = intersection->getObject()->getMaterial();
    Vector normal = intersection->getObject()->normal(*intersection);
    Vector point = intersection->getPoint();
    double ran = RANDOM(0,1);
    if (ran < material.getKd()) {
	// Store photon
	if (bounces > 0) 
	    globalphotonmap->store(power,point,ray.getDirection(),normal);
	// Reflect diffusely 
	Vector dir = normal.randomHemisphere();
	Ray new_ray = Ray(point + 0.1*dir,dir,-1);
	power = power;// * material.getDiffuseColor(*intersection);
	return trace(new_ray, power, bounces + 1) + 1;
    } else if (ran < material.getKd() + material.getKs()) {
	// Reflect specularly
	Vector dir = -1 * ray.getDirection();
	dir = dir.reflect(normal);
	Ray new_ray = Ray(point,dir,0);
	return trace(new_ray, power, bounces + 1);
    } else if (ran < material.getKt() + material.getKd() + material.getKs()) {
	double ior = material.getEta();
	Vector T = ray.getDirection().refract(normal,ior);
	if (!(T == Vector(0,0,0))) {
	    Ray new_ray = Ray(point+0.1*T,T,ior);
	    return trace(new_ray, power, bounces + 1);
	} else {
	    // Total internal reflection
	    Vector dir = -1 * ray.getDirection();
	    dir = dir.reflect(normal);
	    Ray new_ray = Ray(point,dir,0);
	    return trace(new_ray, power, bounces + 1);
	}
    } else {
	// Store photon
	if (bounces > 0) 
	    globalphotonmap->store(power,point,ray.getDirection(),normal);
	return 1;
    }
}

