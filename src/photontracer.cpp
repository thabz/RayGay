
#include <iostream>
#include <vector>
#include <time.h>

#include "photontracer.h"
#include "photonmap.h"
#include "spacesubdivider.h"
#include "scene.h"
#include "object.h"
#include "ray.h"
#include "intersection.h"
#include "lights/lightsource.h"
#include "materials/material.h"
#include "math/functions.h"
#include "math/halton.h"
#include "stats.h"

#define MAX_BOUNCES 20

using namespace std;

PhotonTracer::PhotonTracer(Scene* scene, SpaceSubdivider* space, PhotonMap* photonmap) {
    this->scene = scene;
    this->space = space;
    this->photonmap = photonmap;
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
	Vector light_power = Vector(1.0,1.0,1.0);
	storedPhotons += trace(light->getRandomPhotonRay(),light_power,0);
    }
    Stats::getUniqueInstance()->put("Photons stored",max_photons);
    Stats::getUniqueInstance()->put("Photontracing time (seconds)",time(NULL)-beginTime);
}

int PhotonTracer::trace(const Ray& ray, Vector power, int bounces) {
    if (bounces > MAX_BOUNCES) 
	return 0;

    Stats::getUniqueInstance()->inc("Photon rays traced");
    if (!space->intersect(ray)) {
	Stats::getUniqueInstance()->inc("Photons lost in void");
	return 0;
    }
    Intersection* intersection = space->getLastIntersection();
    const Material& material = intersection->getObject()->getMaterial();
    double ran = RANDOM(0,1);
    if (ran < material.getKd()) {
	// Store photon
	photonmap->store(power,intersection->getPoint(),ray.getDirection());
	
	// Reflect diffusely 
	Vector normal = intersection->getObject()->normal(*intersection);
	//Vector reflected_direction = Math::perturbVector(normal,DEG2RAD(89),qmcsequence);
	Vector reflected_direction = Math::perturbVector(normal,DEG2RAD(89));
	Ray new_ray = Ray(intersection->getPoint(),reflected_direction,0);
	power = power.length() * material.getDiffuseColor(*intersection);
	return trace(new_ray, power, bounces + 1) + 1;
    } else if (ran < material.getKd() + material.getKs()) {
	// Reflect specularly
	Vector normal = intersection->getObject()->normal(*intersection);
	Vector reflected_direction = -1 * ray.getDirection();
	reflected_direction = reflected_direction.reflect(normal);
	// TODO: Should I reflect with materials specular color?
	Ray new_ray = Ray(intersection->getPoint(),reflected_direction,0);
	return trace(new_ray, power, bounces + 1);
    } else {
	// Store photon
	Vector power = Vector(1.0,1.0,1.0);
	photonmap->store(power,intersection->getPoint(),ray.getDirection());
	return 1;
    }
}

