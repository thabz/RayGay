
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
#include "stats.h"

#define MAX_DEPTH 5

using namespace std;

PhotonTracer::PhotonTracer(Scene* scene, SpaceSubdivider* space, PhotonMap* photonmap) {
    this->scene = scene;
    this->space = space;
    this->photonmap = photonmap;
}

void PhotonTracer::trace(int max_photons) {
    time_t beginTime = time(NULL);
    Ray ray;
    std::vector<Lightsource*> lights = scene->getLightsources();
    unsigned int photons_per_lightsource = max_photons / lights.size();
    for(unsigned int i = 0; i < lights.size(); i++) {
	Lightsource* light = lights[i];
	for(unsigned int j = 0; j < photons_per_lightsource;) {
	    if (trace(light->getRandomPhotonRay())) {
		j++;
	    }
	}
    }
    Stats::getUniqueInstance()->put("Photontracing time (seconds)",time(NULL)-beginTime);
}

bool PhotonTracer::trace(const Ray& ray) {
    Stats::getUniqueInstance()->inc("Photon rays traced");
    if (!space->intersect(ray)) {
	return false;
    }
    Intersection* intersection = space->getLastIntersection();
    const Material& material = intersection->getObject()->getMaterial();
    double ran = RANDOM(0,1);
    if (ran < material.getKd()) {
	// Reflect diffusely 
	Vector normal = intersection->getObject()->normal(*intersection);
	Vector reflected_direction = Math::perturbVector(normal,DEG2RAD(170));
	Ray new_ray = Ray(intersection->getPoint(),reflected_direction,0);
	return trace(new_ray);
    } else if (ran < material.getKd() + material.getKs()) {
	// Reflect specularly
	Vector normal = intersection->getObject()->normal(*intersection);
	Vector reflected_direction = -1 * ray.getDirection();
	reflected_direction = reflected_direction.reflect(normal);
	Ray new_ray = Ray(intersection->getPoint(),reflected_direction,0);
	return trace(new_ray);
    } else {
	// Store photon
	Vector power = Vector(100000.0,100000.0,100000.0);
	photonmap->store(power,intersection->getPoint(),ray.getDirection());
	Stats::getUniqueInstance()->inc("Photons stored");
	return true;
    }
}

