
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
#include "math/matrix.h"
#include "math/halton.h"
#include "stats.h"

#define MAX_BOUNCES 20

using namespace std;

/**
 * Traces photons through the scene.
 *
 * This is responsible for getting photons from the lightsources,
 * tracing them through the scene and stored them in the photonmap
 */
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
    double ran = RANDOM(0,1);
    if (ran < material.transmission_coefficient) {
	double ior = material.indice_of_refraction;
	Vector T = ray.getDirection().refract(normal,ior);
	if (!(T == Vector(0,0,0))) {
	    Ray new_ray = Ray(intersection->getPoint()+0.1*T,T,ior);
	    return trace(new_ray, power, bounces + 1);
	}
    }
    if (ran < material.getKd()) {
	// Store photon
	if (bounces > 0) 
	    photonmap->store(power,intersection->getPoint(),ray.getDirection());

	// Reflect diffusely 
	//Vector reflected_direction = Math::perturbVector(normal,DEG2RAD(89),qmcsequence);
	//Vector reflected_direction = Math::perturbVector(normal,DEG2RAD(89));


	Matrix m = Matrix::matrixOrient(normal);
	m = m.inverse();
	double u = 2*M_PI*RANDOM(0,1);
	double v = RANDOM(0,1);
	double s = sqrt(v);
	double s1 = sqrt(1.0-v);
	Vector reflected_direction = Vector(cos(u)*s,sin(u)*s,s1);
	reflected_direction = m*reflected_direction;
	reflected_direction.normalize();
        
	Ray new_ray = Ray(intersection->getPoint() + 0.1*reflected_direction,reflected_direction,-1);
	power = power;// * material.getDiffuseColor(*intersection);
	return trace(new_ray, power, bounces + 1) + 1;
    } else if (ran < material.getKd() + material.getKs()) {
	// Reflect specularly
	Vector reflected_direction = -1 * ray.getDirection();
	reflected_direction = reflected_direction.reflect(normal);
	// TODO: Should I reflect with materials specular color?
	Ray new_ray = Ray(intersection->getPoint(),reflected_direction,0);
	return trace(new_ray, power, bounces + 1);
    } else {
	// Store photon
	if (bounces > 0) 
	    photonmap->store(power,intersection->getPoint(),ray.getDirection());
	return 1;
    }
}

