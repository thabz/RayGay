
#include <iostream>
#include <vector>
#include <pthread.h>

#include "photontracer.h"
#include "causticsmap.h"
#include "globalphotonmap.h"
#include "space/kdtree.h"
#include "scene.h"
#include "objects/object.h"
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
PhotonTracer::PhotonTracer(Scene* scene, KdTree* space, GlobalPhotonMap* globalphotonmap, CausticsMap* causticsmap) {
    this->scene = scene;
    this->space = space;
    this->causticsmap = causticsmap;
    this->globalphotonmap = globalphotonmap;
    this->qmcsequence = new Halton(2,2);
}

PhotonTracer::~PhotonTracer() {
    delete qmcsequence;
}

void* threadDo(void* obj) {
    PhotonTracer* tracer = (PhotonTracer*) obj;
    tracer->trace();
    return NULL;
}

void PhotonTracer::trace(int threads_num) {
    Stats::getUniqueInstance()->beginTimer("Photontracing");
    pthread_t threads[threads_num];
    // Spawn threads
    for(int i=0; i < threads_num; i++) {
	pthread_create(&threads[i], NULL, threadDo, this);
    }
    // Wait for threads to finish
    for (int i=0; i<threads_num; i++) {
	pthread_join(threads[i], NULL);
    }
    Stats::getUniqueInstance()->endTimer("Photontracing");
}
    
void PhotonTracer::trace() {
    Ray ray;
    const std::vector<Lightsource*>& lights = scene->getLightsources();
    int ligths_num = lights.size();
    int i = 0;
    while ((!globalphotonmap->isFull()) || (!causticsmap->isFull())) {
	i = (i + 1) % ligths_num;
	Lightsource* light = lights[i];
	RGB light_power = light->getPower();
	trace(light->getRandomPhotonRay(),light_power,0);
    }
}

void PhotonTracer::trace(const Ray& ray, RGB power, int bounces) {
    if (bounces > MAX_BOUNCES) 
	return;

    Stats::getUniqueInstance()->inc(STATS_PHOTON_RAYS_TRACED);
    Intersection intersection;
    if (!space->intersect(ray,&intersection)) {
	Stats::getUniqueInstance()->inc(STATS_PHOTONS_LOST_IN_VOID);
	return;
    }
    const Material* material = intersection.getObject()->getMaterial();
    Vector normal = intersection.getNormal();
    Vector point = intersection.getPoint();
    double ran = RANDOM(0,1);
    if (ran < material->getKd()) {
	// Store photon
	if (bounces > 0) {
	    if (ray.isCaustic()) {
		causticsmap->store(power,point,ray.getDirection());
	    } else {
		globalphotonmap->store(power,point,ray.getDirection(),normal);
	    }
	}
	if (globalphotonmap->isFull()) {
	    return; // Quick escape when only caustics are interesting
	} else {
	    // Reflect diffusely 
	    Vector dir = normal.randomHemisphere();
	    Ray new_ray = Ray(point + 0.1*dir,dir,-1);
	    new_ray.specularBounces = ray.specularBounces;
	    new_ray.diffuseBounces = ray.diffuseBounces + 1;
	    power = power; // TODO: Modify power
	    return trace(new_ray, power, bounces + 1);
	}
    } else if (ran < material->getKd() + material->getKs()) {
	// Reflect specularly
	Vector dir = -1 * ray.getDirection();
	dir = dir.reflect(normal);
	Ray new_ray = Ray(point+0.1*dir,dir,0);
	new_ray.specularBounces = ray.specularBounces + 1;
	new_ray.diffuseBounces = ray.diffuseBounces;
	return trace(new_ray, power, bounces + 1);
    } else if (ran < material->getKt() + material->getKd() + material->getKs()) {
	double ior = material->getEta();
	Vector T = ray.getDirection().refract(normal,ior);
	if (!(T == Vector(0,0,0))) {
	    Ray new_ray = Ray(point+0.1*T,T,ior);
	    new_ray.specularBounces = ray.specularBounces + 1;
	    new_ray.diffuseBounces = ray.diffuseBounces;
	    return trace(new_ray, power, bounces + 1);
	} else {
	    // Total internal reflection
	    Vector dir = -1 * ray.getDirection();
	    dir = dir.reflect(normal);
	    Ray new_ray = Ray(point+0.1*dir,dir,0);
	    new_ray.specularBounces = ray.specularBounces + 1;
	    new_ray.diffuseBounces = ray.diffuseBounces;
	    return trace(new_ray, power, bounces + 1);
	}
    } else {
	// Store photon
	if (bounces > 0) {
	    if (ray.isCaustic()) {
		causticsmap->store(power,point,ray.getDirection());
	    } else {
		globalphotonmap->store(power,point,ray.getDirection(),normal);
	    }
	}
	return;
    }
}

