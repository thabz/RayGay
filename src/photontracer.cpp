
#include <vector>

#include "photontracer.h"
#include "photonmap.h"
#include "spacesubdivider.h"
#include "scene.h"
#include "ray.h"
#include "intersection.h"
#include "lights/lightsource.h"


PhotonTracer::PhotonTracer(Scene* scene, SpaceSubdivider* space, PhotonMap* photonmap) {
    this->scene = scene;
    this->space = space;
    this->photonmap = photonmap;
}

Vector randomUnitVector() {
    Vector result;
    while (true) {
	double x = RANDOM(0,1);
	double y = RANDOM(0,1);
	double z = RANDOM(0,1);
	if (x*x + y*y + z*z < 1.0) {
	    result = Vector(x,y,z);
	    result.normalize();
	    return result;
	}
    }
}

void PhotonTracer::trace(int max_photons) {
    Ray ray;
    std::vector<Lightsource*> lights = scene->getLightsources();
    unsigned int photons_per_lightsource = lights.size() / 3;
    for(unsigned int i = 0; i < lights.size(); i++) {
	for(unsigned int j = 0; j < photons_per_lightsource; j++) {
	    ray = Ray(lights[i]->getPosition(),randomUnitVector(),0);
	    trace(ray);
	}
    }
}

void PhotonTracer::trace(const Ray& ray) {
    if (!space->intersect(ray))
	return;
    Intersection* intersection = space->getLastIntersection();
    RGB power = RGB(1.0,1.0,1.0);
    photonmap->store(power,intersection->getPoint(),ray.getDirection());
}

