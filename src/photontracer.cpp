
#include <vector>

#include "photontracer.h"
#include "photonmap.h"
#include "spacesubdivider.h"
#include "scene.h"
#include "lights/lightsource.h"


PhotonTracer::PhotonTracer(Scene* scene, SpaceSubdivider* space, PhotonMap* photonmap) {
    this->scene = scene;
    this->space = space;
    this->photonmap = photonmap;
}

void PhotonTracer::trace(int max_photons) {
    std::vector<Lightsource*> lights = scene->getLightsources();
    unsigned int photons_per_lightsource = lights.size() / 3;
    for(unsigned int i = 0; i < lights.size(); i++) {
	castrays(lights[i], photons_per_lightsource);
    }
}

void PhotonTracer::castrays(Lightsource* lightsource, unsigned int max_photons) {

}


