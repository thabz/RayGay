
#ifndef PHOTON_TRACER
#define PHOTON_TRACER

class Scene;
class PhotonMap;
class SpaceSubdivider;
class Lightsource;

class PhotonTracer {

    public:
	PhotonTracer(Scene* scene, SpaceSubdivider* space, PhotonMap* photonmap);
	void trace(int max_photons);
	
    private:
	void castrays(Lightsource* lightsource, unsigned int max_photons);

	Scene* scene;
	SpaceSubdivider* space;
	PhotonMap* photonmap;
};

#endif


