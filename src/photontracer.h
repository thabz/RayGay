
#ifndef PHOTON_TRACER
#define PHOTON_TRACER

class Scene;
class PhotonMap;
class SpaceSubdivider;
class Lightsource;
class Ray;

class PhotonTracer {

    public:
	PhotonTracer(Scene* scene, SpaceSubdivider* space, PhotonMap* photonmap);
	void trace(int max_photons);
	int trace(const Ray& ray, int bounces);
	
    private:
	Scene* scene;
	SpaceSubdivider* space;
	PhotonMap* photonmap;
};

#endif


