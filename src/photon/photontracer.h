
#ifndef PHOTON_TRACER
#define PHOTON_TRACER

class Scene;
class SpaceSubdivider;
class Lightsource;
class Ray;
class Vector;
class QMCSequence;
class RGB;
class CausticsMap;
class GlobalPhotonMap;

/**
 * Phase one of a photonraytracer. This will trace photons
 * from the lights, bounce them through the scene and register
 * the absorbed photons in both a global- and a caustic-photonmap.
 */
class PhotonTracer {

    public:
	PhotonTracer(Scene* scene, SpaceSubdivider* space, GlobalPhotonMap* globalphotonmap, CausticsMap* photonmap);
	~PhotonTracer();
	void trace();
	void trace(const Ray& ray, RGB power, int bounces);
	
    private:
	Scene* scene;
	SpaceSubdivider* space;
	CausticsMap* causticsmap;
	GlobalPhotonMap* globalphotonmap;
	QMCSequence* qmcsequence;
};

#endif


