#ifndef PHOTON_RENDERER_H
#define PHOTON_RENDERER_H

#include "renderer.h"

class RGB;
class Ray;
class Object;
class Intersection;
class PhotonSettings;
class GlobalPhotonMap;
class CausticsMap;
class QMCSequence;
class IrradianceCache;

/**
 * Implementation of Renderer that supply a raytracer using photonmaps.
 */
class PhotonRenderer : public Renderer {

    public:
	/// Default constructor
	PhotonRenderer(RendererSettings* settings, Scene* scene, SpaceSubdivider* spc);
	// Destructor
	virtual ~PhotonRenderer();

	/// This populates the photon maps
	void init();

    private:
	/// The photonmap to use
	GlobalPhotonMap* globalphotonmap;
	CausticsMap* causticsphotonmap;
	IrradianceCache* irradiance_cache;

	QMCSequence* qmc_sequence;

	RGBA getPixel(const Vector2& v);

	RGB shade(const Ray&, const Intersection&, int depth);
	RGBA trace(const Ray&, int depth);
	RGBA traceSub(bool intersected, const Ray&, int depth);
	RGBA tracePrimary(const Ray&);
	RGB getDiffuseIrradiance(const Vector& point, const Vector& normal, const Vector& ray_dir) const;
	Vector finalGather(const Vector& point, const Vector& normal,const Vector& raydir, int gatherRays, int depth, double* hmd) const;
};


#endif
