#ifndef PATH_TRACER_H
#define PATH_TRACER_H

#include "renderer.h"
#include "space/kdtree.h"
#include "intersection.h"

class RGB;
class Ray;
class Object;
class Intersection;
class QMCSequence;

///  A simple pathtracer
class Pathtracer : public Renderer {

    public:
	/// Default constructor
	Pathtracer(RendererSettings* settings, Image* img, Scene* scene, KdTree* spc, RenderJobPool* job_pool, unsigned int thread_id);

    private:
	RGBA getPixel(const Vector2& v);

	RGB shade(const Ray&, const Intersection&, const int depth);
	RGBA trace(const Ray&, const int depth);
	RGBA traceSub(const bool intersected, const Intersection& i, const Ray&, const int depth);
	RGBA tracePrimary(const Ray&);

	QMCSequence* gloss_sequence;
};

inline
RGBA Pathtracer::tracePrimary(const Ray& ray) {
    Stats::getUniqueInstance()->inc(STATS_PRIMARY_RAYS_CAST);
    Intersection i;
    bool intersected = space->intersectPrimary(ray,&i);
    if (intersected) {
	int samples = renderersettings->camera_paths;
	RGBA result = RGBA(0.0,0.0,0.0,0.0);
	for(int j = 0; j < samples; j++) {
	    result = result + traceSub(intersected, i, ray, 1);
	}
	return result / double(samples) ;
    } else {
	return traceSub(intersected, i, ray, 1);
    }
}

inline
RGBA Pathtracer::trace(const Ray& ray, const int depth) {
    Stats::getUniqueInstance()->inc(STATS_SECONDARY_RAYS_CAST);
    Intersection i;
    bool intersected = space->intersect(ray,&i);
    return traceSub(intersected, i, ray, depth);
}

#endif
