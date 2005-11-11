#ifndef PATH_TRACER_H
#define PATH_TRACER_H

#include "renderer.h"
#include "space/kdtree.h"
#include "intersection.h"
#include "math/qmcsequence.h"

class RGB;
class Ray;
class Object;
class Intersection;

#define MAX_DEPTH 7

///  A simple pathtracer
class Pathtracer : public Renderer {

    public:
	/// Default constructor
	Pathtracer(RendererSettings* settings, Image* img, Scene* scene, KdTree* spc, RenderJobPool* job_pool, uint32_t thread_id);

    private:
	RGBA getPixel(const Vector2& v);

	RGB shade(const Ray&, const Intersection&, const int depth);
	RGBA trace(const Ray&, const int depth);
	RGBA traceSub(const bool intersected, const Intersection& i, const Ray&, const int depth);
	RGBA tracePrimary(const Ray&);

	vector<QMCSequence*> seqs;
};

inline
RGBA Pathtracer::trace(const Ray& ray, const int depth) {
    Stats::getUniqueInstance()->inc(STATS_SECONDARY_RAYS_CAST);
    Intersection i;
    bool intersected = space->intersect(ray, i);
    return traceSub(intersected, i, ray, depth);
}



#endif
