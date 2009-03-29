#ifndef RAYTRACER_H
#define RAYTRACER_H

#include "renderer.h"
#include "space/kdtree.h"
#include "intersection.h"

class RGB;
class Ray;
class Object;
class Intersection;

///  Implementation of Renderer that supply a raytracer.
class Raytracer : public Renderer {

    public:
	/// Default constructor
	Raytracer(RendererSettings* settings, Image* img, Scene* scene, KdTree* spc, RenderJobPool* job_pool, uint32_t thread_id);

    private:
	RGBA getPixel(const Vector2& v);

	RGBA shade(const Ray&, Intersection&, const int depth);
	RGBA trace(const Ray&, const int depth);
	RGBA traceSub(const bool intersected, Intersection& i, const Ray&, const int depth);
	RGBA tracePrimary(const Ray&);
	RGB calculate_reflection(const Ray& ray, const Intersection& intersection, const int depth, const Material* material);

	static CounterStats* total_rays_cast;
	static CounterStats* primary_rays_cast;
	static CounterStats* secondary_rays_cast;
};

inline
RGBA Raytracer::tracePrimary(const Ray& ray) {
    if (ray.ignore()) {
	return RGBA(0,0,0,0);
    }
    primary_rays_cast->inc();
    Intersection i;
    bool intersected = space->intersectPrimary(ray, i);
    return traceSub(intersected, i, ray, 1);
}

inline
RGBA Raytracer::trace(const Ray& ray, const int depth) {
    secondary_rays_cast->inc();
    Intersection i;
    bool intersected = space->intersect(ray, i);
    return traceSub(intersected, i, ray, depth);
}

#endif
