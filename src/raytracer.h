#ifndef RAYTRACER_H
#define RAYTRACER_H

#include "renderer.h"
#include "space/spacesubdivider.h"
#include "intersection.h"

class RGB;
class Ray;
class Object;
class Intersection;

///  Implementation of Renderer that supply a raytracer.
class Raytracer : public Renderer {

    public:
	/// Default constructor
	Raytracer(RendererSettings* settings, Scene* scene, SpaceSubdivider* spc);

	void init() {};

    private:
	RGBA getPixel(const Vector2& v);

	RGB shade(const Ray&, const Intersection&, const int depth);
	RGBA trace(const Ray&, const int depth);
	RGBA traceSub(const bool intersected, const Intersection& i, const Ray&, const int depth);
	RGBA tracePrimary(const Ray&);
};

inline
RGBA Raytracer::tracePrimary(const Ray& ray) {
   // Stats::getUniqueInstance()->inc("Primary camera rays cast");
    Intersection i;
    bool intersected = space->intersectPrimary(ray,&i);
    return traceSub(intersected, i, ray, 1);
}

inline
RGBA Raytracer::trace(const Ray& ray, const int depth) {
    // Discard reflected ray if the amount of colour it
    // contributes to the pixel is too small.
    //if (weight < 0.004) // 1 / 256 = 0.004
    //	return RGBA(0,0,0,0.0);

    //Stats::getUniqueInstance()->inc("Secondary camera rays cast");
    Intersection i;
    bool intersected = space->intersect(ray,&i);
    return traceSub(intersected, i, ray, depth);
}

#endif
