#ifndef RAYTRACER_H
#define RAYTRACER_H

#include "renderer.h"

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

	RGB shade(const Ray&, const Intersection&, int depth);
	RGBA trace(const Ray&, int depth);
	RGBA traceSub(bool intersected, const Ray&, int depth);
	RGBA tracePrimary(const Ray&);
};


#endif
