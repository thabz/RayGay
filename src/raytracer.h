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
	Raytracer();

    private:
	RGB getPixel(const Vector2& v);

	RGB shade(const Ray&, const Intersection&, int depth);
	RGB trace(const Ray&, int depth);
	RGB traceSub(bool intersected, const Ray&, int depth);
	RGB tracePrimary(const Ray&);
};


#endif
