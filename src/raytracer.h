#ifndef RAYTRACER_H
#define RAYTRACER_H

#include "renderer.h"

class RGB;
class Ray;
class Intersection;

///  Implementation of Renderer that supply a raytracer.
class Raytracer : public Renderer {

    public:
	/// Default constructor
	Raytracer();

    private:
	RGB getPixel(double x, double y);

	RGB shade(const Ray&, const Intersection&, int depth);
	RGB trace(const Ray&, int depth);
};


#endif
