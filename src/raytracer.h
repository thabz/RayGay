#ifndef RAYTRACER_H
#define RAYTRACER_H

#include "renderer.h"

class RGB;
class Ray;
class Intersection;

class Raytracer : public Renderer {

    public:
	Raytracer();

    private:
	RGB getPixel(double x, double y);

	RGB shade(const Ray&, Intersection&, int depth);
	RGB trace(const Ray&, int depth);
};


#endif
