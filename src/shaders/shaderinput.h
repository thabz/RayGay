
#ifndef SHADERS_SHADERINPUT_H
#define SHADERS_SHADERINPUT_H

#include <vector>
#include "math/vector.h"
#include "math/vector2.h"
#include "intersection.h"
#include "ray.h"
#include "lights/lightsource.h"

class ShaderInput {
    public:
	/// Intersection point
	Vector& P;
	/// Surface normal at intersection point
	Vector& N;
	/// Incident ray direction
	Vector& I;
	/// Ray origin
	Vector& E;
	/// Surface texture coordinates
	Vector2& uv;
	/// Light sources
	vector<Lightsource*>* lights;
	/// Ray bounces
	int depth;
};

#endif
