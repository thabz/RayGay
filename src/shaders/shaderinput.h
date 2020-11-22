
#ifndef SHADERS_SHADERINPUT_H
#define SHADERS_SHADERINPUT_H

#include "intersection.h"
#include "lights/lightsource.h"
#include "math/vector.h"
#include "math/vector2.h"
#include "ray.h"
#include <vector>

/**
 * The input for shaders
 */
class ShaderInput {
public:
  /// Intersection point
  Vector &P;

  /// Surface normal at intersection point
  Vector &N;

  /// Incident ray direction
  Vector &I;

  /// Vantage (eye) point
  Vector &E;

  /// Surface texture coordinates
  Vector2 &uv;

  /// Light sources
  vector<Lightsource *> *lights;

  /// Ray bounces
  int depth;
};

#endif
