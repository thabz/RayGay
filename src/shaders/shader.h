
#ifndef SHADERS_SHADER_H
#define SHADERS_SHADER_H

#include "image/rgb.h"
#include "materials/material.h"
#include "shaders/shaderinput.h"

/**
 * A shader does the BRDF calculations.
 */
class Shader {
public:
  Shader(Material *material);
  virtual ~Shader(){};
  virtual RGB shade(const ShaderInput &shaderInput) = 0;

protected:
  Material *material;
  Vector bump(const ShaderInput &shaderInput);

  // Helper methods
  RGB trace(const Vector &P, const Vector &D);
  double gather(const Vector &P, const Vector &N);
  Vector refract(const Vector &I, const Vector &N, double ior);
  Vector reflect(const Vector &I, const Vector &N);
  Vector normalize(const Vector &v);
  Vector faceforward(const Vector &N, const Vector &I);
  double noise(const Vector &v);
  RGB mix(const RGB &a, const RGB &b, double m);
  double smoothstep(double maxi, double mini, double t);
  //	RGB spline(double t, const RGB& a, const RGB& b, ...);
};

#endif
